{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE ScopedTypeVariables #-}

module RT where

import           Prelude

import           GHC.Conc                       ( unsafeIOToSTM )

import           GHC.Stats
import           System.IO

import           Control.Monad
import           Control.Monad.Reader
import           Control.Concurrent
import           Control.Concurrent.STM
import           Data.Time.Clock.System
import           Data.Int
import           Data.IORef
import           Data.Unique
import           Data.Dynamic

import           Types
import           Parser


evalExpr :: Expr -> ProgState -> (AttrVal -> STM ()) -> STM ()
evalExpr !expr !pgs !exit = case expr of
  Paren !exprInTx ->
    evalExpr exprInTx pgs { pgs'in'tx = True } $ \ !val -> exitProc pgs exit val
  FnApp !lhx !rhx -> evalExpr lhx pgs $ \case
    RefValue (Object _ !osv) -> do
      !os <- readTVar osv
      case fromDynamic os of
        -- a callable host object, make the strict function application
        Just (hostProc :: HostProc) ->
          hostProc rhx pgs $ \ !val -> exitProc pgs exit val
        Nothing -> case fromDynamic os of
          Just (hostIO :: HostIO) ->
            scheduleIO pgs (hostIO rhx pgs exit) $ \() -> pure ()
          -- the tricky treat, make it as if a sequence, when left-hand is not a
          -- callable host object
          Nothing -> evalExpr rhx pgs exit
    -- ditto, left-hand is even not an object
    _ -> evalExpr rhx pgs exit
  BinOp !opSym !lhx !rhx -> infixOp pgs opSym lhx rhx exit
  LitObj                 -> newObj $ \ !obj -> exitProc pgs exit $ RefValue obj
  LitInt !val            -> exitProc pgs exit $ IntValue val
  LitStr !val            -> exitProc pgs exit $ StrValue val
  LitNil                 -> exitProc pgs exit NilValue
  Attr !attr ->
    objGetAttr (pgs'scope pgs) attr $ \ !val -> exitProc pgs exit val


runTXS :: Object -> Expr -> IO AttrVal
runTXS !global !ast = do
  !tq <- newTQueueIO
  let !pgs =
        ProgState { pgs'in'tx = False, pgs'scope = global, pgs'task'queue = tq }
  !progExit <- newTVarIO NilValue
  atomically $ writeTQueue tq $ Right $ evalExpr ast pgs $ writeTVar progExit
  driveProg tq
  readTVarIO progExit

driveProg :: TQueue (Either (IO ()) (STM ())) -> IO ()
driveProg !tq = atomically (tryReadTQueue tq) >>= \case
  Nothing          -> return ()
  Just (Left !job) -> do
    job
    driveProg tq
  Just (Right !job) -> do
    atomically job
    driveProg tq

data ProgState = ProgState {
    pgs'in'tx :: !Bool
  , pgs'scope :: !Object
  , pgs'task'queue :: !(TQueue (Either (IO ()) (STM())))
  }

type HostProc = Expr -> ProgState -> (AttrVal -> STM ()) -> STM ()
type HostIO = Expr -> ProgState -> (AttrVal -> STM ()) -> IO ()

exitProc :: ProgState -> (AttrVal -> STM ()) -> AttrVal -> STM ()
exitProc !pgs !exit !val = if pgs'in'tx pgs
  then exit val
  else writeTQueue (pgs'task'queue pgs) $ Right $! exit val

-- | To trigger IO action from a 'HostProc', note the 'IO' action will only be
-- performed after current transaction is committed. 
--
-- CAVEAT: This can break the transaction boundary intuited at scripting level,
--         if the stem exit of a 'HostProc' is only passed to 'scheduleIO' as
--         the 'exitNextTx'.
scheduleIO :: forall a . ProgState -> IO a -> (a -> STM ()) -> STM ()
scheduleIO !pgs !act !exitNextTx = writeTQueue tq $ Left $ do
  !result <- act
  atomically $ writeTQueue tq $ Right $! exitNextTx result
  where !tq = pgs'task'queue pgs


infixOp :: ProgState -> String -> Expr -> Expr -> (AttrVal -> STM ()) -> STM ()
infixOp !pgs !sym !lhx !rhx !exit = builtinOp sym
 where
  builtinOp "+" = evalExpr lhx pgs $ \case
    IntValue !lhi -> evalExpr rhx pgs $ \case
      IntValue !rhi -> exitProc pgs exit $ IntValue $ lhi + rhi
      _             -> error "right-hand to (+) not a number"
    _ -> error "left-hand to (+) not a number"
  builtinOp "-" = evalExpr lhx pgs $ \case
    IntValue !lhi -> evalExpr rhx pgs $ \case
      IntValue !rhi -> exitProc pgs exit $ IntValue $ lhi - rhi
      _             -> error "right-hand to (-) not a number"
    _ -> error "left-hand to (-) not a number"
  builtinOp "*" = evalExpr lhx pgs $ \case
    IntValue !lhi -> evalExpr rhx pgs $ \case
      IntValue !rhi -> exitProc pgs exit $ IntValue $ lhi * rhi
      _             -> error "right-hand to (*) not a number"
    _ -> error "left-hand to (*) not a number"
  builtinOp "/" = evalExpr lhx pgs $ \case
    IntValue !lhi -> evalExpr rhx pgs $ \case
      IntValue !rhi -> exitProc pgs exit $ IntValue $ div lhi rhi
      _             -> error "right-hand to (/) not a number"
    _ -> error "left-hand to (/) not a number"
  builtinOp "++" = evalExpr lhx pgs $ \ !lhv -> evalExpr rhx pgs
    $ \ !rhv -> exitProc pgs exit $ StrValue $ toString lhv ++ toString rhv
  builtinOp "." = case rhx of
    Attr !attr -> evalExpr lhx pgs $ \case
      RefValue !tgtObj -> objGetAttr tgtObj attr $ exitProc pgs exit
      _                -> error "left-hand of dot-notation not an object"
    _ -> error "right-hand of dot-notation not an attribute addressor"
  builtinOp "@" = evalExpr rhx pgs $ \case
    StrValue !attr -> evalExpr lhx pgs $ \case
      RefValue !tgtObj -> objGetAttr tgtObj attr $ exitProc pgs exit
      _                -> error "left-hand of at-notation not an object"
    _ -> error "right-hand of at-notation not a string"
  builtinOp "=" = evalExpr rhx pgs $ \ !rhv -> case lhx of
    Attr !attr -> objSetAttr (pgs'scope pgs) attr rhv $ exitProc pgs exit
    BinOp "." !tgtExpr (Attr !tgtAttr) -> evalExpr tgtExpr pgs $ \case
      RefValue !tgtObj -> objSetAttr tgtObj tgtAttr rhv $ exitProc pgs exit
      _                -> error "left-hand of dot-notation not an object"
    BinOp "@" !tgtExpr !tgtAddr -> evalExpr tgtAddr pgs $ \case
      StrValue !tgtAttr -> evalExpr tgtExpr pgs $ \case
        RefValue !tgtObj -> objSetAttr tgtObj tgtAttr rhv $ exitProc pgs exit
        _                -> error "left-hand of at-notation not an object"
      _ -> error "right-hand of at-notation not a string"
    _ -> error $ "unexpected left-hand of assignment: " ++ show lhx
  builtinOp !sym = error $ "bug: unexpected operator: " ++ sym


defaultGlobals :: IO Object
defaultGlobals = do
  !globalsVar <- newTVarIO undefined
  atomically $ newObj $ \ !globals ->
    seqcontSTM
        [ \ !exit ->
            newObj' hp $ \ !hpo -> objSetAttr globals nm (RefValue hpo) exit
        | (nm, hp) <-
          [ ("assert"     , assertHP)
          , ("print"      , printHP)
          , ("concur"     , concurHP)
          , ("repeat"     , repeatHP)
          , ("metricOneTx", metricOneTxHP)
          ]
        ]
      $ const
      $ writeTVar globalsVar globals
  readTVarIO globalsVar
 where
  assertHP :: HostProc -- manual currying implemented here
  assertHP !argExpr !pgs !exit = evalExpr argExpr pgs $ \ !arg -> do
    let assert1HP !argExpr1 !pgs1 !exit1 = evalExpr argExpr1 pgs $ \ !arg1 ->
          if arg1 == arg
            then exitProc pgs1 exit1 $ StrValue " * assertion passed *"
            else error "* assertion failed *"
    newObj' assert1HP $ exitProc pgs exit . RefValue
  printHP :: HostProc
  printHP !argExpr !pgs !exit = evalExpr argExpr pgs $ \case
    NilValue -> exitProc pgs exit NilValue
    !arg     -> do
      scheduleIO pgs (putStrLn $ toString arg) $ const $ return ()
      exitProc pgs exit NilValue
  concurHP :: HostProc
  concurHP !argExpr !pgs !exit = undefined
  repeatHP :: HostProc
  repeatHP !argExpr !pgs !exit = undefined
  metricOneTxHP :: HostProc
  metricOneTxHP !argExpr !pgs !exit = undefined


seqcontSTM :: forall a . [(a -> STM ()) -> STM ()] -> ([a] -> STM ()) -> STM ()
seqcontSTM !xs !exit = go xs []
 where
  go :: [(a -> STM ()) -> STM ()] -> [a] -> STM ()
  go []         ys = exit $! reverse $! ys
  go (x : rest) ys = x $ \y -> go rest (y : ys)

