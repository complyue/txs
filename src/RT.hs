{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE ScopedTypeVariables #-}

module RT where

import           Prelude

import           GHC.Conc                       ( unsafeIOToSTM )

import           Control.Monad
import           Control.Concurrent
import           Control.Concurrent.STM
import           Control.Concurrent.STM.TSem
import           Data.IORef
import           Data.Int
import           Data.Dynamic

import           Types
import           Utils
import           Diag


evalExpr :: Expr -> ThreadState -> (AttrVal -> STM ()) -> STM ()
evalExpr !expr !ths !exit = case expr of
  Paren !exprInTx ->
    evalExpr exprInTx ths { ths'in'tx = True } $ \ !val -> exitProc ths exit val
  FnApp !lhx !rhx -> evalExpr lhx ths $ \case
    RefValue (Object _ !osv) -> do
      !os <- readTVar osv
      case fromDynamic os of
        -- a callable host object, make the strict function application
        Just (hostProc :: HostProc) ->
          hostProc rhx ths $ \ !val -> exitProc ths exit val
        Nothing -> error $ "an object but not callable by expr: " <> show lhx
    _ -> error $ "not an object (to be called) by expr: " <> show lhx
  BinOp !opSym !lhx !rhx -> infixOp ths opSym lhx rhx exit
  LitObj                 -> newObj $ \ !obj -> exitProc ths exit $ RefValue obj
  LitInt !val            -> exitProc ths exit $ IntValue val
  LitStr !val            -> exitProc ths exit $ StrValue val
  LitNil                 -> exitProc ths exit NilValue
  Attr !attr ->
    objGetAttr (ths'scope ths) attr $ \ !val -> exitProc ths exit val
  Brace !exprInBlock -> evalExpr exprInBlock ths exit


runTXS :: Object -> Expr -> IO AttrVal
runTXS !global !ast = do
  !tq <- newTQueueIO
  let !ths = ThreadState { ths'in'tx      = False
                         , ths'scope      = global
                         , ths'task'queue = tq
                         }
  !progExit <- newTVarIO NilValue
  atomically $ writeTQueue tq $ Right $ evalExpr ast ths $ writeTVar progExit
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

data ThreadState = ThreadState {
    ths'in'tx :: !Bool
  , ths'scope :: !Object
  , ths'task'queue :: !(TQueue (Either (IO ()) (STM())))
  }

type HostProc = Expr -> ThreadState -> (AttrVal -> STM ()) -> STM ()

-- | Helper for proper CPS exit from within a 'HostProc'
--
-- Should join the continuation if the script code has requested a transaction
-- block (by parenthesis quoting a sequence of expressions), otherwise should
-- enqueue the continuation for it to be executed in a separate, subsequent STM
-- transaction.
exitProc :: ThreadState -> (AttrVal -> STM ()) -> AttrVal -> STM ()
exitProc !ths !exit !val = if ths'in'tx ths
  then exit val
  else writeTQueue (ths'task'queue ths) $ Right $! exit val

-- | Schedule an IO action to initiate the subsequent STM transaction, it'll be
-- performed after current STM transaction is committed.
--
-- CAVEAT: This can break the transaction boundary intuited at scripting level,
--         if the stem exit of a 'HostProc' is only passed to 'txContIO' as
--         the 'initNextTx', "ths'in'tx" state should be checked being False
--         for doing that safely.
txContIO :: forall a . ThreadState -> IO a -> (a -> STM ()) -> STM ()
txContIO !ths !act !initNextTx = writeTQueue tq $ Left $ do
  !result <- act
  atomically $ writeTQueue tq $ Right $! initNextTx result
  where !tq = ths'task'queue ths


infixOp
  :: ThreadState -> String -> Expr -> Expr -> (AttrVal -> STM ()) -> STM ()
infixOp !ths !sym !lhx !rhx !exit = builtinOp sym
 where
  builtinOp ";" = evalExpr lhx ths $ const $ evalExpr rhx ths exit
  builtinOp "+" = evalExpr lhx ths $ \case
    IntValue !lhi -> evalExpr rhx ths $ \case
      IntValue !rhi -> exitProc ths exit $ IntValue $ lhi + rhi
      _             -> error "right-hand to (+) not a number"
    _ -> error "left-hand to (+) not a number"
  builtinOp "-" = evalExpr lhx ths $ \case
    IntValue !lhi -> evalExpr rhx ths $ \case
      IntValue !rhi -> exitProc ths exit $ IntValue $ lhi - rhi
      _             -> error "right-hand to (-) not a number"
    _ -> error "left-hand to (-) not a number"
  builtinOp "*" = evalExpr lhx ths $ \case
    IntValue !lhi -> evalExpr rhx ths $ \case
      IntValue !rhi -> exitProc ths exit $ IntValue $ lhi * rhi
      _             -> error "right-hand to (*) not a number"
    _ -> error "left-hand to (*) not a number"
  builtinOp "/" = evalExpr lhx ths $ \case
    IntValue !lhi -> evalExpr rhx ths $ \case
      IntValue !rhi -> exitProc ths exit $ IntValue $ div lhi rhi
      _             -> error "right-hand to (/) not a number"
    _ -> error "left-hand to (/) not a number"
  builtinOp "%" = evalExpr lhx ths $ \case
    IntValue !lhi -> evalExpr rhx ths $ \case
      IntValue !rhi -> exitProc ths exit $ IntValue $ mod lhi rhi
      _             -> error "right-hand to (%) not a number"
    _ -> error "left-hand to (%) not a number"
  builtinOp "++" = evalExpr lhx ths $ \ !lhv -> evalExpr rhx ths
    $ \ !rhv -> exitProc ths exit $ StrValue $ toString lhv ++ toString rhv
  builtinOp "." = case rhx of
    Attr !attr -> evalExpr lhx ths $ \case
      RefValue !tgtObj -> objGetAttr tgtObj attr $ exitProc ths exit
      !badTgtVal ->
        error
          $  "left-hand of dot-notation not an object but: "
          <> show badTgtVal
          <> ", by expr: "
          <> show lhx
    _ -> error "right-hand of dot-notation not an attribute addressor"
  builtinOp "@" = evalExpr rhx ths $ \ !addrVal ->
    let !tgtAttr = toString addrVal
    in  evalExpr lhx ths $ \case
          RefValue !tgtObj -> objGetAttr tgtObj tgtAttr $ exitProc ths exit
          !badTgtVal ->
            error
              $  "left-hand of at-notation not an object but: "
              <> show badTgtVal
  builtinOp "=" = evalExpr rhx ths $ \ !rhv -> case lhx of
    Attr !attr -> objSetAttr (ths'scope ths) attr rhv $ exitProc ths exit
    BinOp "." !tgtExpr (Attr !tgtAttr) -> evalExpr tgtExpr ths $ \case
      RefValue !tgtObj -> objSetAttr tgtObj tgtAttr rhv $ exitProc ths exit
      !badTgtVal ->
        error
          $  "left-hand of dot-notation not an object but: "
          <> show badTgtVal
          <> ", by expr: "
          <> show lhx
    BinOp "@" !tgtExpr !tgtAddr -> evalExpr tgtAddr ths $ \ !addrVal ->
      let !tgtAttr = toString addrVal
      in  evalExpr tgtExpr ths $ \case
            RefValue !tgtObj ->
              objSetAttr tgtObj tgtAttr rhv $ exitProc ths exit
            !badTgtVal ->
              error
                $  "left-hand of at-notation not an object but: "
                <> show badTgtVal
    _ -> error $ "unexpected left-hand of assignment: " ++ show lhx
  builtinOp _ = error $ "bug: unexpected operator: " ++ sym


defaultGlobals :: IO Object
defaultGlobals = do

  !guidCntr   <- newIORef (1 :: Int64)

  !globalsVar <- newTVarIO undefined
  atomically $ newObj $ \ !globals ->
    seqcontSTM
        [ \ !exit ->
            newObj' hp $ \ !hpo -> objSetAttr globals nm (RefValue hpo) exit
        | (nm, hp) <-
          [ ("assert" , assertHP)
          , ("print"  , printHP)
          , ("sleep"  , sleepHP)
          , ("guid"   , guidHP guidCntr)
          , ("concur" , concurHP)
          , ("repeat" , repeatHP)
          , ("diagKit", diagKitCtor)
          ]
        ]
      $ const
      $ writeTVar globalsVar globals
  readTVarIO globalsVar

 where

  assertHP :: HostProc -- manual currying implemented here
  assertHP !msgExpr !ths !exit = evalExpr msgExpr ths $ \ !assertMsg ->
    let assert1HP !expectExpr !ths1 !exit1 =
            evalExpr expectExpr ths1 $ \ !expectVal ->
              let assert2HP !targetExpr !ths2 !exit2 =
                      evalExpr targetExpr ths2 $ \ !targetVal ->
                        if targetVal == expectVal
                          then
                            exitProc ths2 exit2
                            $  StrValue
                            $  " * assertion passed: "
                            <> toString assertMsg
                          else error $ "* assertion failed: " <> toString assertMsg
              in  newObj' assert2HP $ exitProc ths exit1 . RefValue
    in  newObj' assert1HP $ exitProc ths exit . RefValue

  printHP :: HostProc
  printHP !argExpr !ths !exit = evalExpr argExpr ths $ \case
    NilValue -> exitProc ths exit NilValue
    !arg     -> do
      txContIO ths (putStrLn $ toString arg) $ const $ return ()
      exitProc ths exit NilValue

  sleepHP :: HostProc
  sleepHP !usExpr !ths !exit = if ths'in'tx ths
    then error "you don't issue `sleep` from within a transaction"
    else evalExpr usExpr ths $ \case
      IntValue !us ->
        txContIO ths (threadDelay $ fromIntegral us) $ const $ exitProc
          ths
          exit
          NilValue
      !badDelay -> error $ "bad microsend value to sleep: " <> show badDelay

  guidHP :: IORef Int64 -> HostProc
  guidHP !guidCntr _ !ths !exit = if ths'in'tx ths
    then error "you don't issue `guid` from within a transaction"
    else txContIO ths (atomicModifyIORef' guidCntr $ \ !c -> (c + 1, c))
      $ \ !guid -> exitProc ths exit $ IntValue $ fromIntegral guid

  concurHP :: HostProc
  concurHP !nExpr !ths !exit = if ths'in'tx ths
    then error "you don't issue `concur` from within a transaction"
    else evalExpr nExpr ths $ \case
      IntValue !concN | concN > 0 ->
        let concur1HP !concExpr !ths1 !exit1 = do
              !concDoneSem <- newTSem $ fromIntegral $ 1 - concN
              let !initScope = ths'scope ths
                  reportThreadError !anywayAct = \case
                    Left !err -> do
                      !thId <- myThreadId
                      putStrLn $ " * " <> show thId <> " - " <> show err
                      anywayAct
                    Right _ -> anywayAct
                  forkConcur !cntr = if cntr < 1
                    then return ()
                    else
                      (>> forkConcur (cntr - 1))
                      $ flip
                          forkFinally
                          (reportThreadError $ atomically $ signalTSem
                            concDoneSem
                          )
                      $ do
                          !thScopeVar <- newTVarIO undefined
                          atomically $ objClone initScope $ \ !thScope ->
                            objSetAttr thScope "concur'id" (IntValue cntr)
                              $ const
                              $ writeTVar thScopeVar thScope
                          !thScope <- readTVarIO thScopeVar
                          void $ runTXS thScope concExpr
              -- it's checked we are not in a tx, safe for the exit to be
              -- continued onward from next tx
              txContIO ths (forkConcur concN) $ const $ do
                waitTSem concDoneSem
                exitProc ths1 exit1 NilValue
        in  newObj' concur1HP $ exitProc ths exit . RefValue
      !badConcN -> error $ "invalid concurrency number: " <> show badConcN

  repeatHP :: HostProc
  repeatHP !argExpr !ths !exit = evalExpr argExpr ths $ \case
    IntValue !cnt | cnt >= 0 ->
      let repeat1 !repeateeExpr !ths1 !exit1 = doRepeat cnt             where
              doRepeat !cntLeft = if cntLeft < 1
                then exitProc ths1 exit1 NilValue
                else evalExpr repeateeExpr ths1 $ const $ doRepeat (cntLeft - 1)
      in  newObj' repeat1 $ exitProc ths exit . RefValue
    !badCnt ->
      error $ "`repeat` expects a positive number, but given: " <> show badCnt


  diagKitCtor :: HostProc
  diagKitCtor !_argExpr !ths !exit = newObj $ \ !kit -> do
    !rtdsVar <- newTVar []
    let
      threadLocalDiagHP !batchSecondsExpr !ths' !exit' =
        evalExpr batchSecondsExpr ths' $ \case
          IntValue !bat'secs | bat'secs > 0 -> do
            !rtd <- unsafeIOToSTM $ startRuntimeDiagnostic $ fromIntegral
              bat'secs
            modifyTVar' rtdsVar (rtd :)
            newObj $ \ !tld ->
              seqcontSTM
                  [ \ !exit'' -> newObj' hp
                      $ \ !hpo -> objSetAttr tld nm (RefValue hpo) exit''
                  | (nm, hp) <-
                    [ ("metricOneTx", metricOneTxHP rtd)
                    , ("doneDiag"   , doneDiagHP rtd)
                    ]
                  ]
                $ const
                $ exitProc ths' exit'
                $ RefValue tld
          !badBatchSecs ->
            error $ "bad value for batchSeconds: " <> show badBatchSecs
    seqcontSTM
        [ \ !exit'' ->
            newObj' hp $ \ !hpo -> objSetAttr kit nm (RefValue hpo) exit''
        | (nm, hp) <-
          [ ("threadLocalDiag", threadLocalDiagHP)
          , ("summarize"      , summarizeDiag rtdsVar)
          ]
        ]
      $ const
      $ exitProc ths exit
      $ RefValue kit

  metricOneTxHP :: RtDiag -> HostProc
  metricOneTxHP !rtd _ !ths !exit = do
    txContIO ths (encountOneTxCompletion rtd) $ const $ return ()
    exitProc ths exit NilValue

  doneDiagHP :: RtDiag -> HostProc
  doneDiagHP !rtd _ !ths !exit = do
    txContIO ths (doneRuntimeDiagnostic rtd) $ const $ return ()
    exitProc ths exit NilValue

  summarizeDiag :: TVar [RtDiag] -> HostProc
  summarizeDiag !rtdsVar _ !ths !exit = do
    !rtds <- readTVar rtdsVar
    txContIO ths (summarizeDiagnostic rtds) $ const $ return ()
    exitProc ths exit NilValue

