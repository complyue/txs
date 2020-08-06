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
        Nothing -> error $ "an object but not callable by expr: " <> show lhx
    _ -> error $ "not an object (to be called) by expr: " <> show lhx
  BinOp !opSym !lhx !rhx -> infixOp pgs opSym lhx rhx exit
  LitObj                 -> newObj $ \ !obj -> exitProc pgs exit $ RefValue obj
  LitInt !val            -> exitProc pgs exit $ IntValue val
  LitStr !val            -> exitProc pgs exit $ StrValue val
  LitNil                 -> exitProc pgs exit NilValue
  Attr !attr ->
    objGetAttr (pgs'scope pgs) attr $ \ !val -> exitProc pgs exit val
  Brace !exprInBlock -> evalExpr exprInBlock pgs exit


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

-- | Helper for proper CPS exit from within a 'HostProc'
--
-- Should join the continuation if the script code has requested a transaction
-- block (by parenthesis quoting a sequence of expressions), otherwise should
-- enqueue the continuation for it to be executed in a separate, subsequent STM
-- transaction.
exitProc :: ProgState -> (AttrVal -> STM ()) -> AttrVal -> STM ()
exitProc !pgs !exit !val = if pgs'in'tx pgs
  then exit val
  else writeTQueue (pgs'task'queue pgs) $ Right $! exit val

-- | Schedule an IO action to initiate the subsequent STM transaction, it'll be
-- performed after current STM transaction is committed.
--
-- CAVEAT: This can break the transaction boundary intuited at scripting level,
--         if the stem exit of a 'HostProc' is only passed to 'txContIO' as
--         the 'initNextTx', "pgs'in'tx" state should be checked being False
--         for doing that safely.
txContIO :: forall a . ProgState -> IO a -> (a -> STM ()) -> STM ()
txContIO !pgs !act !initNextTx = writeTQueue tq $ Left $ do
  !result <- act
  atomically $ writeTQueue tq $ Right $! initNextTx result
  where !tq = pgs'task'queue pgs


infixOp :: ProgState -> String -> Expr -> Expr -> (AttrVal -> STM ()) -> STM ()
infixOp !pgs !sym !lhx !rhx !exit = builtinOp sym
 where
  builtinOp ";" = evalExpr lhx pgs $ const $ evalExpr rhx pgs exit
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
  builtinOp "%" = evalExpr lhx pgs $ \case
    IntValue !lhi -> evalExpr rhx pgs $ \case
      IntValue !rhi -> exitProc pgs exit $ IntValue $ mod lhi rhi
      _             -> error "right-hand to (%) not a number"
    _ -> error "left-hand to (%) not a number"
  builtinOp "++" = evalExpr lhx pgs $ \ !lhv -> evalExpr rhx pgs
    $ \ !rhv -> exitProc pgs exit $ StrValue $ toString lhv ++ toString rhv
  builtinOp "." = case rhx of
    Attr !attr -> evalExpr lhx pgs $ \case
      RefValue !tgtObj -> objGetAttr tgtObj attr $ exitProc pgs exit
      !badTgtVal ->
        error
          $  "left-hand of dot-notation not an object but: "
          <> show badTgtVal
          <> ", by expr: "
          <> show lhx
    _ -> error "right-hand of dot-notation not an attribute addressor"
  builtinOp "@" = evalExpr rhx pgs $ \ !addrVal ->
    let !tgtAttr = toString addrVal
    in  evalExpr lhx pgs $ \case
          RefValue !tgtObj -> objGetAttr tgtObj tgtAttr $ exitProc pgs exit
          !badTgtVal ->
            error
              $  "left-hand of at-notation not an object but: "
              <> show badTgtVal
  builtinOp "=" = evalExpr rhx pgs $ \ !rhv -> case lhx of
    Attr !attr -> objSetAttr (pgs'scope pgs) attr rhv $ exitProc pgs exit
    BinOp "." !tgtExpr (Attr !tgtAttr) -> evalExpr tgtExpr pgs $ \case
      RefValue !tgtObj -> objSetAttr tgtObj tgtAttr rhv $ exitProc pgs exit
      !badTgtVal ->
        error
          $  "left-hand of dot-notation not an object but: "
          <> show badTgtVal
          <> ", by expr: "
          <> show lhx
    BinOp "@" !tgtExpr !tgtAddr -> evalExpr tgtAddr pgs $ \ !addrVal ->
      let !tgtAttr = toString addrVal
      in  evalExpr tgtExpr pgs $ \case
            RefValue !tgtObj ->
              objSetAttr tgtObj tgtAttr rhv $ exitProc pgs exit
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
  assertHP !msgExpr !pgs !exit = evalExpr msgExpr pgs $ \ !assertMsg ->
    let assert1HP !expectExpr !pgs1 !exit1 =
            evalExpr expectExpr pgs1 $ \ !expectVal ->
              let assert2HP !targetExpr !pgs2 !exit2 =
                      evalExpr targetExpr pgs2 $ \ !targetVal ->
                        if targetVal == expectVal
                          then
                            exitProc pgs2 exit2
                            $  StrValue
                            $  " * assertion passed: "
                            <> toString assertMsg
                          else error $ "* assertion failed: " <> toString assertMsg
              in  newObj' assert2HP $ exitProc pgs exit1 . RefValue
    in  newObj' assert1HP $ exitProc pgs exit . RefValue

  printHP :: HostProc
  printHP !argExpr !pgs !exit = evalExpr argExpr pgs $ \case
    NilValue -> exitProc pgs exit NilValue
    !arg     -> do
      txContIO pgs (putStrLn $ toString arg) $ const $ return ()
      exitProc pgs exit NilValue

  sleepHP :: HostProc
  sleepHP !usExpr !pgs !exit = if pgs'in'tx pgs
    then error "you don't issue `sleep` from within a transaction"
    else evalExpr usExpr pgs $ \case
      IntValue !us ->
        txContIO pgs (threadDelay $ fromIntegral us) $ const $ exitProc
          pgs
          exit
          NilValue
      !badDelay -> error $ "bad microsend value to sleep: " <> show badDelay

  guidHP :: IORef Int64 -> HostProc
  guidHP !guidCntr _ !pgs !exit = if pgs'in'tx pgs
    then error "you don't issue `guid` from within a transaction"
    else txContIO pgs (atomicModifyIORef' guidCntr $ \ !c -> (c + 1, c))
      $ \ !guid -> exitProc pgs exit $ IntValue $ fromIntegral guid

  concurHP :: HostProc
  concurHP !nExpr !pgs !exit = if pgs'in'tx pgs
    then error "you don't issue `concur` from within a transaction"
    else evalExpr nExpr pgs $ \case
      IntValue !concN | concN > 0 ->
        let concur1HP !concExpr !pgs1 !exit1 = do
              !concDoneSem <- newTSem $ fromIntegral $ 1 - concN
              let !initScope = pgs'scope pgs
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
              txContIO pgs (forkConcur concN) $ const $ do
                waitTSem concDoneSem
                exitProc pgs1 exit1 NilValue
        in  newObj' concur1HP $ exitProc pgs exit . RefValue
      !badConcN -> error $ "invalid concurrency number: " <> show badConcN

  repeatHP :: HostProc
  repeatHP !argExpr !pgs !exit = evalExpr argExpr pgs $ \case
    IntValue !cnt | cnt >= 0 ->
      let repeat1 !repeateeExpr !pgs1 !exit1 = doRepeat cnt             where
              doRepeat !cntLeft = if cntLeft < 1
                then exitProc pgs1 exit1 NilValue
                else evalExpr repeateeExpr pgs1 $ const $ doRepeat (cntLeft - 1)
      in  newObj' repeat1 $ exitProc pgs exit . RefValue
    !badCnt ->
      error $ "`repeat` expects a positive number, but given: " <> show badCnt


  diagKitCtor :: HostProc
  diagKitCtor !_argExpr !pgs !exit = newObj $ \ !kit -> do
    !rtdsVar <- newTVar []
    let
      threadLocalDiagHP !batchSecondsExpr !pgs' !exit' =
        evalExpr batchSecondsExpr pgs' $ \case
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
                $ exitProc pgs' exit'
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
      $ exitProc pgs exit
      $ RefValue kit

  metricOneTxHP :: RtDiag -> HostProc
  metricOneTxHP !rtd _ !pgs !exit = do
    txContIO pgs (encountOneTxCompletion rtd) $ const $ return ()
    exitProc pgs exit NilValue

  doneDiagHP :: RtDiag -> HostProc
  doneDiagHP !rtd _ !pgs !exit = do
    txContIO pgs (doneRuntimeDiagnostic rtd) $ const $ return ()
    exitProc pgs exit NilValue

  summarizeDiag :: TVar [RtDiag] -> HostProc
  summarizeDiag !rtdsVar _ !pgs !exit = do
    !rtds <- readTVar rtdsVar
    txContIO pgs (summarizeDiagnostic rtds) $ const $ return ()
    exitProc pgs exit NilValue

