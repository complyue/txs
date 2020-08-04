{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- | data structures and routines for runtime diagnostics, not thread safe
module Diag where

import           Prelude

import           GHC.Stats
import           System.IO

import           Control.Monad
import           Control.Concurrent
import           Data.Time.Clock.System
import           Data.Int
import           Data.IORef


data RtDiag = RtDiag {
    rtd'diag'start :: !(IORef Int64)
  , rtd'diag'finish :: !(IORef Int64)
  , rtd'total'cntr :: !(IORef Int)
  , rtd'bat'start :: !(IORef Int64)
  , rtd'bat'cntr :: !(IORef Int)
  , rtd'bat'seconds :: !Int
  }


startRuntimeDiagnostic :: Int -> IO RtDiag
startRuntimeDiagnostic !batSeconds = do
  MkSystemTime !epochTime _ <- getSystemTime

  !diag'start               <- newIORef epochTime
  !diag'finish              <- newIORef 0
  !total'cntr               <- newIORef 0
  !bat'start                <- newIORef epochTime
  !bat'cntr                 <- newIORef 0

  return $ RtDiag { rtd'diag'start  = diag'start
                  , rtd'diag'finish = diag'finish
                  , rtd'total'cntr  = total'cntr
                  , rtd'bat'start   = bat'start
                  , rtd'bat'cntr    = bat'cntr
                  , rtd'bat'seconds = batSeconds
                  }


doneRuntimeDiagnostic :: RtDiag -> IO ()
doneRuntimeDiagnostic (RtDiag _ !diag'finish _ _ _ _) = do
  MkSystemTime !finishTime _ <- getSystemTime
  writeIORef diag'finish finishTime


encountOneTxCompletion :: RtDiag -> IO ()
encountOneTxCompletion (RtDiag _ _ !total'cntr !bat'start !bat'cntr !bat'sec) =
  do
    modifyIORef' total'cntr (+ 1)

    !bst                    <- readIORef bat'start
    MkSystemTime !curTime _ <- getSystemTime
    let bt = fromIntegral $ curTime - bst
    if bt < bat'sec
      then modifyIORef' bat'cntr (+ 1)
      else do
        writeIORef bat'start curTime
        !bc <- readIORef bat'cntr
        writeIORef bat'cntr 0
        !thId <- myThreadId
        putStr $ show thId
        if bc < bt
          then
            putStr $ "\tSlow: " <> show bc <> " TX in " <> show bt <> " seconds"
          else putStr $ "\tFast: " <> show (bc `div` bt) <> " TPS"

        getRTSStatsEnabled >>= \case
          True -> do
            !rtss <- getRTSStats
            putStrLn
              $  "\t  Heap: "
              <> show
                   (     fromIntegral (max_live_bytes rtss)
                   `div` (1024 * 1024 :: Int64)
                   )
              <> " MB"
          False -> putStrLn ""

        hFlush stdout


summarizeDiagnostic :: [RtDiag] -> IO ()
summarizeDiagnostic !rtds = doSum rtds 0 0
 where
  doSum [] !cnt !costSeconds = do
    putStrLn
      $  "Total "
      <> show cnt
      <> " transactions processed in "
      <> show costSeconds
      <> " seconds."
    when (costSeconds > 0) $ putStrLn $ "Overall TPS: " <> show
      (div cnt costSeconds)

    getRTSStatsEnabled >>= \case
      True -> do
        !rtss <- getRTSStats
        putStrLn
          $  "Heap: "
          <> show
               (fromIntegral (max_live_bytes rtss) `div` (1024 * 1024 :: Int64))
          <> " MB"
      False -> pure ()

    hFlush stdout
  doSum (RtDiag !diag'start !diag'finish !total'cntr _ _ _ : rest) !cnt !costSeconds
    = do
      !startTime  <- readIORef diag'start
      !finishTime <- readIORef diag'finish
      !diagCnt    <- readIORef total'cntr
      doSum rest (cnt + diagCnt)
        $ (costSeconds +)
        $ fromIntegral
        $ finishTime
        - startTime


