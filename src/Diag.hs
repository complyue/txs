{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Diag where

import           Prelude

import           GHC.Stats
import           System.IO

import           Control.Monad
import           Data.Time.Clock.System
import           Data.Int
import           Data.IORef


data RtDiag = RtDiag {
    rtd'diag'start :: !(IORef Int64)
  , rtd'total'cntr :: !(IORef Int)
  , rtd'bat'cntr :: !(IORef Int)
  , rtd'bat'start :: !(IORef Int64)
  , rtd'metric'min'seconds :: !Int
  }

createRuntimeDiagnostic :: Int -> IO RtDiag
createRuntimeDiagnostic !metricMinSec = do
  MkSystemTime !epochTime _ <- getSystemTime

  !diag'start               <- newIORef epochTime
  !bat'start                <- newIORef epochTime

  !total'cntr               <- newIORef 0
  !bat'cntr                 <- newIORef 0

  return $ RtDiag { rtd'diag'start         = diag'start
                  , rtd'total'cntr         = total'cntr
                  , rtd'bat'cntr           = bat'cntr
                  , rtd'bat'start          = bat'start
                  , rtd'metric'min'seconds = metricMinSec
                  }

resetDiagnostic :: RtDiag -> IO ()
resetDiagnostic (RtDiag !diag'start !total'cntr !bat'cntr !bat'start _) = do
  MkSystemTime !epochTime _ <- getSystemTime
  writeIORef diag'start epochTime
  writeIORef bat'start  epochTime
  writeIORef total'cntr 0
  writeIORef bat'cntr   0

encountOneTxCompletion :: RtDiag -> IO ()
encountOneTxCompletion (RtDiag _ !total'cntr !bat'cntr !bat'start !min'sec) =
  do

    atomicModifyIORef' total'cntr $ \ !c -> (c + 1, ())

    MkSystemTime !curTime _ <- getSystemTime
    !bt                     <- atomicModifyIORef' bat'start $ \ !bst ->
      let !bt = fromIntegral (curTime - bst)
      in  if bt < min'sec then (bst, 0) else (curTime, bt)
    if bt < 1
      then atomicModifyIORef' bat'cntr $ \ !c -> (c + 1, ())
      else do

        !bc <- atomicModifyIORef' bat'cntr $ \ !c -> (0, c + 1)
        if bc < bt
          then
            putStr $ "Slow: " <> show bc <> " TX in " <> show bt <> " seconds"
          else putStr $ "Fast: " <> show (bc `div` bt) <> " TPS"

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

summarizeDiagnostic :: RtDiag -> IO ()
summarizeDiagnostic (RtDiag !diag'start !total'cntr _ _ _) = do
  MkSystemTime !finishTime _ <- getSystemTime
  !startTime                 <- readIORef diag'start
  !cnt                       <- readIORef total'cntr
  let !costSeconds = fromIntegral $ finishTime - startTime
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

