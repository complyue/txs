{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Diag where

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


data RtDiag = RtDiag {
    rtd'tx'cntr :: !(IORef Int)
  , rtd'bat'start :: !(IORef Int64)
  , rtd'metric'min'seconds :: !Int
  }

createRuntimeDiagnostic :: Int -> IO RtDiag
createRuntimeDiagnostic !metricMinSec = do
  MkSystemTime !epochTime _ <- getSystemTime

  !tx'cntr                  <- newIORef 0
  !bat'start                <- newIORef epochTime

  return $ RtDiag { rtd'tx'cntr            = tx'cntr
                  , rtd'bat'start          = bat'start
                  , rtd'metric'min'seconds = metricMinSec
                  }

encountOneTxCompletion :: RtDiag -> IO ()
encountOneTxCompletion (RtDiag !tx'cntr !bat'start !min'sec) = do

  MkSystemTime !curTime _ <- getSystemTime
  !bt                     <- atomicModifyIORef' bat'start $ \ !bst ->
    let !bt = fromIntegral (curTime - bst)
    in  if bt < min'sec then (bst, 0) else (curTime, bt)
  if bt < 1
    then atomicModifyIORef' tx'cntr $ \ !c -> (c + 1, ())
    else do

      !bc <- atomicModifyIORef' tx'cntr $ \ !c -> (0, c + 1)
      if bc < bt
        then putStr $ "Slow: " <> show bc <> " TX in " <> show bt <> " seconds"
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

