{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Utils where

import           Prelude

import           Control.Concurrent.STM


seqcontSTM :: forall a . [(a -> STM ()) -> STM ()] -> ([a] -> STM ()) -> STM ()
seqcontSTM !xs !exit = go xs []
 where
  go :: [(a -> STM ()) -> STM ()] -> [a] -> STM ()
  go []         ys = exit $! reverse $! ys
  go (x : rest) ys = x $ \y -> go rest (y : ys)

