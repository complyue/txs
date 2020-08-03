{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Main where

import           Prelude

import           System.IO

import           Types
import           Parser
import           RT


main :: IO ()
main = do
  !src <- getContents
  let !ast = parseTXS src

  putStrLn $ " * Program parsed as: " <> show ast
  putStrLn " *-=TransactionScript=-*"

  (!globals, !summarize) <- defaultGlobals

  !result                <- runTXS globals ast

  case result of
    NilValue      -> pure ()
    StrValue !msg -> putStrLn $ " * Program result string: " <> msg
    _             -> putStrLn $ " * Program result value: " <> show result

  summarize
