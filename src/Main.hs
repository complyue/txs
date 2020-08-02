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

  -- print ast

  !globals <- defaultGlobals

  !result  <- runTXS globals ast

  case result of
    NilValue      -> pure ()
    StrValue !msg -> putStrLn msg
    _             -> print result

