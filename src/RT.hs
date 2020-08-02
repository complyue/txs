{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE ScopedTypeVariables #-}

module RT where

import Prelude

import           GHC.Conc                       ( unsafeIOToSTM )

import           GHC.Stats
import           System.IO

import           Control.Monad
import           Control.Concurrent
import           Control.Concurrent.STM
import           Data.Time.Clock.System
import           Data.Int
import           Data.Text                      ( Text )
-- import qualified Data.Text                     as T
import           Data.IORef
import           Data.Unique
import           Data.Dynamic

-- Data.HashMap/HashSet is enough, but for simply reprod purpose,
-- we'd only use libraries bundled with GHC
import           Data.Map.Strict                ( Map )
import qualified Data.Map.Strict               as Map

