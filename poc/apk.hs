{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE OverloadedLabels, DataKinds, TypeOperators, ViewPatterns,
             PartialTypeSignatures #-}

module Main where

import           Prelude
import           Data.Functor.Identity          ( Identity(..) )
import           GHC.OverloadedLabels           ( IsLabel(..) )

import           Data.Dynamic

import           Named.Internal1


-- * minimum data structures as interface with scripting code

type AttrKey = String
data AttrVal = NilValue
  | IntValue !Integer
  | StrValue !String
  deriving (Eq, Ord, Typeable)
instance Show AttrVal where
  show NilValue      = "nil"
  show (IntValue !x) = show x
  show (StrValue !x) = show x

data ArgsPack = ArgsPack {
    positional'args :: [AttrVal]
  , keyword'args    :: [(AttrKey, AttrVal)]
  }
instance Semigroup ArgsPack where
  (ArgsPack p1 kw1) <> (ArgsPack p2 kw2) = ArgsPack (p1 ++ p2) (kw1 ++ kw2)
instance Monoid ArgsPack where
  mempty = ArgsPack [] []

class Callable a where
  call :: a -> ArgsPack -> (AttrVal -> IO ()) -> IO ()


-- * functions to be callable from scripting code

-- | interfacing Haskell function meant to be easily called by scripting code
assert
  :: "expect" :! AttrVal
  -> "target" :? AttrVal
  -> "message" :! String
  -> IO String
assert (Arg !expect) (ArgF !maybeTarget) (Arg !message) = case maybeTarget of
  Nothing     -> return $ "* assertion not applicable: " <> message
  Just target -> if expect == target
    then return $ "* assertion passed: " <> message
    else error $ "* assertion failed: " <> message


-- mockup & test out
main :: IO ()
main = do

  result <- assert3 ! defaults

  putStrLn $ "Got result: " <> show result

 where

  assert1' = with
    (Param (ArgF @Identity @String @"message" (Identity "as good will")))
    assert

  messageValue = "as good will"
  assert1''    = with (fromLabel @"message" messageValue) assert

  assert1'''   = with (#message "as good will") assert

  assert1 =
    assert --
           ! #message "as good will"

  assert2 =
    assert --
      ! #message "as good will"
      ! #expect (IntValue 333)

  assert3 =
    assert -- 
      ! #message "as good will"
      ! #expect (IntValue 333)
      ! #target (IntValue 333)

  !apk =
    ArgsPack [IntValue 333, StrValue "as good will"] [("target", IntValue 333)]
