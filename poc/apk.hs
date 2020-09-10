{-# LANGUAGE BangPatterns #-}

module Main where

import           Prelude
import           GHC.Generics
import           Data.Dynamic


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

newtype Assert = Assert (
    Expect -> Maybe Target -> Message -> IO Message
  )
type Expect = AttrVal
type Target = AttrVal
type Message = String

instance Callable Assert where

  -- can this get auto-generated ? with https://wiki.haskell.org/GHC.Generics
  call (Assert !assert) (ArgsPack !args !kwargs) !exit = do
    (expect, target, message) <- parseApk
    result                    <- assert expect target message
    exit $ StrValue result
   where

    parseApk :: IO (Expect, Maybe Target, Message)
    parseApk = goParse
      (Left "missing arg: expect", Nothing, Left "missing arg: message")
      args
      kwargs

    goParse (got'expect, got'target, got'message) [] [] = case got'expect of
      Left  msg    -> error msg
      Right expect -> case got'message of
        Left  msg     -> error msg
        Right message -> return (expect, got'target, message)
    goParse (got'expect, got'target, got'message) args' ((name, val) : kwargs')
      = case name of
        "expect" -> case got'expect of
          Right{} -> error "duplicate arg: expect"
          Left{}  -> goParse (Right val, got'target, got'message) args' kwargs'
        "target" -> case got'target of
          Just{}  -> error "duplicate arg: target"
          Nothing -> goParse (got'expect, Just val, got'message) args' kwargs'
        "message" -> case got'message of
          Right{} -> error "duplicate arg: message"
          Left{}  -> case val of
            StrValue message ->
              goParse (got'expect, got'target, Right message) args' kwargs'
            _ -> error "bad arg type for: message"
        _ -> error "unexpected keyword args"
    goParse (got'expect, got'target, got'message) (val : args') [] =
      case got'expect of
        Left{}  -> goParse (Right val, got'target, got'message) args' []
        Right{} -> case got'target of
          Nothing -> goParse (got'expect, Just val, got'message) args' []
          Just{}  -> case got'message of
            Left{} -> case val of
              StrValue message ->
                goParse (got'expect, got'target, Right message) args' []
              _ -> error "bad arg type for: message"
            Right{} -> error "extranous positional args"


-- mockup & test out
main :: IO ()
main =
  call
      (Assert assert)
      (ArgsPack [IntValue 333, StrValue "as good will"]
                [("target", IntValue 333)]
      )
    $ \result -> putStrLn $ "Got result: " <> show result

-- | plain Haskell function meant to be easily called by scripting code
assert :: Expect -> Maybe Target -> Message -> IO Message
assert !expect !maybeTarget !message = case maybeTarget of
  Nothing     -> return $ "* assertion not applicable: " <> message
  Just target -> if expect == target
    then return $ "* assertion passed: " <> message
    else error $ "* assertion failed: " <> message

