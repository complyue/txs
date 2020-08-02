{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Types where

import           Prelude

import           GHC.Conc                       ( unsafeIOToSTM )

import           System.IO
import           GHC.Stats
import           Control.Monad
import           Control.Concurrent
import           Control.Concurrent.STM
import           Data.Time.Clock.System
import           Data.Int
import           Data.IORef
import           Data.Unique
import           Data.Dynamic

-- Data.HashMap/HashSet is enough, but for simply reprod purpose,
-- we'd only use libraries bundled with GHC
import           Data.Map.Strict                ( Map )
import qualified Data.Map.Strict               as Map


-- | AST
data Expr = Attr !String
          | LitNil
          | LitStr !String
          | LitInt !Integer
          | LitObj
          | BinOp !String !Expr !Expr
          | FnApp !Expr !Expr
          | Paren !Expr
  deriving (Show)


-- * Runtime Data Types

type AttrId = String

data AttrVal = NilValue
  | IntValue !Integer
  | StrValue !String
  | RefValue !Object
  deriving (Eq, Ord, Typeable)
instance Show AttrVal where
  show NilValue      = "nil"
  show (IntValue !x) = show x
  show (StrValue !x) = show x
  show RefValue{}    = "<object>"

toString :: AttrVal -> String
toString (StrValue !s) = s
toString !v            = show v

data Object = Object {
      obj'uniq :: !Unique
      -- wrapped storage of native types in host language
    , obj'store :: !(TVar Dynamic)
  }
instance Eq Object where
  (Object !x'id _) == (Object !y'id _) = x'id == y'id
instance Ord Object where
  compare (Object !x'id _) (Object !y'id _) = compare x'id y'id


newObj' :: Typeable a => a -> (Object -> STM ()) -> STM ()
newObj' !objStore !exit = do
  !u <- unsafeIOToSTM newUnique
  !d <- newTVar $ toDyn objStore
  exit $ Object u d

-- mockup of an insertion-order-preserving dict used in realworld case,
-- which being `TVar (HashMap AttrVal Int)` where the Int be addressing a
-- `TVar (Vector (TVar AttrVal))`
type HashStore = Map AttrId (TVar AttrVal)

newObj :: (Object -> STM ()) -> STM ()
newObj !exit = do
  osv <- newTVar (Map.empty :: HashStore)
  newObj' osv exit

objGetAttr :: Object -> AttrId -> (AttrVal -> STM ()) -> STM ()
objGetAttr (Object _ !osv) !attr !exit = fromDynamic <$> readTVar osv >>= \case
  Nothing                      -> error "bug: not a plain obj with hash store"
  Just (hsv :: TVar HashStore) -> Map.lookup attr <$> readTVar hsv >>= \case
    Nothing       -> exit NilValue
    Just !attrVar -> readTVar attrVar >>= exit

objSetAttr :: Object -> AttrId -> AttrVal -> (AttrVal -> STM ()) -> STM ()
objSetAttr (Object _ !osv) !attr !val !exit =
  fromDynamic <$> readTVar osv >>= \case
    Nothing                      -> error "bug: not a plain obj with hash store"
    Just (hsv :: TVar HashStore) -> case val of
      NilValue -> do
        modifyTVar' hsv $ Map.delete attr
        exit val
      _ -> Map.lookup attr <$> readTVar hsv >>= \case
        Nothing -> do
          !attrVar <- newTVar val
          modifyTVar' hsv $ Map.insert attr attrVar
          exit val
        Just !attrVar -> do
          writeTVar attrVar val
          exit val

