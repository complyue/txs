{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Types where

import           Prelude


data Expr = AddrAttr !String
          | LiteralStr !String
          | LiteralInt !Integer
          | BinaryOp !String !Expr !Expr
          | FnApp !Expr !Expr
          | TxGroup !Expr
  deriving (Show)

