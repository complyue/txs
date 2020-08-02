{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Parser where

import           Prelude

import           Control.Monad

import           Text.Parsec
import           Text.ParserCombinators.Parsec
import           Text.ParserCombinators.Parsec.Expr
import           Text.ParserCombinators.Parsec.Language
import qualified Text.ParserCombinators.Parsec.Token
                                               as Token

import           Types


languageDef = haskellStyle
  { Token.reservedNames   = ["do"]
  , Token.reservedOpNames = [" ", "+", "-", "*", "/", "=", ".", "@", "++", "$"]
  }

lexer = Token.makeTokenParser languageDef

whiteSpace = Token.whiteSpace lexer -- parses whitespace
identifier = Token.identifier lexer -- parses an identifier
fap = Text.Parsec.try $ do -- function application, highest precedence
  notFollowedBy (Token.operator lexer)
  return FnApp
operator !sym = Text.Parsec.try $ do -- match an operator
  whiteSpace
  Token.reservedOp lexer sym
  return $ BinaryOp sym
faplow = Text.Parsec.try $ do -- function application, lowest precedence
  whiteSpace
  Token.reservedOp lexer "$"
  return FnApp
parens = Token.parens lexer -- parses surrounding parenthesis
stringLit = Token.stringLiteral lexer -- parse a string
-- todo 'Token.integer' should be better for it handles negation,
-- but that way `8 + 3` will be parsed as `8 (+3)` as we imposed
-- the fap with higher precedence. improve this?
integerLit = Token.decimal lexer -- parses an integer


txsParser :: Parser Expr
txsParser = whiteSpace >> parseExpr

parseExpr :: Parser Expr
parseExpr = buildExpressionParser opsTable $ choice
  [ TxGroup <$> parens parseExpr
  , AddrAttr <$> identifier
  , LiteralStr <$> stringLit
  , LiteralInt <$> integerLit
  ]
 where
  opsTable =
    [ [Infix fap AssocLeft]
    , [Infix (operator ".") AssocLeft, Infix (operator "@") AssocLeft]
    , [Infix (operator "*") AssocLeft, Infix (operator "/") AssocLeft]
    , [Infix (operator "+") AssocLeft, Infix (operator "-") AssocLeft]
    , [Infix (operator "++") AssocLeft]
    , [Infix (operator "=") AssocLeft]
    , [Infix faplow AssocLeft]
    ]

