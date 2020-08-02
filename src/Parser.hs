{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Parser
  ( parseTXS
  )
where

import           Prelude

import           Control.Monad

import           Text.Parsec
import           Text.ParserCombinators.Parsec
import           Text.ParserCombinators.Parsec.Expr
import           Text.ParserCombinators.Parsec.Language
import qualified Text.ParserCombinators.Parsec.Token
                                               as Token

import           Types


parseTXS :: String -> Expr
parseTXS !str = case parse txsParser "" str of
  Left  e -> error $ show e
  Right r -> r


languageDef = haskellStyle
  { Token.reservedNames   = ["do"]
  , Token.reservedOpNames = [" ", "+", "-", "*", "/", "=", ".", "@", "++", "$"]
  }
lexer = Token.makeTokenParser languageDef

whiteSpace = Token.whiteSpace lexer -- parses whitespace
nil = Text.Parsec.try $ do
  Token.symbol lexer "nil"
  notFollowedBy $ Token.identLetter languageDef
identifier = Token.identifier lexer -- parses an identifier
fap = Text.Parsec.try $ do -- function application, highest precedence
  notFollowedBy (Token.operator lexer)
  return FnApp
operator !sym = Text.Parsec.try $ do -- match an operator
  whiteSpace
  Token.reservedOp lexer sym
  return $ BinOp sym
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
objectLit = Token.symbol lexer "{}" -- parses literal object


txsParser :: Parser Expr
txsParser = whiteSpace >> parseExpr

parseExpr :: Parser Expr
parseExpr = buildExpressionParser opsTable $ choice
  [ Paren <$> parens parseExpr
  , LitNil <$ nil
  , Attr <$> identifier
  , LitStr <$> stringLit
  , LitInt <$> integerLit
  , LitObj <$ objectLit
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

