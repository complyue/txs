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

 where

  languageDef = haskellStyle
    { Token.reservedNames   = ["do", "nil"]
    , Token.reservedOpNames = [ " "
                              , "+"
                              , "-"
                              , "*"
                              , "/"
                              , "="
                              , "."
                              , "@"
                              , "++"
                              , "$"
                              ]
    }
  lexer      = Token.makeTokenParser languageDef
  lexeme     = Token.lexeme lexer
  symbol     = void . Token.symbol lexer

  whiteSpace = Token.whiteSpace lexer -- parses whitespace
  unit       = Text.Parsec.try $ do -- parses empty parethesis
    symbol "()"
    notFollowedBy $ Token.identLetter languageDef
    return ()
  nil        = Token.reserved lexer "nil" -- literal nil
  identifier = Token.identifier lexer -- parses an identifier
  fap        = Text.Parsec.try $ do -- function application, highest precedence
    notFollowedBy (Token.operator lexer)
    return FnApp
  operator !sym = Text.Parsec.try $ do -- parses a specified operator
    whiteSpace
    Token.reservedOp lexer sym
    return $ BinOp sym
  -- parses a semicolon as infix operator, only if not at end-of-block
  semicolon = Text.Parsec.try $ do
    whiteSpace
    void $ Token.semi lexer
    notFollowedBy $ eof <|> symbol ")" <|> symbol "}" <|> symbol "]"
    return $ BinOp ";"
  -- consume the semicolon at end-of-block as if not there, or parse fails
  lastSemicolon = Text.Parsec.try $ do
    whiteSpace
    void $ Token.semi lexer
    lookAhead $ eof <|> symbol ")" <|> symbol "}" <|> symbol "]"
    return id
  faplow = Text.Parsec.try $ do -- function application, lowest precedence
    whiteSpace
    Token.reservedOp lexer "$"
    return FnApp
  parens     = lexeme . Token.parens lexer -- parses surrounding parenthesis
  braces     = lexeme . Token.braces lexer -- parses surrounding braces
  stringLit  = Token.stringLiteral lexer -- parse a string
  -- todo 'Token.integer' should be better for it handles negation,
  -- but that way `8 + 3` will be parsed as `8 (+3)` as we imposed
  -- the fap with higher precedence. improve this?
  integerLit = lexeme $ Token.decimal lexer -- parses an integer
  objectLit  = Text.Parsec.try $ symbol "{}" -- parses literal object


  txsParser :: Parser Expr
  txsParser = whiteSpace *> parseExpr <* (whiteSpace >> eof)

  parseExpr :: Parser Expr
  parseExpr = buildExpressionParser opsTable $ choice
    [ LitNil <$ unit -- we parse the empty parethesis as if a literal nil
    , Paren <$> parens parseExpr
    , LitObj <$ objectLit
    , Brace <$> braces parseExpr
    , LitNil <$ nil
    , Attr <$> identifier
    , LitStr <$> stringLit
    , LitInt <$> integerLit
    ]
   where
    opsTable =
      [ [Infix (operator ".") AssocLeft, Infix (operator "@") AssocLeft]
      , [Infix fap AssocLeft]
      , [ Infix (operator "*") AssocLeft
        , Infix (operator "/") AssocLeft
        , Infix (operator "%") AssocLeft
        ]
      , [Infix (operator "+") AssocLeft, Infix (operator "-") AssocLeft]
      , [Infix (operator "++") AssocLeft]
      , [Infix (operator "=") AssocRight]
      , [Infix faplow AssocRight]
      , [Infix semicolon AssocLeft, Postfix lastSemicolon]
      ]

