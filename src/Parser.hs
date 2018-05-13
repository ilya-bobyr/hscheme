{-# LANGUAGE FlexibleContexts #-}

module Parser (
    readExpr
  , readExprList
  , parseFullExpr
  , parseFullExprList

  , runParseFullExpr
  , runParseFullExprList
) where

import Prelude hiding (readFile)

import Text.Parsec (Parsec, ParseError)
import Text.Parsec.Prim (many, try, parserFail, parse, (<|>), (<?>)
                        , getParserState, stateInput)
import Text.Parsec.Char (anyChar, oneOf, letter, char, digit, noneOf
                        , spaces, string)
import Text.Parsec.Combinator (between, eof, many1, endBy, sepBy
                              , option)
import Text.Parsec.String (Parser)

import Numeric (readFloat, readDec)

import Control.Monad (unless)

import Debug.Trace (traceM)

import Ast (LispVal(..))
import Error (ThrowsError, LispError(..), throwError)


-- seeNextAt :: String -> Int -> Parsec String u ()
-- seeNextAt label n = do
--   s <- getParserState
--   let out = take n (stateInput s)
--   traceM $ label ++ ": " ++ show out


parseAtom :: Parser LispVal
parseAtom = do
  let specialInitial = oneOf "!$%&|*/:<=>?_~"
      initial = letter <|> specialInitial

      specialSubsequent = oneOf "+-.@"
      subsequent = initial <|> digit <|> specialSubsequent

      identifier = do
        first <- initial
        rest <- many subsequent
        return $ Atom $ first : rest

      peculiarIdentifier =
        fmap Atom $     string "+"
                    <|> string "-"
                    <|> string "..."
                    <|> do first <- string "->"
                           rest <- many subsequent
                           return $ first ++ rest

  res <- try identifier <|> peculiarIdentifier
  spaces
  return res

parseBool :: Parser LispVal
parseBool = do
  _ <- char '#'
  res <- (try $ char 't' >> return (Bool True))
           <|> (char 'f' >> return (Bool False))
  spaces
  return res

parseNumber :: Parser LispVal
parseNumber = do
  let
    integer :: Parser LispVal
    integer = do
      sign <- option '+' (char '-' <|> char '+')
      v <- many1 digit
      let [(val, "")] = readDec v
      case sign of
        '+' -> return $ Number val
        '-' -> return $ Number (-val)
        _   -> error "Invalid result form the option parser."

    float :: Parser LispVal
    float = do
      sign <- option '+' (char '-' <|> char '+')
      v1 <- many digit
      _ <- char '.'
      v2 <- many digit
      unless (length v1 > 0 || length v2 > 0)
        $ parserFail "Expecting digits on either side of '.'"
      let [(val, "")] = readFloat $ v1 ++ "." ++ v2
      case sign of
        '+' -> return $ Float val
        '-' -> return $ Float (-val)
        _   -> error "Invalid result from the option parser."

  res <- try float <|> integer
  spaces
  return res

parseCharacter :: Parser LispVal
parseCharacter = do
  _ <- string "#\\"
  res <- try spaceLit <|> try newlineLit <|> charValue <?>
         "\"space\", \"newline\", or any one character"
  spaces
  return res
  where
    spaceLit = do
      _ <- string "space"
      return $ Character ' '
    newlineLit = do
      _ <- string "newline"
      return $ Character '\n'
    charValue = do
      c <- anyChar
      return $ Character c


parseString :: Parser LispVal
parseString = do
  _ <- char '"'
  x <- many escapeOrChar
  _ <- char '"'
  spaces
  return $ String $ x
  where
    escapeOrChar =
          do _ <- char '\\'
             c <- anyChar
             return c
      <|> noneOf "\\\""

parseList :: Parser LispVal
parseList = do
  char '(' >> spaces
  res <- fmap List $ parseExpr `sepBy` spaces
  spaces >> char ')'
  spaces
  return res

parseDottedList :: Parser LispVal
parseDottedList = do
  char '(' >> spaces
  h <- parseExpr `endBy` spaces
  t <- char '.' >> spaces >> parseExpr
  spaces >> char ')'
  spaces
  return $ DottedList h t

parseQuoted :: Parser LispVal
parseQuoted = do
  _ <- char '\''
  x <- parseExpr
  return $ List [Atom "quote", x]

parseExpr :: Parser LispVal
parseExpr = do
  res <-     try parseNumber
         <|> try parseBool
         <|> try parseCharacter
         <|> try parseString
         <|> try parseAtom
         <|> try parseQuoted
         <|> try parseList
         <|> parseDottedList
  spaces
  return res

parseFullExpr :: Parser LispVal
parseFullExpr = do
  res <- parseExpr
  eof
  return res

parseFullExprList :: Parser [LispVal]
parseFullExprList = do
  res <- parseExpr `endBy` spaces
  eof
  return res

runOrFail :: Parser a -> String -> Either ParseError a
runOrFail parser input = parse parser "lisp" input

readOrThrow :: Parser a -> String -> ThrowsError a
readOrThrow parser input =
  case runOrFail parser input of
    Left err -> throwError $ Parser err
    Right val -> return val

runParseFullExpr :: String -> Either ParseError LispVal
runParseFullExpr = runOrFail parseFullExpr

runParseFullExprList :: String -> Either ParseError [LispVal]
runParseFullExprList = runOrFail parseFullExprList

readExpr :: String -> ThrowsError LispVal
readExpr = readOrThrow parseFullExpr

readExprList :: String -> ThrowsError [LispVal]
readExprList = readOrThrow parseFullExprList
