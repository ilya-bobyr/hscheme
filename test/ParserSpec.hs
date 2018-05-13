{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE ScopedTypeVariables #-}

module ParserSpec (
  spec
  ) where

import Test.Hspec (Spec, describe, it, shouldBe)
import Test.QuickCheck (property, withMaxSuccess)
import Test.QuickCheck.Arbitrary (Arbitrary(..))
import Test.QuickCheck.Gen (Gen, choose, elements, frequency, listOf,
                           sized, vectorOf)
import Test.QuickCheck.Modifiers (PrintableString(..))
import Numeric (showFFloat)

import Parser (runParseFullExpr)
import Ast (LispVal(..), AtomName(..))


newtype ListOfNumbers = ListOfNumbers [Integer]
                      deriving (Show)

instance Arbitrary ListOfNumbers where
  arbitrary = fmap ListOfNumbers $ (listOf arbitrary :: Gen [Integer])

newtype MaybeSomeSpace = MaybeSomeSpace String
                       deriving (Show)

instance Arbitrary MaybeSomeSpace where
  arbitrary = fmap MaybeSomeSpace $
    frequency [ (3, return "")
              , (2, return " ")
              , (1, return "   ")
              ]

spec :: Spec
spec = do
  canParseNumbers
  canParseAtoms
  canParseLists
  canParseAnExpr

canParseNumbers :: Spec
canParseNumbers =
  describe "Can parse numbers" $ do
    it "should parse an integer" $ property $
      \x -> runParseFullExpr (show x) == Right (Number x)

    it "should parse a floating point number" $ property $
      \x -> runParseFullExpr (showFFloat Nothing x "") == Right (Float x)

canParseAtoms :: Spec
canParseAtoms =
  describe "Can parse atoms" $ do
    it "should parse a symbol" $ property $
      \(AtomName atom) ->
        runParseFullExpr atom `shouldBe` Right (Atom atom)

    it "should parse '#t'" $
      runParseFullExpr "#t" `shouldBe` Right (Bool True)

    it "should parse '#f'" $
      runParseFullExpr "#f" `shouldBe` Right (Bool False)

    it "should parse a quoted string" $ property $
      \(PrintableString s) -> runParseFullExpr ('"' : quoteString s ++ "\"")
            `shouldBe` Right (String s)

    it "should parse #\\space characters" $ do
      runParseFullExpr "#\\space" `shouldBe` Right (Character ' ')

    it "should parse #\\newline characters" $ do
      runParseFullExpr "#\\newline" `shouldBe` Right (Character '\n')

    it "should parse a quoted character" $ property $
      \c -> runParseFullExpr ("#\\" ++ characterName c)
            `shouldBe` Right (Character c)

  where characterName ' '  = "space"
        characterName '\n' = "newline"
        characterName c    = c : []

        quoteString ('"' :ss) = "\\\"" ++ quoteString ss
        quoteString ('\\':ss) = "\\\\" ++ quoteString ss
        quoteString (s:ss)    = s : quoteString ss
        quoteString [] = []

canParseLists :: Spec
canParseLists =
  describe "Can parse lists" $ do
    it "should parse a list of numbers" $ property $
      \(ListOfNumbers xs) ->
        let input = '(' : (unwords $ map show xs) ++ ")"
        in runParseFullExpr input  == Right (List $ fmap Number xs)

canParseAnExpr :: Spec
canParseAnExpr =
  describe "Can parse any expression" $ do
    it "should parse non-empty expression" $ property $
      \(vs :: LispVal) -> runParseFullExpr (show vs) == Right vs
