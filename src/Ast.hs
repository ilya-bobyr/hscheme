{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE DeriveGeneric #-}

module Ast (
    LispVal(..)

  -- For testing
  , AtomName(..)
) where

import Data.IORef (IORef)
import Control.Monad (ap)
import GHC.Generics (Generic(..))
import Numeric (showFFloat)
import Test.QuickCheck (genericShrink)
import Test.QuickCheck.Arbitrary (Arbitrary(..), arbitraryPrintableChar)
import Test.QuickCheck.Gen (Gen, choose, elements, listOf, listOf1, oneof,
                            sized, resize, vectorOf)
import System.IO (Handle)


import Error (ThrowsError, IOThrowsError)


type Env = IORef [(String, IORef LispVal)]

data LispVal = Atom String
             | List [LispVal]
             | DottedList [LispVal] LispVal
             | Number Integer
             | Float Float
             | Character Char
             | String String
             | Bool Bool
             | PrimitiveFunc String ([LispVal] -> ThrowsError LispVal)
             | Func { params :: [String]
                    , vararg :: (Maybe String)
                    , body :: [LispVal]
                    , closure :: Env
                    }
             | IOFunc String ([LispVal] -> IOThrowsError LispVal)
             | Port String Handle
             deriving (Generic)

instance Eq LispVal where
  (Atom x) == (Atom y) = x == y
  (List x) == (List y) = x == y
  (DottedList x1 x2) == (DottedList y1 y2) = x1 == y1 && x2 == y2
  (Number x) == (Number y) = x == y
  (Float x) == (Float y) = x == y
  (Character x) == (Character y) = x == y
  (String x) == (String y) = x == y
  (Bool x) == (Bool y) = x == y
  (PrimitiveFunc x _) == (PrimitiveFunc y _) = x == y
  (Func paramsX varargX bodyX closureX) == (Func paramsY varargY bodyY closureY) =
    paramsX == paramsY
    && varargX == varargY
    && bodyX == bodyY
    && closureX == closureY
  (IOFunc x _) == (IOFunc y _) =
    (not (null x) || not (null y)) && x == y
  (Port _ x) == (Port _ y) = x == y

instance Show LispVal where
  show (Atom v) = v
  show (List vs) = '(' : (unwords $ map show vs) ++ ")"
  show (DottedList vs l) = '(' : (unwords $ map show vs)
                               ++ " . " ++ (show l) ++ ")"
  show (Number v) = show v
  show (Float v) = showFFloat Nothing v ""

  show (Character c) = "#\\" ++ (characterByName c)
    where characterByName ' '  = "space"
          characterByName '\n' = "newline"
          characterByName v    = [v]

  show (String s) = '"' : quoteString s ++ "\""
    where
        quoteString ('"' :ss) = "\\\"" ++ quoteString ss
        quoteString ('\\':ss) = "\\\\" ++ quoteString ss
        quoteString (c:ss)    = c : quoteString ss
        quoteString [] = []

  show (Bool True) = "#t"
  show (Bool False) = "#f"

  show (PrimitiveFunc name _) = "<primitive: " ++ name ++ ">"
  show (Func { params = args
             , vararg = varargs
             , body = body
             , closure = env}) =
    "(lambda (" ++ unwords (map show args)
    ++ (case varargs of
          Nothing -> ""
          Just arg -> " . " ++ arg)
    ++ ") ...)"
  show (IOFunc name _) =
    if not $ null name
    then "<io primitive: " ++ name ++ ">"
    else "<io primitive>"
  show (Port desc _) =
    if not $ null desc
    then "<io port: " ++ desc ++ ">"
    else "<io port>"

newtype AtomName = AtomName String
                 deriving (Show, Generic)

atomName :: AtomName -> String
atomName (AtomName s) = s

instance Arbitrary AtomName where
  arbitrary = fmap AtomName $ sized $ \n -> do
      len <- choose (0, n - 1)
      first <- elements initial
      rest <- vectorOf len $ elements subsequent
      return (first : rest)
    where
      letter = ['A'..'Z'] ++ ['a'..'z']
      digit = ['0'..'9']
      specialInitial = "!$%&*/:<=>?_~"
      initial = letter ++ specialInitial
      specialSubsequent = "+-.@"
      subsequent = initial ++ digit ++ specialSubsequent

  shrink = genericShrink

instance Arbitrary LispVal where
  arbitrary =
    let alternatives 0 =
          [ (Atom . atomName) `fmap` (arbitrary :: Gen AtomName)
          , Number `fmap` arbitrary
          , Float `fmap` arbitrary
          , Character `fmap` arbitraryPrintableChar
          , String `fmap` listOf arbitraryPrintableChar
          , Bool `fmap` arbitrary
          ]
        alternatives n | n > 0 =
          let smaller = resize (n `div` 2) arbitrary
          in alternatives 0
             ++ [ List `fmap` listOf smaller
                , return DottedList `ap` (listOf smaller) `ap` smaller
                ]

        value n = oneof (alternatives n)

    in sized value

  {- genericShrink does not work with LispVal after I have added the
     PrimitiveFunc and the Func constructores.  The problem is that
     genericShrink requires an instance of CoArbitrary for LispVal as
     it contains function values.  And CoArbitrary requires Aribitrary
     instance for all the function argument types.  The problem arises
     with Env, that is an IORef.  There is no way to construct an
     IORef outside of an IO context.  So I can not define an Arbitrary
     instance for the IORef.

     As I do not actaully construct PrimitiveFunc or Func instances,
     it is possible to write a shrink implementation, that would just
     omit those cases.  Unfortunately, I do not think I can use
     genericShrink, meaning I would have to write shrinking manually
     for all the cases.  It is a bit more work than I would like to do
     at the moment.  So, I'll just disable shrinking for now.
  -}
  -- shrink = genericShrink
