{-# LANGUAGE ExistentialQuantification #-}

module Evaluator.BuiltIn (
    isSymbol
  , isList
  , isInteger
  , isReal
  , isNumber
  , isChar
  , isBoolean
  , symbolToString
  , stringToSymbol
  , car
  , cdr
  , cons
  , eqv
  , eqvImpl
  , equal

  , primitives
  , ioPrimitives
) where

import Control.Monad (liftM)
import System.IO (IOMode(..))

import Ast (LispVal(..))
import Error (LispError(..), ThrowsError, throwError, catchError
             , IOThrowsError, liftThrows)
import Evaluator.BuiltIn.IO as IO
import Evaluator.BuiltIn.String as S
import Evaluator.BuiltIn.Util (unpackNum, unpackStr, unpackBool
                              , numericBinOp, numBoolBinop, strBoolBinop
                              , boolBoolBinop, unaryOp)
import Evaluator.Environment (Env, nullEnv, bindVars)


isSymbol :: LispVal -> LispVal
isSymbol (Atom _) = Bool True
isSymbol _        = Bool False

isList :: LispVal -> LispVal
isList (List _)                 = Bool True
isList (DottedList _ (List [])) = Bool True
isList _                        = Bool False

isInteger :: LispVal -> LispVal
isInteger (Number _) = Bool True
isInteger (Float v)  = Bool $ v == fromIntegral (round v)
isInteger _          = Bool False

isReal :: LispVal -> LispVal
isReal (Number _) = Bool True
isReal (Float v)  = Bool True
isReal _          = Bool False

isNumber :: LispVal -> LispVal
isNumber (Number _) = Bool True
isNumber (Float _)  = Bool True
isNumber _          = Bool False

isChar :: LispVal -> LispVal
isChar (Character _) = Bool True
isChar _             = Bool False

isBoolean :: LispVal -> LispVal
isBoolean (Bool _) = Bool True
isBoolean _        = Bool False

symbolToString :: LispVal -> LispVal
symbolToString (Atom v) = String v
symbolToString _        = Bool False

stringToSymbol (String v) = Atom v
stringToSymbol _          = Bool False

car :: [LispVal] -> ThrowsError LispVal
car [List (x : xs)]         = return x
car [DottedList (x : xs) _] = return x
car [arg] = throwError $ TypeMismatch "pair" (show arg)
car args  = throwError $ NumArgs 1 (map show args)

cdr :: [LispVal] -> ThrowsError LispVal
cdr [List (x : xs)]         = return $ List xs
cdr [DottedList [_] x]      = return x
cdr [DottedList (_ : xs) z] = return $ DottedList xs z
cdr [arg] = throwError $ TypeMismatch "pair" (show arg)
cdr args  = throwError $ NumArgs 1 (map show args)

cons :: [LispVal] -> ThrowsError LispVal
cons [x, List xs]         = return $ List $ x : xs
cons [x, DottedList xs z] = return $ DottedList (x : xs) z
cons [x, y]               = return $ DottedList [x] y
cons args = throwError $ NumArgs 2 (map show args)

eqvImpl :: LispVal -> LispVal -> Bool
eqvImpl (Atom x) (Atom y)           = x == y
eqvImpl (List x) (List y) =
  (length x == length y) && (all (uncurry eqvImpl) $ zip x y)
eqvImpl (DottedList xs x) (DottedList ys y) =
  eqvImpl (List $ xs ++ [x]) (List $ ys ++ [y])
eqvImpl (Number x) (Number y)       = x == y
eqvImpl (Float x) (Float y)         = x == y
eqvImpl (Character x) (Character y) = x == y
eqvImpl (String x) (String y)       = x == y
eqvImpl (Bool x) (Bool y)           = x == y
eqvImpl _ _                         = False

eqv :: [LispVal] -> ThrowsError LispVal
eqv [x, y] = return $ Bool $ eqvImpl x y
eqv args = throwError $ NumArgs 2 (map show args)

data Unpacker = forall a. Eq a => AnyUnpacker (LispVal -> ThrowsError a)

unpackEquals :: LispVal -> LispVal -> Unpacker -> ThrowsError Bool
unpackEquals x y (AnyUnpacker unpacker) =
  do unpackedX <- unpacker x
     unpackedY <- unpacker y
     return $ unpackedX == unpackedY
  `catchError` (const $ return False)

equal :: [LispVal] -> ThrowsError LispVal
equal [(List x), (List y)] =
  return $ Bool $ (length x == length y)
                  && (all equalPair $ zip x y)
  where equalPair (a, b) = case equal [a, b] of
                             Left err -> False
                             Right (Bool val) -> val
equal [(DottedList xs x), (DottedList ys y)] =
  equal [List $ xs ++ [x], List $ ys ++ [y]]
equal [x, y] = do
  primitiveEqual <- liftM or $ mapM (unpackEquals x y) [
      AnyUnpacker unpackNum
    , AnyUnpacker unpackStr
    , AnyUnpacker unpackBool
    ]
  return $ Bool $ (primitiveEqual || eqvImpl x y)
equal args = throwError $ NumArgs 2 (map show args)


primitives :: [(String, [LispVal] -> ThrowsError LispVal)]
primitives = [
    ("=", numBoolBinop (==))
  , ("<", numBoolBinop (<))
  , (">", numBoolBinop (>))
  , ("/=", numBoolBinop (/=))
  , (">=", numBoolBinop (>=))
  , ("<=", numBoolBinop (<=))

  , ("&&", boolBoolBinop (&&))
  , ("||", boolBoolBinop (||))

  , ("+", numericBinOp (+))
  , ("-", numericBinOp (-))
  , ("*", numericBinOp (*))
  , ("/", numericBinOp div)
  , ("mod", numericBinOp mod)
  , ("quotient", numericBinOp quot)
  , ("remainder", numericBinOp rem)

  , ("symbol?", unaryOp isSymbol)
  , ("list?", unaryOp isList)
  , ("integer?", unaryOp isInteger)
  , ("real?", unaryOp isReal)
  , ("number?", unaryOp isNumber)
  , ("char?", unaryOp isChar)
  , ("boolean?", unaryOp isBoolean)

  -- 6.3.3  Symbols

  , ("symbol->string", unaryOp symbolToString)
  , ("string->symbol", unaryOp stringToSymbol)

  -- 6.3.5  Strings

  , ("string?", unaryOp S.isString)
  , ("make-string", S.makeString)
  , ("string", S.string)
  , ("string-length", S.stringLength)
  , ("string-ref", S.stringRef)
  -- , ("string-set!")

  , ("string=?", strBoolBinop (==))
  , ("string-ci=", S.strCiBoolBinop (==))
  , ("string<?", strBoolBinop (<))
  , ("string>?", strBoolBinop (>))
  , ("string<=?", strBoolBinop (<=))
  , ("string>=?", strBoolBinop (>=))
  , ("string-ci<?", S.strCiBoolBinop (<))
  , ("string-ci>?", S.strCiBoolBinop (>))
  , ("string-ci<=?", S.strCiBoolBinop (<=))
  , ("string-ci>=?", S.strCiBoolBinop (>=))

  , ("substring", S.substring)
  , ("string-append", S.stringAppend)

  , ("string->list", S.stringToList)
  , ("list->string", S.listToString)

  , ("string-copy", S.stringCopy)
  -- , ("string-fill!")

  , ("car", car)
  , ("cdr", cdr)
  , ("cons", cons)

  , ("eq?", eqv)
  , ("eqv?", eqv)
  , ("equal?", equal)
  ]

ioPrimitives :: [(String, [LispVal] -> IOThrowsError LispVal)]
ioPrimitives = [
  -- "apply" is defined in the Evaluator.hs
    ("open-input-file", IO.makePort ReadMode)
  , ("open-output-file", IO.makePort WriteMode)
  , ("close-input-port", IO.closePort)
  , ("close-output-port", IO.closePort)
  , ("read", IO.readProc)
  , ("write", IO.writeProc)
  , ("read-contents", IO.readContents)
  , ("read-all", IO.readAll)
  ]
