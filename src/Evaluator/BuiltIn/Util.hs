module Evaluator.BuiltIn.Util (
    numericBinOp
  , boolBinop
  , numBoolBinop
  , strBoolBinop
  , boolBoolBinop
  , unaryOp

  , unpackNum
  , unpackChar
  , unpackStr
  , unpackBool
) where

import Ast (LispVal(..))
import Error (LispError(..), ThrowsError, throwError)

numericBinOp :: (Integer -> Integer -> Integer)
             -> [LispVal]
             -> ThrowsError LispVal
numericBinOp op []         = throwError $ NumArgs 2 []
numericBinOp op params@[_] = throwError $ NumArgs 2 (map show params)
numericBinOp op params     = mapM unpackNum params
                             >>= return . Number . foldl1 op

boolBinop :: (LispVal -> ThrowsError a)
          -> (a -> a -> Bool)
          -> [LispVal]
          -> ThrowsError LispVal
boolBinop unpacker op args =
  if length args /= 2
  then throwError $ NumArgs 2 (map show args)
  else do left <- unpacker $ args !! 0
          right <- unpacker $ args !! 1
          return $ Bool $ left `op` right

numBoolBinop = boolBinop unpackNum
strBoolBinop = boolBinop unpackStr
boolBoolBinop = boolBinop unpackBool

unpackNum :: LispVal -> ThrowsError Integer
unpackNum (Number n) = return n
unpackNum (String str) =
  let parsed = reads str
  in if null parsed
     then throwError $ TypeMismatch "number" str
     else return $ fst $ parsed !! 0
unpackNum (List [n]) = unpackNum n
unpackNum value      = throwError $ TypeMismatch "number" (show value)

unpackChar :: LispVal -> ThrowsError Char
unpackChar (Character v) = return $ v
unpackChar value         = throwError $ TypeMismatch "char" (show value)

unpackStr :: LispVal -> ThrowsError String
unpackStr (Number v) = return $ show v
unpackStr (Float v)  = return $ show v
unpackStr (String s) = return s
unpackStr (Bool v)   = return $ show v
unpackStr value      = throwError $ TypeMismatch "string" (show value)

unpackBool :: LispVal -> ThrowsError Bool
unpackBool (Bool v) = return v
unpackBool value    = throwError $ TypeMismatch "boolean" (show value)

unaryOp :: (LispVal -> LispVal)
        -> [LispVal]
        -> ThrowsError LispVal
unaryOp op []    = throwError $ NumArgs 1 []
unaryOp op [arg] = return $ op arg
unaryOp op args  = throwError $ NumArgs 1 (map show args)
