module Evaluator.BuiltIn.String (
    isString
  , makeString
  , string
  , stringLength
  , stringRef
  , strCiBoolBinop
  , substring
  , stringAppend
  , stringToList
  , listToString
  , stringCopy
) where

import Data.Char (toLower)
import Control.Applicative ((<$>))
import Control.Monad (liftM, sequence)
import Text.Printf (printf)

import Ast (LispVal(..))
import Error (LispError(..), ThrowsError, throwError, catchError)
import Evaluator.BuiltIn.Util (unpackNum, unpackChar, unpackStr)


isString :: LispVal -> LispVal
isString (String _) = Bool True
isString _          = Bool False

makeString :: [LispVal] -> ThrowsError LispVal
makeString [kArg] = do
  k <- unpackNum kArg
  return $ String $ replicate (fromInteger k) '\0'
makeString [kArg, charArg] = do
  k <- unpackNum kArg
  char <- unpackChar charArg
  return $ String $ replicate (fromInteger k) char
makeString args = throwError $ NumArgs 2 (map show args)

string :: [LispVal] -> ThrowsError LispVal
string charsArg = do
  liftM String $ sequence $ map unpackChar charsArg

stringLength :: [LispVal] -> ThrowsError LispVal
stringLength [strArg] = do
  str <- unpackStr strArg
  return $ Number $ toInteger $ length str
stringLength args = throwError $ NumArgs 1 (map show args)

stringRef :: [LispVal] -> ThrowsError LispVal
stringRef [strArg, kArg] = do
  str <- unpackStr strArg
  k <- liftM fromIntegral $ unpackNum kArg
  let strLen = length str
  if k < 0 || k >= strLen
    then throwError $ FailedBuiltIn "string-ref"
         $ printf "number must be between 0 and %d, but got %d"
         (strLen - 1) k
    else return $ Character $ str !! k

strCiBoolBinop :: (String -> String -> Bool)
               -> [LispVal]
               -> ThrowsError LispVal
strCiBoolBinop op args =
  if length args /= 2
  then throwError $ NumArgs 2 (map show args)
  else do left <- liftM (map toLower) $ unpackStr $ args !! 0
          right <- liftM (map toLower) $ unpackStr $ args !! 1
          return $ Bool $ left `op` right

substring :: [LispVal] -> ThrowsError LispVal
substring [strArg, startArg, endArg] = do
  str <- unpackStr strArg
  start <- liftM fromIntegral $ unpackNum startArg
  end <- liftM fromIntegral $ unpackNum endArg
  let strLen = length str
      result | start < 0 = throwError $ FailedBuiltIn "substring"
                           $ printf "start too small: %d" start
             | end > strLen = throwError $ FailedBuiltIn "substring"
                              $ printf "end too big: %d" end
             | start > end = throwError $ FailedBuiltIn "substring"
                             $ printf "not start <= end: %d, %d" start end
             | otherwise = return $ String $ take (end - start) $ drop start str
  result

stringAppend :: [LispVal] -> ThrowsError LispVal
stringAppend args = String <$> concat <$> mapM unpackStr args

stringToList :: [LispVal] -> ThrowsError LispVal
stringToList [strArg] = do
  str <- unpackStr strArg
  return $ List $ map Character str
stringToList args = throwError $ NumArgs 1 (map show args)

listToString :: [LispVal] -> ThrowsError LispVal
listToString [List listArg] = do
  String <$> mapM unpackChar listArg
listToString args = throwError $ NumArgs 1 (map show args)

stringCopy :: [LispVal] -> ThrowsError LispVal
stringCopy [strArg] = return strArg
stringCopy args = throwError $ NumArgs 1 (map show args)
