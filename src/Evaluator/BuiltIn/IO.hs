module Evaluator.BuiltIn.IO (
    makePort
  , closePort
  , readProc
  , writeProc
  , readContents
  , load
  , readAll
) where

import Control.Monad (liftM)
import Control.Monad.IO.Class (liftIO)
import System.IO (Handle, IOMode(..)
                 , openFile, readFile
                 , hClose, hGetLine, hPrint
                 , stdin, stdout)

import Ast (LispVal(..))
import Parser (readExpr, readExprList)
import Error (LispError(..), throwError, IOThrowsError, liftThrows)


makePort :: IOMode -> [LispVal] -> IOThrowsError LispVal
makePort mode [String filename] =
  liftM (Port filename) $ liftIO $ openFile filename mode

closePort :: [LispVal] -> IOThrowsError LispVal
closePort [Port _ port] = liftIO $ hClose port >> (return $ Bool True)
closePort _             = return $ Bool False

readProc :: [LispVal] -> IOThrowsError LispVal
readProc []            = readProc [Port "[stdin]" stdin]
readProc [Port _ port] = (liftIO $ hGetLine port) >>= liftThrows . readExpr
readProc [arg]         = throwError $ TypeMismatch "port" (show arg)
readProc args          = throwError $ NumArgs 1 (map show args)

writeProc :: [LispVal] -> IOThrowsError LispVal
writeProc []                 = throwError $ NumArgs 1 []
writeProc [obj]              = writeProc [obj, Port "[stdout]" stdout]
writeProc [obj, Port _ port] = liftIO $ hPrint port obj >> (return $ Bool True)
writeProc [obj, arg]         = throwError $ TypeMismatch "port" (show arg)
writeProc args               = throwError $ NumArgs 2 (map show args)

readContents :: [LispVal] -> IOThrowsError LispVal
readContents [String filename] = liftM String $ liftIO $ readFile filename
readContents [arg]             = throwError $ TypeMismatch "string" (show arg)
readContents args              = throwError $ NumArgs 1 (map show args)

load :: String -> IOThrowsError [LispVal]
load filename = (liftIO $ readFile filename) >>= liftThrows . readExprList

readAll :: [LispVal] -> IOThrowsError LispVal
readAll [String filename] = liftM List $ load filename
readAll [arg]             = throwError $ TypeMismatch "string" (show arg)
readAll args              = throwError $ NumArgs 1 (map show args)
