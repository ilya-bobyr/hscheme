module Main where

import Prelude hiding (readFile)

import Control.Monad (liftM)
import System.Environment (getArgs)
import System.IO (hFlush, hPutStrLn, stdout, stderr)

import Ast (LispVal(Atom, List, String))
import Parser (readExpr)
import Evaluator (eval, evalLoad, primitiveBindings)
import Evaluator.Environment (Env, bindVars)
import Error (trapError, extractValue, liftThrows, runIOThrows)


flushStr :: String -> IO ()
flushStr str = putStr str >> hFlush stdout

readPrompt :: String -> IO String
readPrompt prompt = flushStr prompt >> getLine

evalString :: Env -> String -> IO String
evalString env expr =
  runIOThrows
  $ liftM show
  $ (liftThrows $ readExpr expr) >>= eval env

evalAndPrint :: Env -> String -> IO ()
evalAndPrint env expr = evalString env expr >>= putStrLn

runOne :: [String] -> IO ()
runOne (path : args) = do
  env <- primitiveBindings
         >>= flip bindVars [("args", List $ map String args)]
  (runIOThrows
   $ liftM show
   $ evalLoad env path)
    >>= hPutStrLn stderr

until_ :: Monad m
       => (a -> Bool)
       -> m a
       -> (a -> m ())
       -> m ()
until_ pred prompt action = do
  result <- prompt
  if pred result
    then return ()
    else action result >> until_ pred prompt action

runRepl :: IO ()
runRepl =
  primitiveBindings
  >>= until_ (== "") (readPrompt "Lisp> ") . evalAndPrint


main :: IO ()
main = do
  args <- getArgs
  if null args
    then runRepl
    else runOne args
