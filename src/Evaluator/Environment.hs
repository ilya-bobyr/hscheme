module Evaluator.Environment (
    Env
  , nullEnv
  , getVar
  , setVar
  , defineVar
  , bindVars
) where

import Data.IORef (IORef, newIORef, readIORef, writeIORef)
import Control.Monad (liftM)
import Control.Monad.Except (ExceptT, runExceptT, throwError)
import Control.Monad.IO.Class (liftIO)

import Ast (LispVal)
import Error (LispError(..), ThrowsError, trapError, extractValue
             , IOThrowsError)


type Env = IORef [(String, IORef LispVal)]

nullEnv :: IO Env
nullEnv = newIORef []

isBound :: Env -> String -> IO Bool
isBound envRef var =
  readIORef envRef
  >>= return . maybe False (const True) . lookup var

getVar :: Env -> String -> IOThrowsError LispVal
getVar envRef var =
  do env <- liftIO $ readIORef envRef
     maybe
       (throwError $ UnboundVar "Getting an unbound variable" var)
       (liftIO . readIORef)
       (lookup var env)

setVar :: Env -> String -> LispVal -> IOThrowsError LispVal
setVar envRef var value =
  do env <- liftIO $ readIORef envRef
     maybe
       (throwError $ UnboundVar "Setting an unbound variable" var)
       (liftIO . (flip writeIORef value))
       (lookup var env)
     return value

defineVar :: Env -> String -> LispVal -> IOThrowsError LispVal
defineVar envRef var value =
  do alreadyDefined <- liftIO $ isBound envRef var
     if alreadyDefined
       then setVar envRef var value
       else liftIO $
            do valueRef <- newIORef value
               env <- readIORef envRef
               writeIORef envRef ((var, valueRef) : env)
               return value

bindVars :: Env -> [(String, LispVal)] -> IO Env
bindVars envRef bindings = do
  env <- readIORef envRef
  liftM (++ env) (mapM addBinding bindings) >>= newIORef
  where addBinding (var, value) = do
          ref <- newIORef value
          return (var, ref)
