module Evaluator (
    eval
  , evalLoad

  , primitiveBindings
) where

import Data.Maybe (isJust)
import Control.Monad (liftM, mapM)
import Control.Monad.IO.Class (liftIO)

import Ast (LispVal(..))
import Error (LispError(..), ThrowsError, throwError, catchError
             , IOThrowsError, liftThrows)
import Evaluator.BuiltIn (primitives, ioPrimitives)
import Evaluator.BuiltIn.SpecialForms as SF
import Evaluator.BuiltIn.IO as IO
import Evaluator.Environment (Env, getVar, setVar, defineVar, bindVars)
import Evaluator.Environment as E


makeFunc :: (Maybe String)
         -> Env
         -> [LispVal]
         -> [LispVal]
         -> IOThrowsError LispVal
makeFunc varargs env params body =
  do paramNames <- sequence
       $ map (\v -> case v of
                      Atom name -> return name
                      arg -> throwError $ BadFunctionArgument (show arg))
             params
     return $ Func paramNames varargs body env
makeNormalFunc = makeFunc Nothing
makeVarArgFunc = makeFunc . Just . show


eval :: Env -> LispVal -> IOThrowsError LispVal
eval env (Atom id) = getVar env id
eval _   v@(Number _) = return v
eval _   v@(Character _) = return v
eval _   v@(String _) = return v
eval _   v@(Bool _) = return v

eval _   (List [Atom "quote", v]) = return v
eval env (List [Atom "if", pred, a, b]) = SF.if_ (eval env) pred a b
eval env (List (Atom "cond" : clauses)) = SF.cond (eval env) clauses
eval env (List (Atom "case" : (key : clauses))) =
  SF.case_ (eval env) key clauses

eval env (List [Atom "set!", Atom var, form]) =
  eval env form >>= setVar env var

eval env (List [Atom "define", Atom var, form]) =
  eval env form >>= defineVar env var
eval env (List (Atom "define" : List (Atom var : params) : body)) =
  makeNormalFunc env params body >>= defineVar env var
eval env (List (Atom "define" : DottedList (Atom var : params) varargs : body)) =
  makeVarArgFunc varargs env params body >>= defineVar env var

eval env (List (Atom "lambda" : List params : body)) =
  makeNormalFunc env params body
eval env (List (Atom "lambda" : DottedList params varargs : body)) =
  makeVarArgFunc varargs env params body
eval env (List (Atom "lambda" : varargs@(Atom _) : body)) =
  makeVarArgFunc varargs env [] body

eval env (List [Atom "load", String filename]) =
  evalLoad env filename

eval env (List (funcExpr : args)) =
  do func <- eval env funcExpr
     argVals <- mapM (eval env) args
     apply func argVals

eval _   badForm =
  throwError $ BadSpecialForm "Unrecognized special form" (show badForm)

evalLoad :: Env -> String -> IOThrowsError LispVal
evalLoad env filename =
  IO.load filename >>= liftM last . mapM (eval env)


apply :: LispVal -> [LispVal] -> IOThrowsError LispVal
apply (PrimitiveFunc _ func) args = liftThrows $ func args

apply (Func params varargs body closure) args
  | (varargs == Nothing && length params == length args)
    || (isJust varargs && length params <= length args) =
    let remaningArgs = drop (length params) args
        evalBody env = liftM last $ mapM (eval env) body
        bindVarArgs (Just argName) env =
          liftIO $ bindVars env [(argName, List $ remaningArgs)]
        bindVarArgs Nothing env = return env
    in
      (liftIO $ bindVars closure $ zip params args)
      >>= bindVarArgs varargs
      >>= evalBody

apply (Func params _ _ _) args =
  throwError $ NumArgs (toInteger $ length params) (map show args)

apply (IOFunc _ func) args = func args

apply arg _ =
  throwError $ NotFunction "Can not call as a function" (show arg)


applyProc :: [LispVal] -> IOThrowsError LispVal
applyProc [func, List args] = apply func args
applyProc (func : args) = apply func args


primitiveBindings :: IO Env
primitiveBindings =
  nullEnv
  >>= (flip bindVars
        $    map (makeFunc PrimitiveFunc) primitives
          ++ map (makeFunc IOFunc) ioPrimitives
          ++ [makeFunc IOFunc ("apply", applyProc)]
      )
  where makeFunc constructor (name, func) = (name, constructor name func)
