module Evaluator.BuiltIn.SpecialForms (
    if_
  , cond
  , case_
) where

import Control.Monad (liftM)

import Ast (LispVal(..))
import Error (LispError(..), ThrowsError, throwError, IOThrowsError)
import Evaluator.BuiltIn (eqvImpl)


type Eval = LispVal -> IOThrowsError LispVal

if_ :: Eval -> LispVal -> LispVal -> LispVal -> IOThrowsError LispVal
if_ eval pred a b =
  do result <- eval pred
     case result of
       Bool True -> eval a
       Bool False -> eval b
       otherwise  -> throwError $ TypeMismatch "boolean" (show result)


cond :: Eval -> [LispVal] -> IOThrowsError LispVal
cond eval clauses =
  let
    checkClause :: [LispVal] -> IOThrowsError (Maybe LispVal)
    checkClause [test, Atom "=>", funcExpr] =
      do result <- eval test
         func <- eval funcExpr
         case result of
           Bool False -> return Nothing
           otherwise -> do result <- eval $ List [func
                                                 , List [Atom "quote", result]]
                           return $ Just result
    checkClause (test : statements@(_:_)) =
      do result <- eval test
         case result of
           Bool False -> return Nothing
           otherwise -> do result <- liftM last $ mapM eval statements
                           return $ Just result
    checkClause [test] =
      do result <- eval test
         case result of
           Bool False -> return Nothing
           otherwise -> return $ Just result
    checkClause [] =
      throwError $ BadSpecialForm "'cond' clause is empty"
                                  (show $ List [])

    processClausess :: [LispVal] -> IOThrowsError LispVal
    processClausess [List [Atom "else"]] =
      throwError
      $ BadSpecialForm "'else' clause with no statements"
                       (show $ List [Atom "else"])
    processClausess [List (Atom "else" : statements)] =
      liftM last $ mapM eval statements
    processClausess (List clause : rest) =
      do clauseRes <- checkClause clause
         case clauseRes of
           Nothing -> processClausess rest
           Just result -> return result
    processClausess [] = return $ Bool False
    processClausess (unexpected:_) =
      throwError $ BadSpecialForm "Expecting a 'cond' clause" (show unexpected)
  in
    processClausess clauses

case_ :: Eval -> LispVal -> [LispVal] -> IOThrowsError LispVal
case_ eval keyExpr clauses =
  let
    checkClause :: LispVal -> [LispVal] -> [LispVal] -> IOThrowsError (Maybe LispVal)

    checkClause _ tests [] =
      throwError $ BadSpecialForm "'case' clause with no sttements"
                   (show $ List [List tests])

    checkClause key tests statements =
      let match = or $ map (\test -> eqvImpl key test) tests
      in if match
         then do result <- liftM last $ mapM eval statements
                 return $ Just result
         else do return Nothing

    processClausess :: LispVal -> [LispVal] -> IOThrowsError LispVal
    processClausess _ [List [Atom "else"]] =
      throwError
      $ BadSpecialForm "'else' clause with no statements"
                       (show $ List [Atom "else"])
    processClausess _ [List (Atom "else" : statements)] =
      liftM last $ mapM eval statements
    processClausess key (List (List tests : statements) : rest) =
      do clauseRes <- checkClause key tests statements
         case clauseRes of
           Nothing -> processClausess key rest
           Just result -> return result
    processClausess _ [] = return $ Bool False
    processClausess _ (unexpected:_) =
      throwError $ BadSpecialForm "Expecting a 'case' clause" (show unexpected)
  in do key <- eval keyExpr
        processClausess key clauses
