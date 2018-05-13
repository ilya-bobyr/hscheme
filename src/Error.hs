module Error (
    LispError(..)
  , ThrowsError
  , trapError
  , extractValue

  , throwError
  , catchError

  , IOThrowsError
  , liftThrows
  , runIOThrows
) where

import Control.Monad.Except
import Text.Parsec (ParseError)


data LispError = NumArgs Integer [String]
               | TypeMismatch String String
               | Parser ParseError
               | BadSpecialForm String String
               | NotFunction String String
               | BadFunctionArgument String
               | FailedBuiltIn String String
               | UnboundVar String String
               | Default String

showError :: LispError -> String
showError (NumArgs expected found) =
  "Expected " ++ show expected ++ " args; found values: "
  ++ (unwords found)
showError (TypeMismatch expected found) =
  "Invalid type: expected " ++ expected ++ ", found " ++ found
showError (Parser parseErr) = "Parse error at " ++ show parseErr
showError (BadSpecialForm message form) = message ++ ": " ++ form
showError (NotFunction message func) = message ++ ": " ++ show func
showError (BadFunctionArgument arg) = "Invalid function argument: " ++ arg
showError (FailedBuiltIn func message) = "Error: " ++ func ++ ": " ++ message
                                         ++ " [" ++ func ++ "]"
showError (UnboundVar message varName) = message ++ ": " ++ varName
showError (Default message) = message

instance Show LispError where
  show = showError

type ThrowsError = Either LispError

trapError action = catchError action (return . show)

extractValue :: ThrowsError a -> a
extractValue (Right val) = val

type IOThrowsError = ExceptT LispError IO

liftThrows :: ThrowsError a -> IOThrowsError a
liftThrows (Left err) = throwError err
liftThrows (Right val) = return val

runIOThrows :: IOThrowsError String -> IO String
runIOThrows action =
  runExceptT (trapError action) >>= return . extractValue
