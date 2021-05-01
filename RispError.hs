module RispError  where
import Risp
import Text.ParserCombinators.Parsec hiding (State)

data RispError = NumArgs Integer [Risp]
    | TypeMismatch String Risp
    | Parser ParseError
    | BadSpecialForm String Risp
    | NotFunction String String
    | UnboundVar String -- the variable has not been declared
    | VarAlreadyExists String
    | ReservedKeyword String
    | EmptyCharSet
    | Default String

showError :: RispError -> String
showError (NumArgs expected found) = "Expected " ++ show expected ++ " args; found values " ++ unwordsList found
showError (TypeMismatch expected found) = "Invalid type: expected " ++ expected ++ ", found " ++ show found
showError (Parser parseErr) = "Parse error at " ++ show parseErr
showError (BadSpecialForm message form) = message ++ ": " ++ show form
showError (NotFunction message func) = message ++ ": " ++ show func
showError (UnboundVar varname) = "Getting an unbound variable: " ++ varname
showError (VarAlreadyExists varname) = "Already existed var: " ++ varname
showError (ReservedKeyword varname) = "Illegal use of reserved keyword: " ++ varname
showError EmptyCharSet = "CharSet matches 0 characters"
showError (Default message) = show message
instance Show RispError where show = showError

type EitherError = Either RispError

-- get the right value of Either
extractValue :: EitherError a -> a
extractValue (Right val) = val