{-# LANGUAGE BlockArguments #-}
{-# OPTIONS_GHC -Wno-deferred-type-errors #-}
{-# LANGUAGE ExistentialQuantification #-}
import Data.Set hiding (foldl)
import Text.ParserCombinators.Parsec
import Control.Monad.Except


import System.Environment
import System.IO
import Data.List

data Risp = CharSet (Set Char)
    | Func [Risp]
    | Atom String
    | Number Integer
    | RegExp String

parseNumber :: Parser Risp
parseNumber = Number . read <$> many digit

escapedChars :: Parser Char
escapedChars = do
    char '\\' -- one backslash
    x <- oneOf "\\'nrt" --backslash, single quote, n, r or t
    return $ case x of
        '\\' -> '\\'
        '\'' -> '\''
        'n' -> '\n'
        'r' -> '\r'
        't' -> '\t'

parseChar :: Parser Risp
parseChar = do
    char '\'' --single quote
    x <- escapedChars <|> noneOf "'\\"
    char '\'' --single quote
    return $ CharSet $ singleton x

symbol :: Parser Char
symbol = oneOf "!$%&|*+-/:<=>?@^_~" -- ~

parseAtom :: Parser Risp
parseAtom = do
    first <- letter <|> symbol
    rest <- many (letter <|> symbol <|> digit)
    let atom = first:rest
    return (Atom atom)

spaces1 :: Parser ()
spaces1 = skipMany1 space

parseFunc :: Parser Risp
parseFunc =
    between beg end (Func <$> sepEndBy parseExpr spaces1)
    where
        beg = char '(' >> skipMany space
        end = skipMany space >> char ')'

parseExpr :: Parser Risp
parseExpr = parseFunc
    <|> parseAtom
    <|> parseChar
    <|> parseNumber
    -- <|> 


-------------- show------------------
unwordsList :: [Risp] -> String
unwordsList = unwords . Prelude.map showVal
instance Show Risp where show = showVal

showVal :: Risp -> String
showVal (CharSet set) = "[" ++ show set ++ "]"
showVal (Atom name) = name
showVal (Number number) = show number
showVal (Func list) = "(" ++ unwordsList list ++ ")"
showVal (RegExp str) = show str

------------- Error --------------
data RispError = NumArgs Integer [Risp]
    | TypeMismatch String Risp
    | Parser ParseError
    | BadSpecialForm String Risp
    | NotFunction String String
    | UnboundVar String String -- the variable has not been declared
    | Default String

showError :: RispError -> String
showError (NumArgs expected found) = "Expected " ++ show expected ++ " args; found values " ++ unwordsList found
showError (TypeMismatch expected found) = "Invalid type: expected " ++ expected ++ ", found " ++ show found
showError (Parser parseErr) = "Parse error at " ++ show parseErr
showError (BadSpecialForm message form) = message ++ ": " ++ show form
showError (NotFunction message func) = message ++ ": " ++ show func
showError (UnboundVar message varname) = message ++ ": " ++ varname
showError (Default message) = show message
instance Show RispError where show = showError

type ThrowsError = Either RispError

-- get the right value of Either
extractValue :: ThrowsError a -> a
extractValue (Right val) = val

-------------- EVAL (set, and (math)) --------------
eval :: Risp -> ThrowsError Risp
eval (Func ((Atom "union"): args)) =
    do
        listOfEvaledArgs <- mapM eval args
        listOfSets <- mapM extractCharSet listOfEvaledArgs
        let resultSet = unions listOfSets
        return $ CharSet resultSet
eval (Func ((Atom funcName): args)) =
    do
        listOfEvaledArgs <- mapM eval args
        return $ Func (Atom funcName: listOfEvaledArgs)
eval (Func (wrongHead : _)) = throwError $ TypeMismatch "FuncKeyword/Atom" wrongHead
eval val@(RegExp regExp) = throwError $ TypeMismatch "not regExp" val
eval x = return x -- maybe should not include Atom

------------ TRANSLATE ------------------
translate :: Risp -> ThrowsError Risp
translate val@(Number num) = throwError $ TypeMismatch "not number" val
translate (CharSet charSet) = return $ RegExp $ "[" ++ toAscList charSet ++ "]" -- TODO: add escape characters
translate val@(Atom atom) = throwError $ TypeMismatch "not atom" val
translate (Func ((Atom "concat") : args)) =
    do
        listOfTranslatedArgs <- mapM translate args
        listOfRegExp <- mapM extractRegExp listOfTranslatedArgs
        let concatedString = concat listOfRegExp
        return $ RegExp $ "(" ++ concatedString ++ ")"
translate (Func [Atom "repeatRange", pattrn, Number min, Number max]) =
    do
        translatedPattern <- translate pattrn
        translatedString <- extractRegExp translatedPattern
        return $ RegExp $ "(" ++ translatedString ++ "{" ++ show min ++ "," ++ show max ++ "}" ++ ")"
translate (Func ((Atom "or") : args)) =
    do
        listOfTranslatedArgs <- mapM translate args
        listOfRegExp <- mapM extractRegExp listOfTranslatedArgs
        return $ RegExp $ "(" ++ intercalate "|" listOfRegExp ++ ")"

-- translate obj = return obj

extractRegExp :: Risp -> ThrowsError String
extractRegExp (RegExp string) = return string
extractRegExp notRegExp = throwError $ TypeMismatch "RegExp" notRegExp


extractCharSet :: Risp -> ThrowsError (Set Char)
extractCharSet (CharSet set) = return set
extractCharSet notCharSet = throwError $ TypeMismatch "CharSet" notCharSet


----------- MAIN ---------------
readExpr :: String -> ThrowsError Risp
readExpr input = case parse parseExpr "risp" input of
     Left err -> throwError $ Parser err
     Right val -> return val

trapError :: (MonadError a m, Show a) => m String -> m String
trapError action = catchError action (return . show)

main :: IO ()
main = do
     args <- getArgs
     let evaled = fmap show $ readExpr (head args) >>= eval >>= translate
     putStrLn $ extractValue $ trapError evaled

