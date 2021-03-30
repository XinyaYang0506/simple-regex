{-# LANGUAGE BlockArguments #-}
{-# OPTIONS_GHC -Wno-deferred-type-errors #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE TupleSections #-}
import Data.Set hiding (foldl)
import Text.ParserCombinators.Parsec hiding (State)
import Control.Monad.Except
import Data.Data
import Data.Typeable
import Data.Maybe

import System.Environment
import System.IO
import Data.List
import Control.Monad.State
-- import Extra.Control.Monad.Extra

data Anchor = StartOfLine | EndOfLine | WordBoundary deriving (Typeable, Data)
data Risp = CharSet (Set Char)
    | Func [Risp]
    | Atom String
    | Number Integer
    | RegExp String
    | Anchor Anchor

parseAnchor :: Parser Risp
parseAnchor = do
    char '@'
    x <- try (string "StartOfLine") <|> try (string "EndOfLine") <|> try (string "WordBoundary")
    return $ Anchor case x of
        "StartOfLine" -> StartOfLine
        "EndOfLine" -> EndOfLine
        "WordBoundary" -> WordBoundary

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
parseExpr = parseAnchor
    <|> parseFunc
    <|> parseAtom
    <|> parseChar
    <|> parseNumber


-------------- show------------------
unwordsList :: [Risp] -> String
unwordsList = unwords . Prelude.map showVal
instance Show Risp where show = showVal

showVal :: Risp -> String
showVal (Anchor anchor) = "@" ++ (showConstr . toConstr) anchor
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
    | UnboundVar String -- the variable has not been declared
    | VarAlreadyExists String
    | Default String

showError :: RispError -> String
showError (NumArgs expected found) = "Expected " ++ show expected ++ " args; found values " ++ unwordsList found
showError (TypeMismatch expected found) = "Invalid type: expected " ++ expected ++ ", found " ++ show found
showError (Parser parseErr) = "Parse error at " ++ show parseErr
showError (BadSpecialForm message form) = message ++ ": " ++ show form
showError (NotFunction message func) = message ++ ": " ++ show func
showError (UnboundVar varname) = "Getting an unbound variable: " ++ varname
showError (VarAlreadyExists varname) = "Already existed var: " ++ varname
showError (Default message) = show message
instance Show RispError where show = showError

type ThrowsError = Either RispError

-- get the right value of Either
extractValue :: ThrowsError a -> a
extractValue (Right val) = val

-------------- EVAL (set, and (math)) --------------
eval :: Risp -> StateT EnvStack ThrowsError Risp
-- eval :: Risp -> ThrowsError Risp
eval (Func ((Atom "union"): args)) =
    do
        listOfEvaledArgs <- mapM eval args
        listOfSets <- lift $ mapM extractCharSet listOfEvaledArgs
        let resultSet = unions listOfSets
        return $ CharSet resultSet
eval (Func ((Atom funcName): args)) =
    do
        listOfEvaledArgs <- mapM eval args
        return $ Func (Atom funcName: listOfEvaledArgs)
eval (Func (wrongHead : _)) = lift $ throwError $ TypeMismatch "FuncKeyword/Atom" wrongHead
eval val@(RegExp regExp) = lift $ throwError $ TypeMismatch "not regExp" val
eval x = return x -- maybe should not include Atom

------------ TRANSLATE ------------------
translate :: Risp -> ThrowsError Risp
translate (Anchor StartOfLine) = return $ RegExp "^"
translate (Anchor EndOfLine) = return $ RegExp "$"
translate (Anchor WordBoundary) = return $ RegExp "\\b"
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
     let some = readExpr (head args) >>= \risp -> runStateT (eval risp) initialEnvStack
     let result = some >>= \tuple -> translate (fst tuple)
     putStrLn $ extractValue $ trapError (fmap show result)

type StackFrame = [(String, Risp)]
type EnvStack = [StackFrame]

initialEnvStack :: EnvStack
initialEnvStack = [[]]

defineVar :: String -> Risp -> StateT EnvStack ThrowsError ()
defineVar varName risp =
    do
        alreadyExists <- isBound varName
        if alreadyExists
            then lift (throwError $ VarAlreadyExists varName)
            else do
                topFrame <- pop
                push $ (varName, risp) : topFrame

-- bind several variables in a new stack frame
bindVars :: [(String, Risp)] -> StateT EnvStack ThrowsError()
bindVars bindings = do
    -- let strList = Data.List.map fst bindings
    boolList <- mapM (isBound . fst) bindings
    let firstTrueIndex = elemIndex True boolList
    case firstTrueIndex of
        Just i -> lift (throwError . VarAlreadyExists . fst $ bindings !! i)
        Nothing -> do push bindings



readVar :: String -> StateT EnvStack ThrowsError Risp
readVar varName = StateT \envStack -> case lookup varName $ concat envStack of
    Just val -> return (val, envStack)
    _ -> throwError $ UnboundVar varName

-- idState :: State EnvStack ()
-- idState = state ((),)

--  determine if a given variable is already bound in the environment
-- we search the entire stack because we don't want to allow a user to change
-- the value associated with a variable name
isBound :: String -> StateT EnvStack ThrowsError Bool
isBound varName = StateT \envStack -> return (isJust $ lookup varName $ concat envStack, envStack)

-- redefining variable
-- let a = 1;
-- let a = 2;
-- (define a 1)
-- (define a 2)
-- (define (lambda ... (let (a 5) (lambda .. (...)))))
-- (let (a 7) (call it))
-- mutating variable
-- let mut b = &mut 1;
-- *b = 2;

-- mutating reference
-- let mut c = &mut 2;
-- c = &mut 3;

-- s -> s a
-- s should be a list like [(name, value)]
-- if we want to read a variable, a would be the value
-- imaginary stack: [[(name, value)]]

--

pop :: StateT EnvStack ThrowsError StackFrame
pop = StateT $ \(x:xs) -> return (x,xs)

push :: StackFrame -> StateT EnvStack ThrowsError ()
push a = StateT $ \xs -> return ((), a:xs)
