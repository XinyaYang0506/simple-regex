import Data.Set
import Text.ParserCombinators.Parsec


data Risp = Char Char
    | CharSet (Set Char)
    | Func [Risp]
    | Atom String
    | Number Integer

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
    return $ Char x

symbol :: Parser Char
symbol = oneOf "!$%&|*+-/:<=>?@^_~"

parseAtom :: Parser Risp
parseAtom = do
    first <- letter <|> symbol
    rest <- many (letter <|> symbol <|> digit)
    let atom = first:rest
    return (Atom atom)

parseFunc :: Parser Risp
parseFunc =
    between beg end (Func <$> sepEndBy parseExpr spaces)
    where
        beg = char '(' >> skipMany space
        end = skipMany space >> char ')'

parseExpr :: Parser Risp
parseExpr = parseAtom
    <|> parseChar
    <|> parseFunc
    <|> parseNumber

-------------- show------------------
unwordsList :: [Risp] -> String
unwordsList = unwords . Prelude.map showVal
instance Show Risp where show = showVal

showVal :: Risp -> String
-- Char Char
--     | CharSet (Set Char)
--     | Func [Risp]
--     | Atom String
--     | Number Integer
showVal (Char char) = "'" ++ [char] ++ "'"
showVal (Atom name) = name
showVal (Number number) = show number
showVal (Func list) = "(" ++ unwordsList list ++ ")"

-- (specialSyntax "abcd")
-- [abcd]