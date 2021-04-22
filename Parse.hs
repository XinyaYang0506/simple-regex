{-# LANGUAGE BlockArguments #-}
module Parse (readExpr) where
import Risp
import RispError
import Control.Monad.Except
import Text.ParserCombinators.Parsec hiding (State)
import Data.Set

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

readExpr :: String -> EitherError Risp
readExpr input = case parse parseExpr "risp" input of
        Left err -> throwError $ Parser err
        Right val -> return val