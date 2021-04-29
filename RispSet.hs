module RispSet where
import Data.Char
import Data.Set
import Risp

createContiguousSet :: Char -> Char -> Set Char
createContiguousSet start end = fromList $ Prelude.map chr [(ord start)..(ord end)]

lowerLetters = createContiguousSet 'a' 'z'
upperLetters = createContiguousSet 'A' 'Z'
digits = createContiguousSet '0' '9'
whitespace = fromList [' ', '\t', '\r', '\n', '\f', '\v']
word = lowerLetters `union` upperLetters `union` digits `union` singleton '_'

initialCharSets = [[
    ("digits", CharSet $ Positive digits),
    ("lowercase_letters", CharSet $ Positive lowerLetters),
    ("uppercase_letters", CharSet $ Positive upperLetters),
    ("whitespace", CharSet $ Positive whitespace),
    ("word", CharSet $ Positive word),
    ("letters", CharSet $ Positive $ lowerLetters `union` upperLetters)]]
    