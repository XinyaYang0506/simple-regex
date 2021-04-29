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
    ("digits", CharSet digits),
    ("lowercase_letters", CharSet lowerLetters),
    ("uppercase_letters", CharSet upperLetters),
    ("whitespace", CharSet whitespace),
    ("word", CharSet word),
    ("letters", CharSet $ lowerLetters `union` upperLetters)]]
    