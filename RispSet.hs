module RispSet where
import Data.Char
import Data.Set
import Risp

createContiguousSet :: Char -> Char -> Set Char
createContiguousSet start end = fromList $ Prelude.map chr [(ord start)..(ord end)]

lowerLetters = createContiguousSet 'a' 'z'
upperLetters = createContiguousSet 'A' 'Z'
whitespace = fromList [' ', '\t', '\r', '\n', '\f', '\v']

initialCharSets = [[
    ("digits", CharSet $ createContiguousSet '0' '9'),
    ("lowercase_letters", CharSet lowerLetters),
    ("uppercase_letters", CharSet upperLetters),
    ("whitespace", CharSet whitespace),
    ("letters", CharSet $ lowerLetters `union` upperLetters)]]
