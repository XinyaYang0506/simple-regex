{-# LANGUAGE DeriveDataTypeable #-}
module Risp where

import Data.Set hiding (foldl)
import Data.Data
import Data.Typeable

data Anchor = StartOfLine | EndOfLine | WordBoundary deriving (Typeable, Data)
data Risp = CharSet (Set Char)
    | Func [Risp]
    | Atom String
    | Number Integer
    | RegExp String
    | Anchor Anchor

-- show
showVal :: Risp -> String
showVal (Anchor anchor) = "@" ++ (showConstr . toConstr) anchor
showVal (CharSet set) = "[" ++ show set ++ "]"
showVal (Atom name) = name
showVal (Number number) = show number
showVal (Func list) = "(" ++ unwordsList list ++ ")"
showVal (RegExp str) = show str

unwordsList :: [Risp] -> String
unwordsList = unwords . Prelude.map showVal
instance Show Risp where show = showVal