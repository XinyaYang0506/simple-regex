{-# LANGUAGE DeriveDataTypeable #-}
module Risp where

import Data.Set hiding (foldl, map)
import Data.Data
import Data.Typeable

type StackFrame = [(String, Risp)]
type EnvStack = [StackFrame]

data Anchor = StartOfLine | EndOfLine | WordBoundary deriving (Typeable, Data)
data Risp = CharSet (Set Char)
    | List [Risp]
    | FuncDefinition{ params :: [String], closure :: EnvStack, body :: Risp}
    | Atom String
    | Number Integer
    | RegExp String
    | Anchor Anchor
    | String String

-- show
showVal :: Risp -> String
showVal (Anchor anchor) = "@" ++ (showConstr . toConstr) anchor
showVal (CharSet set) = "[" ++ show set ++ "]"
showVal (Atom name) = name
showVal (Number number) = show number
showVal (List list) = "(" ++ unwordsList list ++ ")"
showVal FuncDefinition { params = params, closure = closure, body = body } =
    "(lambda (" ++ unwords (map show params) ++ ") ...)"
showVal (RegExp str) = str
showVal (String str) = str

unwordsList :: [Risp] -> String
unwordsList = unwords . Prelude.map showVal
instance Show Risp where show = showVal