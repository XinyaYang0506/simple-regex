{-# LANGUAGE DeriveDataTypeable #-}
module Risp where

import Data.Set hiding (foldl, map)
import Data.Data
import Data.Typeable

type StackFrame = [(String, Risp)]
type EnvStack = [StackFrame]

data Anchor = StartOfLine | EndOfLine | WordBoundary deriving (Typeable, Data, Eq)
data Risp = CharSet EitherCharSet
    | List [Risp]
    | FuncDefinition{ params :: [String], closure :: EnvStack, body :: Risp}
    | Atom String
    | Number Integer
    | RegExp String
    | Anchor Anchor
    | String String
    | CaptureGroupName String
    deriving Eq

data EitherCharSet = Positive (Set Char) | Negative (Set Char) deriving Eq

-- show
showVal :: Risp -> String
showVal (Anchor anchor) = "@" ++ (showConstr . toConstr) anchor
showVal (CharSet (Positive set)) = "[" ++ show set ++ "]"
showVal (CharSet (Negative set)) = "[^" ++ show set ++ "]"
showVal (Atom name) = name
showVal (Number number) = show number
showVal (List list) = "(" ++ unwordsList list ++ ")"
showVal FuncDefinition { params = params, closure = closure, body = body } =
    "(lambda (" ++ unwords (map show params) ++ ") ...)"
showVal (RegExp str) = str
showVal (String str) = str
showVal (CaptureGroupName name) = "{" ++ name ++ "}"

unwordsList :: [Risp] -> String
unwordsList = unwords . Prelude.map showVal
instance Show Risp where show = showVal