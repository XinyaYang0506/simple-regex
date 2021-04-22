module RispEval(eval, translate) where
import Risp
import Stack
import RispError
import Control.Monad.State
import Data.Set hiding (foldl)
import Control.Monad.Except
import Data.List

extractRegExp :: Risp -> EitherError String
extractRegExp (RegExp string) = return string
extractRegExp notRegExp = throwError $ TypeMismatch "RegExp" notRegExp


extractCharSet :: Risp -> EitherError (Set Char)
extractCharSet (CharSet set) = return set
extractCharSet notCharSet = throwError $ TypeMismatch "CharSet" notCharSet

-------------- EVAL (set, and (math)) --------------
eval :: Risp -> StateT EnvStack EitherError Risp
-- eval :: Risp -> EitherError Risp
eval (Func ((Atom "union"): args)) =
    do
        listOfEvaledArgs <- mapM eval args
        listOfSets <- lift $ mapM extractCharSet listOfEvaledArgs
        let resultSet = unions listOfSets
        return $ CharSet resultSet
eval (Func [Atom "define", Atom name, form]) = do
    value <- eval form
    defineVar name value

eval (Func ((Atom funcName): args)) =
    do
        listOfEvaledArgs <- mapM eval args
        return $ Func (Atom funcName: listOfEvaledArgs)
eval (Func (wrongHead : _)) = lift $ throwError $ TypeMismatch "FuncKeyword/Atom" wrongHead
eval (Atom varName) = readVar varName

eval val@(RegExp regExp) = lift $ throwError $ TypeMismatch "not regExp" val --this type should only appear in translate
eval x = return x -- maybe should not include Atom

------------ TRANSLATE ------------------
translate :: Risp -> EitherError Risp
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