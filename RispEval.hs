module RispEval(eval, translate) where
import Risp
import RispSet
import Stack
import RispError
import Control.Monad.State
import Data.Set hiding (foldl, map)
import Control.Monad.Except
import Data.List
import Data.Char

extractRegExp :: Risp -> EitherError String
extractRegExp (RegExp string) = return string
extractRegExp notRegExp = throwError $ TypeMismatch "RegExp" notRegExp

extractCharSet :: Risp -> EitherError (Set Char)
extractCharSet (CharSet set) = return set
extractCharSet notCharSet = throwError $ TypeMismatch "CharSet" notCharSet

intersections :: (Foldable f, Ord a) => f (Set a) -> Set a
intersections = foldl1 intersection

-------------- EVAL (set, and (math)) --------------
eval :: Risp -> StateT EnvStack EitherError Risp
-- eval :: Risp -> EitherError Risp
eval (List ((Atom "union"): args)) =
    do
        listOfEvaledArgs <- mapM eval args
        listOfSets <- lift $ mapM extractCharSet listOfEvaledArgs
        let resultSet = unions listOfSets
        return $ CharSet resultSet

eval (List ((Atom "intersection"): args)) =
    do
        listOfEvaledArgs <- mapM eval args
        listOfSets <- lift $ mapM extractCharSet listOfEvaledArgs
        let resultSet = intersections listOfSets
        return $ CharSet resultSet
eval (List [Atom "define", Atom name, form]) = do
    value <- eval form
    defineVar name value
eval (List [Atom "lambda", List params, body]) = do
    envStack <- get
    return $ FuncDefinition {
        params = map showVal params,
        closure = envStack,
        body = body
    }
eval (List ((Atom funcName): args)) =
    do
        listOfEvaledArgs <- mapM eval args
        -- return $ List (Atom funcName: listOfEvaledArgs)
        funcIsBound <- isBound funcName
        if funcIsBound
            then do
                func <- readVar funcName
                case func of
                    FuncDefinition { params = params, closure = closure, body = body } -> do
                        oldEnvStack <- get
                        put closure -- the envstack to be closure
                        bindVars $ zip params listOfEvaledArgs -- add these vars into envstack                        
                        result <- eval body
                        put oldEnvStack
                        return result
                    _ -> lift $ throwError $ TypeMismatch "function" $ Atom funcName
            else return $ List (Atom funcName : listOfEvaledArgs)
eval (List (wrongHead : _)) = lift $ throwError $ TypeMismatch "FuncKeyword/Atom" wrongHead
eval (Atom varName) = readVar varName

eval val@(RegExp regExp) = lift $ throwError $ TypeMismatch "not regExp" val --this type should only appear in translate
eval x = return x -- maybe should not include Atom

------------ TRANSLATE ------------------
simplify :: Set Char -> String
simplify set
  | set `intersection` word == word = "\\w" ++ simplify (set `difference` word)
  | set `intersection` whitespace == whitespace = "\\s" ++ simplify (set `difference` whitespace)
  | set `intersection` digits == digits = "\\d" ++ simplify (set `difference` digits)
  | otherwise = init $ tail $ show $ concatMap rangeToString (foldl formRanges [] (toAscList set))

-- Each pair of chars is a range
formRanges :: [(Char, Char)] -> Char -> [(Char, Char)]
formRanges [] newChar = [(newChar, newChar)]
formRanges ((low, high) : tail) newChar = if ord newChar == ord high + 1
    then (low, newChar) : tail
    else (newChar, newChar) : ((low, high) : tail)

rangeToString :: (Char, Char) -> String
rangeToString ('0', '9') = "\\d"
rangeToString (low, high) = if low == high
    then [low]
    else [low, '-', high]

translate :: Risp -> EitherError Risp
translate (Anchor StartOfLine) = return $ RegExp "^"
translate (Anchor EndOfLine) = return $ RegExp "$"
translate (Anchor WordBoundary) = return $ RegExp "\\b"
translate val@(Number num) = throwError $ TypeMismatch "not number" val
translate (CharSet charSet) = return $ RegExp $ "[" ++ simplify charSet ++ "]"
translate val@(Atom atom) = throwError $ TypeMismatch "not atom" val
translate (List ((Atom "concat") : args)) =
    do
        listOfTranslatedArgs <- mapM translate args
        listOfRegExp <- mapM extractRegExp listOfTranslatedArgs
        let concatedString = concat listOfRegExp
        return $ RegExp $ "(" ++ concatedString ++ ")"
translate (List [Atom "repeatRange", pattrn, Number min, Number max]) =
    do
        translatedPattern <- translate pattrn
        translatedString <- extractRegExp translatedPattern
        return $ RegExp $ "(" ++ translatedString ++ "{" ++ show min ++ "," ++ show max ++ "}" ++ ")"
translate (List ((Atom "or") : args)) =
    do
        listOfTranslatedArgs <- mapM translate args
        listOfRegExp <- mapM extractRegExp listOfTranslatedArgs
        return $ RegExp $ "(" ++ intercalate "|" listOfRegExp ++ ")"
translate (List [Atom "optional", pattrn]) =
    do
        translatedPattern <- translate pattrn
        translatedString <- extractRegExp translatedPattern
        return $ RegExp $ "(" ++ translatedString ++ "?)"
translate (List [Atom "at_least_0_times", pattrn]) =
    do
        translatedPattern <- translate pattrn
        translatedString <- extractRegExp translatedPattern
        return $ RegExp $ "(" ++ translatedString ++ "*)"
translate (List [Atom "at_least_1_time", pattrn]) =
    do
        translatedPattern <- translate pattrn
        translatedString <- extractRegExp translatedPattern
        return $ RegExp $ "(" ++ translatedString ++ "+)"
translate val@(FuncDefinition _ _ _) = return val