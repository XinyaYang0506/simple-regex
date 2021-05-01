module RispEval(eval, translate) where
import Risp
import RispSet
import Stack
import RispError
import Control.Monad.State
import Data.Set hiding (foldl, map)
import Control.Monad.Except
import Data.List hiding (union)
import Data.Char

extractRegExp :: Risp -> EitherError String
extractRegExp (RegExp string) = return string
extractRegExp notRegExp = throwError $ TypeMismatch "RegExp" notRegExp

extractCharSet :: Risp -> EitherError EitherCharSet
extractCharSet (CharSet set) = return set
extractCharSet notCharSet = throwError $ TypeMismatch "CharSet" notCharSet

-- charIntersections :: (Foldable f) => f CharSet -> CharSet
-- intersections = foldl1 charIntersection

charUnion :: EitherCharSet -> EitherCharSet -> EitherCharSet
charUnion (Positive p1) (Positive p2) = Positive $ p1 `union` p2
charUnion (Positive p) (Negative n) = Negative $ n `difference` p
charUnion (Negative n) (Positive p) = Negative $ n `difference` p
charUnion (Negative n1) (Negative n2) = Negative $ n1 `union` n2

charIntersection :: EitherCharSet -> EitherCharSet -> EitherCharSet
charIntersection (Positive p1) (Positive p2) = Positive $ p1 `intersection` p2
charIntersection (Positive p) (Negative n) = Positive $ p `difference` n
charIntersection (Negative n) (Positive p) = Positive $ p `difference` n
charIntersection (Negative n1) (Negative n2) = Negative $ n1 `union` n2

charDiff :: EitherCharSet -> EitherCharSet -> EitherCharSet
charDiff (Positive p1) (Positive p2) = Positive $ p1 `difference` p2
charDiff (Positive p) (Negative n) = Positive $ p `intersection` n
charDiff (Negative n) (Positive p) = Negative $ p `union` n
charDiff (Negative n1) (Negative n2) = Positive $ n2 `difference` n1
-------------- EVAL (set, and (math)) --------------
eval :: Risp -> StateT EnvStack EitherError Risp
-- eval :: Risp -> EitherError Risp
eval (List [Atom "negate", arg]) = 
    do
        evaledArg <- eval arg
        eitherSet <- lift $ extractCharSet evaledArg
        case eitherSet of
            Positive set -> return $ CharSet $ Negative set
            Negative set -> return $ CharSet $ Positive set
eval (List ((Atom "negate") : args)) = lift $ throwError $ NumArgs 1 args
eval (List ((Atom "union"): args)) =
    do
        listOfEvaledArgs <- mapM eval args
        listOfSets <- lift $ mapM extractCharSet listOfEvaledArgs
        let resultSet = foldl1 charUnion listOfSets
        return $ CharSet resultSet
eval (List ((Atom "intersection"): args)) =
    do
        listOfEvaledArgs <- mapM eval args
        listOfSets <- lift $ mapM extractCharSet listOfEvaledArgs
        let resultSet = foldl1 charIntersection listOfSets
        return $ CharSet resultSet
eval (List [Atom "diff", arg1, arg2]) =
    do
        evaledArg1 <- eval arg1
        evaledArg2 <- eval arg2
        set1 <- lift $ extractCharSet evaledArg1
        set2 <- lift $ extractCharSet evaledArg2
        let resultSet = charDiff set1 set2
        return $ CharSet resultSet
eval (List ((Atom "diff") : args)) = lift $ throwError $ NumArgs 2 args
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
  | otherwise = escapeCaretAtBeginning $ init $ tail $ show $ concatMap rangeToString (foldl formRanges [] (toAscList set)) --init and tail get rid of quotation marks

escapeCaretAtBeginning :: String -> String
escapeCaretAtBeginning ('^' : tail) = "\\^" ++ tail
escapeCaretAtBeginning string = string
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
translate (CharSet (Positive charSet)) = if charSet == empty
    then throwError EmptyCharSet
    else return $ RegExp $ "[" ++ simplify charSet ++ "]"
translate (CharSet (Negative charSet)) = if charSet == empty
    then return $ RegExp "."
    else return $ RegExp $ "[^" ++ simplify charSet ++ "]"
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