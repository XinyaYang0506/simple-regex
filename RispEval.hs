module RispEval(eval, translate, numberCaptureGroups, identifyRisp, testConcatWeirdness) where
import Risp
import RispCharSet
import Stack
import RispError
import Control.Monad.State
import Data.Set hiding (foldl, map, insert)
import Control.Monad.Except
import Data.List hiding (union, insert)
import Data.Char
import qualified Data.Map as Map

extractRegExp :: Risp -> EitherError String
extractRegExp (RegExp string) = return string
extractRegExp notRegExp = throwError $ TypeMismatch "RegExp" notRegExp

extractCharSet :: Risp -> EitherError EitherCharSet
extractCharSet (CharSet set) = return set
extractCharSet notCharSet = throwError $ TypeMismatch "CharSet" notCharSet

charSetUnion :: EitherCharSet -> EitherCharSet -> EitherCharSet
charSetUnion (Positive p1) (Positive p2) = Positive $ p1 `union` p2
charSetUnion (Positive p) (Negative n) = Negative $ n `difference` p
charSetUnion (Negative n) (Positive p) = Negative $ n `difference` p
charSetUnion (Negative n1) (Negative n2) = Negative $ n1 `union` n2

charSetIntersection :: EitherCharSet -> EitherCharSet -> EitherCharSet
charSetIntersection (Positive p1) (Positive p2) = Positive $ p1 `intersection` p2
charSetIntersection (Positive p) (Negative n) = Positive $ p `difference` n
charSetIntersection (Negative n) (Positive p) = Positive $ p `difference` n
charSetIntersection (Negative n1) (Negative n2) = Negative $ n1 `union` n2

charSetDiff :: EitherCharSet -> EitherCharSet -> EitherCharSet
charSetDiff (Positive p1) (Positive p2) = Positive $ p1 `difference` p2
charSetDiff (Positive p) (Negative n) = Positive $ p `intersection` n
charSetDiff (Negative n) (Positive p) = Negative $ p `union` n
charSetDiff (Negative n1) (Negative n2) = Positive $ n2 `difference` n1
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
        let resultSet = foldl1 charSetUnion listOfSets
        return $ CharSet resultSet
eval (List ((Atom "intersection"): args)) =
    do
        listOfEvaledArgs <- mapM eval args
        listOfSets <- lift $ mapM extractCharSet listOfEvaledArgs
        let resultSet = foldl1 charSetIntersection listOfSets
        return $ CharSet resultSet
eval (List [Atom "diff", arg1, arg2]) =
    do
        evaledArg1 <- eval arg1
        evaledArg2 <- eval arg2
        set1 <- lift $ extractCharSet evaledArg1
        set2 <- lift $ extractCharSet evaledArg2
        let resultSet = charSetDiff set1 set2
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

-- eval (List [Atom "create_capture_group", CaptureGroupName name, form]) = 
--     do 
--         evaledForm <- eval form
--         return List [Atom "create_capture_group", Capture]
-- -- (concat 'a' (create_capture_group {cap} (union 'a' 'b')))
-- -- (List [(Atom "concat"), 'a', (List [(Atom "create_capture_group"), {cap}, [ab]])])

-- -- (define weird (create_capture_group {cap} 'a'))
-- -- a([ab])
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
numberCaptureGroups :: Map.Map String Int -> Risp -> Map.Map String Int
numberCaptureGroups mapSoFar (List [Atom "create_capture_group", CaptureGroupName name, form]) =
    let newMap = Map.insert name (1 + Map.size mapSoFar) mapSoFar in
        numberCaptureGroups newMap form
    -- insert name currentNum (numberCaptureGroups form (currentNum + 1))
numberCaptureGroups mapSoFar (List ((Atom funcName): args)) = foldl numberCaptureGroups mapSoFar args
numberCaptureGroups mapSoFar _ = mapSoFar

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

translate :: Map.Map String Int -> Risp -> EitherError Risp
translate _ (Anchor StartOfLine) = return $ RegExp "^"
translate _ (Anchor EndOfLine) = return $ RegExp "$"
translate _ (Anchor WordBoundary) = return $ RegExp "\\b"
translate _ val@(Integer num) = throwError $ TypeMismatch "not number" val
translate _ (CharSet (Positive charSet)) = if charSet == empty
    then throwError EmptyCharSet
    else return $ RegExp $ "[" ++ simplify charSet ++ "]"
translate _ (CharSet (Negative charSet)) = if charSet == empty
    then return $ RegExp "."
    else return $ RegExp $ "[^" ++ simplify charSet ++ "]"
translate _ val@(Atom atom) = throwError $ TypeMismatch "not atom" val
translate captureMap (List ((Atom "concat") : args)) =
    do
        listOfTranslatedArgs <- mapM (translate captureMap) args
        listOfRegExp <- mapM extractRegExp listOfTranslatedArgs
        let concatedString = concat listOfRegExp
        return $ RegExp $ "(?:" ++ concatedString ++ ")"
translate captureMap (List [Atom "repeat_range", pattrn, Integer min, Integer max]) =
    do
        translatedPattern <- translate captureMap pattrn
        translatedString <- extractRegExp translatedPattern
        return $ RegExp $ "(?:" ++ translatedString ++ "{" ++ show min ++ "," ++ show max ++ "}" ++ ")"
translate captureMap (List ((Atom "or") : args)) =
    do
        listOfTranslatedArgs <- mapM (translate captureMap) args
        listOfRegExp <- mapM extractRegExp listOfTranslatedArgs
        return $ RegExp $ "(?:" ++ intercalate "|" listOfRegExp ++ ")"
translate captureMap (List [Atom "optional", pattrn]) =
    do
        translatedPattern <- translate captureMap pattrn
        translatedString <- extractRegExp translatedPattern
        return $ RegExp $ "(?:" ++ translatedString ++ "?)"
translate captureMap (List [Atom "at_least_0_times", pattrn]) =
    do
        translatedPattern <- translate captureMap pattrn
        translatedString <- extractRegExp translatedPattern
        return $ RegExp $ "(?:" ++ translatedString ++ "*)"
translate captureMap (List [Atom "at_least_1_time", pattrn]) =
    do
        translatedPattern <- translate captureMap pattrn
        translatedString <- extractRegExp translatedPattern
        return $ RegExp $ "(?:" ++ translatedString ++ "+)"
translate captureMap (List [Atom "create_capture_group", CaptureGroupName name, pattrn]) =
    do
        let has_bind_capture_groups = identifyRisp (\risp -> risp == Atom "bind_capture_groups") pattrn
        if has_bind_capture_groups
            then throwError $ Default "Cannot bind capture groups inside search pattern"
            else do
                translatedPattern <- translate captureMap pattrn
                translatedString <- extractRegExp translatedPattern
                return $ RegExp $ "(" ++ translatedString ++ ")"
translate _ (List (Atom "create_capture_group" : args)) = throwError $ NumArgs 2 args
translate captureMap (CaptureGroupName name) =
    do
        case Map.lookup name captureMap of
            Just n -> return $ RegExp $ "\\" ++ show n
            Nothing -> throwError $ UnboundCaptureGroupName name
-- (create_capture_group {cap} (bind_capture_groups 'a' 'a'))
-- (create_capture_group {cap} rplc)
translate _ (List [Atom "bind_capture_groups", search, replace]) =
    do
        let has_create_capture_group = identifyRisp (\risp -> risp == Atom "create_capture_group") replace
        if has_create_capture_group
            then throwError $ Default "Cannot create capture groups inside replace pattern"
            else translate (numberCaptureGroups Map.empty search) replace
translate _ val@(FuncDefinition _ _ _) = return val

identifyRisp :: (Risp -> Bool) -> Risp -> Bool
identifyRisp predicate risp = predicate risp || (case risp of
                                                List lst -> any (identifyRisp predicate) lst
                                                FuncDefinition _ _ body -> predicate body
                                                _ -> False)

testConcatWeirdness = identifyRisp
    (\risp -> risp == Atom "bind_capture_groups")
    
