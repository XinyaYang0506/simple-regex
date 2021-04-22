{-# LANGUAGE BlockArguments #-}
module Stack (EnvStack, initialEnvStack, defineVar, readVar) where
import Risp
import RispError
import RispSet
import Control.Monad.State
import Control.Monad.Except
import Data.List
import Data.Maybe



type StackFrame = [(String, Risp)]
type EnvStack = [StackFrame]

initialEnvStack :: EnvStack
initialEnvStack = initialCharSets

defineVar :: String -> Risp -> StateT EnvStack EitherError Risp
defineVar varName risp =
    do
        alreadyExists <- isBound varName
        if alreadyExists
            then lift (throwError $ VarAlreadyExists varName)
            else do
                topFrame <- pop
                push $ (varName, risp) : topFrame
                return risp

-- bind several variables in a new stack frame
bindVars :: [(String, Risp)] -> StateT EnvStack EitherError()
bindVars bindings = do
    -- let strList = Data.List.map fst bindings
    boolList <- mapM (isBound . fst) bindings
    let firstTrueIndex = elemIndex True boolList
    case firstTrueIndex of
        Just i -> lift (throwError . VarAlreadyExists . fst $ bindings !! i)
        Nothing -> do push bindings

readVar :: String -> StateT EnvStack EitherError Risp
readVar varName = StateT \envStack -> case lookup varName $ concat envStack of
    Just val -> return (val, envStack)
    _ -> throwError $ UnboundVar varName

-- idState :: State EnvStack ()
-- idState = state ((),)

--  determine if a given variable is already bound in the environment
-- we search the entire stack because we don't want to allow a user to change
-- the value associated with a variable name
isBound :: String -> StateT EnvStack EitherError Bool
isBound varName = StateT \envStack -> return (isJust $ lookup varName $ concat envStack, envStack)

-- redefining variable
-- let a = 1;
-- let a = 2;
-- (define a 1)
-- (define a 2)
-- (define (lambda ... (let (a 5) (lambda .. (...)))))
-- (let (a 7) (call it))
-- mutating variable
-- let mut b = &mut 1;
-- *b = 2;

-- mutating reference
-- let mut c = &mut 2;
-- c = &mut 3;

-- s -> s a
-- s should be a list like [(name, value)]
-- if we want to read a variable, a would be the value
-- imaginary stack: [[(name, value)]]

--

pop :: StateT EnvStack EitherError StackFrame
pop = StateT $ \(x:xs) -> return (x,xs)

push :: StackFrame -> StateT EnvStack EitherError ()
push a = StateT $ \xs -> return ((), a:xs)