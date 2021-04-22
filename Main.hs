{-# LANGUAGE BlockArguments #-}
{-# OPTIONS_GHC -Wno-deferred-type-errors #-}
{-# LANGUAGE ExistentialQuantification #-}
import Risp
import Parse
import RispError
import Stack
import RispEval
import Control.Monad.Except

import System.IO
import Control.Monad.State
-- import Extra.Control.Monad.Extra


-- Flush result through IO

flushStr :: String -> IO ()
flushStr str = putStr str >> hFlush stdout

readPrompt :: String -> IO String
readPrompt prompt = flushStr prompt >> getLine

-- evalAndPrint :: String -> StateT EnvStack IO ()
-- evalAndPrint :: EnvStack -> String -> (EnvStack, IO ())

-- evalAndPrint :: EnvRef -> String -> IO ()
-- evalAndPrint envRef expr =  evalString envRef expr >>= putStrLn
evalAndPrint :: String -> EnvStack -> IO EnvStack
evalAndPrint expr oldEnvStack  =
    let errorMonadPair = readExpr expr >>= \risp -> runStateT (eval risp) oldEnvStack in
    case errorMonadPair of
        Left err -> do
            print err
            return oldEnvStack
        Right (risp, newEnvStack) -> do
            let errorMonadRisp = translate risp
            case errorMonadRisp of
                Left err -> do
                    print err
                    return oldEnvStack
                Right risp -> do
                    print risp
                    return newEnvStack

-- evalString :: EnvStack -> String -> (EnvStack, IO String)
-- evalString envStack expr = do
--     errorMonadPair <- readExpr expr >>= \risp -> runStateT (eval risp) envStack

--     return (newStack, show evaluated) -- this is as if IO (...,...)

-- until_ :: Monad m => (a -> Bool) -> m a -> (a -> m ()) -> m ()
until_ pred prompt action initValue = do
    result <- prompt --result is a string
    if pred result
        then return ()
        else action result initValue >>= until_ pred prompt action


runRepl :: IO ()
runRepl = until_ (== "quit") (readPrompt "Lisp>>> ") evalAndPrint initialEnvStack
----------- MAIN ---------------


trapError :: (MonadError a m, Show a) => m String -> m String
trapError action = catchError action (return . show)

-- main :: IO ()
-- main = do
--      args <- getArgs
--      let some = readExpr (head args) >>= \risp -> runStateT (eval risp) initialEnvStack
--      let result = some >>= \tuple -> translate (fst tuple)
--      putStrLn $ extractValue $ trapError (fmap show result)

main :: IO ()
main = runRepl
-- main = do
--     args <- getArgs
--     if Data.List.null args then runRepl else return




