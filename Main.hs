{-# LANGUAGE BlockArguments #-}
-- {-# OPTIONS_GHC -Wno-deferred-type-errors #-}
{-# LANGUAGE ExistentialQuantification #-}
import Risp
import Parse
import RispError
import Stack
import RispEval
import Control.Monad.Except
import Data.List
import System.IO
import System.Environment
import Control.Monad.State

-- Flush result through IO
flushStr :: String -> IO ()
flushStr str = putStr str >> hFlush stdout

readPrompt :: String -> IO String
readPrompt prompt = flushStr prompt >> getLine

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

until_ pred prompt action initValue = do
    result <- prompt --result is a string
    if pred result
        then return ()
        else action result initValue >>= until_ pred prompt action


runRepl :: IO ()
runRepl = until_ (== "quit") (readPrompt "Risp>>> ") evalAndPrint initialEnvStack
----------- MAIN ---------------


trapError :: (MonadError a m, Show a) => m String -> m String
trapError action = catchError action (return . show)

runOne :: String -> IO ()
runOne expr = let errorMonadPair = readExpr expr >>= \risp -> runStateT (eval risp) initialEnvStack in
     let result = errorMonadPair >>= \tuple -> translate (fst tuple) in
     putStrLn $ extractValue $ trapError (fmap show result)

main :: IO ()
main = do
    args <- getArgs
    if Data.List.null args then runRepl else runOne (head args)




