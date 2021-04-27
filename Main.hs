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

evalAndPrint :: Risp -> EnvStack -> IO EnvStack
evalAndPrint risp oldEnvStack  = do
    -- print risp
    case risp of
        List [Atom "load", String path] -> do
            exprList <- readFile path
            print exprList
            -- parseEvalAndPrint exprList oldEnvStack
            let eitherRispList = readExprList exprList
            case eitherRispList of 
                Right rispList -> foldM (flip evalAndPrint) oldEnvStack rispList
                Left err -> do
                    print err
                    return oldEnvStack
            
            -- evalAndPrint str oldEnvStack
            -- (readFile path) >>= readExprList
            -- evalAndPrint
        _ -> case runStateT (eval risp) oldEnvStack of
                Left err -> do
                    print err
                    return oldEnvStack
                Right (newRisp, newEnvStack) -> do
                    case translate newRisp of
                        Left err -> do
                            print err
                            return oldEnvStack
                        Right translatedRisp -> do
                            print translatedRisp
                            return newEnvStack
parseEvalAndPrint :: String -> EnvStack -> IO EnvStack
parseEvalAndPrint expr oldEnvStack  =
    case readExpr expr of
        Right risp -> do
            evalAndPrint risp oldEnvStack
        Left err -> do
            print err
            return oldEnvStack
            


until_ pred prompt action initValue = do
    result <- prompt --result is a string
    if pred result
        then return ()
        else action result initValue >>= until_ pred prompt action


runRepl :: IO ()
runRepl = until_ (== "quit") (readPrompt "Risp>>> ") parseEvalAndPrint initialEnvStack
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




