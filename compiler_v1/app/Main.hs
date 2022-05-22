module Main where

import Data.List
import Control.Monad.State

import System.IO
import System.Environment

import Parser
import Ast
import CodeGen
import Symbols

--main = do
--    args <- getArgs
--    case args of
--        [] -> error "Provide file name"
--        [filename] -> do
--            line <- readFile filename
--            case sourceCodeP line of
--                Right res -> putStrLn $ joinN $ map show res 
--                Left err -> print err
main = do
    args <- getArgs
    compile args

compile :: [String] -> IO ()
compile [filename] = do
        line <- readFile filename
        case sourceCodeP line of
            Right res ->
                let (_, ps1) = runState (fillSymbolTable res) $ (ProgrammState [] regTable [] [] 0 4096)
                    (_, ps2) = runState (codeGen res) $ ps1
                in putStrLn $ printProgrammState $ ps2
            Left err -> print err

compile [] = do
    putStrLn "Provide file name"

-- parse [x] = do
--     putStrLn "Provide mode you want to run compiler with"

-- showAst source = do 
--     case sourceCodeP source of
--      Right res -> putStrLn $ joinN $ map (joinN . prettyPrint) res

