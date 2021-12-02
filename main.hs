module Main where

import Data.List
import Control.Monad.State

import System.IO
import System.Environment

import Parser
import Ast
import CodeGen
import Symbols

main = do
    args <- getArgs
    parse args

parse :: [String] -> IO ()
parse [filename, mode] = do
        line <- readFile filename
        case sourceCodeP line of
            Right res -> 
                let (_, ps1) = runState (fillSymbolTable res) $ (ProgrammState [] regTable [] [] 0)
                    (_, ps2) = runState (codeGen res) $ ps1
                in case mode of
                    "--ast" -> showAst line
                    "--full-state" -> putStrLn $ printProgrammState $ ps2
                    "--asm" -> putStrLn $ show $ psCode ps2
            Left err -> print err

parse [x] = do
    putStrLn "Provide mode you want to run compiler with"

showAst source = do 
    case sourceCodeP source of
     Right res -> putStrLn $ joinN $ map (joinN . prettyPrint) res

