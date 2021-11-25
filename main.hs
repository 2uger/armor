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
    case args of
        [] -> error "Provide file name"
        [filename] -> do 
            line <- readFile filename
            case sourceCodeP line of
                                            -- fill Global Symbol Table
                Right res -> let (_, ps1) = runState (fillSymbolTable res) $ (ProgrammState [] regTable [] [])
                                 (_, ps2) = runState (codeGenFuncCall res) $ ps1
                             in putStrLn $ printProgrammState $ ps2
                Left err -> print err
