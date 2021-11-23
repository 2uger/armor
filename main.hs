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
            case parseSourceCode line of
                Right res -> let (ps, r) = runState (fillSymbolTable res) $ (ProgrammState [] [1, 2, 3, 4, 5, 6] [])
                             in putStrLn $ printProgrammState $ snd $ runState (codeGen res) $ r
                Left err -> print err
