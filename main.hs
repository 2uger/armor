module Main where

import Data.List

import System.IO
import System.Environment

import Parser
import Ast
import CodeGen

main = do
    args <- getArgs
    case args of
        [] -> error "Provide file name"
        [filename] -> do 
            line <- readFile filename
            case parseSourceCode line of
                Right res -> putStrLn $ intercalate "\n" $ fst $ genCode res [] regTable
                Left err -> print err
