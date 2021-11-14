module Main where

import System.IO
import System.Environment

import Parser
import Ast

main = do
    args <- getArgs
    case args of
        [] -> error "Provide file name"
        [filename] -> do 
            line <- readFile filename
            case parseSourceCode line of
                Right res -> putStrLn $ joinN $ prettyAst [res]
                Left err -> print err
