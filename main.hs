module Main where

import System.IO
import System.Environment

import Parser

main = do
    args <- getArgs
    case args of
        [] -> error "Provide file name"
        [filename] -> do 
            line <- readFile filename
            case parseSourceCode line of
                Right res -> putStrLn $ show res
                Left err -> print err
