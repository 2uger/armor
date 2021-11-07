module Main where

import Parser

main = do
    line <- getLine
    case runSumParse line of
        Right res -> putStrLn $ show res
        Left err -> print err
