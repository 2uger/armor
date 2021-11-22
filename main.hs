module Main where

import Data.List
import Control.Monad.State

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
                Right res -> let ps = ProgrammState [] [1, 2, 3, 4, 5, 6] []
                             in putStrLn $ show $ runState (genCode res) $ ps
                Left err -> print err
