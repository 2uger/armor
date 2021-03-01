import qualified Lexer as Lx
import qualified Parser as Ps

import System.Environment
import System.IO
import System.IO.Error

import Control.Exception

main = mainFunc 

mainFunc :: IO ()
mainFunc = do
            (fileName:args) <- getArgs
            handle <- openFile fileName ReadMode
            fileString <- hGetContents handle
            let tokens = Lx.parseTokens fileString
                (isOk, x) = Ps.parse Ps.S [Lx.TokenTypeInt, Lx.TokenPlus, Lx.TokenTypeInt]
            putStrLn $ show isOk
            putStrLn $ show x
            putStrLn $ show tokens
            hClose handle

errorHandler :: IOError -> IO ()
errorHandler e
    | isDoesNotExistError e = putStrLn "Invalid file name"
    | otherwise = putStrLn "Error occured"

