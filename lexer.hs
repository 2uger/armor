module Main where

import Data.Char

main :: IO ()
main = do
  file_as_str <- getLine
  let tokens = parseTokens file_as_str
  putStrLn $ show tokens

data Token = TokenPlus String 
           | TokenMinus String
           | TokenEqual String
           | TokenIf String
           | TokenWhiteSpace
           | TokenInvalid
           deriving (Show, Eq)

printResults [] = error "Empty List!"
printResults (x:xs)= (show x ++ " | ") : printResults xs

parseTokens :: String -> [Token]
parseTokens [] = []
parseTokens input = 
        let 
            (token, remain) = driveTable 0 "" input
        in 
            token : parseTokens remain

-- driveTable scanning token till final state or Error
driveTable :: Int -> String -> String -> (Token, String)
driveTable currState tokenStr [] = (TokenInvalid, "Invalid token")
driveTable currState tokenStr (c:xs) = 
    let 
        (nextSt, isConsume) = nextState currState c
        (newTokenStr, remain) = nextTokenString tokenStr c xs isConsume
        (done, token) = finalState nextSt newTokenStr 
    in
        if done
            then (token, remain)
        else driveTable nextSt newTokenStr remain
        

nextTokenString :: String -> Char -> String -> Bool -> (String, String)
nextTokenString tokenStr c remain isConsume
    | isConsume = (tokenStr ++ [c], remain)
    | not isConsume = (tokenStr, remain)


-- finalState define is current state is final state or not
finalState :: Int -> String -> (Bool, Token)
finalState 1 tokenStr = (True, TokenPlus tokenStr)
finalState 2 tokenStr = (True, TokenMinus tokenStr)
finalState 3 tokenStr = (True, TokenEqual tokenStr)
finalState 5 tokenStr = (True, TokenIf tokenStr)
finalState 99 tokenStr = (True, TokenWhiteSpace)
finalState 999 tokenStr = (True, TokenInvalid)
finalState _ tokenStr = (False, TokenIf tokenStr)


-- nextState consume initial state and next char
-- :return (next state, should you consume next symbol)
nextState :: Int -> Char -> (Int, Bool)
nextState 0 c 
        | c == '+' = (1, True)  
        | c == '-' = (2, True)
        | c == ' ' = (99, True)
        | c == '=' = (3, True)
        | c == 'i' = (4, True)
        | otherwise = (999, False)
nextState 1 c
        | c == ' ' = (99, True)
        | otherwise = (999, False) 
nextState 2 c
        | c == ' ' = (99, True)
        | otherwise = (999, False) 
nextState 3 c 
        | c == ' ' = (99, True)
        | otherwise = (999, False)
nextState 4 c
        | c == 'f' = (5, True)
        | c == ' ' = (999, False)
nextState 5 c
        | c == ' ' = (99, True)
        | otherwise = (999, True)
