{- 
    Scanner for tokens like:
    (+, -, =, *, id)
    Based on simple DFA without backtracking
-}

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
           | TokenMul String
           | TokenID String
           | TokenWhiteSpace
           | TokenInvalid String
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
driveTable currState tokenStr [] = (TokenInvalid tokenStr, "")
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
    | not isConsume = (tokenStr, c:remain)


-- finalState define is current state is final state or not
finalState :: Int -> String -> (Bool, Token)
finalState x tokenStr
        | x == 1 = (True, TokenPlus tokenStr)
        | x == 2 = (True, TokenMinus tokenStr)
        | x == 3 = (True, TokenMul tokenStr)
        | x == 4 = (True, TokenEqual tokenStr)
        | x == 6 = (True, TokenID tokenStr)
        | x == 7 = (True, TokenWhiteSpace)
        | x == 99 = (True, TokenInvalid tokenStr)
        | otherwise = (False, TokenInvalid tokenStr)


-- nextState consume initial state and next char
-- :return (next state, should you consume next symbol)
nextState :: Int -> Char -> (Int, Bool)
nextState 0 c 
        | c == '+' = (1, True)  
        | c == '-' = (2, True)
        | c == '*' = (3, True)
        | c == '=' = (4, True)
        | c `elem` ['a'..'z'] = (5, True)
        | c == ' ' = (7, True)
        | otherwise = (99, True)
nextState 5 c
        | c `elem` ['a'..'z'] = (5, True)
        | otherwise = (6, False)  
