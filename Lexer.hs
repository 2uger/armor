{- 
    Scanner for tokens like:
    (+, -, =, *, id)
    Based on simple DFA without backtracking
-}

module Lexer where

import Data.Char

--instance Show Token where
 --   show TokenPlus = "TOKEN PLUS"

printResults :: [Token] -> String
printResults [] = error "Empty List!"
printResults (x:xs)= (show x ++ " | ") ++ printResults xs

removeWhiteSpace :: [Token] -> [Token]
removeWhiteSpace [] = []
removeWhiteSpace tokens = [token | token <- tokens, token /= TokenWhiteSpace]

parseTokens :: String -> [Token]
parseTokens [] = []
parseTokens input = 
        let 
            (token, remain) = driveTable 0 "" input
        in 
            removeWhiteSpace $ token : parseTokens remain

-- By state and next char it detect if it correct and next state
driveTable :: Int -> String -> String -> (Token, String)
driveTable currState tokenStr [] = (TokenInvalid tokenStr, "")
driveTable state tokenStr (c:xs) = 
    let 
        (state', isConsume) = nextState state c
        (tokenStr', remain) = nextTokenString tokenStr c xs isConsume
        (done, token) = finalState state' tokenStr' 
    in
        if done
            then (token, remain)
        else driveTable state' tokenStr' remain
        

nextTokenString :: String -> Char -> String -> Bool -> (String, String)
nextTokenString tokenStr c remain isConsume
    | isConsume = (tokenStr ++ [c], remain)
    | not isConsume = (tokenStr, c:remain)


-- finalState define is current state is final state or not
finalState :: Int -> String -> (Bool, Token)
finalState x tokenStr
        | x == 1    = (True, Term "+" )
        | x == 2    = (True, TokenMinus tokenStr)
        | x == 3    = (True, TokenMul )
        | x == 4    = (True, TokenEqual tokenStr)
        | x == 5    = (True, TokenLParen)
        | x == 6    = (True, TokenRParen)
        | x == 7    = (True, TokenWhiteSpace)
        | x == 8    = (True, TokenSemicolon)
        | x == 12   = (True, TokenTypeInt)
        | x == 14   = (True, TokenID)
        | x == 16   = (True, TokenNum)
        | x == 99   = (True, TokenInvalid tokenStr)
        | otherwise = (False, TokenNum)


-- nextState consume initial state and next char
-- :return (next state, should you consume next symbol)
nextState :: Int -> Char -> (Int, Bool)
nextState 0 c 
        | c == '+' = (1, True)  
        | c == '-' = (2, True)
        | c == '*' = (3, True)
        | c == '=' = (4, True)
        | c == '(' = (5, True)
        | c == ')' = (6, True)
        | c == ' ' = (7, True)
        | c == ';' = (8, True)
        | c == 'i' = (9, True)
        | c `elem` ['a'..'z'] = (13, True)
        | c `elem` ['1'..'9'] = (15, True)
        | otherwise = (99, True)

nextState 9 c
        | c == 'n' = (10, True)
        | c `elem` ['a'..'z'] || c `elem` ['1'..'9'] = (13, True)
        | otherwise = (99, True)

nextState 10 c
        | c == 't' = (11, True)
        | c `elem` ['a'..'z'] || c `elem` ['1'..'9'] = (13, True)
        | otherwise = (99, True)

nextState 11 c
        | c == ' ' = (12, False)
        | c `elem` ['a'..'z'] || c `elem` ['1'..'9'] = (13, True)
        | otherwise = (99, True)

nextState 13 c
        | c `elem` ['a'..'z'] || c `elem` ['1'..'9'] = (13, True)
        | c == ' ' = (14, False)
        | otherwise = (99, True) 

nextState 15 c
        | c `elem` ['1'..'9'] = (15, True)
        | c `elem` ['a'..'z'] = (13, True)
        | c `elem` ['(', ')', '*', '=', ';', '+', '-'] = (16, False)
        | c == ' ' = (16, False)
        | otherwise = (15, False)
