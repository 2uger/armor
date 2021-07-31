{- 
    Scanner for tokens from grammar.txt 
    Based on simple DFA by table driven approach
-}
module Lexer where

import ParserTypes 

import Data.Char

printResults :: [Terminal] -> String
printResults [] = error "Empty List!"
printResults (x:xs)= (show x ++ " | ") ++ printResults xs


parseTokens :: String -> [Terminal]
parseTokens [] = []
parseTokens input = 
        let 
            (token, remain) = driveTable 0 "" input
        in 
            token : parseTokens remain
  where
    --removeWhiteSpace :: [Token] -> [Token]
    --removeWhiteSpace [] = []
    --removeWhiteSpace tokens = [token | token <- tokens, token /= TokenWhiteSpace]


-- By state and next char it detect if it correct and next state
driveTable :: Int -> String -> String -> (Terminal, String)
driveTable currState tokenStr [] = (TermEmpty, "")
driveTable state tokenStr (c:xs) = 
    let 
        (nstate, isConsume, isFinalState) = nextState state c
        (tokenStr', remain) = nextTokenString tokenStr c xs isConsume
    in
        case isFinalState of
            True -> (tokenByState nstate, remain)
            False -> driveTable nstate tokenStr' remain
        

nextTokenString :: String -> Char -> String -> Bool -> (String, String)
nextTokenString tokenStr c remain isConsume
    | isConsume = (tokenStr ++ [c], remain)
    | not isConsume = (tokenStr, c:remain)


-- finalState define is current state is final state or not
tokenByState :: Int -> Terminal
tokenByState state 
    | state == 1  = TermPlus          
    | state == 2  = TermMinus
    | state == 3  = TermMultiply
    | state == 4  = TermDivide
    | state == 5  = TermDivRemainder
    | state == 6  = TermEqual
    | state == 7  = TermLParen
    | state == 8  = TermRParen
    | state == 9  = TermLSqBracket
    | state == 10  = TermRSqBracket
    | state == 11  = TermLBrace
    | state == 12  = TermRBrace
    | state == 13  = TermBackQuote
    | state == 14  = TermComma
    | state == 15  = TermColon

    | state == 16  = TermInt
    | state == 23  = TermBool
    | state == 26  = TermNumConst
    | state == 25  = TermId
    | state == 99 = TermEmpty

-- nextState consume initial state and next char
-- :return (next state, should you consume next symbol)
nextState :: Int -> Char -> (Int, Bool, Bool)
nextState 0 c 
    | c == '+' = (1, True, True)  
    | c == '-' = (2, True, True)
    | c == '*' = (3, True, True)
    | c == '/' = (4, True, True)
    | c == '%' = (5, True, True)
    | c == '=' = (6, True, True)

    | c == '(' = (7, True, True)
    | c == ')' = (8, True, True)
    | c == '[' = (9, True, True)
    | c == ']' = (10, True, True)
    | c == '{' = (11, True, True)
    | c == '}' = (12, True, True)

    | c == ';' = (13, True, True)
    | c == ',' = (14, True, True)
    | c == ':' = (15, True, True)
    | c == ' ' = (0, True, False)
    
    -- Start state for TermInt
    | c == 'i' = (14, True, False)
    -- Start state for TermBool
    | c == 'b' = (15, True, False)
    -- Start state for TermChar
    | c == 'c' = (16, True, False)

   -- -- Start state for TermStatic
   -- | c == 's' = (17, True)
   -- -- Start state for TermWhile
   -- | c == 's' = (18, True)
   -- -- Start state for TermReturn
   -- | c == 's' = (19, True)
   -- -- Start state for TermBreak
   -- | c == 's' = (20, True)

   -- -- Start state for TermNumConst
    | c `elem` ['1'..'9'] = (26, True, False)
   -- -- Start state for TermStringConst
   -- | c `elem` ['a'..'z'] = (22, True)
   -- -- Start state for TermCharConst
   -- | c == '\'' = (23, True)

   -- Start state for TermId
    | c `elem` ['a'..'z'] = (24, True, False)
    | otherwise = error "I dont know what is it"

nextState 14 c
    | c == 'n' = (15, True, False)
    | c `elem` ['a'..'z'] = (34, True, False)

nextState 15 c
    | c == 't' = (16, True, False)
    
nextState 16 c
    | c == ' ' = (16, True, True)
    | c `elem` ['a'..'z'] = (34, True, False)
    | otherwise = error "Unexpected symbol after type declaration"

-- For bool type
nextState 17 c
    | c == 'o' = (18, True, False)
    | otherwise = (34, True, False)

nextState 18 c
    | c == 'o' = (19, True, False)
    | otherwise = (34, True, False)

nextState 19 c
    | c == 'l' = (18, True, False)
    | otherwise = (34, True, False)

nextState 20 c
    | c == ' ' = (20, True, True)
    | c `elem` ['a'..'z'] = (34, True, False)
    | otherwise = error "Unexpected symbol after type declaration"

-- For char type
-- nextState 21-25
-- For NumConst
nextState 26 c
    | c `elem` ['0'..'9'] = (26, True, False)
    | c `elem` [']', ';', ')'] = (26, False, True)
    | c == ' ' = (26, True, True)
    | otherwise = error "Unexpected symbol after numbers"

nextState 34 c
    | c `elem` ['a' .. 'z'] = (34, True, False)
    | c `elem` ['0' .. '9'] = (34, True, False)
    | otherwise = (34, True, True)
-- For id name
nextState 24 c
    | c `elem` ['a'..'z'] = (24, True, False)
    | c `elem` ['1'..'9'] = (24, True, True)
    | c == ' ' = (25, True, True)
    | otherwise = error "Unexpected symbol while pars Id"
