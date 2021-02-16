{- 
    1. Translate grammar into haskell representation
        -Parse file
        -Make map in a way {NonT: [NonT, Term, NonT],
                            NonT: [..]}
    2. Make a parse table 
        -Need construct first and follow sets
    3. Write a LL(1) parser by table driven approach

-}

parseGrammar = 

firstSet = 

followSet = 

grammar = fromList [
    (Symbol "funcSt", (Symbol "TokenPlus")),
    (Symbol "NonTerm", (Symbol "TOKENNNNS"))
]

parseTable = fromList [(Symbol "Non terminal", parseTableSymbol1)]
parseTableSymbol1 = fromList [(Symbol "Symb", grammar_rule)]

data Symbol = Symbol String

parser :: [Token] -> String
parser input = 
    let 
        isMatch = parseToken input [Symbol "funcSt", Symbol "$"]
    in
        case isMatch of 
            True -> "Parser sucess"
            False -> "Parser failed"

{-
- Take list of tokens and stack for storing symbols
-}
parseToken :: [Symbol] -> [Symbol] -> Bool
parseStr (token:remain) (topSymb:stack)
    | topSymb == token        = parseStr remain stack 
    | matchRule token topSymb = let 
                                stack' = parseTable top token
                            in 
                                parseToken input stack'
    | otherwise = False

parseToken _ [] = True
        
matchRule :: Symbol -> Symbol -> Bool
matchRule token symbol = 
    let 
        symbols = parseTable symbol token
    in 
        case of symbols
            [x]     -> True
            symbols -> True
            []      -> True

parseTable :: Symbol -> Symbol -> [Symbol] 
parseTable topSymbol token = 
    let 
        production = lookup topSymbol parseTable
        grammarRule = lookup token production
        lhs = tail $ grammar !! grammarRule
    in 
        lhs
