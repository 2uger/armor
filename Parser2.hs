{- 
    1. Translate grammar into haskell representation
        -Parse file
        -Make map in a way {NonT: [NonT, Term, NonT],
                            NonT: [..]}
    2. Make a parse table 
        -Need construct first and follow sets
    3. Write a LL(1) parser by table driven approach

-}

grammar = fromList [
    (Symbol "funcSt", (Symbol "TokenPlus")),
    (Symbol "NonTerm", (Symbol "TOKENNNNS"))
]

-- List of terminals(tokens) to make differ between 
-- production and terminals, because symbol represent both
tokens = [Symbol "+", Symbol "ID"]

parseGrammar = 

{- 
 - return Map in format [(Symbol, Set),..]
-}
firsSet :: Map k v -> Map k v -> Map k v 
firstSet (grammarRule:grammar) firstSymbol = 
    let 
        throughGrammar' = throughGrammar first
        firstSymbol' = map throughGrammar' grammar
    in 
        firstSymbol'

throughGrammar :: Map k v -> (k, v) -> Map k v
throughGrammar firstSet grammarRule = 
    let 
        lhs = head grammarRule
        rhs = tail grammarRule
        set = first firstSet
    in
        set:
        
throughRhs firstSet rhs set= 
    let 
        symbol = head rhs
    in 
        set:first symbol firstSet

first :: Symbol -> Map k v -> Map k v
first symbol firstSet = 
    |symbol `elem` tokens = firstSet(symbol)
    |symbol `elem` nonTerminals = 
                            let 
                                rhs = grammar[symbol]
                                symbol = head rhs
                                flag = isEmptyString symbol
                            in case of flag True -> firstSet(symbol) - E : first rest firstSet
                                            Fasle -> firstSet
    |otherwise = False


first [] firstSet = firstSet
    


followSet :: Map k v -> Map k v -> Map k v
followSet grammar follow = 

throughGrammar followSet grammarRule = 
    let
        rhs = head grammarRule
        lhs = tail grammarRule
        followSymbol = followSymb
    in

followSymbol symbols
    | is terminal last element = firstSet(symbol) - E
    | not is terminal last element  || terminal last and firstSet(terminal) U E = 
    
    

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
