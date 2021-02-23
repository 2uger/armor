import qualified Data.Map as Map

data Symbol = Symbol String

grammar = Map.fromList [
    (Symbol "funcSt", (Symbol "TokenPlus", Symbol "NonTerm")),
    (Symbol "NonTerm", (Symbol "TOKENNNNS", Symbol "NonTerm"))
]

-- List of terminals(tokens) to make differ between 
-- production and terminals, because symbol represent both
tokens = [Symbol "+", Symbol "ID"]

nonTerminals = [Symbol "funcSt"]

firstSymbolSet :: Map k v -> Map k v -> Map k v 
firstSymbolSet grammar firstSymbSet = 
    let 
        firstSymbSet' = firstSymbolSetTokens firstSymbSet
        firstSymbSet'' = throughGrammar firstSymbSet' grammar
    in 
        firstSymbSet''

throughGrammar :: Map k v -> (k, v) -> Map k v
throughGrammar firstSymbSet (grammarRule:grammar) = 
    let 
        lhs = head grammarRule
        rhs = tail grammarRule
        rhsSet = first rhs firstSymbSet
    in
        rhsSet:throughGrammar firstSymbSet grammar 

throughGrammar firstSymbSet [] = firstSymbSet
 
-- Iterate through rhs of current symbol
throughRhs (symbol:rest) firsSymbSet
    | symbol `elem` tokens = firstSymbSet[symbol]
    | symbol `elem` nonTerminals = 
                        let
                            firstSet = first symbol firstSymbSet
                        in 
                            firstSet:throughRhs rest firstSymbSet

-- Recursively call first on symbol to find his firstSet
first :: [Symbol] -> Map k v -> [(k, [v])]
first symbol firstSymbSet = 
    | symbol `elem` tokens = firstSymbSet[symbol]
    | symbol `elem` nonTerminals = 
                            let 
                                rhs = grammar[symbol]
                            in case of (isEmptyString symbol firstSymbSet) 
                                   True -> (firstSet(symbol) - E) : first (head rhs) firstSymbSet
                                   False -> firstSymbSet
    |otherwise = False

-- Will check if E in first set of that symbol
isEmptyString 

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
    
