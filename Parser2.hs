{- 
    1. Translate grammar into haskell representation
        -Parse file
        -Make map in a way {NonT: [NonT, Term, NonT],
                            NonT: [..]}
    2. Make a parse table 
        -Need construct first and follow sets
    3. Write a LL(1) parser by table driven approach

-}

parseTable = [(Symbol, [Symbol]), (), (), ()]

data Token = TokenSum | TokenMinus

data Production = expr | funcSt

data Symbol = NonTerminal Production | Terminal Token

parser :: [Token] -> String
parser input = 
    let 
        isMatch = parseToken input ['$']
    in
        case isMatch of True -> "Parser sucess"
                        False -> "Parser failed"

parseToken :: [Token] -> [Symbol] -> Bool
parseToken (token:remain) (top:stack)
        | top == token           = parser remain stack 
        | matchRule    token top = let 
                                      stack' = parseTable top token

                                   in 
                                      parseToken input stack'
        | otherwise = False

parseToken _ [] = True
        
matchRule :: Token -> Symbol -> Bool
matchRule token symbol = let 
                            symbols = parseTable symbol token
                         in 
                            case symbols of [x] -> True
                                         of symbols -> True
                                         of [] -> True

parseTable :: Production -> Token -> [Symbol] 
parseTable nterm token = 
    | nterm == Expr = case token of .. 
    | nterm == Expr =
