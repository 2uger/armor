
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

