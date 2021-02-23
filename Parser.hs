{- 
    1. Translate grammar into haskell representation
        -Parse file
        -Make map in a way {NonT: [NonT, Term, NonT],
                            NonT: [..]}
    2. Make a parse table 
        -Construct first and follow set
    3. Write a LL(1) parser by table driven approach

-}

import ParseGrammar (parseGrammar, )
import FirstFollow (firstSymbolSet, followSymbolSet, )


data Symbol = Symbol String deriving (Show, Read)

data Rule = Rule { prod :: Symbol, rhs :: [Symbol] } deriving (Show) 

data Grammar = Grammar [Rule] deriving (Show)

--------------Create grammar--------------------

grammarFile = "grammar.txt"

main :: IO ()
main = do
         grammarText <- readFile grammarFile
         let
             grammar = parseGrammar grammarText
          

--------------Parser Itself--------------------

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
