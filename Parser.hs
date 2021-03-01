module Parser where
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
--import FirstFollow (firstSymbolSet, followSymbolSet, )
--import TableDrive ()

import System.IO
--------------Create grammar--------------------

grammarFile = "grammar.txt"

main = mainFunc

mainFunc :: IO ()
mainFunc = do
         grammarText <- readFile grammarFile
         let
             grammar = parseGrammar grammarText
             firstSet = firstSymbolSet grammar
         putStrLn $ show grammar

