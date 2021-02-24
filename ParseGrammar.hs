module ParseGrammar
( parseGrammar ) where

import ParserTypes

import Data.Maybe (isJust, fromJust)
import Data.List (sortBy)
import Data.Function (on)

parseGrammar :: String -> Grammar 
parseGrammar textGrammar = map grammarRule grammarLines
  where 
    removeWS = \x -> x /= ""
    removeComments = \x -> head x /= '#'
    grammarLines = map words . filter removeComments . filter removeWS $ lines textGrammar

grammarRule :: [String] -> Rule
grammarRule (symbol:remain) = Rule (Symbol symbol) rhs 
    where 
        rhs = initRhs remain

initRhs :: [String] -> [Symbol]
initRhs (symbol:remain) = (Symbol symbol :: Symbol) : initRhs remain 
initRhs [] = []

