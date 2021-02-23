module ParseGrammar
( parseGrammar ) where

import Parser (Symbol, Rule, Grammar,)

import Data.Maybe (isJust, fromJust)
import Data.List (sortBy)
import Data.Function (on)

parseGrammar :: String -> Grammar
parseGrammar textGrammar = map grammarRule grammarLines
  where 
      removeWS = \x -> x /= ""
      removeComments = \x -> head x /= '#'
      grammarLines = map words . filter removeComments . filter removeWS $ lines textGrammar

grammarRule :: [String] -> Maybe Rule
grammarRule (symbol:remain) = Just $ Rule (Symbol symbol) rhs 
    where 
        rhs = initRhs remain
grammarRule [] = Nothing

initRhs :: [String] -> [Symbol]
initRhs (symbol:remain) = (Symbol symbol :: Symbol) : initRhs remain 
initRhs [] = []

