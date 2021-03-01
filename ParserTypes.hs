module ParserTypes where

import Data.Either

data Terminal = Term String | Dollar | Epsilon deriving (Show, Read)

data NonTerminal = NTerm String deriving (Show)

type Symbol = Either Terminal NonTerminal

data Rule = Rule { lhs :: NonTerminal, rhs :: [[Symbol]] } deriving (Show) 

data Grammar = Grammar [Rule]
