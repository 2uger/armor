module ParserTypes where

import Data.Either

data Terminal = Term String | Dollar | Epsilon deriving (Show, Eq, Ord, Read)

data NonTerminal = NTerm String deriving (Show, Eq, Ord)

type Symbol = Either Terminal NonTerminal

data Production = Production { lhs :: NonTerminal, rhs :: [Symbol] } deriving (Show) 

type Grammar = [Production]
