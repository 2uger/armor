module FirstFollow where

import ParserTypes

import qualified Data.Map as Map
import qualified Data.Set as Set


type FirstSet = Set.Set Terminal
type FirstSetMap = Map.Map NonTerminal FirstSet

type VisitedFirst = Set.Set (NonTerminal, Bool)

-- Set that show is non terminal could derive to 
-- empty string
firstSetEmptyString = Set.fromList [(NTerm "fsdkjf", True)]

--
-- Utilities
--
-- Return set of all non terminals from grammar
getNonTerminals :: Grammar -> Set.Set NonTerminal 
getNonTerminals (prod:remain) = Set.union (Set.singleton (lhs rule)) (getNonTerminals remain)
getNonTerminals [] = Set.empty

-- Insert terminal into first set map of non terminal
insertIntoFirstSet :: FirstSetMap -> NonTerminal -> Set.Set Terminal -> FirstSetMap
insertIntoFirstSet firstSet nterm termSet = Map.insertWith Set.union nterm termSet firstSet

-- Init empty set for all non terminals
firstSetInit :: Grammar -> FirstSetMap
firstSetInit grammar = Map.fromList 
                     $ fmap f 
                     $ Set.toList 
                     $ getNonTerminals grammar
  where 
    f = \x -> (x, Set.empty)

-- Return list of all rhs production for current non terminal
getRhsList :: Grammar -> NonTerminal -> [[Symbol]]
getRhsList (prod:remain) nterm
    | lhs prod == nterm = [rhs prod] : getRhsList remain nterm
    | otherwise = getRhsList remain nterm

-- Add Empty string into First set of current production
-- if prod :: 'E' | ...
--firstSetEmptyString :: Grammar -> FirstSetMap -> FirstSetMap
--firstSetEmptyString [] firstSet = firstSet
--firstSetEmptyString (prod:remain) firstSet
--    | [Left Epsilon] `elem` (rhs rule) = firstSetEmptyString remain $ insertIntoFirstSet firstSet prod (Set.singleton Epsilon) 
--    | otherwise = firstSetEmptyString remain firstSet
--  where 
--    prod = lhs rule

first :: [Symbol] -> FirstSetMap
first rhs = internalFirst visitedFirst rhs
  where
      visitedFirst = Set.map f $ getNonTerminals grammar 
      f = \x -> (x, False)

internalFirst :: VisitedFirst -> [Symbol] -> Set.Set Terminal
internalFirst visitedFirst (X:b)= 
    -- If X is empty = return Empty set
    case X of
      Right terminal = Set.singleton terminal
      Left nterm = if !(isVisited X)
                   then map (Set.union Set.emptySet) 
                        $ map internalFirst (makeVisitedTrue nterm) rhsList 
                   else if symbolDerivesEmpty
                        then Set.union Set.emptySet 
                                       $ internalFirst (makeVisited nterm) b
  where
    rhsSymbolList = getRhsSymbolList grammar nterm
    isVisited nterm = Set.member (nterm, True) visitedFirst
    makeVisitedTrue nterm = Set.insert (nterm, True) 
                            $ Set.delFromSet visitedFirst (nterm, False)
    symbolDerivesEmpty nterm = 

--
--
--
--
--followSet :: Map k v -> Map k v -> Map k v
--followSet grammar follow = 
--
--throughGrammar followSet grammarRule = 
--    let
--        rhs = head grammarRule
--        lhs = tail grammarRule
--        followSymbol = followSymb
--    in
--
--followSymbol symbols
--    | is terminal last element = firstSet(symbol) - E
--    | not is terminal last element  || terminal last and firstSet(terminal) U E = 
--    
