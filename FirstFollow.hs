module FirstFollow where

import ParserTypes

import qualified Data.Map as Map
import qualified Data.Set as Set


type FirstSet = Set.Set Terminal
type FirstSetMap = Map.Map NonTerminal FirstSet

--
-- Utilities
--
--getFirstSet :: NonTerminal -> FirstSetMap -> Set.Set Terminal
--getFirstSet nonTerminal firstMap = f $ Map.lookup nonTerminal firstMap 
  --where 
    --f Nothing = Set.empty
    --f (Just set) = set

--mergeIntoFirstSet :: FirstSetMap -> NonTerminal -> FirstSet -> FirstSetMap
--mergeIntoFirstSet map nonTerm newSet = Map.insertWith Set.union nonTerm newSet map

getNonTerminals :: Grammar -> Set.Set NonTerminal 
getNonTerminals (rule:remain) = Set.union (Set.singleton (lhs rule)) (getNonTerminals remain)
getNonTerminals [] = Set.empty

insertIntoFirstSet :: FirstSetMap -> NonTerminal -> Terminal -> FirstSetMap
insertIntoFirstSet firstSet nterm term = Map.insertWith Set.union nterm (Set.singleton term) firstSet
-- 
-- Create first set for current grammar
--
--firstSet :: Grammar -> FirstSetMap
--firstSet (rule:remain) = first grammar (firstInit grammar)
--  where
    -- Init empty sets for all non terminals
firstSetInit :: Grammar -> FirstSetMap
firstSetInit grammar = Map.fromList 
                     $ fmap f 
                     $ Set.toList 
                     $ getNonTerminals grammar
  where 
    f = \x -> (x, Set.empty)

-- Add Empty string into First set of current production
-- if prod :: 'E' | ...
firstSetEmptyString :: Grammar -> FirstSetMap -> FirstSetMap
firstSetEmptyString (rule:remain) firstSet
    | [Left Epsilon] `elem` (rhs rule) = firstSetEmptyString remain $ insertIntoFirstSet firstSet prod Epsilon 
    | not $ [Left Epsilon] `elem` (rhs rule) = firstSetEmptyString remain firstSet
  where 
    prod = lhs rule

-- Add terminals if it's first symbol in rhs 
--firstSetTerminal :: Grammar -> FirstSetMap
--firstSetTerminal grammar firstSet = 
--
--firstSetRhs :: Grammar -> FirstSetMap -> FirstSetMap
--first (rule:grammar) firstSet = 
--  where 
--    firstRhs :: Rule -> Set Terminal
--    firstRhs Rule x (yi:y) = 
--        | yi == Right terminal = Set.singleton terminal
--        | yi == Left nonterminal = if Set.member Epsilon firstOfNt
--                                   then Set.Union termsForY rirstRhs Rule x y
--                                   else termsForY
--      where
--        firstOfNt = getFirstSet nonterminal firstSet 
--        termsForY = Set.delete Epsilon firstOfNt
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
