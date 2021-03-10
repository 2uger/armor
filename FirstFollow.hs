module FirstFollow where

import ParserTypes

import qualified Data.Map as Map
import qualified Data.Set as Set


type FirstSet = Set.Set Terminal
type FirstSetMap = Map.Map NonTerminal FirstSet

--
-- Utilities
--
-- Return set of all non terminals from grammar
getNonTerminals :: Grammar -> Set.Set NonTerminal 
getNonTerminals (prod:remain) = Set.union (Set.singleton (lhs rule)) (getNonTerminals remain)
getNonTerminals [] = Set.empty

-- Insert terminal into first set map of non terminal
insertIntoFirstSet :: FirstSetMap -> NonTerminal -> Set.Set Terminal -> FirstSetMap
insertIntoFirstSet firstSet nterm term = Map.insertWith Set.union nterm term firstSet

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
firstSetEmptyString :: Grammar -> FirstSetMap -> FirstSetMap
firstSetEmptyString [] firstSet = firstSet
firstSetEmptyString (prod:remain) firstSet
    | [Left Epsilon] `elem` (rhs rule) = firstSetEmptyString remain $ insertIntoFirstSet firstSet prod (Set.singleton Epsilon) 
    | otherwise = firstSetEmptyString remain firstSet
  where 
    prod = lhs rule

-- Return list of all rhs production for current non terminal
getRhsList :: Grammar -> NonTerminal -> [[Symbol]]
getRhsList (prod:remain) nterm
    | lhs prod == nterm = [rhs prod] : getRhsList remain nterm
    | otherwise = getRhsList remain nterm


firstSetRhs :: Grammar -> FirstSetMap -> FirstSetMap
first (rule:grammar) firstSet = 
  where 
    firstRhs :: Rule -> Set.Set Terminal
    firstRhs Rule lhs (X:b) = 
        case X of
          Right terminal = Set.singleton terminal
          Left nterm = if !visitedFirst(X)
                       then Set.Union map (firstRhs lhs) rhsList
                       else case symbolDerivesEmpty(X) of
                              True -> firstRhs b
                              False -> Set.empty

      where
          rhsList = getRhsList grammar nterm

internalFirst :: [Symbol] -> Set.Set Terminal
internalFirst (X:b) = 
    case X of
      Right terminal = Set.singleton terminal
      Left nterm = if !visitedFirst(X)
                   then Set.Union map (firstRhs lhs) rhsList
                   else case symbolDerivesEmpty(X) of
                          True -> firstRhs b
                          False -> Set.empty

  where
          rhsList = getRhsList grammar nterm

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
