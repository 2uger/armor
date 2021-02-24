import ParserTypes

import Data.Map as Map (Map, member, lookup, insertWith)
import Data.Set as Set (Set, fromList)


data FirstSetMap = FirstSetMap (Data.Map.Map NonTerminal (Set Terminal)) deriving (Show, Read)

--
-- Utilities
--
getFirstSet :: NonTerminal -> FirstSetMap -> Set.Set Terminal
getFirstSet nonTerminal firstMap = f $ Map.lookup nonTerminal firstMap 
  where 
    f Nothing = Set.Set.empty
    f (Just set) = set

mergeIntoFirstSet :: FirstSetMap -> NonTerminal -> FirstSet -> FirstSetMap
mergeFirstSet map nonTerm newSet = Map.insertWith Set.union nonTerm newSet map

getNonTerminals :: Grammar -> Set NonTerminal

-- 
-- Create first set for current grammar
--
firstSet :: Grammar -> FirstSetMap
firstSet (grammarRule:remain) = first grammar (firstInit grammar)
  where
    -- Init empty sets for all non terminals
    firstInit :: Grammar -> FirstSetMap
    firstInit grammar = FirstSetMap $ 
        Map.fromList $ fmap f $ Set.toList $ getNonTerminals grammar
      where 
        f nt = (nt, Set.empty)

    -- Add Empty string for First set of every symbols
    firstEmptyString :: Grammar -> FirstSetMap

    -- Add terminals if it's first symbol in rhs 
    firstTerminal :: Grammar -> FirstSetMap

    first :: Grammar -> FirstSetMap -> FirstSetMap
    first (rule:grammar) firstSet = 
      where 
        firstRhs :: Rule -> Set Terminal
        firstRhs Rule x (yi:y) = 
            | yi == Right terminal = Set.singleton terminal
            | yi == Left nonterminal = if Set.member Epsilon firstOfNt
                                       then Set.Union termsForY rirstRhs Rule x y
                                       else termsForY
          where
            firstOfNt = getFirstSet nonterminal firstSet 
            termsForY = Set.delete Epsilon firstOfNt




followSet :: Map k v -> Map k v -> Map k v
followSet grammar follow = 

throughGrammar followSet grammarRule = 
    let
        rhs = head grammarRule
        lhs = tail grammarRule
        followSymbol = followSymb
    in

followSymbol symbols
    | is terminal last element = firstSet(symbol) - E
    | not is terminal last element  || terminal last and firstSet(terminal) U E = 
    
