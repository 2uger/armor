module Parser where

import ParserTypes
import ParseTree

import qualified Data.Map as Map
import qualified Data.Set as Set

type FirstSet = Set.Set Terminal
type FirstSetMap = Map.Map NonTerminal FirstSet

nonTermError :: NonTerminal -> String
nonTermError nterm = "Catching error while trying parse non terminal rule: " ++ show nterm

termError :: [Terminal] -> Terminal -> String

termError expect actual = "\nCatching error while trying parse terminal\nExpected: " ++ show expect ++ "\nActual: " ++ show actual

termError expect _ = "\nTrying to parse terminal " ++ show expect ++ "\nBut there is empty one\n"

-- Return terminals and node for parse tree
-- throwing an error when catch false terminal

parseProgram :: [Terminal] -> Maybe ([Terminal], ParseTree)
parseProgram terms =
    case res of
        Just (nterms, nnodes) -> Just (nterms, NodeProgram (nnodes !! 0))
        _                     -> error $ nonTermError Program
  where
    res = Just (terms, []) >>= parse DeclList

parse :: NonTerminal -> ([Terminal], [ParseTree]) -> Maybe ([Terminal], [ParseTree])

parse DeclList (terms, nodes) =
    case res of
        Just (nterms, nnodes) -> Just (nterms, [NodeDeclList (nnodes !! 0)] ++ nodes)
        _                     -> error $ nonTermError DeclList
  where
    res = Just (terms, []) >>= parse Decl

parse Decl (terms, nodes) =
    case res of
        Just (nterms, nnodes) -> Just (nterms, [NodeDecl (nnodes !! 0)] ++ nodes)
        _                     -> error $ nonTermError DeclList
  where
    res = Just (terms, []) >>= parse VarDecl


parse VarDecl (terms, nodes) =
    case res of
        Just (nterms, nnodes) -> Just (nterms, [NodeVarDecl (nnodes !! 0) (nnodes !! 1) (nnodes !! 2)] ++ nodes)
        _                     -> error $ nonTermError VarDecl
  where
    res = Just (terms, []) >>= parse TypeSpec >>= parse VarDeclInit >>= parseTerm TermBackQuote

parse TypeSpec ((term:xs), nodes)
    | term `elem` [TermInt, TermBool, TermChar] = Just (xs, [Leaf term] ++ nodes)
    | otherwise = error $ termError [TermInt, TermBool, TermChar] term

parse VarDeclInit (terms, nodes) =
    case res of
        Just (nterms, nnodes) -> Just (nterms, [NodeVarDeclInit (nnodes !! 0) (nnodes !! 1) (nnodes !! 2)] ++ nodes)
        _                     -> error $ nonTermError VarDeclInit
  where
    res = Just (terms, []) >>= parse VarDeclId >>= parseTerm TermColon >>= parse SimpleExpr

parse VarDeclId (terms, nodes) =
    case res of
        Just (nterms, nnodes) -> Just (nterms, [NodeVarDeclId (nnodes !! 0) (nnodes !! 1) (nnodes !! 2) (nnodes !! 3)] ++ nodes)
        _                     -> error $ nonTermError DeclList
  where
    res = Just (terms, []) >>= parseTerm TermId >>= parseTerm TermLSqBracket >>= parseTerm TermNumConst >>= parseTerm TermRSqBracket

parse SimpleExpr (terms, nodes) = Just (terms, [NodeSimpleExpr EmptyTree EmptyTree] ++ nodes)

--createParseTreeNode :: ParseTree -> [ParseTree] -> ParseTree
--createParseTreeNode pt nodes
--    | length nodes == 1 = pt (nodes !! 0)
--    | length nodes == 2 = pt (nodes !! 0) (nodes !! 1)
--    | length nodes == 3 = pt (nodes !! 0) (nodes !! 1) (nodes !! 2)
--    | length nodes == 4 = pt (nodes !! 0) (nodes !! 1) (nodes !! 2) (nodes !! 3)
--    | length nodes == 5 = pt (nodes !! 0) (nodes !! 1) (nodes !! 2) (nodes !! 3) (nodes !! 4)

parseTerm :: Terminal -> ([Terminal], [ParseTree]) -> Maybe ([Terminal], [ParseTree])
parseTerm expect ((term:xs), nodes)
    | expect == term = Just (xs, [Leaf term] ++ nodes) 
    | otherwise = error $ termError [expect] term

parseTerm expect ([], nodes) = error $ termError [expect] TermEmpty 
