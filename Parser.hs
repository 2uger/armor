module Parser where

import ParserTypes
import ParseTree

import qualified Data.Map as Map
import qualified Data.Set as Set

type FirstSet = Set.Set Terminal
type FirstSetMap = Map.Map NonTerminal FirstSet

-- Return terminals and node for parse tree
-- throwing an error when catch false terminal

parse Program terms =
    let (terms', nodeDeclList) = parse DeclList terms
    in (terms', NodeProgram nodeDeclList)

parse DeclList terms =
    let (terms', nodeDecl) = parse Decl terms
    in (terms', NodeDeclList nodeDecl)

parse Decl terms =
    let (terms', nodeVarDecl) = parse FuncDecl terms
    in (terms', NodeDecl nodeVarDecl)

parse VarDecl terms = 
    let (terms', nodeTypeSpec) = parse TypeSpec terms
        ((term:xs), nodeVarDeclList) = parse VarDeclList terms'
    in case term of
           TermBackQuote -> (xs, NodeVarDecl nodeTypeSpec nodeVarDeclList TermBackQuote)
           otherwise -> error $ "Error parsing TermBackQuote" ++ show terms'
           
parse TypeSpec (term:xs)
    | term == TermInt = (xs, NodeTypeSpec TermInt)
    | term == TermBool = (xs, NodeTypeSpec TermBool)
    | term == TermChar = (xs, NodeTypeSpec TermChar)
    | otherwise = error "Error parsing type spec"

parse VarDeclList terms = 
    let (terms', nodeVarDeclInit) = parse VarDeclInit terms
    in (terms', NodeVarDeclList nodeVarDeclInit)

parse VarDeclInit terms = 
    let (terms', nodeVarDeclId) = parse VarDeclId terms
        (terms'', nodeVarDeclInit) = parse SimpleExpr $ parseColon terms'
    in (terms'', NodeVarDeclInit nodeVarDeclId TermColon nodeVarDeclInit)
  where
    parseColon (term:xs)
        | term == TermColon = xs
        | otherwise = error "Error parsing Colon"

parse VarDeclId terms =
    let
        res = parseTerm terms >>= parseTerm >>= parseTerm >>= parseTerm
    in case res of
           Just terms' -> (terms', NodeVarDeclId TermId TermLSqBracket TermNumConst TermRSqBracket) 
           Nothing -> error "Failed to match strings"
  where 
    parseTerm :: [Terminal] -> Maybe [Terminal]
    parseTerm (TermId:xs) = Just xs
    parseTerm (TermLSqBracket:xs) = Just xs
    parseTerm (TermNumConst:xs) = Just xs
    parseTerm (TermRSqBracket:xs) = Just xs
    parseTerm _ = Nothing

parse SimpleExpr terms =
    (terms, NodeSimpleExpr EmptyTree EmptyTree)


-- TODO: parseTerm match TermRParen all time
--       think about make this thing much cooler
parse FuncDecl terms =
    (terms''''', NodeFuncDecl nodeTypeSpec TermId TermLSqBracket nodeParms TermRSqBracket nodeStmt)
  where
    (terms', nodeTypeSpec) = parse TypeSpec terms
    terms'' = case parseTerm terms' >>= parseTerm of
                  Just terms -> terms
                  Nothing -> error "Error while parsing id and lparen"
    (terms''', nodeParms) = parse Parms terms''
    terms'''' = case parseTerm terms''' of
                 Just terms -> terms
                 Nothing -> error "Errro while parsing rparen"
    (terms''''', nodeStmt) = parse Stmt terms''''

    parseTerm (TermId:xs) = Just xs
    parseTerm (TermLParen:xs) = Just xs
    parseTerm (TermRParen:xs) = Just xs
    parseTerm _ = Nothing

parse Parms terms = let (terms', nodeParmType) = parse ParmType terms
                    in (terms', NodeParms nodeParmType)

parse ParmType terms = let (terms', nodeTypeSpec) = parse TypeSpec terms
                           (terms'', nodeParmId) = parse ParmId terms'
                       in (terms'', NodeParmType nodeTypeSpec nodeParmId)

parse ParmId (term:xs)
    | term == TermId = (xs, NodeParmId TermId)
    | otherwise = error "Error while parsing ID"

parse Stmt terms = (terms, NodeStmt EmptyTree)
