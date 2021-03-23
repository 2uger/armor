module Parser
() where

import ParserTypes
import ParseTree

import qualified Data.Map as Map
import qualified Data.Set as Set
{-
 - Wrtie RD parser function 
 - Think about representation of Parse tree foa
 -}

type FirstSet = Set.Set Terminal
type FirstSetMap = Map.Map NonTerminal FirstSet

type ParseOutput = ([Terminal], Bool, String, ParseTree)

-- Check if input token is one of nonterminal first set
inFirstSet :: NonTerminal -> Terminal -> Bool
inFirstSet nterm term = False 

parse :: NonTerminal -> [Terminal] -> ParseOutput
parse Program ts = 
    let (stream', b, er, pt') = parse DeclList ts
    in (stream', b, "", NodeProgramm pt') 

parse DeclList ts =
    let (stream', b1, er, pt') = parse Decl ts
        (stream'', b2, er', pt'') = parse DeclListN stream'
    in (stream'', b1 && b2, "", NodeDeclList pt' pt'')

parse DeclListN ts@(t:stream)
    | inFirstSet DeclListN t = 
        let (stream', b1, er, pt') = parse Decl ts
            (stream'', b2, er', pt'') = parse DeclListN stream'
        in (stream'', b1 && b2, "",  NodeDeclListN pt' pt'')
    | otherwise = (ts, True, "", NodeDeclListN EmptyTree EmptyTree)

parse Decl ts = 
    let (stream', b1, er, ptVarDecl) = parse VarDecl ts
    in case b1 of
           True -> (stream', b1, "",  NodeDecl ptVarDecl)
           otherwise -> let (stream'', b2, er, ptFuncDecl) = parse FuncDecl stream'
                        in (stream'', b2, er, NodeDecl ptFuncDecl)

parse VarDecl ts = 
    let (stream', b1, er, ptTS) = parse TypeSpec ts
        (stream'', b2, er', ptVDL) = parse VarDeclList stream'
    in case head stream'' of 
           TermBackQuote  -> (tail stream'', True, "", NodeVarDecl ptTS ptVDL TermBackQuote)
           otherwise      -> (stream'', False, "Missing token back quote", 
                              NodeVarDecl ptTS ptVDL TermEmpty)

parse ScopedVarDecl ts@(t:stream)
    | t == TermStatic = 
        let (stream', b1, er', pt')    = parse TypeSpec stream
            (stream'', b2, er'', pt'') = parse VarDeclList stream'
        in case head stream'' of 
               TermBackQuote -> (tail stream'', True, "", treeNode TermStatic pt' pt'' TermBackQuote)
               otherwise     -> (stream'', False, "Missing token back quote", EmptyTree)
    | otherwise = 
        let (stream', b1, er', pt') = parse TypeSpec ts
            (stream'', b2, er'', pt'') = parse VarDeclList stream'
        in case head stream'' of
               TermBackQuote -> (tail stream'', True, "", treeNode TermEmpty pt' pt'' TermBackQuote)
               otherwise     -> (stream'', False, "Missing token back quote", EmptyTree)
  where
    treeNode = NodeScopedVarDecl 

parse TypeSpec ts@(t:stream)
    | inFirstSet TypeSpec t = 
        case t of
            TermInt  -> (stream, True, "", treeNode TermInt)
            TermBool -> (stream, True, "", treeNode TermBool)
            TermChar -> (stream, True, "", treeNode TermChar)
            otherwise -> (stream, False, "Bad type spec token", treeNode TermEmpty)
    | otherwise = (ts, False, "Bad type spec token", treeNode TermEmpty)
  where
    treeNode = NodeTypeSpec

parse VarDeclList ts@(t:stream)=
    let (stream', b1, er', pt') = parse VarDeclInit ts
        (stream'', b2, er'', pt'') = parse VarDeclListN stream' 
    in (stream'', b1 && b2, "", NodeVarDeclList pt' pt'')

parse VarDeclListN ts@(t:stream)
    | t == TermComma = 
        let (stream', b1, er', pt') = parse VarDeclInit ts
            (stream'', b2, er'', pt'') = parse VarDeclListN stream'
        in (stream'', b1 && b2, er'', NodeVarDeclListN TermComma pt' pt' )
    | otherwise = (ts, True, "", NodeVarDeclListN TermEmpty EmptyTree EmptyTree)

parse VarDeclInit ts = 
    let (stream', b1, er, ptVarDeclId) = parse VarDeclId ts
    in case head stream' of
           TermDoubleDot   -> let (stream'', b2, er2, ptSimpleExpr) = parse SimpleExpr stream'
                         in (stream'', b2, er2, treeNode ptVarDeclId TermColon ptSimpleExpr)
           otherwise -> (stream', b1, "", treeNode ptVarDeclId TermEmpty EmptyTree)
  where
    treeNode = NodeVarDeclInit

parse VarDeclId ts@(t:nt:stream)
    | t == TermId = case nt of
                         TermLSqBracket -> case matchLongStr of
                                         True -> (drop 2 stream, True, "", 
                                                  NodeVarDeclId TermId TermLSqBracket TermNumConst TermRSqBracket)
                                         False -> (stream, False, 
                                                   "Missing tokenNumCons or tokenRParen", 
                                                   EmptyTree)
                         otherwise -> (nt:stream, True, "", NodeVarDeclId TermId TermEmpty TermEmpty TermEmpty)
    | otherwise = (ts, False, "Missing token ID", EmptyTree)

  where 
    matchLongStr = (take 2 stream == [TermNumConst, TermRSqBracket])

parse FuncDecl ts = (ts, True, "", EmptyTree)
parse SimpleExpr ts = (ts, True, "", EmptyTree)

--parse FuncDecl ts = 
--    let
--        (stream', b1, er1, ptTypeSpec) = parse TypeSpec ts
--        (stream'', b2)                 = (tail stream') (head stream' == TokenID) 
--        (stream''', b3)                = (tail stream'') (head stream'' == TokenLParen)
--        (stream'''', b4, er4, ptParms) = parse Parms stream'''
--        (stream''''', b5)              = (tail stream'''') (head stream'''' == TokenRParen)
--        (stream'''''', b6)             = parse Stmt stream'''''
--    in (stream'''''', b1 && b2 && b3 && b4 && b5 && b6, er4, NodeFuncDecl)
--
--parse Parms ts@(t:st) =
--    | inFirstSet Parms t = 
--        let (st', b1, pt) = parse ParmList ts
--        in (st', b1, NodeParms pt)
--    | otherwise = (ts, NodeParms TermEpsilon)
--
--parse ParmList ts@(t:st) = 
--    (ts, True, NodeParmList EmptyNode EmptyNode)
