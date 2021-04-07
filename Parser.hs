module Parser
(parse) where

import ParserTypes
import ParseTree

import qualified Data.Map as Map
import qualified Data.Set as Set

type FirstSet = Set.Set Terminal
type FirstSetMap = Map.Map NonTerminal FirstSet

-- Every parse func will return bool flag showing
-- is parsing ok or not, string will show error output
-- if error occured
type ParseOutput = ([Terminal], Bool, [String], ParseTree)

-- Check if input token is one of nonterminal first set
inFirstSet :: NonTerminal -> Terminal -> Bool
inFirstSet nterm term = False 

-- TermInt TermId TermBackQuote

parse :: NonTerminal -> [Terminal] -> ParseOutput
parse Program ts = 
    let (stream', status, err, ptDeclList) = parse DeclList ts
    in case status of
           True  -> (stream', status, [""], NodeProgramm ptDeclList) 
           False -> (stream', status, err, NodeProgramm ptDeclList)

parse DeclList ts =
    let (stream', status', err', ptDecl) = parse Decl ts
        (stream'', status'', err'', ptDeclListN) = parse DeclListN stream'
    in (stream'', status' && status'', err' ++ err'', NodeDeclList ptDecl ptDeclListN)

parse DeclListN ts@(t:stream)
    | inFirstSet DeclListN t = 
        let (stream', status', err', ptDecl) = parse Decl ts
            (stream'', status'', err'', ptDeclListN) = parse DeclListN stream'
        in (stream'', status' && status'', err' + err'',  NodeDeclListN ptDecl ptDeclListN)
    | otherwise = (ts, True, "", NodeDeclListN EmptyTree EmptyTree)

parse Decl ts = 
    let (stream', status', err', ptVarDecl) = parse VarDecl ts
    in case status' of
           True -> (stream', status', err',  NodeDecl ptVarDecl)
           _    -> let (stream'', status'', err'', ptFuncDecl) = parse FuncDecl stream'
                   in (stream'', status'', err'', NodeDecl ptFuncDecl)

parse VarDecl ts = 
    let (stream', status', err', ptTypeSpec) = parse TypeSpec ts
        (stream'', status'', err'', ptVarDeclList) = parse VarDeclList stream'
    in case head stream'' of 
           TermBackQuote -> (tail stream'', True, "", NodeVarDecl ptTypeSpec ptVarDeclList TermBackQuote)
           _             -> (stream'', False, "Missing token back quote", 
                              NodeVarDecl ptTypeSpec ptVarDeclList TermEmpty)

parse ScopedVarDecl ts@(t:stream)
    | t == TermStatic = 
        let (stream', status', err', ptTypeSpec) = parse TypeSpec stream
            (stream'', status'', err'', ptVarDeclList) = parse VarDeclList stream'
        in case head stream'' of 
               TermBackQuote -> (tail stream'', True, "", treeNode TermStatic ptTypeSpec ptVarDeclList TermBackQuote)
               _             -> (stream'', False, "Missing token back quote", EmptyTree)
    | otherwise = 
        let (stream', status', err', ptTypeSpec) = parse TypeSpec ts
            (stream'', status'', err'', ptVarDeclList) = parse VarDeclList stream'
        in case head stream'' of
               TermBackQuote -> (tail stream'', True, "", treeNode TermEmpty ptTypeSpec ptVarDeclList TermBackQuote)
               _             -> (stream'', False, "Missing token back quote", EmptyTree)
  where
    treeNode = NodeScopedVarDecl 

parse TypeSpec ts@(t:stream)
    | t == TermInt = (stream, True, "", treeNode TermInt)
    | t == TermBool = (stream, True, "", treeNode TermBool)
    | t == TermChar = (stream, True, "", treeNode TermChar)
    | otherwise = (ts, False, "Bad type spec token", treeNode TermEmpty)
  where
    treeNode = NodeTypeSpec

parse VarDeclList ts@(t:stream)=
    let (stream', status', err', ptVarDeclInit) = parse VarDeclInit ts
        (stream'', status'', err'', ptVarDeclListN) = parse VarDeclListN stream' 
    in (stream'', status' && status'', err' ++ err'', NodeVarDeclList ptVarDeclInit ptVarDeclListN)

parse VarDeclListN ts@(t:stream)
    | t == TermComma = 
        let (stream', status', err', ptVarDeclInit) = parse VarDeclInit ts
            (stream'', status'', err'', ptVarDeclListN) = parse VarDeclListN stream'
        in (stream'', status' && status'', err' ++ err'', NodeVarDeclListN TermComma ptVarDeclInit ptVarDeclListN )
    | otherwise = (ts, True, "", NodeVarDeclListN TermEmpty EmptyTree EmptyTree)

parse VarDeclInit ts = 
    let (stream', status', err', ptVarDeclId) = parse VarDeclId ts
    in case head stream' of
           TermDoubleDot -> let (stream'', status'', err'', ptSimpleExpr) = parse SimpleExpr stream'
                            in (stream'', status'', err' ++ err'', treeNode ptVarDeclId TermColon ptSimpleExpr)
           _             -> (stream', status', "", treeNode ptVarDeclId TermEmpty EmptyTree)
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
                                               _     -> (nt:stream, True, "", NodeVarDeclId TermId TermEmpty TermEmpty TermEmpty)
    | otherwise = (ts, False, "Missing token ID", EmptyTree)

  where 
    matchLongStr = (take 2 stream == [TermNumConst, TermRSqBracket])

parse FuncDecl ts = 
    let ((t':nt':stream'), status', err', ptTypeSpec) = parse TypeSpec ts
    in case t' of
           TermId -> case nt' of
                         TermLParen -> let ((t'':stream''), status'', err'', ptParms) = parse Parms stream'
                                       in case t'' of
                                              TermRParen -> parse Stmt stream''
                                              _          -> (t'':stream'', False, 
                                                             "Missing right brace in func decl", EmptyTree)
                           _        -> (nt:stream', False, "Missing left brace in func declaration", EmptyTree) 
           _      -> (t':nt':stream', False, "Missing ID afte type specification", EmptyTree)

parse Parms ts = 
    let (stream', status', err', prParmList) = parse ParmList ts
    in case status' of
           True -> (stream', status', err', ptParmList)
           False -> (stream', True, "", EmptyTree)  

parse ParmList ts = 
    let (stream', status', err', prParmTypeList) = parse ParmTypeList ts
        (stream'', status'', err'', prParmListN) = parse ParmListN stream'
    in (stream'', status' && status'', err' ++ err'', NodeParmList prParmTypeList prParmListN)

parse ParmListN ts@(t:stream) =
    case t of
        TermComma -> let (stream', status', err', prParmTypeList) = parse ParmTypeList stream
                         (stream'', status'', err'', prParmListN) = parse ParmListN stream'
                     in (stream'', status'  && status'', err' ++ err'', NodeParmListN prParmTypeList prParmListN)
        _         -> (ts, True, "", EmptyTree)

parse ParmTypeList ts = 
    let (stream', status', err', prTypeSpec) = parse TypeSpec ts
        (stream'', status'', err'', prParmId) = parse ParmId stream'
    in (stream'', status' && status'', err' ++ err'', NodeParmTypeList  prTypeSpec prParmId)

parse ParmId ts@(t:nt:nnt:stream) = 
    | t == TermId = case nt of
                        TermLSqBracket -> case nnt of
                                              TermRSqBracket -> (stream, True, "", NodeParmId t nt nnt)
                                              _              -> (nnt:stream, False, "Missing right square bracket", NodeParmId t nt EmptyTree)
                      _              -> (nt:nnt:stream, True, "", NodeParmId t EmptyTree EmptyTree EmptyTree)
    | otherwise = (ts, False, "Miss match parm id", EmptyTree)

parse Stmt ts 
    | status' = parse'
    | status'' = parse''
    | status''' = parse'''
    | status'''' = parse''''
    | status''''' = parse'''''
    | otherwise = (ts, False, "Failed to parse one of the statement form", EmptyTree)
  where
    parse' = parse ExprStmt ts
    (stream', status', err', ptExprStmt) = parse'

    parse'' = parse CompoundStmt stream'
    (stream'', status'', err'', prCompoundStmt) = parse''

    parse''' = parse IterStmt stream''
    (stream''', satus''', err''', prIterStmt) = parse'''

    parse'''' = parse ReturnStmt stream'''
    (stream'''', status'''', err'''', ptReturnStmt) = parse''''

    parse''''' = parse BreakStmt stream''''
    (stream''''', status''''', err''''', ptReturnStmt) = parse'''''

parse ExprStmt ts@(t:stream)
    | status' = case t' of 
                    TermColon -> (stream', True, "", NodeExprStmt ptExpr t)
                    _         -> (t':stream', False, "Missing colon after expr", EmptyTree)
    | otherwise = case t of
                      TermColon -> (stream, True, "", NodeExprStmt EmptyTree t)
                      _         -> (ts, False, "Missing colon in stmt", EmptyTree)
    let (stream', status', err', prExpr) = parse Expr ts
    in case of
  where
    parse' = parse Expr ts
    ((t':stream'), status', err', prExpr) = parse Expr ts

parse CompoundStmt ts@(t:stream)
    | t == TermLBrace = case status' of 
                            True -> case status'' of
                                        True -> case head stream'' of
                                                    TermRBrace -> (stream'', True, "", 
                                                                   NodeCompoundStmt ptLocalDecls ptStmtList)
                                                    _          -> (stream'', False, 
                                                                   "Missing right brace after stmt list", EmptyTree)
                                        _    -> (stream', status'', err'', EmptyTree)
                            _    -> (stream, status', err', EmptyTree)
    | otherwise = (ts, False, "Missing left brace in compound statement", EmptyTree)
  where
    parse' = parse LocalDecls stream
    (stream', status', err', ptLocalDecls) = parse'

    parse'' = parse StmtList stream'
    (stream'', status'', err'', ptStmtList) = parse''

parse StmtList ts =
    | status' = case status' of
                    True -> parse''
                    False -> (stream', False, err'', EmptyTree)
    | otherwise = (ts, False, err', EmptyTree)
  where
    parse' = parse Stmt ts
    (stream', status', err', ptStmt) = parse'

    parse'' = parse StmtListN stream'
    (stream'', status'', err'', ptStmtListN) = parse''

parse StmtListN ts
    | status' = parse'
    | otherwise = (ts, True, "", NodeStmtListN EmptyTree)
  where
    parse' = parse Stmt ts
    (stream', status', err', ptStmt) = parse'

parse LocalDecl ts
    | status' = case status'' of
                    True -> (stream'', True, "", NodeLocalDecl ptScopedVarDecl ptLocalDeclN) 
                    _    -> (stream', status'', err'', EmptyTree)
    | otherwise = (ts, True, "", EmptyTree)
  where
    parse' = parse ScopedVarDecl ts
    (stream', status', err', ptScopedVarDecl) = parse'

    parse'' = parse LocadDeclN stream'
    (stream'', status'', err'', ptLocalDeclN) = parse''

parse LocalDeclN ts
    | status' = case status'' of
                    True -> (stream'', True, NodeLocalDecl ptScopedVarDecl ptLocalDeclN) 
                    _    -> error "Failed to parse LocalDeclN"
    | otherwise = (ts, True, EmptyTree)
  where
    parse' = parse ScopedVarDecl ts
    (stream', status', ptScopedVarDecl) = parse'

    parse'' = parse LocadDeclN stream'
    (stream'', status'', ptLocalDeclN) = parse''

parse IterStmt ts@(t:nt:stream)
    | t == TermWhile = case nt of
                           TermLParen -> 
                               case statusSE of
                                   True -> 
                                       case head streamSE of
                                           True -> 
                                               case statusStmt of
                                                   True -> (streamStmt, True, 
                                                            NodeIterStmt t nt ptSimpleExpr (head streamSE) ptStmt)
                                                   _    -> (ts, False, EmptyTree)
                                           other-> 
                                               (ts, False, EmptyTree)
                                    _   ->
                                        (ts, False, EmptyTree)
                            other     ->
                                (ts, False, EmptyTree)
    | otherwise = (ts, False, EmptyTree)
  where
    parseSE = parse SimpleExpr stream
    (streamSE, statusSE, ptSimplExpr) = parseSE

    parseStmt = parse Stmt tail streamSE
    (streamStmt, statusStmt, ptStmt) = parseStmt

parse ReturnStmt ts@(t:nt:stream)
    | t == TermReturn = case statusExpr of
                            True -> case head streamExpr of
                                        TermColon -> (tail streamExpr, True, treeNode t ptExpr (head streamExpr))
                                        _         -> (ts, False, EmptyTree)
                            False -> case nt of
                                         TermColon -> (stream, True, treeNode t nt)
                                         _         -> (ts, False, EmptyTree)
    | otherwise = (ts, False, EmptyTree)
                            
  where
    parseExpr = parse Expr ts
    (streamExpr, statusExpr, ptExpr) = parseExpr
    treeNode = NodeReturnStmt

parse StmtList ts
parse SimpleExpr ts = (ts, True, "", EmptyTree)

parse x [] = ([], True, "", EmptyTree)

parse SumExpr ts = 
    let (stream', b1, er1, pt, ast) = parse MulExpr ts
        (stream'', b1, er2, pt, ast2) = parse SumExprN stream'
    in ExprAdd ast ast2

parse MulExpr ts(t:stream) = 
    (.., .., .., ExprAdd(ExprValue t, ExprEmpty))
    
        
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
