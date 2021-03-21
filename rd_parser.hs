{-
 - Wrtie RD parser function 
 - Think about representation of Parse tree foa
 -}

type FirstSet = Set.Set Terminal
type FirstSetMap = Map.Map NonTerminal FirstSet

type ParseOutput = ([Terminal], Bool, PT)

-- Check if input token is one of nonterminal first set
inFirstSet :: NonTerminal -> Terminal -> Bool
inFirstSet nterm term = 


parse :: NonTerminal -> [Terminal] -> PT -> ParseOutput
parse Program ts@(t:stream) (NodeProgramm pt) = 
    let (stream', b, pt') = parse DeclList ts
    in (stream', b, NodeProgramm pt') 

parse DeclList ts@(t:stream) = 
    let (stream', b1, pt') = parse Decl ts
        (stream'', b2, pt'') = parse DeclListN stream'
    in (stream'', b1 && b2, NodeDeclList pt' pt'')

parse DeclListN ts@(t:stream)
    | inFirstSet DeclListN t = 
        let (stream', b1, pt') = parse Decl stream
            (stream'', b2, pt'') = parse DeclListN stream'
        in (stream'', b1 && b2, NodeDeclListN pt' pt'')
    | otherwise = (ts, True)

-- ERRROROROOR
parse Decl ts@(t:stream) = 
    let (stream', b1) = parse VarDecl stream
    in case b1 of
           True -> (stream', b1)
           False -> parse FuncDecl stream

parse VarDecl ts@(t:stream) = 
    let (stream', b1) = parse TypeSpec stream
        (stream'', b2) = parse VarDeclList stream'
    in case head stream'' of 
           TokenBackQuote -> (True, tail stream'')
           otherwise      -> (False, stream'')

parse ScopedfVarDecl ts@(t:stream)
    | t == TermStatic = 
        let (stream', b1) = parse TypeSpec stream
            (stream'', b2) = parse VarDeclList stream'
        in case head steam'' of 
               TokenBaskQUote -> (True, tail stream'')
               otherwise      -> (False, stream'')
    | otherwise = 
        let (stream', b1) = parse TypeSpec stream
            (stream'', b2) = parse VarDeclList stream'
        in case head stream'' of
               TokenBackQuote -> (True, tail stream'')
               otherwise -> (False, stream'')

parse TypeSpec ts@(t:stream)
    | inFirstSet TypeSpec t = (True, stream)
    | otherwise = (False, ts)

parse VarDeclList ts@(t:stream)=
    let (stream', b1) = parse VarDeclInit ts
        (stream'', b2) = parse VarDeclListN stream' 
    in (b1 && b2, stream'')

parse VarDeclListN ts@(t:stream)
    | t == TermZap = 
        let (stream', b1) = parse VarDeclInit ts
            (stream'', b2) = parse VarDeclListN stream'
        in (b1 && b2, stream'')
    | otherwise = (True, ts)

parse VarDeclInit ts@(t:stream) = 
    let (b1, stream') = parse VarDeclId ts
    in case head stream' of
         TokenDD -> parse SimpleExpr stream'
         otherwise -> (b1, stream')

parse VarDeclId ts@(t:nt:stream) = 
    | nt == TokenID = case nt of
                          TokenLSP -> case matchLongStr of ->
                                          True -> (True, drop 2 stream)
                                          False -> (False, stream)
                          otherwise -> (True, nt:stream)
    | otherwise = (False, ts)

  where 
    matchLongStr = take 2 stream == [TokenNumConst, TokenRParen] 

parse FuncDecl ts@(t:stream) = 
    let
        (b1, stream') = parse TypeSpec ts
        (b2, stream'') = (head stream' == TokenID) (tail stream')
        (b3, stream''') = (head stream'' == TokenLParen) (tail stream'')
        (b4, stream'''') = parse Parms stream'''
        (b5, stream''''') = (head stream'''' == TokenRParen) (tail stream'''')
        (b6, stream'''''') = parse Stmt stream'''''
    in (b1 && b2 && b3 && b4 && b5 && b6, stream'''''')

parse Parms ts@(t:st) =
    | inFirstSet Parms t = 
        let (b1, st', pt) = parse ParmList ts
        in (b1, st', NodeParms pt)
    | otherwise = (True, ts, NodeParms TermEpsilon)
