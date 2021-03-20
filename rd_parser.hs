{-
 - Wrtie RD parser function 
 - Think about representation of Parse tree foa
 -}

data Terminal = TermEpsilon 
              | TermBackQuote 
              | TermINT 
              | TermBOOL
              | TermCHAR
              | TermComma
              | TermDot... 

data NonTerminal = Program 
                 | DeclList 
                 | DeclListN 
                 | VarDecl
                 | ScopedVarDecl 
                 | TypeSpec
                 | 



-- Parse tree 
data PT = EmptyTree 
        | NodeProgramm ParseTree 
        | NodeDeclList ParseTree
        | NodeDeclListN ParseTree ParseTree
        | NodeDecl ParseTree
        | NodeTypeSpec Terminal


treeInsert :: ParsTree -> ParseTree -> ParseTree
treeInsert 
treeInsert NodeDecl t NodeDeclList l r = 
    | node == NodeDecl = NodeDeclList node r
    | node == NodeDeclListN = NodeDeclList l node
    | otherwise = NodeDeclList l r


type FirstSet = Set.Set Terminal
type FirstSetMap = Map.Map NonTerminal FirstSet

-- Check if input token is one of nonterminal first set
inFirstSet :: NonTerminal -> Terminal -> Bool
inFirstSet nterm term = 


parse :: NonTerminal -> [Terminal] -> ([Terminal], Bool)
parse Program ts@(t:stream) = 
    let (stream', b) = parse DeclList ts
    in (stream', b) 

parse DeclList ts@(t:stream) = 
    let (stream', b1) = parse Decl ts
        (stream'', b2) = parse DeclListN stream'
    in (stream'', b1 && b2)

parse DeclListN ts@(t:stream)
    | inFirstSet DeclListN t = 
        let (stream', b1) = parse Decl stream
            (stream'', b2) = parse DeclListN stream'
        in (stream'', b1 && b2)
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

parse ScopedVarDecl ts@(t:stream)
                                     
                               
parse VarDecl ts@(t:stream) = 
    let 
        stream' = parse TypeSpec ts
        stream'' = parse VarDeclList stream'
    in case of
           TokenBackQuote -> stream'' 

parse TypeSpec ts@(t:stream) = 
    | t == TokenInt = (True, stream)
    | t = TokenBool = (True, stream)
    | t = TokenChar = (True, stream)
    | otherwise = (False, 

parse DeclListN
