{-
 - Wrtie RD parser function 
 - Think about representation of Parse tree foa
 -}

data Terminal = Epsilon | TStatic | TInt | 

data NonTerminal = Program 
                 | DeclList 
                 | DeclListN 
                 | VarDecl
                 | ScopedVarDecl 



-- Parse tree 
data PT = NodeProgram AST | NodeDeclList AST AST | NodeDecl AST AST

NodeDeclList

rdParser :: [Terminal] -> AST -> Bool

parse :: NonTerminal -> [Terminal] -> (Bool, [Terminal])
parse Program ts@(t:stream) = 
    let 
        () = parse DeclList ts
    in () 

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
