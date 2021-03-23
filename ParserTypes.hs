module ParserTypes where

data Terminal = TermEpsilon 
              | TermId
              | TermBackQuote 
              | TermInt 
              | TermBool
              | TermChar
              | TermComma
              | TermStatic
              | TermNumConst
              | TermDoubleDot
              | TermColon
              
              | TermLParen
              | TermRParen
              | TermLSqBracket
              | TermRSqBracket
              | TermLBrace
              | TermRBrace
            
              | TermEmpty
              deriving (Eq, Show, Read)

data NonTerminal = Program 
                 | DeclList 
                 | Decl
                 | DeclListN 

                 | VarDecl
                 | ScopedVarDecl 
                 | TypeSpec
                 | VarDeclList
                 | VarDeclListN
                 | VarDeclInit
                 | VarDeclId
                
                 | FuncDecl
                 | SimpleExpr
                 deriving (Show, Read)
