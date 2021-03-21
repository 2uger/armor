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
        | NodeDeclList ParseTree ParseTree
        | NodeDeclListN ParseTree ParseTree
        | NodeDecl ParseTree
        | NodeTypeSpec Terminal


data Type = 
data ASTNode = Decl String Type Value Code ASTNode
