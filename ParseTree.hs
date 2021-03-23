module ParseTree where

import ParserTypes

data ParseTree = EmptyTree 
               | NodeProgramm ParseTree 
               | NodeDeclList ParseTree ParseTree
               | NodeDeclListN ParseTree ParseTree
               | NodeDecl ParseTree

               | NodeVarDecl  ParseTree ParseTree Terminal
               | NodeScopedVarDecl Terminal ParseTree ParseTree Terminal
               | NodeTypeSpec Terminal
               | NodeVarDeclList ParseTree ParseTree
               | NodeVarDeclListN Terminal ParseTree ParseTree
               | NodeVarDeclInit ParseTree Terminal ParseTree
               | NodeVarDeclId Terminal Terminal Terminal Terminal
               deriving (Show, Read)
