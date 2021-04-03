module ParseTree where

import ParserTypes

-- Some production could produce 'E'(empty string)
-- EmptyTree will represent that sign
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

               | NodeFuncDecl ParseTree Terminal Terminal ParseTree Terminal ParseTree
               | NodeParms ParseTree
               | NodeParmList Terminal ParseTree ParseTree
               | NodeParmTypeList ParseTree ParseTree
               | NodeParmId Terminal Terminal Terminal

               | NodeStmt ParseTree
               | NodeExprStmt ParseTree Terminal
               | NodeCompoundStmt Terminal ParseTree ParseTree Terminal
               | NodeStmtList ParseTree ParseTree
               | NodeStmtListN ParseTree
               | NodeLocalDecl ParseTree ParseTree
               | NodeLocalDeclN ParseTree ParseTree
               | NodeIterStmt Terminal Terminal ParseTree Terminal ParseTree
               | NodeReturnStmt Terminal ParseTree Terminal
               | NodeBreakStmt Terminal Terminal

               | NodeExpr ParseTree Terminal ParseTree
               | NodeSimpleExpr ParseTree ParseTree
               | NodeSimpleExprN Terminal ParseTree ParseTree
               | NodeAndExpr ParseTree ParseTree
               | NodeAndExprN Terminal ParseTree ParseTree
               | NodeUnaryRelExpr Terminal ParseTree

               | NodeRelExpr ParseTree
               | NodeSumExpr ParseTree ParseTree
               | NodeSumExprN ParseTree ParseTree ParseTree
               | NodeMulExpr ParseTree ParseTree ParseTree
               | NodeSumOp Terminal
               | NodeMulOp Terminal
               | NodeFactor ParseTree
               | NodeMutable Termina
               | NodeImmutable
               | NodeCall
               | NodeArgs
               | NodeArgsList
               | NodeConstant
               deriving (Show, Read)

-- It's a nice util called graphviz(gnu package)
-- Using simple language DOT it will help to represent
-- whole tree(both of them, AST and parse tree)
-- Input of DOT-land looks like this:
-- graph G {
--  NodeProgram -> NodeDeclList
--  NodeDeclList -> NodeDecl
--  NodeDeclList -> NodeDeclListN
--  ...
-- }

representNode :: ParseTree -> String 
representNode (NodeProgramm t) =
    "Program -> DeclList;\n" ++ representNode t

representNode (NodeDeclList t t1) = 
    "DeclList -> Decl;\n \ 
    \DeclList -> DeclListN;\n" 
    ++ representNode t
    ++ representNode t1

representNode (NodeDecl t) = 
    case t of
        NodeVarDecl x y z -> "Decl -> VarDecl\n" 
                             ++ representNode t
        NodeFuncDecl x y z -> "Decl -> FunclDecl\n"
                              ++ representNode t

representNode (NodeVarDecl t t1 l) = 
    "VarDecl -> TypeSpec;\n \
    \VarDecl -> VarDeclList;\n \
    \VarDecl -> TermBackQuote;\n"
    ++ representNode t ++ representNode t1

representNode (NodeTypeSpec l) = 
    "TypeSpec -> " ++ show l ++ "\n"

representNode (NodeVarDeclList t t1) = 
    "VarDeclList -> VarDeclInit;\n \
    \VarDeclList -> VarDeclListN;\n"
    ++ representNode t ++ representNode t1

representNode (NodeVarDeclInit t l t1) = 
    "VarDeclInit -> VarDeclId;\n \
    \VarDeclInit ->" ++ show l ++ ";\n"
    ++ "VarDeclInit -> SimpleExpr;\n"
    ++ representNode t
    ++ representNode t1

representNode (NodeVarDeclId l l1 l2 l3) = 
    "VarDeclId ->" ++ show l ++ ";\n"
    ++ "VarDeclId ->" ++ show l1 ++ ";\n"
    ++ "VarDeclId ->" ++ show l2 ++ ";\n"
    ++ "VarDeclId ->" ++ show l3 ++ ";\n"

representNode _ = ""
