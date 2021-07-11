module AbstractSyntaxTree where

-- Block structured language consists of three main constructions
-- declaration(variable declaration,
--             func declaration) 
-- statement(statement is a process of somehow change programm state)
-- expression(something that will have evaluated value
--            2 + 2 will have value 4
--            a will have value of a which we init or declare before)

-- ****** Declaration ******
-- Declaration describes expressions like
-- int a = 10;
-- char m = 'a';
-- int func(int x) {
--      int m = x * 45;
--      return m;
-- };
data Ast = NodeStmt Statement Ast
         | NodeDeclaration Declaration Ast
         | NodeExpression Expression Ast
         | NodeEmpty

data Declaration = VarDeclaration { varName :: String
                                  , varType :: Type
                                  , varInitValue :: Expression }
                 | FuncDeclaration { funcName :: String
                                   , funcType :: Type
                                   , funcBody :: StatementBlock} 
                 deriving(Show, Read)

data Type = TypeBool 
          | TypeInt 
          | TypeChar 
          | TypeVoid
          | TypeArray
          | TypeFunction Type ParamList

data ParamList = ParamList String Type ParamList

-- ****** Statement ******
data Statement = StmtLocalVarDecl VarDeclaration 
               | StmtExpression Expression
               | StmtIfElse IfElse 
               | StmtForLoop ForLoop
               | StmtReturn Return
                 deriving(Show, Read)

data StmtIfElse = StmtIfElse { evaluateExpr :: Expression
                             , ifBody :: Statement
                             , elseBody :: Statement }

data ForLoop = ForLoop { foLoopInitExpr :: Expression
                       , forLoopevaluateExpr :: Expression
                       , nextExpr :: Expression
                       , body :: Expression }

data Return = Return Expression

-- Statement block will contain multiple statements


-- ****** Expression ******
data Expression = ExprEmpty
                -- Use variable that was declared earlier
                | ExprNameReference String 
                -- Hardcode constant(NumConst or StringCons)
                | ExprValue Value 

                | ExprAdd Expression Expression
                | ExprSub Expression Expression
                | ExprMul Expression Expression
                | ExprDiv Expression Expression
                | ExprAnd Expression Expression
                | ExprOr  Expression Expression
            
                | ExprFuncCall Expression Expression
                | ExprFuncArgs Expression Expression


-- ****** Functions to work with AST  ******
-- Parse VarDecl to Ast
createAst :: ParseTree -> Ast
createAst NodeVarDecl typeSpec declInit _ =
    VarDeclaration varName varType varValue 
    (varName, varValue) = createAst NodeVarDeclInit declInit

createAst NodeVarDeclInit (varId varName) _ expr =
    | expr == EmptyTree = (varName, ExprValue 0)

createAst NodeSimpleExpr andExpr simple

createAst NodeVarDeclInit TermId varName TermColon NodeSimpleExpr =
    (varName, createAst SimpleExpr)
createAst NodeTypeSpec varType
    | varType == TermInt = TypeInt
    | varType == TermBool = TypeBool
    | varType == TermChar = TypeChar

---->> NodeDeclaration VarDeclaration "name" TypeInt 2
                    




createAstNode :: ParseTree -> ParseTree -> Ast
createAstNode NodeVarDecl typeSpec declList = 

varDeclAstNode typeSpec NodeVarDeclList declInit declList =
    | declList == EmptyTree = ()
    | otherwise = createAstNode declList
  where
    declAst = NodeDeclaration 

createAstNode NodeDeclListN comma declInit declListN = 
    | declListN == EmptyTree = createAstNode NodeDeclListN declListN
    | otherwise = 

createAstNode typeSpec NodeDeclInit id colon expr = 
    | expr == EmptyTree = 
  where
    declAst = NodeDeclaration VarDeclaration id typeSpec

exprAstNode SimpleExpr
