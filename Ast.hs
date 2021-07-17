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


-- ****** Create AST from ParseTree  ******

parseParseTree :: ParseTree -> Ast
parseParseTree (pt n)
    | n == NodeVarDecl = createAst n
    | otherwise = parseParseTree n

-- Will create AST recursively parse all demanding parts
createAst :: ParseTree -> Ast

createAstVarDeclaration (NodeVarDecl varType declInit _) = VarDecl $ createAst  
  where
    declType = parseDeclarationType varType
    
parseVarDeclInit (_ id _ expr) = (id, parseExpression expr)

parseExpression (NodeSimpleExpression l r) =  ExprAnd $ parseExpression r

parseExpression (NodeSimpleExpressionN l m r)
    -- check thath node is not empty
    | l == TermOr = ExprOr parseExpression




parseExpression (NodeAndExpr l r) = ExprAnd $ parseExpression l $ parseExpression r

parseExpression (NodeAndExprN l m r) = 

parseExpression (NodeUnaryRelExpr l r) =

parseExpression (NodeRelExpr l) = 

parseExpression (NodeRelExpr l) = parseExpression l

parseExpression (NodeSumExpr l r) = 

parseExpression (NodeSumExprN l m r) = parseExpression  

parseExpression (NodeMulExpr l m r)
    -- got only value or name in mul expr(x, name, 2, 23 ...)
    | l == NodeFactor = ExprValue l







parseDeclarationType :: ParseTree -> Type
parseDeclarationType (_ typeSpec)
    | typeSpec == TermInt = TypeInt
    | typeSpec == TermBool = TypeBool
    | typeSpec == TermChar = TypeChar
    | otherwise = error "Wrong type!!!\n"

