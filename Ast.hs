module Ast where

import ParseTree
import ParserTypes

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
--data Ast = NodeStmt Statement Ast
--         | NodeDeclaration Declaration Ast
--         | NodeExpression Expression Ast
--         | NodeEmpty
--
--data Declaration = VarDeclaration { varName :: String
--                                  , varType :: Type
--                                  , varInitValue :: Expression }
--                 | FuncDeclaration { funcName :: String
--                                   , funcType :: Type
--                                   , funcBody :: StatementBlock} 
--                 deriving(Show, Read)
--
--data Type = TypeBool 
--          | TypeInt 
--          | TypeChar 
--          | TypeVoid
--          | TypeArray
--          | TypeFunction Type ParamList
--
--data ParamList = ParamList String Type ParamList
--
---- ****** Statement ******
--data Statement = StmtLocalVarDecl VarDeclaration 
--               | StmtExpression Expression
--               | StmtIfElse IfElse 
--               | StmtForLoop ForLoop
--               | StmtReturn Return
--                 deriving(Show, Read)
--
--data IfElse = IfElse { evaluateExpr :: Expression
--                             , ifBody :: Statement
--                             , elseBody :: Statement }
--
--data ForLoop = ForLoop { foLoopInitExpr :: Expression
--                       , forLoopevaluateExpr :: Expression
--                       , nextExpr :: Expression
--                       , body :: Expression }
--
--data Return = Return Expression

-- Statement block will contain multiple statements


-- ****** Expression ******
data Expression = ExprEmpty
                -- Use variable that was declared earlier
                | ExprNameReference String 
                -- Hardcode constant(NumConst or StringCons)
                | ExprValueInt Int 
                | ExprValueChar Char
                | ExprValueString String
                | ExprValueBool Bool
                -- Array. Left - name. Right - index.
                | ExprSubscript Expression Expression

                | ExprAssign Expression Expression
                | ExprAdd Expression Expression
                | ExprSub Expression Expression
                | ExprMul Expression Expression
                | ExprDiv Expression Expression
                | ExprAnd Expression Expression
                | ExprOr  Expression Expression
                | ExprNot Expression
            
                | ExprFuncCall Expression Expression
                | ExprArg Expression Expression
                | ExprFuncArgs Expression Expression
                deriving (Show, Read, Eq)


-- ****** Create AST from ParseTree  ******

--parseParseTree :: ParseTree -> Ast
--parseParseTree (pt n)
--    | n == NodeVarDecl = createAst n
--    | otherwise = parseParseTree n
--
---- Will create AST recursively parse all demanding parts
--createAst :: ParseTree -> Ast
--
--createAstVarDeclaration (NodeVarDecl varType declInit _) = VarDecl $ createAst  
--  where
--    declType = parseDeclarationType varType
--    
--parseVarDeclInit (_ id _ expr) = (id, parseExpression expr)
--
--parseExpression (NodeSimpleExpression l r) =  ExprAnd $ parseExpression r
--
--parseExpression (NodeSimpleExpressionN l m r)
--    -- check thath node is not empty
--    | l == TermOr = ExprOr parseExpression
--
--
--
--

--parseExpression (NodeSimpleExpression l r) = ExprOr (parseExpression l) $ parseExpression r
--
--parseExpression (NodeSimpleExpressionN l m r)
--    | r /= EmptyTree = ExprOr (parseExpression m) $ parseExpression r
--    | otherwise = parseExpression m
--
--parseExpression (NodeAndExpr l r) = ExprAnd (parseExpression l) $ parseExpression r
--
--parseExpression (NodeAndExprN (l term) m r)
--    | r /= EmptyTree = ExprAnd (parseExpression m) $ parseExpression r
--    | otherwise = parseExpression m 
--
--parseExpression (NodeUnaryRelExpr (l term) r)
--    | l == TermNot = ExprNot parseExpression r
--    | otherwise = parseExpression r

--parseExpression (NodeRelExpr l) = parseExpression l
--

parseExpression _ = ExprValueInt 2
parseExpression (NodeSumExpr factor next) = ExprAdd (parseFactor l) $ parseExpression r 

parseExpression (NodeSumExprN _ factor next)
    | r /= EmptyTree = ExprAdd (parse factor) (parseExpression next)
    | otherwise = parse factor 

parseExpression (NodeMulExpr factor next)
    | next /= EmptyTree = ExprMul (parse factor) $ parseExpression next 
    | otherwise = parse factor

parseExpression (NodeMulExprN _ factor next)
    | next /= EmptyTree = ExprMul (parse factor) $ parseExpression next
    | otherwise = parse factor

parse (NodeFactor v) = parse v 

parse (NodeMutable (Leaf (TermId id)) _ expr _)
    | expr /= EmptyTree = ExprSubscript (ExprName id) $ parseExpression expr
    | otherwise = ExprName id

parse (NodeImmutable l m r)
    | l == TermLParen = parseExpression m
    | l == NodeCall = parseExpression l 
    | l == Constant = parseExpression l

parse (NodeCall id _ args _) = ExprFuncCall (ExprName id) $ (ExprArgs parseExpression args)

parse (NodeConstant (Leaf (TermNumConst x))) = ExprValueInt x
parse (NodeConstant (Leaf (TermCharConst x))) = ExprValueChar x
parse (NodeConstant (Leaf (TermStringConst x))) = ExprValueString x
parse (NodeConstant (Leaf (TermBoolConst x))) = ExprValueBool x
parse (NodeConstant x) = error $ "Got bullshit" ++ (show x)






--parseDeclarationType :: ParseTree -> Type
--parseDeclarationType (_ typeSpec)
--    | typeSpec == TermInt = TypeInt
--    | typeSpec == TermBool = TypeBool
--    | typeSpec == TermChar = TypeChar
--    | otherwise = error "Wrong type!!!\n"

