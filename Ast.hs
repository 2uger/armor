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
-- data Ast = NodeStmt Statement Ast
--          | NodeDeclaration Declaration Ast
--          | NodeExpression Expression Ast
--          | NodeEmpty
-- 
-- data Declaration = VarDeclaration { varName :: String
--                                   , varType :: Type
--                                   , varInitValue :: Expression }
--                  | FuncDeclaration { funcName :: String
--                                    , funcType :: Type
--                                    , funcBody :: StatementBlock} 
--                  deriving(Show, Read)
-- 
-- data Type = TypeBool 
--           | TypeInt 
--           | TypeChar 
--           | TypeVoid
--           | TypeArray
--           | TypeFunction Type ParamList
-- 
-- data ParamList = ParamList String Type ParamList
-- 
-- -- ****** Statement ******
-- data Statement = StmtLocalVarDecl VarDeclaration 
--                | StmtExpression Expression
--                | StmtIfElse IfElse 
--                | StmtForLoop ForLoop
--                | StmtReturn Return
--                  deriving(Show, Read)
-- 
-- data IfElse = IfElse { evaluateExpr :: Expression
--                              , ifBody :: Statement
--                              , elseBody :: Statement }
-- 
-- data ForLoop = ForLoop { foLoopInitExpr :: Expression
--                        , forLoopevaluateExpr :: Expression
--                        , nextExpr :: Expression
--                        , body :: Expression }
-- 
-- data Return = Return Expression
-- 
--  Statement block will contain multiple statements


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

-- Simple AST expression interpreter
-- Ability:
-- Operations on INT
-- Sum on String
-- Logical operations on Bool

data OperationType = Sum | Sub | Mul | Div | And | Or deriving (Eq, Show)

-- Simple expression interpreter
expression :: Expression -> Maybe Expression 
expression exp =
    case exp of
        ExprAdd l r -> expr Sum (expression l) (expression r) 
        ExprSub l r -> expr Sub (expression l) (expression r)

        ExprMul l r -> expr Mul (expression l) (expression r)
        ExprDiv l r -> expr Div (expression l) (expression r)

        ExprAnd l r -> expr And (expression l) (expression r)
        ExprOr l r  -> expr Or (expression l) (expression r)

        ExprValueInt value -> Just (ExprValueInt value)
        ExprValueString value -> Just (ExprValueString value)
        ExprValueBool value -> Just (ExprValueBool value)

-- Adding Int type
expr op (Just (ExprValueInt l_value)) (Just (ExprValueInt r_value))
    | op == Sum = Just (ExprValueInt (l_value + r_value))
    | op == Sub = Just (ExprValueInt (l_value - r_value))

    | op == Mul = Just (ExprValueInt (l_value * r_value))
    | op == Div = Just (ExprValueInt (l_value `div` r_value))

expr op (Just (ExprValueBool l_value)) (Just (ExprValueBool r_value))
    | op == And = Just (ExprValueBool (l_value && r_value))
    | op == Or = Just (ExprValueBool (l_value || r_value))

-- Adding String type
expr op (Just (ExprValueString l_value)) (Just (ExprValueString r_value))
    | op == Sum = Just (ExprValueString (l_value ++ r_value))
    | otherwise = Nothing

-- Otherwise error
expr _ _ _ = Nothing
