module Ast where

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
data Ast a = Leaf | Node a (Ast a) (Ast a)

data ExprType = TypeBool 
              | TypeInt 
              | TypeChar 
              | TypeVoid
              | TypeFunction ExprType ExprType
              deriving(Show, Read, Eq)

data BinaryOp = OpMultiply
              | OpDivide
              | OpPlus
              | OpMinus
              deriving(Show, Read, Eq)


-- ****** Expression ******
data Expression = ExprEmpty
                -- Use variable that was declared earlier
                | VarRef String 
                | VarDef ExprType String Expression
                -- Hardcode constant
                | ExprValueInt Integer 
                | ExprValueChar Char
                | ExprValueBool Bool

                | ExprIncrem Expression
                | ExprDecrem Expression

                | Block [Expression]
                | ExprBinOp BinaryOp Expression Expression

                | FuncDef { retType :: ExprType 
                          , funcName :: String 
                          , funcArgs :: [Expression] 
                          , funcBlock :: [Expression] 
                          , retExpr :: Maybe Expression }
                | ExprIfElse Expression [Expression] [Expression]
                deriving (Show, Read, Eq)

---- Simple expression interpreter
--expression :: Expression -> Maybe Expression 
--expression exp =
--    case exp of
--        ExprAdd l r -> expr Sum (expression l) (expression r) 
--        ExprSub l r -> expr Sub (expression l) (expression r)
--
--        ExprMul l r -> expr Mul (expression l) (expression r)
--        ExprDiv l r -> expr Div (expression l) (expression r)
--
--        ExprAnd l r -> expr And (expression l) (expression r)
--        ExprOr l r  -> expr Or (expression l) (expression r)
--
--        ExprValueInt value -> Just (ExprValueInt value)
--        ExprValueString value -> Just (ExprValueString value)
--        ExprValueBool value -> Just (ExprValueBool value)
--
---- Adding Int type
--expr :: OperationType -> Maybe Expression -> Maybe Expression -> Maybe Expression
--expr op (Just (ExprValueInt l_value)) (Just (ExprValueInt r_value))
--    | op == Sum = Just (ExprValueInt (l_value + r_value))
--    | op == Sub = Just (ExprValueInt (l_value - r_value))
--
--    | op == Mul = Just (ExprValueInt (l_value * r_value))
--    | op == Div = Just (ExprValueInt (l_value `div` r_value))
--
--expr op (Just (ExprValueBool l_value)) (Just (ExprValueBool r_value))
--    | op == And = Just (ExprValueBool (l_value && r_value))
--    | op == Or = Just (ExprValueBool (l_value || r_value))
--
---- Adding String type
--expr op (Just (ExprValueString l_value)) (Just (ExprValueString r_value))
--    | op == Sum = Just (ExprValueString (l_value ++ r_value))
--    | otherwise = Nothing
--
---- Otherwise error
--expr _ _ _ = Nothing
