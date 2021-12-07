module Ast where

import Data.List

-- Block structured language consists of three main constructions
-- declaration(variable declaration, func declaration) 
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
data ExprType = TypeBool 
              | TypeInt 
              | TypeChar 
              | TypeVoid
              deriving(Read, Eq)

instance Show ExprType where
    show TypeBool = "bool"
    show TypeInt = "int"
    show TypeChar = "char"
    show TypeVoid = "void"

data BinaryOp = OpMultiply
              | OpDivide
              | OpPlus
              | OpMinus
              deriving(Read, Eq)

instance Show BinaryOp where
    show OpMultiply = "*"
    show OpDivide = "/"
    show OpPlus = "+"
    show OpMinus = "-"

-- ****** Expression ******
data Expression = ExprEmpty
                -- Use variable that was declared earlier
                | VarRef String 
                | VarDef ExprType String Expression
                | VarDecl ExprType String
                | VarAssign String Expression
                -- Hardcode constant
                | ExprValueInt Integer 
                | ExprValueChar Char
                | ExprValueBool Bool

                | ExprIncrem Expression
                | ExprDecrem Expression

                | Block [Expression]
                | FuncParms [Expression]
                | ExprBinOp BinaryOp Expression Expression

                | FuncDef ExprType String Expression Expression 
                | FuncDecl ExprType String Expression
                | FuncCall String [Expression]
                | RetExpr Expression

                | ExprIfElse Expression Expression Expression
                | ExprStmt Expression String Expression
                deriving (Show, Read, Eq)

class PrettyExpr a where
    prettyPrint :: a -> [String]

-- Return list of expression for all type of Expression
-- ["exp1", "exp2"] so we can add indent into certain elements
instance PrettyExpr Expression where
    prettyPrint expr = case expr of
        VarRef ref -> [ref]
        VarDef t n expr -> [unwords [show t, n, "=", unwords $ prettyPrint expr]]
        VarDecl t name -> [show t ++ " " ++ name]
        VarAssign name expr -> [name ++ " = " ++ (unwords $ prettyPrint expr)] 

        FuncDef ret name args block -> [unwords ["Func: ", show ret, show name], 
                                        (unwords $ prettyPrint args) ++ " {"] 
                                       ++  prettyPrint block ++ ["}"]
        ExprIfElse stm exprIf exprElse -> ["if (" ++ (unwords $ prettyPrint stm) ++ ") {"]
                                          ++ (indentBlock $ prettyPrint exprIf) 
                                          ++ ["} " ++ "else" ++ " {"] 
                                          ++ (indentBlock $ prettyPrint exprElse) 
                                          ++ ["}"]

        FuncParms args -> ["Args: " ++ (joinC $ map (unwords . prettyPrint) args)]
        RetExpr e -> ["Return "  ++ (unwords $ prettyPrint e)]

        Block e -> concat (map (indentBlock . prettyPrint) e)

        ExprValueInt val -> [show val]
        ExprValueChar val -> [show val]
        ExprValueBool val -> [show val]

        ExprBinOp op exprL exprR -> [(unwords $ prettyPrint exprL) 
                                     ++ " " 
                                     ++ (show op) 
                                     ++ " " 
                                     ++ (unwords $ prettyPrint exprR)]
        ExprIncrem e -> [(unwords $ prettyPrint e) ++ "++"]
        ExprDecrem e -> [(unwords $ prettyPrint e) ++ "--"]
        ExprStmt eL sign eR -> [(unwords $ prettyPrint eL) ++ " " ++ sign ++ " " ++ (unwords $ prettyPrint eR)]
        _                -> ["Nothing"]

indentBlock = map("  " ++)

prettyAst expr = map (joinN . prettyPrint) expr

joinN = intercalate "\n"
joinC = intercalate ","

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
