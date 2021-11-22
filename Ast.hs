module Ast where

import Data.List
import qualified Data.Map as Map


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
intSize = 4 :: Int

    

--removeSymbol :: String -> GSymbolTable -> GSymbolTable
--removeSymbol name table = filter (not . ((==) name) . symbName) table
--
--lookupSymbol :: String -> GSymbolTable -> Maybe Symbol
--lookupSymbol name table = find (((==) name) . symbName) table
--
--fillSymbolTable :: [Expression] -> GSymbolTable -> GSymbolTable
--fillSymbolTable (expr:xs) table = 
--    case expr of
--        VarDecl varType name -> fillSymbolTable xs $ f (Symbol name varType 2 4097 ExprEmpty 0) table
--        FuncDecl funcType name parms -> fillSymbolTable xs $ f (Symbol name funcType 0 0 parms 1) table
--        _ -> table
--  where
--    f s t = addSymbol s t
--
--fillSymbolTable [expr] table =
--    case expr of
--        VarDecl varType name -> table ++ [Symbol name varType 2 4096 ExprEmpty 0]
--        FuncDecl funcType name parms -> table ++ [Symbol name funcType 0 0 parms 1]
--        _ -> table
--
--fillSymbolTable [] table = table
                                      
                                      

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

                | FuncDef { retType :: ExprType 
                          , funcName :: String 
                          , funcArgs :: Expression 
                          , funcBlock :: Expression }
                | FuncDecl ExprType String Expression
                | FuncCall String [Expression]
                | RetExpr Expression
                | ExprIfElse Expression Expression Expression
                | ExprStmt Expression Expression
                deriving (Show, Read, Eq)

class PrettyExpr a where
    prettyPrint :: a -> [String]

-- Return list of expression for all type of Expression
-- ["exp1", "exp2"] so we can add indent into certain elements
instance PrettyExpr Expression where
    prettyPrint expr = case expr of
        VarRef ref -> [unwords ["Var", ref]]
        VarDef t n expr -> [unwords [show t, n, "=", unwords $ prettyPrint expr]]

        FuncDef ret name args block -> [unwords ["FUNCTION: ", show ret, show name], unwords $ prettyPrint args] 
                                       ++  prettyPrint block
        ExprIfElse stm exprIf exprElse -> ["IF ("] ++ prettyPrint stm ++ [") {"] 
                                          ++ (indentBlock $ prettyPrint exprIf) 
                                          ++ ["} " ++ "ELSE" ++ " {"] 
                                          ++ (indentBlock $ prettyPrint exprElse) ++ ["}"]

        FuncParms args -> ["ARGS: " ++ (joinC $ map (unwords . prettyPrint) args)]
        RetExpr e -> ["RETURN "  ++ (unwords $ prettyPrint e)]

        Block e -> "BLOCK: {" : concat (map (indentBlock . prettyPrint) e)

        ExprValueInt val -> [show val]
        ExprValueChar val -> [show val]
        ExprValueBool val -> [show val]

        ExprBinOp op exprL exprR -> [(unwords $ prettyPrint exprL) 
                                     ++ "  " 
                                     ++ (show op) 
                                     ++ "  " 
                                     ++ (unwords $ prettyPrint exprR)]
        ExprIncrem e -> [(unwords $ prettyPrint e) ++ "++"]
        ExprDecrem e -> [(unwords $ prettyPrint e) ++ "--"]
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
