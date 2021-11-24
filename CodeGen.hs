module CodeGen where

import Control.Monad.State
import Data.List

import Ast
import Symbols

-- default registers
regTable :: RegTable
regTable = [0, 1, 2, 3, 4, 5, 6, 7, 8, 9, 10]

genCode :: Expression -> State ProgrammState ()
genCode (ExprIfElse stmt exprIf exprElse) = do
    genCode stmt
    genCode exprIf
    genCode exprElse

genCode (ExprStmt exprL exprR) = do
    let cmd = case (exprL, exprR) of
                  (VarRef nameL, VarRef nameR) -> stmtCmd nameL nameR
    state <- get
    put $ state { psCode = (psCode state) ++ cmd }
  where
    stmtCmd l r = ["MOV r0, " ++ l, "MOV r1, " ++ r, "CMP r0, r1"]

genCode (Block (expr:xs)) = do
    codeGenBinOp expr
    state <- get
    put state

genCode x = error $ show x

-- Code generation for Binary Operations

codeGen :: [Expression] -> State ProgrammState Int
codeGen (expr:xs) = do
    case expr of 
        VarDef _ _ _  -> codeGenBinOp expr
        _ -> codeGen xs

codeGen [m] = do
    error $ show m
    return 2

codeGenBinOp :: Expression -> State ProgrammState Int
codeGenBinOp (VarDef exprType name expr) = do
    symbol <- lookupSymbol name
    let symbolBinding = gsBinding symbol
    resReg <- codeGenBinOp expr
    let cmd = "MOV " ++ "[" ++ show symbolBinding ++ "], R" ++ show resReg
    state <- get
    put $ state { psCode = psCode state ++ [cmd] }
    return symbolBinding

codeGenBinOp (ExprBinOp op exprL exprR) = do
    freeReg <- allocateReg
    r1 <- codeGenBinOp exprL
    r2 <- codeGenBinOp exprR
    let cmd = genCmd op freeReg r1 r2
    releaseReg r1
    releaseReg r2
    state <- get
    put $ state { psCode = psCode state ++ [cmd] }
    return freeReg

codeGenBinOp (VarRef name) = do
    freeReg <- allocateReg
    symbol <- lookupSymbol name
    let symbolBinding = gsBinding symbol
    let cmd = "MOV " ++ "R" ++ show freeReg ++ ", [" ++ show symbolBinding ++ "]"
    state <- get
    put $ state { psCode = (psCode state) ++ [cmd] }
    return freeReg

codeGenBinOp (ExprValueInt value) = do
    freeReg <- allocateReg
    let cmd = "MOV " ++ "R" ++ show freeReg ++ ", #" ++ (show value)
    state <- get
    put $ state { psCode = (psCode state) ++ [cmd] }
    return $ freeReg

codeGenBinOp x = error $ show x

-- to show differen operands for the same mnemonics
-- Reg 1, Memory 4096, Value 2
data OperandType a = Reg a | Memory a | Value a

genCmd :: BinaryOp -> Int -> Int -> Int -> String
genCmd op r0 r1 r2
    | op == OpMultiply = "MUL " ++ regTmpl
    | op == OpDivide = "DIV " ++ regTmpl
    | op == OpPlus = "ADD " ++ regTmpl
    | op == OpMinus = "SUB " ++ regTmpl
  where
    regTmpl = "R" ++ show r0 ++ ", " ++ "R" ++ show r1 ++ ", " ++ "R" ++ show r2

allocateReg :: State ProgrammState Int
allocateReg = do
    state <- get
    let freeReg = head $ psRT state
    put state { psRT = drop 1 $ psRT state }
    return freeReg

releaseReg :: Int -> State ProgrammState ()
releaseReg reg = do
    state <- get
    let table = psRT state
    put $ state { psRT = insert reg table }
