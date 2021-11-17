module CodeGen where

import Data.List

import Ast

type Code = [String]

genCode :: Expression -> Code -> RegTable -> (Code, RegTable)
genCode (ExprIfElse stmt exprIf exprElse) code table = (code ++ ifElseCode, table''') 
  where
    (code', table') = genCode stmt code table
    (code'', table'') = genCode exprIf code' table'
    (code''', table''') = genCode exprElse code'' table''
    ifElseCode = code' ++ ["L0"] ++ code'' ++ ["L1"] ++ code'''

genCode (ExprStmt exprL exprR) code table =
    case (exprL, exprR) of
        (VarRef nameL, VarRef nameR) -> (cmd nameL nameR, table)
  where
    cmd l r = ["MOV r0, " ++ l, "MOV r1, " ++ r, "CMP r0, r1"]

genCode (Block (expr:xs)) code table = (cmd ++ cmd', table'')
  where
    (_, cmd, table') = codeGen expr [] regTable
    (_, cmd', table'') = codeGen expr [] table'

-- Code generation for Binary Operations with raw operands
type RegTable = [Int]

regTable :: RegTable
regTable = [0, 1, 2, 3, 4, 5, 6]

codeGen :: Expression -> [String] -> RegTable -> (Int, [String], RegTable)
codeGen (VarDef exprType name expr) code table = (exprRes, code', table')
  where
    (exprRes, code', table') = codeGen expr code table

codeGen (ExprBinOp op exprL exprR) code table = (resReg, code'' ++ [cmd], table''')
  where
    cmd = genCmd op resReg lRes rRes
    (lRes, code', table') = codeGen exprL code table
    (rRes, code'', table'') = codeGen exprR code' table'
    (resReg, table''') = allocateReg table''

codeGen (VarRef name) code table = (freeReg, code ++ [cmd], table')
  where
    cmd = "MOV " ++ "R" ++ show freeReg ++ ", " ++ name
    (freeReg, table') = allocateReg table

codeGen (ExprValueInt value) code table = (regNum, code ++ [cmd], table')
  where
   cmd = "MOV " ++ "R" ++ show regNum ++ ", #" ++ (show value)
   (regNum, table') = allocateReg table

codeGen x _ _ = error $ show x

genCmd :: BinaryOp -> Int -> Int -> Int -> String
genCmd op r0 r1 r2
    | op == OpMultiply = "MUL " ++ regTmpl
    | op == OpDivide = "DIV " ++ regTmpl
    | op == OpPlus = "ADD " ++ regTmpl
    | op == OpMinus = "SUB " ++ regTmpl
  where
    regTmpl = "R" ++ show r0 ++ ", " ++ "R" ++ show r1 ++ ", " ++ "R" ++ show r2


allocateReg :: RegTable -> (Int, RegTable)
allocateReg (reg:table) = (reg, table)

allocateReg _ = error "ERROR"

freeReg :: RegTable -> Int -> RegTable
freeReg (freeReg:table) reg = table

freeReg _ _ = error "EROORO"
