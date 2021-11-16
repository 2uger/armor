module CodeGen where

import Data.List

import Ast


-- Code generation for Binary Operations with raw operands
type RegTable = [Int]

regTable :: RegTable
regTable = [0, 1, 2, 3, 4, 5, 6]

codeGen :: Expression -> [String] -> RegTable -> (Int, [String], RegTable)
codeGen (ExprBinOp op exprL exprR) code table = (lRes, code'' ++ [cmd], table''')
  where
    cmd = genCmd op resReg lRes rRes
    (lRes, code', table') = codeGen exprL code table
    (rRes, code'', table'') = codeGen exprR code' table'
    (resReg, table''') = allocateReg table''

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
