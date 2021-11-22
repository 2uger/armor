module CodeGen where

import Control.Monad.State

import Data.List

import Ast

type Code = [String]
type RegTable = [Int]


genCode :: Expression -> State ProgrammState ()
genCode (ExprIfElse stmt exprIf exprElse) = do
    genCode stmt
    genCode exprIf
    genCode exprElse

genCode (ExprStmt exprL exprR) = do
    let cmd = case (exprL, exprR) of
                  (VarRef nameL, VarRef nameR) -> stmtCmd nameL nameR
    state <- get
    put $ state { code = (code state) ++ cmd }
  where
    stmtCmd l r = ["MOV r0, " ++ l, "MOV r1, " ++ r, "CMP r0, r1"]

genCode (Block (expr:xs)) = do
    codeGenBinOp expr
    state <- get
    put state

genCode x = error $ show x

-- Code generation for Binary Operations

rregTable :: RegTable
rregTable = [0, 1, 2, 3, 4, 5, 6]

data ProgrammState = ProgrammState { code :: Code
                                   , regTable :: RegTable
                                   , symbolTable :: GSymbolTable }
                                   deriving(Show, Eq)

codeGenBinOp :: Expression -> State ProgrammState Int
codeGenBinOp (VarDef exprType name expr) = do
    codeGenBinOp expr

codeGenBinOp (ExprBinOp op exprL exprR) = do
    freeReg <- allocateReg
    r1 <- codeGenBinOp exprL
    r2 <- codeGenBinOp exprR
    let cmd = genCmd op freeReg r1 r2
    releaseReg r1
    releaseReg r2
    state <- get
    put $ state { code = code state ++ [cmd] }
    return freeReg

codeGenBinOp (VarRef name) = do
    freeReg <- allocateReg
    let cmd = "MOV" ++ "R" ++ show freeReg ++ ", #" ++ name
    state <- get
    put $ state { code = (code state) ++ [cmd] }
    return freeReg

codeGenBinOp (ExprValueInt value) = do
    freeReg <- allocateReg
    let cmd = "MOV " ++ "R" ++ show freeReg ++ ", #" ++ (show value)
    state <- get
    put $ state { code = (code state) ++ [cmd] }
    return $ freeReg

codeGenBinOp x = error $ show x

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
    let freeReg = head $ regTable state
    put state { regTable = drop 1 $ regTable state }
    return freeReg

releaseReg :: Int -> State ProgrammState ()
releaseReg reg = do
    state <- get
    let table = regTable state
    put $ state { regTable = insert reg table }

type GSymbolTable = [GlobalSymbol]

type LSymbolTable = [LocalSymbol]

data GlobalSymbol = GlobalSymbol { symbName :: String
                     , symbType :: ExprType
                     , size :: Int
                     , memBinding :: Int
                     , parmList :: Expression
                     -- address of starting code of function
                     , fLabel :: Int }
                     deriving (Show, Eq)

data LocalSymbol = LocalSymbol String ExprType

addSymbol :: GlobalSymbol -> State ProgrammState () 
addSymbol symb = do
    state <- get
    let table = symbolTable state
    case elem symb table of
        True -> put $ state { symbolTable = table ++ [symb] }
        False -> error "Same symbol exist" 
