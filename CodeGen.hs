module CodeGen where

import Control.Monad.State
import Data.List

import Ast
import Symbols

-- default registers
regTable :: RegTable
regTable = [0, 1, 2, 3, 4, 5, 6, 7, 8, 9, 10]

-- main function that will delegate 
-- others functions code generation depending on current expression
codeGen :: [Expression] -> State ProgrammState Int
codeGen (expr:xs) = do
    case expr of 
        ExprIfElse _ _ _ -> do
            codeGenIfElse expr
            codeGen xs
        FuncCall _  _ -> do 
            codeGenFuncCall expr 
            codeGen xs
        VarAssign _ _  -> do 
            codeGenBinOp expr
            codeGen xs
        FuncDef _ _ _ _ -> do
            codeGenFunc expr
            codeGen xs
        RetExpr _ -> do
            codeGenReturn expr
            codeGen xs
        x -> codeGen xs

codeGen [] = do return 0

codeGenFuncCall :: Expression -> State ProgrammState Int
codeGenFuncCall (FuncCall fName (p:parms)) =
    case p of
        VarRef vName ->
            do
                state <- get
                symbol <- lookupSymbol vName
                let mem = case symbol of
                              Left gSymb -> show $ gsBinding gSymb
                              Right lSymb -> "BP-" ++ (show $ lsBpOff lSymb)
                let cmd = "PUSH " ++ "[" ++ mem ++ "]"
                put $ state { psCode = psCode state ++ [cmd] } 
                codeGenFuncCall $ FuncCall fName parms
        ExprValueInt value ->
            do
                state <- get
                let cmd = "PUSH " ++ "#" ++ show value
                put $ state { psCode = psCode state ++ [cmd] }
                codeGenFuncCall $ FuncCall fName parms
        sm -> error $ show sm

codeGenFuncCall (FuncCall fName _) = do 
    let cmds = ["SUB SP 2", "BL " ++ fName, "\n"]
    updateCode cmds
    return 0 

-- arg-1
-- arg-2...
-- return value
-- return address (CPU push that value itself, after function call another function)
-- old BP
-- loc_1 (new BP point there)
-- loc_2
codeGenFunc (FuncDef fType fName (FuncParms parms) (Block expr)) = do
    fillLocalTable parms [] (length parms + 1) 
    let label = fName ++ ":"
    let pushBP = "PUSH BP"
    let newBP = "MOV BP, SP"
    state <- get
    put $ state { psCode = psCode state ++ [label, pushBP, newBP] } 
    codeGen expr
    --cleanLocalTable
    return 0

codeGenReturn :: Expression -> State ProgrammState Int
codeGenReturn (RetExpr e) = do
    resReg <- codeGenReturn e
    let cmds = ["MOV " ++ "[BP-6] " ++ "R" ++ show resReg,
                "LDR " ++ "LR " ++ "[BP-4]",
                "RET "]
    state <- get
    put $ state { psCode = psCode state ++ cmds }
    return 0

-- return register where final value is located
codeGenReturn (VarRef name) = do
    symbol <- lookupSymbol name 
    case symbol of
        Left gSym -> do
            let symBind = gsBinding gSym
            freeReg <- allocateReg
            let cmd = ["LDR " ++ "R" ++ show freeReg ++ " [" ++ show symBind ++ "]"]
            updateCode cmd
            return freeReg
        Right lSym -> do
            let bpOffset = lsBpOff lSym
            freeReg <- allocateReg
            let cmd = ["LDR " ++ "R" ++ show freeReg ++ " [BP-" ++ show bpOffset ++ "]"]
            updateCode cmd
            return freeReg

codeGenReturn (ExprBinOp op le re) = do
    resReg <- codeGenBinOp $ ExprBinOp op le re
    return resReg

updateCode :: [String] -> State ProgrammState ()
updateCode code = do
    state <- get
    put $ state { psCode = psCode state ++ code }
    return ()

codeGenIfElse :: Expression -> State ProgrammState Int
codeGenIfElse (ExprIfElse stmt exprIf exprElse) = do
    codeGenIfElse stmt
    codeGenIfElse exprIf
    codeGenIfElse exprElse

codeGenIfElse (ExprStmt exprL sign exprR) = do
    state <- get
    let elseL = "L" ++ (show $ (psLabels state + 1))
    let cmd = case (exprL, exprR) of
                  (VarRef nameL, VarRef nameR) -> stmtCmd nameL nameR elseL
    put $ state { psCode = (psCode state) ++ cmd }
    return 0
  where
    stmtCmd l r elseL = ["MOV r0, " ++ l, "MOV r1, " ++ r, "CMP r0, r1", "BEQ " ++ elseL]

codeGenIfElse (Block (expr:xs)) = do
    state <- get
    let currL = psLabels state
    put $ state { psCode = (psCode state) ++ ["L" ++ show currL ++ ":"] ,
                  psLabels = currL + 1}
    codeGenBinOp expr
    return 0


-- Code generation for Binary Operations
codeGenBinOp :: Expression -> State ProgrammState Int
codeGenBinOp (VarAssign name expr) = do
    symbol <- lookupSymbol name
    case symbol of
        Left gSym -> do
            let symbolBinding = gsBinding gSym
            resReg <- codeGenBinOp expr
            let cmd = ["MOV " ++ "[" ++ show symbolBinding ++ "], R" ++ show resReg]
            updateCode cmd
            return symbolBinding
        Right lSym -> error "Can change only global vars for now"

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
    case symbol of
        Left gSym -> do
            let binding = gsBinding gSym
            let cmd = ["MOV " ++ "R" ++ show freeReg ++ ", [" ++ show binding ++ "]"]
            updateCode cmd
            return freeReg
        Right lSym -> do
            let off = lsBpOff lSym
            let cmd = ["LDR R" ++ show freeReg ++ ", [BP-" ++ show off ++ "]"]
            updateCode cmd
            return freeReg

codeGenBinOp (ExprValueInt value) = do
    freeReg <- allocateReg
    let cmd = ["MOV " ++ "R" ++ show freeReg ++ ", #" ++ (show value)]
    updateCode cmd
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
    let freeReg = head $ psRT state
    put state { psRT = drop 1 $ psRT state }
    return freeReg

releaseReg :: Int -> State ProgrammState ()
releaseReg reg = do
    state <- get
    let table = psRT state
    put $ state { psRT = insert reg table }
