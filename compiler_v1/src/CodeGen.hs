module CodeGen where

import Control.Monad.State
import Data.List

import Ast
import Symbols
import Utils
import Const

-- default registers
regTable :: RegTable
regTable = [0, 1, 2, 3, 4, 5, 6, 7, 8, 9, 10]

-- main function that will delegate 
-- others functions code generation depending on current expression
codeGen :: [Expression] -> State ProgrammState Int
codeGen (expr:xs) = do
    case expr of 
        ExprIfElse {} -> do
            codeGenIfElse expr
            codeGen xs
        FuncCall {} -> do 
            codeGenFuncCall expr 
            codeGen xs
        VarAssign {}  -> do 
            codeGenBinOp expr
            codeGen xs
        FuncDef {} -> do
            codeGenFunc expr
            codeGen xs
        RetExpr _ -> do
            codeGenReturn expr
            codeGen xs
        ExprIncrem _ -> do
            codeGenIncrem expr
            codeGen xs
        x -> codeGen xs

codeGen [] = do return 0

codeGenFuncCall :: Expression -> State ProgrammState Int
codeGenFuncCall (FuncCall fName (p:parms)) =
    case p of
        VarRef vName -> do
            state <- get
            symbol <- lookupSymbol vName
            let mem = case symbol of
                          Left gSymb -> show $ gsBinding gSymb
                          Right lSymb -> "BP-" ++ show (lsBpOff lSymb)
            freeReg <- allocateReg
            let cmds = ["LDR R" ++ show freeReg ++ ", [" ++ mem ++ "]", 
                        "PUSH " ++ "{R" ++ show freeReg ++ "}"]
            updateCode cmds
            codeGenFuncCall $ FuncCall fName parms
        ExprValueInt value -> do
                state <- get
                freeReg <- allocateReg
                let cmds = ["MOV R" ++ show freeReg ++ ", #" ++ show value, 
                            "PUSH " ++ "{R" ++ show freeReg ++ "}"]
                updateCode cmds
                releaseReg freeReg 
                codeGenFuncCall $ FuncCall fName parms
        sm -> error $ show sm

codeGenFuncCall (FuncCall fName _) = do 
    freeReg <- allocateReg
    let cmds = ["SUB SP, SP, #2", 
                "BL " ++ fName, 
                "ADD SP, SP, #2",
                "POP " ++ "{R" ++ show freeReg ++ "}"]
    updateCode cmds
    return freeReg

codeGenFuncCall x = error $ "Calling CodeGen FuncCall with bad Expression Type: " ++ show x

-- arg-1
-- arg-2...
-- return value
-- return address (CPU push that value itself, after function call another one)
-- old BP
-- loc_1 (new BP point there)
-- loc_2
codeGenFunc (FuncDef fType fName (FuncParms parms) (Block expr)) = do
    -- BP offset depending on how much parms function should consume
    -- and const value of RetValue space, RetAddress, OldBP

    -- TODO: fill local table only with function arguments, but need to use variables
    --       through the whole function declaration
    fillLocalTable parms [] (length parms * intSize + 3 * instrSize) 
    let funcLable = "\n" ++ fName ++ ":"
    let cmd = ["PUSH BP", "MOV BP, SP"]
    updateCode $ funcLable : cmd
    codeGen expr
    cleanLocalTable
    return 0

codeGenFunc x = error $ "Calling CodeGen Func with bad Expression Type: " ++ show x

codeGenReturn :: Expression -> State ProgrammState Int
codeGenReturn (RetExpr e) = do
    resReg <- codeGenReturn e
    freeReg <- allocateReg
    let cmds = ["STR " ++ "R" ++ show resReg ++ ", [BP-" ++ show (instrSize * 3) ++ "]",
                "MOV SP, BP",
                "ADD SP, SP, #2",
                "LDR " ++ "R" ++ show freeReg ++ ", [BP-" ++ show (instrSize * 2) ++ "]",
                "MOV PC, " ++ "R" ++ show freeReg ++ "\n"]
    releaseReg resReg
    releaseReg freeReg
    state <- get
    updateCode cmds
    return 0

-- return register where final value is located
codeGenReturn (VarRef name) = do
    symbol <- lookupSymbol name 
    case symbol of
        Left gSym -> do
            let symBind = gsBinding gSym
            freeReg <- allocateReg
            let cmd = ["LDR " ++ "R" ++ show freeReg ++ ", [" ++ show symBind ++ "]"]
            releaseReg freeReg
            updateCode cmd
            return freeReg
        -- TODO: make it work with local variables
        Right lSym -> do
            let bpOff = lsBpOff lSym
            freeReg <- allocateReg
            let cmd = ["LDR " ++ "R" ++ show freeReg ++ " [BP-" ++ show bpOff ++ "]"]
            updateCode cmd
            return freeReg

codeGenReturn (ExprBinOp op l r) = do codeGenBinOp $ ExprBinOp op l r

codeGenReturn x = error $ "Callign CodeGen Return with bad Expression Type: " ++ show x

codeGenIncrem :: Expression -> State ProgrammState Int
codeGenIncrem (ExprIncrem (VarRef name)) = do
    symbol <- lookupSymbol name
    let mem = case symbol of
                  Left gSymb -> show $ gsBinding gSymb
                  Right lSymb -> "BP-" ++ show (lsBpOff lSymb)
    freeReg <- allocateReg
    let code  = ["LDR " ++ "R" ++ show freeReg ++ ", [" ++ mem ++ "]",
                 "ADD " ++ "R" ++ show freeReg ++ ", R" ++ show freeReg ++ ", #1",
                 "STR " ++ "R" ++ show freeReg ++ ", [" ++ mem ++ "]" ]
    updateCode code
    releaseReg freeReg
    return 0

codeGenIncrem x = error $ "Calling CodeGen Increment with bad Expression Type: " ++ show x

codeGenIfElse :: Expression -> State ProgrammState Int
codeGenIfElse (ExprIfElse stmt exprIf exprElse) = do
    codeGenIfElse stmt
    codeGenIfElse exprIf
    codeGenIfElse exprElse

codeGenIfElse (ExprStmt (VarRef nameL) sign (VarRef nameR)) = do
    state <- get
    freeReg1 <- allocateReg
    freeReg2 <- allocateReg
    let elseLable = "L" ++ show (psLabels state + 1)
    symbolL <- lookupSymbol nameL
    symbolR <- lookupSymbol nameR

    let symbolLAddr = case symbolL of
                       Left gSym -> gsBinding gSym
                       Right lSym -> error "Can not use local variables in if-else statement"
    let symbolRAddr = case symbolR of
                       Left gSym -> gsBinding gSym
                       Right lSym -> error "Can not use local variables in if-else statement"

    let cmd = stmtCmd freeReg1 freeReg2 symbolLAddr symbolRAddr elseLable
    put $ state { psCode = psCode state ++ cmd }
    releaseReg freeReg1
    releaseReg freeReg2
    return 0
  where
    stmtCmd lReg rReg lAddr rAddr elseL = ["STR r" ++ show lReg ++ ", " ++ "[" ++ show lAddr ++ "]", 
                                           "STR r" ++ show rReg ++ ", " ++ "[" ++ show rAddr ++ "]", 
                                           "CMP r" ++ show lReg ++ ", r" ++ show rReg, 
                                           cmpInstruction ++ " " ++ elseL]
    cmpInstruction = case sign of
                         "==" -> "BNE"
                         ">"  -> "BLT"
                         "<"  -> "BGT"
                         _    -> "BNE"

codeGenIfElse (Block (expr:xs)) = do
    state <- get
    let currLable = psLabels state
    put $ state { psCode = psCode state ++ ["L" ++ show currLable ++ ":"] ,
                  psLabels = currLable + 1}
    codeGenBinOp expr
    return 0

codeGenIfElse x = error $ show x

-- Code generation for Binary Operations
codeGenBinOp :: Expression -> State ProgrammState Int
codeGenBinOp (VarAssign name expr) = do
    symbol <- lookupSymbol name
    case symbol of
        Left gSym -> do
            let symbolBinding = gsBinding gSym
            resReg <- codeGenBinOp expr
            let cmd = ["STR " ++ "R" ++ show resReg ++ ", [" ++ show symbolBinding ++ "]" ]
            updateCode cmd
            releaseReg resReg
            return symbolBinding
        Right lSym -> error "Can not use local variables in var assignment"

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
            let cmd = ["LDR " ++ "R" ++ show freeReg ++ ", [" ++ show binding ++ "]"]
            updateCode cmd
            return freeReg
        Right lSym -> do
            let bpOff = lsBpOff lSym
            freeReg <- allocateReg
            let cmd = ["LDR " ++ "R" ++ show freeReg ++ " [BP-" ++ show bpOff ++ "]"]
            updateCode cmd
            return freeReg

codeGenBinOp (ExprValueInt value) = do
    freeReg <- allocateReg
    let cmd = ["MOV " ++ "R" ++ show freeReg ++ ", #" ++ show value]
    updateCode cmd
    return freeReg

codeGenBinOp (FuncCall name e) = do
    codeGenFuncCall $ FuncCall name e

codeGenBinOp x = error $ show x
