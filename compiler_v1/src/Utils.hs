module Utils where

import Control.Monad.State
import Data.List

import Ast
import Symbols


updateCode :: [String] -> State ProgrammState ()
updateCode code = do
    state <- get
    put $ state { psCode = psCode state ++ code }
    return ()

-- generate ASM command for binary operations
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
    let regTable = psRT state
    case length regTable of
        0 -> error "No more registers to use"
        _ -> do
            put state { psRT = drop 1 regTable }
            return $ head regTable

releaseReg :: Int -> State ProgrammState ()
releaseReg reg = do
    state <- get
    let table = psRT state
    put $ state { psRT = insert reg table }
