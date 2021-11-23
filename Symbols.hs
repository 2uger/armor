module Symbols where

import Control.Monad.State
import Data.List

import Ast

type Code = [String]
type RegTable = [Int]

data ProgrammState = ProgrammState { code :: Code
                                   , regTable :: RegTable
                                   , symbolTable :: GSymbolTable }
                                   deriving(Show, Eq)

printProgrammState :: ProgrammState -> String
printProgrammState ps = "Code: \n" 
                        ++ (joinN $ map ("   " ++) $ code ps) ++ "\n"
                        ++ "Register table: \n"
                        ++ (show $ regTable ps)  ++ "\n"
                        ++ "Symbol table: \n"
                        ++ (joinN $ map show $ symbolTable ps)
  where
    joinN = intercalate "\n"

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
        False -> put $ state { symbolTable = table ++ [symb] }
        True -> error "Same symbol exist" 

removeSymbol :: String -> GSymbolTable -> GSymbolTable
removeSymbol name table = filter (not . ((==) name) . symbName) table

lookupSymbol :: String -> State ProgrammState GlobalSymbol
lookupSymbol name = do
    state <- get
    let table = symbolTable state
    case find (((==) name) . symbName) table of
        Just s -> return s
        Nothing -> error "No such symbol in table"

fillSymbolTable :: [Expression] -> State ProgrammState ()
fillSymbolTable (expr:xs) = 
    case expr of
        VarDecl varType name -> do
            let symbol = GlobalSymbol name varType 2 4096 ExprEmpty 0
            addSymbol symbol 
            fillSymbolTable xs
        FuncDecl funcType name parms -> do
            let symbol = GlobalSymbol name funcType 0 0 parms 1
            addSymbol symbol
            fillSymbolTable xs
        _ -> do
            fillSymbolTable xs

fillSymbolTable [] = do
    return ()
