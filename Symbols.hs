module Symbols where

import Control.Monad.State
import Data.List

import Ast

type Code = [String]
type RegTable = [Int]

data ProgrammState = ProgrammState { psCode :: Code
                                   -- table of registers
                                   , psRT :: RegTable
                                   -- table of global symbols
                                   , psGST :: GSymbolTable }
                                   deriving(Show, Eq)

printProgrammState :: ProgrammState -> String
printProgrammState ps = "Code: \n" 
                        ++ (joinN $ map ("   " ++) $ psCode ps) ++ "\n"
                        ++ "Register table: \n"
                        ++ (show $ psRT ps)  ++ "\n"
                        ++ "Symbol table: \n"
                        ++ (joinN $ map show $ psGST ps)
  where
    joinN = intercalate "\n"

type GSymbolTable = [GlobalSymbol]

type LSymbolTable = [LocalSymbol]

data GlobalSymbol = GlobalSymbol { gsName :: String
                                 , gsType :: ExprType
                                 , gsSize :: Int
                                 -- memory location of symbol
                                 , gsBinding :: Int
                                 , gsParms :: Expression
                                 -- address of starting code of function
                                 , gsFlable :: Int }
                                 deriving (Show, Eq)

data LocalSymbol = LocalSymbol String ExprType

addSymbol :: GlobalSymbol -> State ProgrammState () 
addSymbol symb = do
    state <- get
    let table = psGST state
    case elem symb table of
        False -> put $ state { psGST = table ++ [symb] }
        True -> error "Same symbol exist" 

removeSymbol :: String -> GSymbolTable -> GSymbolTable
removeSymbol name table = filter (not . ((==) name) . gsName) table

lookupSymbol :: String -> State ProgrammState GlobalSymbol
lookupSymbol name = do
    state <- get
    let table = psGST state
    case find (((==) name) . gsName) table of
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
