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
                                   , psGST :: GSymbolTable 
                                   -- new local symbol table for every function
                                   , psLST :: [LSymbolTable]
                                   -- will show current label number
                                   , psLabels :: Int}
                                   deriving(Show, Eq)

printProgrammState :: ProgrammState -> String
printProgrammState ps = "Code: \n" 
                        ++ (joinN $ map ("   " ++) $ psCode ps) ++ "\n"
                        ++ "Register table: \n"
                        ++ (show $ psRT ps)  ++ "\n"
                        ++ "Global Symbol table: \n"
                        ++ (joinN $ map show $ psGST ps)
                        ++ "Local Symbol table: \n"
                        ++ (joinN $ map show $ psLST ps)
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
                                 , gsFlabel :: Int }
                                 deriving (Show, Eq)
-- LocalSymbol name type (BP offset)
data LocalSymbol = LocalSymbol { lsName :: String
                               , lsType :: ExprType
                               , lsBpOff :: Int }
                               deriving (Show, Eq)

addSymbol :: GlobalSymbol -> State ProgrammState () 
addSymbol symb = do
    state <- get
    let table = psGST state
    case elem symb table of
        False -> put $ state { psGST = table ++ [symb] }
        True -> error $ "Symbol " ++ gsName symb ++ " already exists"

removeSymbol :: String -> GSymbolTable -> GSymbolTable
removeSymbol name table = filter (not . ((==) name) . gsName) table

lookupSymbol :: String -> State ProgrammState (Either GlobalSymbol LocalSymbol)
lookupSymbol name = do
    state <- get
    let gTable = psGST state
    let lTable = case length $ psLST state of
                     0 -> []
                     _ -> last $ psLST state
        
    case find (((==) name) . lsName) lTable of
        Just s -> return $ Right s 
        Nothing -> case find (((==) name) . gsName) gTable of
            Just s -> return $ Left s
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
