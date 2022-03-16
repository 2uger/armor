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
                                   , psLabels :: Int
                                   -- use for DATA segment to bind vars into memory
                                   , psBind :: Int }
                                   deriving(Show, Eq)

printCodeSection :: ProgrammState -> String 
printCodeSection ps = joinN $ map ("   " ++) $ psCode ps

printDataSection :: ProgrammState -> String 
printDataSection ps = joinN $ ["   " ++ show (gsBinding var) ++ ":" ++ gsName var | var <- psGST ps]

printProgrammState :: ProgrammState -> String
printProgrammState ps = "Code: \n" 
                        ++ printCodeSection ps ++ "\n"
                        ++ "Data: \n"
                        ++ printDataSection ps ++ "\n"
                        ++ "GLOBAL SYMBOL TABLE: \n"
                        ++ joinN (map show $ psGST ps) ++ "\n"
                        ++ "LOCAL SYMBOL TABLE: \n"
                        ++ joinN (map show $ psLST ps)


type GSymbolTable = [GlobalSymbol]

type LSymbolTable = [LocalSymbol]

data GlobalSymbol = GlobalSymbol { gsName :: String
                                 , gsType :: ExprType
                                 , gsSize :: Int
                                 -- memory location of symbol
                                 , gsBinding :: Int
                                 , gsParms :: Expression}
                                 deriving (Eq)

instance Show GlobalSymbol where
    show (GlobalSymbol n t s b ps) =
        "Name: " ++ n ++ "\n"
        ++ "Type: " ++ show t ++ "\n"
        ++ "Size: " ++ show s ++ "\n"
        ++ "Binding: " ++ show b ++ "\n"
        ++ "Parms: " ++ show ps ++ "\n"

-- LocalSymbol name type (BP offset)
data LocalSymbol = LocalSymbol { lsName :: String
                               , lsType :: ExprType
                               , lsBpOff :: Int }
                               deriving (Show, Eq)

addSymbol :: GlobalSymbol -> State ProgrammState () 
addSymbol symb = do
    state <- get
    let table = psGST state
    if symb `elem` table then
        error $ "Symbol " ++ gsName symb ++ " already exists"
    else
        put $ state { psGST = table ++ [symb] }

removeSymbol :: String -> State ProgrammState ()
removeSymbol name = do
    state <- get
    let table = psGST state
    let tableN = filter (not . ((==) name) . gsName) table
    put $ state { psGST = tableN }
    return ()

-- lookup symbol by name first off all in local table, then in global one
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
            Nothing -> error $ "No such symbol in table: " ++ name

-- goes through whole programm and collect all global symbols
fillSymbolTable :: [Expression] -> State ProgrammState ()
fillSymbolTable (expr:xs) = 
    case expr of
        VarDecl varType name -> do
            let offset = memOffset varType
            bind <- memBind offset
            let symbol = GlobalSymbol name varType offset bind ExprEmpty
            addSymbol symbol 
            fillSymbolTable xs
        FuncDecl funcType name parms -> do
            let symbol = GlobalSymbol name funcType 0 0 parms
            addSymbol symbol
            fillSymbolTable xs
        _ -> do
            fillSymbolTable xs
  where
    -- return current free location in DATA segment
    memBind :: Int -> State ProgrammState Int
    memBind offset = do
        state <- get
        let bind = psBind state
        put $ state { psBind = bind + offset }
        return bind

    -- return memory offset depending on Type
    memOffset :: ExprType -> Int
    memOffset t = case t of
                      TypeInt  -> 2
                      TypeChar -> 1
                      _        -> 0

fillSymbolTable [] = do
    return ()

-- remove current scope of local symbol table
cleanLocalTable :: State ProgrammState ()
cleanLocalTable = do
    state <- get
    let newLST = case length $ psLST state of
                     0 -> []
                     _ -> init $ psLST state
    put $ state { psLST = newLST }

-- create local symbol table for current scope function
fillLocalTable :: [Expression] -> LSymbolTable -> Int -> State ProgrammState Int
fillLocalTable ((VarDecl varType varName):xs) table offset = do
    fillLocalTable xs (table ++ [LocalSymbol varName varType offset]) (offset-2)

fillLocalTable [] table _ = do
    state <- get
    put $ state { psLST = psLST state ++ [table] }
    return 0
