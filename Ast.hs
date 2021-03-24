module Ast where

-- C language consists of three main constructions
-- declaration, statement expression

data Type = TypeBool | TypeInt | TypeChar

data Declaration = { DeclName :: String
                   , DeclType :: Type
                   , DeclValue :: Value
                   , DeclCode :: String }

data Statement = 
