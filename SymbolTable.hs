data SymbolTableEntry = SymbolTableEntry { scope :: Int
                                         , 
-- ****** Functions to work with Symbol Table  ******
-- Symbol table actually will store whole information about var of func
-- It will handle scopes, types and other information to know how to work
-- with exact identifier name
-- Symbol table building happenes when we traverse AST

openScope :: 
closeScope :: 

processNode :: 
-- Write down whole information about current node
enterSymbol ::
-- Return block describing symbol
retrieveSymbol :: 
