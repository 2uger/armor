module Typecheck where

import Ast

typeEquals :: Expression -> Bool
typeEquals (ExprBinOp operand exprL exprR) =
    case (exprTypeCheck exprL == exprTypeCheck exprR) of
        True -> True
        False -> error "Bad types"

exprTypeCheck :: Expression -> ExprType
exprTypeCheck (VarRef name) = lookupSymbol  

    


