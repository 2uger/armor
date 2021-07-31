import System.Environment
import System.IO
import System.IO.Error

import Control.Exception

import ParserTypes
import ParseTree
import Ast

--testVarTerms = [TermInt, TermId, TermLSqBracket, TermNumConst, TermRSqBracket, TermColon, TermBackQuote]
--
--testFuncTerms = [TermInt, TermId, TermLParen, TermInt, TermId, TermRParen, TermLBrace, TermRBrace]
--
--testIterStmt = [TermWhile, TermLParen, TermRParen]
--
--testAstExpression = []
--
--testParseAst = []

m1 = NodeMulExprN (Leaf TermEmpty) (NodeFactor (NodeConstant (Leaf (TermNumConst 2)))) EmptyTree
m = NodeMulExprN (Leaf TermEmpty) (NodeFactor (NodeConstant (Leaf (TermNumConst 2)))) m1
mm = NodeFactor (NodeConstant (Leaf (TermNumConst 5)))


and = ExprAnd (ExprValueInt 5) (ExprValueInt 44)
mul = ExprMul (ExprValueInt 4) (ExprValueInt 1991) 
sum_1 = ExprAdd (ExprValueInt 20) mul
sum_2 = ExprAdd (ExprValueInt 25) sum_1
sum_string = ExprAdd (ExprValueString "Hello") (ExprValueString " world")

aand = ExprOr (ExprValueBool True) (ExprValueBool False)


testReturnStmt = [TermReturn, TermBackQuote]
testBreakStmt = [TermBreak, TermBackQuote]

mmain = expression aand 


