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



testReturnStmt = [TermReturn, TermBackQuote]
testBreakStmt = [TermBreak, TermBackQuote]

mmain = parseExpression (NodeMulExpr mm m)


