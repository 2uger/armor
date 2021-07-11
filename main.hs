import System.Environment
import System.IO
import System.IO.Error

import Control.Exception

import ParserTypes
import ParseTree
import Parser

testVarTerms = [TermInt, TermId, TermLSqBracket, TermNumConst, TermRSqBracket, TermColon, TermBackQuote]

testFuncTerms = [TermInt, TermId, TermLParen, TermInt, TermId, TermRParen, TermLBrace, TermRBrace]

testIterStmt = [TermWhile, TermLParen, TermRParen]

testReturnStmt = [TermReturn, TermBackQuote]
testBreakStmt = [TermBreak, TermBackQuote]


