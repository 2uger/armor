import System.Environment
import System.IO
import System.IO.Error

import Control.Exception

import ParserTypes
import ParseTree
import Parser

testVarTerms = [TermInt, TermId, TermLSqBracket, TermNumConst, TermRSqBracket, TermColon, TermBackQuote]

testFuncTerms = [TermInt, TermId, TermRSqBracket, TermInt, TermId, TermRParen]

mainFunc :: String 
mainFunc = show $ parse Program testFuncTerms

