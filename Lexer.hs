module Lexer where

import Text.Parsec.String (Parser)
import Text.Parsec.Language (emptyDef)
import Text.Parsec.Prim (many)

import qualified Text.Parsec.Token as Token

lexer :: Token.TokenParser ()
lexer = Token.makeTokenParser style
  where
    ops = [ "+", "++", "*", "-", "/"]
    names = ["if", "else", "return", "=="]
    style = emptyDef {
               Token.commentLine = "//"
             , Token.commentStart = "**"
             , Token.commentEnd = "**"
             , Token.caseSensitive = True
             , Token.reservedOpNames = ops
             , Token.reservedNames = names
             }

-- Call this functions to parse different types of tokens
integer :: Parser Integer
integer    = Token.integer lexer
float      = Token.float lexer
char      = Token.charLiteral lexer
string     = Token.stringLiteral lexer
parens     = Token.parens lexer
braces     = Token.braces lexer
commaSep   = Token.commaSep lexer
semiSep    = Token.semiSep lexer
identifier = Token.identifier lexer
whitespace = Token.whiteSpace lexer
reserved   = Token.reserved lexer
reservedOp = Token.reservedOp lexer

operator :: Parser String
operator = do
    c <- Token.opStart emptyDef
    cs <- many $ Token.opLetter emptyDef
    return (c:cs)
