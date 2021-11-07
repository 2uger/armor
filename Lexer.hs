module Lexer where

import Text.Parsec (parse)
import Text.Parsec.String (Parser)
import Text.Parsec.Language (emptyDef)

import qualified Text.Parsec.Token as Token

lexer :: Token.TokenParser ()
lexer = Token.makeTokenParser style
  where
    ops = [ "+", "++", "*", "-", "/"]
    names = ["if", "else"]
    style = emptyDef {
               Token.commentLine = "//"
             , Token.commentStart = "**"
             , Token.commentEnd = "**"
             , Token.caseSensitive = True
             , Token.reservedOpNames = ops
             , Token.reservedNames = names
             }

-- Call this functions to parse different types of tokens
integer    = Token.integer lexer
float      = Token.float lexer
char       = Token.charLiteral lexer
string     = Token.stringLiteral lexer
parens     = Token.parens lexer
braces     = Token.braces lexer
commaSep   = Token.commaSep lexer
semiSep    = Token.semiSep lexer
identifier = Token.identifier lexer
whitespace = Token.whiteSpace lexer
reserved   = Token.reserved lexer
reservedOp = Token.reservedOp lexer
