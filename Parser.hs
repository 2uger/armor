module Parser where

import Text.Parsec (parse, many)
import Text.Parsec.String (Parser)

import Ast
import Lexer


parseInt :: Parser Expression
parseInt = ExprInt <$> integer

parseChar :: Parser Expression
parseChar = ExprChar <$> char

parseVar :: Parser Expression
parseVar = Variable <$> identifier

parseIfElse :: Parser Expression
parseIfElse = do
    reserved "if"
    cond <- parens expr
    ifBranch <- block
    elseBranch <- block

    return $ IfElse cond ifBranch elseBranch


block :: Parser Expression
block = Block <$> braces $ many $
    do exp <- expr
       reserved ";"
       return exp

sumParse :: Parser Integer
sumParse = do
  first <- integer
  reservedOp "+"
  second <- integer
  return (first + second)

increment :: Parser Double
increment = do
    iff <- reserved "ifff"
    first <- float
    reservedOp "++"
    return (first + 2)



runSumParse = parse parseVar "<stdin>"
