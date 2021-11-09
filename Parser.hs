module Parser where

import Text.Parsec
import Text.Parsec.String (Parser)

import Ast
import Lexer

parseOp :: Parser String
parseOp = do
    whitespace
    operand <- operator
    whitespace
    return operand

parseInt :: Parser Expression
parseInt = ExprValueInt <$> integer

parseChar :: Parser Expression
parseChar = ExprValueChar <$> cchar

parseExpr :: Parser Expression
parseExpr = try parseInt 
        <|> try parseChar 
        <|> try parseIncrem
        <|> try parseDefine
        <|> try parseIfElse

parseExprType :: Parser ExprType
parseExprType = do
    exprType <- identifier
    case exprType of
        "int" -> return TypeInt
        "char" -> return TypeChar
        "bool" -> return TypeBool
        "void" -> return TypeVoid
        _      -> return TypeVoid

parseDefine :: Parser Expression
parseDefine = do
    varType <- parseExprType
    varName <- identifier
    reserved "="
    value <- try parseInt <|> try parseChar
    return $ VarDef varType varName value

parseIfElse :: Parser Expression
parseIfElse = do
    reserved "if"
    cond <- parens parseExpr
    ifBranch <- parseBlock
    reserved "else"
    elseBranch <- parseBlock

    return $ ExprIfElse cond ifBranch elseBranch


parseBlock :: Parser [Expression]
parseBlock = braces $ many $
    do exp <- parseExpr
       reserved ";"
       return exp

sumParse :: Parser Integer
sumParse = do
  first <- integer
  reservedOp "+"
  second <- integer
  return (first + second)

parseIncrem :: Parser Expression
parseIncrem = do
    varRef <- identifier
    reservedOp "++"
    return $ ExprIncrem $ VarRef varRef

parseDecrem :: Parser Integer
parseDecrem = do
    value <- integer
    reservedOp "--"
    return (value - 1)

parseFunction :: Parser Expression
parseFunction = do
    retType <- parseExprType
    funcName <- identifier
    args <- parens $ commaSep parseDefine
    retExpr <- optionMaybe $ do
        reserved "return"
        retExpr <- parseIncrem
        return retExpr
    reserved "->"
    exprBlock <- parseBlock
    return $ FuncDef retType funcName args exprBlock retExpr

parseMainFunc :: Parser Expression
parseMainFunc = do
    funcDef <- parseFunction
    reservedOp ";"
    return funcDef

parseSourceCode = parse parseMainFunc "<stdin>"
