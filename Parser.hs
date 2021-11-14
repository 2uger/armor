module Parser where

import Text.Parsec
import Text.Parsec.String (Parser)

import qualified Text.Parsec.Expr as Ex

import Ast
import Lexer

binary s f assoc = Ex.Infix (reservedOp s >> return (ExprBinOp f)) assoc

table = [[binary "*" OpMultiply Ex.AssocLeft,
          binary "/" OpDivide Ex.AssocLeft]
        ,[binary "+" OpPlus Ex.AssocLeft,
          binary "-" OpMinus Ex.AssocLeft]]

parseExpr :: Parser Expression
parseExpr = Ex.buildExpressionParser table factor

factor :: Parser Expression
factor = try parseInt 
     <|> try parseChar 
     <|> try parseIncrem
     <|> try parseDefine
     <|> try parseDecl
     <|> try parseVar
     <|> try parseIfElse
     <|> try parseReturn
     <|> parens parseExpr

parseInt :: Parser Expression
parseInt = ExprValueInt <$> integer

parseChar :: Parser Expression
parseChar = ExprValueChar <$> cchar

parseVar :: Parser Expression
parseVar = VarRef <$> identifier

parseExprType :: Parser ExprType
parseExprType = do
    exprType <- identifier
    case exprType of
        "int" -> return TypeInt
        "char" -> return TypeChar
        "bool" -> return TypeBool
        "void" -> return TypeVoid
        _      -> return TypeVoid

parseStmt :: Parser Expression
parseStmt = do
    exprL <- parseExpr
    reserved "=="
    exprR <- parseExpr
    return $ ExprStmt exprL exprR

parseDecl :: Parser Expression
parseDecl = do
    varType <- parseExprType
    varName <- identifier
    return $ VarDecl varType varName

parseDefine :: Parser Expression
parseDefine = do
    varType <- parseExprType
    varName <- identifier
    reserved "="
    value <- parseExpr
    return $ VarDef varType varName value

parseIfElse :: Parser Expression
parseIfElse = do
    reserved "if"
    cond <- parens parseStmt
    ifBranch <- parseBlock
    reserved "else"
    elseBranch <- parseBlock

    return $ ExprIfElse cond (Block ifBranch) (Block elseBranch)

parseBlock :: Parser [Expression]
parseBlock = braces $ many $
    do exp <- parseExpr
       reserved ";"
       return exp

parseIncrem :: Parser Expression
parseIncrem = do
    varIdent <- identifier
    reservedOp "++"
    return $ ExprIncrem $ VarRef varIdent

parseDecrem :: Parser Expression
parseDecrem = do
    varIdent <- identifier
    reservedOp "--"
    return $ ExprDecrem $ VarRef varIdent

parseFunction :: Parser Expression
parseFunction = do
    retType <- parseExprType
    funcName <- identifier
    args <- parens $ commaSep parseDecl
    reserved "->"
    exprBlock <- parseBlock
    return $ FuncDef retType funcName (FuncArgs args) (Block exprBlock)

parseReturn :: Parser Expression
parseReturn = do
    reserved "return"
    expr <- parseExpr
    return $ RetExpr expr

parseMainFunc :: Parser Expression
parseMainFunc = do
    funcDef <- parseFunction
    reservedOp ";"
    return funcDef

parseSourceCode = parse parseMainFunc "<stdin>"
