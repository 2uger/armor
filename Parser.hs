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
     <|> try parseVarAssign
     <|> try parseFunction
     <|> try parseFuncCall
     <|> try parseFuncDecl
     <|> try parseIncrem
     <|> try parseVarDefine
     <|> try parseVarDecl
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

parseVarDecl :: Parser Expression
parseVarDecl = do
    varType <- parseExprType
    varName <- identifier
    return $ VarDecl varType varName

parseVarDefine :: Parser Expression
parseVarDefine = do
    varType <- parseExprType
    varName <- identifier
    reserved "="
    value <- parseExpr
    return $ VarDef varType varName value

parseVarAssign :: Parser Expression
parseVarAssign = do
    varName <- identifier
    reserved "="
    value <- parseExpr
    return $ VarAssign varName value

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
    parms <- parens $ commaSep parseVarDecl
    reserved "->"
    exprBlock <- parseBlock
    return $ FuncDef retType funcName (FuncParms parms) (Block exprBlock)

parseFuncDecl :: Parser Expression
parseFuncDecl = do
    retType <- parseExprType
    funcName <- identifier
    parms <- parens $ commaSep parseVarDecl
    return $ FuncDecl retType funcName (FuncParms parms)

parseFuncCall :: Parser Expression
parseFuncCall = do
    funcName <- identifier
    parms <- parens $ commaSep parseVar
    return $ FuncCall funcName parms

parseReturn :: Parser Expression
parseReturn = do
    reserved "return"
    expr <- parseExpr
    return $ RetExpr expr

parseMainFunc :: Parser [Expression]
parseMainFunc = many $ do
    expr <- parseExpr
    reserved ";"
    return expr

parseSourceCode = parse parseMainFunc "<stdin>"
