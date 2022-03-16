module Parser where

import Text.Parsec
import Text.Parsec.String (Parser)
import qualified Text.Parsec.Expr as Ex

import Ast
import qualified Lexer as Lx

binary s f assoc = Ex.Infix (Lx.reserved s >> return (ExprBinOp f)) assoc

table = [[binary "*" OpMultiply Ex.AssocLeft,
          binary "/" OpDivide Ex.AssocLeft]
        ,[binary "+" OpPlus Ex.AssocLeft,
          binary "-" OpMinus Ex.AssocLeft]]

exprP :: Parser Expression
exprP = Ex.buildExpressionParser table factor

factor :: Parser Expression
factor = try intP 
     <|> try charP 
     <|> try varAssignP
     <|> try funcDefP
     <|> try funcCallP
     <|> try funcDeclP
     <|> try varDefineP
     <|> try varDeclP
     <|> try incremP
     <|> try varP
     <|> try ifElseP
     <|> try returnP
     <|> Lx.parens exprP

intP :: Parser Expression
intP = ExprValueInt <$> Lx.integer

charP :: Parser Expression
charP = ExprValueChar <$> Lx.char

varP :: Parser Expression
varP = VarRef <$> Lx.identifier

exprTypeP :: Parser ExprType
exprTypeP = do
    exprType <- Lx.identifier
    case exprType of
        "int" -> return TypeInt
        "char" -> return TypeChar
        "void" -> return TypeVoid
        _      -> return TypeVoid

stmtPEq :: Parser Expression
stmtPEq = do
    exprL <- exprP
    try $ Lx.reserved "=="
    exprR <- exprP
    return $ ExprStmt exprL "==" exprR

stmtPGt :: Parser Expression
stmtPGt = do
    exprL <- exprP
    try $ Lx.reserved ">"
    exprR <- exprP
    return $ ExprStmt exprL ">" exprR

stmtPLt :: Parser Expression
stmtPLt = do
    exprL <- exprP
    try $ Lx.reserved "<"
    exprR <- exprP
    return $ ExprStmt exprL "<" exprR

varDeclP :: Parser Expression
varDeclP = do
    varType <- exprTypeP
    varName <- Lx.identifier
    return $ VarDecl varType varName

varDefineP :: Parser Expression
varDefineP = do
    varType <- exprTypeP
    varName <- Lx.identifier
    Lx.reserved "="
    value <- exprP
    return $ VarDef varType varName value

varAssignP :: Parser Expression
varAssignP = do
    varName <- Lx.identifier
    Lx.reserved "="
    value <- exprP
    return $ VarAssign varName value

ifElseP :: Parser Expression
ifElseP = do
    Lx.reserved "if"
    cond <- Lx.parens (try stmtPEq <|> try stmtPGt <|> try stmtPLt)
    ifBranch <- blockP
    Lx.reserved "else"
    elseBranch <- blockP
    return $ ExprIfElse cond (Block ifBranch) (Block elseBranch)

blockP :: Parser [Expression]
blockP = Lx.braces $ many $
    do exp <- exprP
       Lx.reserved ";"
       return exp

incremP :: Parser Expression
incremP = do
    varIdent <- Lx.identifier
    Lx.reserved "++"
    return $ ExprIncrem $ VarRef varIdent

funcDefP :: Parser Expression
funcDefP = do
    retType <- exprTypeP
    funcName <- Lx.identifier
    parms <- Lx.parens $ Lx.commaSep varDeclP
    Lx.reserved "->"
    exprBlock <- blockP
    return $ FuncDef retType funcName (FuncParms parms) (Block exprBlock)

funcDeclP :: Parser Expression
funcDeclP = do
    retType <- exprTypeP
    funcName <- Lx.identifier
    parms <- Lx.parens $ Lx.commaSep varDeclP
    return $ FuncDecl retType funcName (FuncParms parms)

funcCallP :: Parser Expression
funcCallP = do
    funcName <- Lx.identifier
    parms <- Lx.parens $ Lx.commaSep exprP
    return $ FuncCall funcName parms

returnP :: Parser Expression
returnP = do
    Lx.reserved "return"
    expr <- exprP
    return $ RetExpr expr

mainFuncP :: Parser [Expression]
mainFuncP = many $ do
    expr <- exprP
    Lx.reserved ";"
    return expr

sourceCodeP = parse mainFuncP "<stdin>"
