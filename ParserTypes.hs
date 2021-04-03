module ParserTypes where

-- Terminal will represent tokens from Lexer part
-- Just to make it comfortable to work with them while 
-- make parsing stage
data Terminal = TermId
              | TermInt Int 
              | TermBool Bool
              | TermChar Char

              | TermNumConst Int
              | TermCharConst Char
              | TermStringConst String

              | TermBackQuote -- ; 
              | TermComma     -- ,
              | TermColon     -- :

              | TermLParen      -- (
              | TermRParen      -- )
              | TermLSqBracket  -- [
              | TermRSqBracket  -- ]
              | TermLBrace      -- }
              | TermRBrace      -- {

              -- Keywords
              | TermStatic
              | TermWhile
              | TermReturn
              | TermBreak

              | TermPlus
              | TermMinus
              | TermMultiply
              | TermDivide
              | TermDivRemainder
              | TermEqual
              | TermIncrement
              | TermDecrement

              | TermEmpty
              deriving (Eq, Show, Read)

data NonTerminal = Program 
                 | DeclList 
                 | Decl
                 | DeclListN 

                 | VarDecl
                 | ScopedVarDecl 
                 | TypeSpec
                 | VarDeclList
                 | VarDeclListN
                 | VarDeclInit
                 | VarDeclId
                
                 | FuncDecl
                 | Parms
                 | ParmList
                 | ParmListN
                 | ParmTypeList
                 | ParmID

                 | Stmt
                 | ExprStmt
                 | CompoundStmt

                 | StmtList
                 | StmtListN
                 
                 | LocalDecl
                 | LocalDeclN

                 | IterStmt
                 | ReturnStmt
                 | BreakStmt

                 | Expr
                 | SimpleExpr
                 | SimpleExprN
                 | AndExpr
                 | AndExprN
                 | UnaryRelExpr

                 | RelExpr
                 | SumExpr
                 | SumExprN
                 | MulExpr
                 | SumOp
                 | MulOp

                 | Factor
                 | Mutable
                 | Immutable
                 | Call
                 | Args
                 | ArgsList
                 | Constant
                 deriving (Show, Read)
