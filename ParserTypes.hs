module ParserTypes where

-- Terminal will represent tokens from Lexer part
-- Just to make it comfortable to work with them while 
-- make parsing stage
data Terminal = TermId 
              | TermInt
              | TermBool
              | TermChar

              | TermNumConst
              | TermCharConst
              | TermStringConst
              | TermBoolConst

              | TermBackQuote -- ; 
              | TermComma     -- ,
              | TermColon     -- :

              | TermLParen      -- (
              | TermRParen      -- )
              | TermLSqBracket  -- [
              | TermRSqBracket  -- ]
              | TermLBrace      -- {
              | TermRBrace      -- }

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
                 | ParmType
                 | ParmId

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
                 | MulExprN
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
