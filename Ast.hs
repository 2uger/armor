module Ast where

-- C-ish language consists of three main constructions
-- declaration(variable declaration,
--             func declaration) 
-- statement(statement is process of somehow changing programm
--           state)
-- expression(something that will have evaluated value
--            2 + 2 will have value 4
--            a will have value of a which we init or declare before)

-- ****** Declaration ******
-- Declaration describes expressions like
-- int a = 10;
-- char m = 'a';
-- int func(int x) {
--      int m = x * 45;
--      return m;
-- };
data VarDeclaration = { Name :: String
                      , Type :: Type
                      , InitValue :: Expression }

data FuncDeclaration = { Name :: String
                       , Type :: Type
                       , Code :: StatementBlock}

data Type = TypeBool 
          | TypeInt 
          | TypeChar 
          | TypeVoid
          | TypeArray
          | TypeFunction Type ParamList

data ParamList = ParamList String Type ParamList

-- ****** Statement ******
data Statement = VarDecl | Expression | IfElse | ForLoop | Return

data ExprIfElse = ExprIfElse { Condition :: Expression,
                               IfCode :: StatementBlock,
                               ElseCode :: StatementBlock }

data ExprForLoop = ExprForLoop { InitExpr :: Expression,
                                 Condition :: Expression,
                                 NextIter :: Expression,
                                 Body :: StatementBlock }

-- Statement block will contain multiple statements
data StatementBlock a = EmptyStmt | Stmt a StatementBlock a


-- ****** Expression ******
data Expression = ExprName String 
                | ExprValue Value 
                | ExprAdd { Left :: Expression,
                            Right :: Expression }
                | ExprSub { Left :: Expression,
                            Right :: Expression }
                | ExprMul { Left :: Expression,
                            Right :: Expression }
                | ExprDiv { Left :: Expression,
                            Right :: Expression }
                | ExprAnd { Left :: Expression,
                            Right :: Expression }
                | ExprOr  { Left :: Expression,
                            Right :: Expression }
