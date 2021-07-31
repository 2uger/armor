### Main parts of syntax analysis:
  - Lexer (Lexer.hs)
  - Parser (Parser.hs)
  -- Create concrete parse tree with nodes from ParseTree.hs
  - Abstract syntax tree (Ast.hs)
  -- Define all AST node types(processing simple expression)
### Short overview of what to think about:
  - Lexical analyzer(simple tokens analyz)
    * Regular expression and FSM
    * Think about DFA vs NFA
    * REGex into DFA
    * NFA into DFA
    * Backtracking
    * Plan:
        - Tokenizer by DFA(scan main keywords, signs, data types and id's)
  - Parser(output is parse tree)
    * Context free grammar, Parser types(TD, BU), types of grammar itself.
    * TopDown vs BottomUp 
    * LL vs LR
    * Build LL(1) parser:
        - Find follow and first
        - Create Parse Table from this
        - Write parser(stack, parse table, parser itself)
        - Make right grammar for that(non-determenistic, lack of left recursion)
    * Build recursion descent parser:
        - After some time, realized that it's some way uncomfortable to write
          such parser in Haskell.
    * AST vs parse tree(concrete)
    * Parse tree could be represented as production, AST not!!!
  - Ast
    * Creating AST node types from ADT in Haskell
    * Think about pattern to be able to iterating through tree with different purposes

### TODO:
Actually build a language
