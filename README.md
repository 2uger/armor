# TODO:
* Think about right representation of intermediate info
  between parser function(return code or error...)
* After write parser through the end
* Output is parse Tree via GRAPHVIZ
* Represent all nodes in ast(try to build one)
# Compiler for C-ish language
Use Haskell for that(pattern matching, functional language...).
Start from basic model of compiler and write module after module.
Implement full C compiler is hard as we know...
So it's implement simple grammar in grammar.txt

Basic structure is:
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
    * Build recursion descent parser
    * AST vs parse tree(concrete)
    * Parse tree could be represented as production, AST not!!!
  - Semantic analyzer(It's all about semantic correctness)
  - Intermediate code representation
  - Code optimizer
  - Assembly output
