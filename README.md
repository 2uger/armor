# Compiler for C
Start exciting journey. Use Haskell for that(pattern matching, functional language...).
Start from basic model of compiler and write module after module.

Basic structure is:
  - Lexical analyzer(simple tokens analyz)
    * Regular expression and FSM
  - Parser(output is parse tree)
    * Context free grammar, Parser types(TD, BU), types of grammar itself.
  - Semantic analyzer(It's all about semantic correctness)
  - Intermediate code representation
  - Code optimizer
  - Assembly output
