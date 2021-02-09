# Compiler for C
Start exciting journey. Use Haskell for that(pattern matching, functional language...).
Start from basic model of compiler and write module after module.

Basic structure is:
  - Lexical analyzer(simple tokens analyz)
    * Regular expression and FSM
    * Think about DFA vs NFA
    * NFA into DFA
    * Backtracking
    * Plan:
        - Tokenizer by DFA(scan main keywords, signs, data types and id's)
  - Parser(output is parse tree)
    * Context free grammar, Parser types(TD, BU), types of grammar itself.
    * TopDown vs BottomUp 
    * LL vs LR
    * Plan:
        - Write LR-ish parser doesnt worth it, but demand to understand adv/disadv
        - Make LL1 grammar parser(both approaches: driven table and recursive descent)
  - Semantic analyzer(It's all about semantic correctness)
  - Intermediate code representation
  - Code optimizer
  - Assembly output
