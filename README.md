# Compiler for C-ish language
Use Haskell for that(pattern matching, functional language...).
Start from basic model of compiler and write module after module.
Implement full C compiler is hard as we know...
The goal is to be able to compile code such that:
> int/void main(int args) {
>   int m = 2;
>   int p[3] = {4, 2, 8};
>   m = p[0] + p[2];
>   while (1) {
>       printf("Symbol n%d is %d\n", p[2]);
>       break;
>   return m;

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
  - Semantic analyzer(It's all about semantic correctness)
  - Intermediate code representation
  - Code optimizer
  - Assembly output
