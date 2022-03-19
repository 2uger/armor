#### Compiler
Home made simple compiler written in Haskell and include all main parts:
* Lexer
* Parser
* Syntax and Semantic analysis
* Ast processing and ASM output

Simple programm looks like:
```sh
int r;
int c;
int f (int m, int t) ->  {
    if (m == t) {
        c = 2 * t + m;
    } else {
        c = 1;
    };
    return c;
};
int main(int k) -> {
    c = f(2, k);
    return c;
}
```
### How to run
* stack build --compiler ghc-{ghc-version} --system-ghc
* stack exec compiler-exe --compiler ghc-{ghc-version} --system-ghc m3.io

Create basic, but fundamentals components of compiler.
Written fully on Haskell for practice purposes.

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
