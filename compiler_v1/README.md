## Compiler
### Create basic, but fundamentals components of compiler:
* Lexer
* Parser
* Syntax and Semantic analysis
* Ast processing and ASM output

### Sources:
* [Full roadmap for compiler fundamentals](https://silcnitc.github.io/roadmap.html)
* [Example of compiler written in Haskell](https://habr.com/ru/post/581234/)

### What to do:
* Type inference and correction
* Proper function declaration
* New data types

### Example:
Simple programm looks like:
```sh
int r;
int m;

m = m + r;

void func(int k, int l) -> {
    if (r == m) {
        r = 2 * 2;
    } else {
        m = 2 * r;
    };
    k++;
    return l;
};

m = func(2, 2);
```
ASM output:
```sh
Text:
   LDR R1, [4098]
   LDR R2, [4096]
   ADD R0, R1, R2
   STR R0, [4098]
   
func:
   PUSH BP
   MOV BP, SP
   STR r0, [4096]
   STR r1, [4098]
   CMP r0, r1
   BNE L1
   L0:
   MOV R0, #2
   MOV R1, #2
   MUL R0, R0, R1
   STR R0, [4096]
   L1:
   MOV R0, #2
   LDR R1, [4096]
   MUL R0, R0, R1
   STR R0, [4098]
   LDR R0, [BP-10]
   ADD R0, R0, #1
   STR R0, [BP-10]
   LDR R0 [BP-8]
   STR R0, [BP-6]
   MOV SP, BP
   ADD SP, SP, #2
   LDR R0, [BP-4]
   MOV PC, R0

   MOV R0, #2
   PUSH {R0}
   MOV R0, #2
   PUSH {R0}
   SUB SP, SP, #2
   BL func
   ADD SP, SP, #2
   POP {R0}
   STR R0, [4098]
Data: 
   4096:r
   4098:m
```

### How to run
* stack build --compiler ghc-{ghc-version} --system-ghc
* stack exec compiler-exe --compiler ghc-{ghc-version} --system-ghc m3.io
### Short overview of what to think about:
  - Lexical analyzer(simple tokens analyz)
    * Regular expression and FSM
    * Think about DFA vs NFA
    * REGex into DFA
    * NFA into DFA
    * Backtracking
  - Parser(output is parse tree)
    * Context free grammar, Parser types(TD, BU), types of grammar itself.
    * TopDown vs BottomUp 
    * LL vs LR
    * AST vs parse tree(concrete)
    * Parse tree could be represented as production, AST not!!!
