Compiler for C-like language.  
### Main features:
* Generate code straight to assembly.  
* Use simple techniques for code parsing and generation
* Understand how to implement:
  - Function code generation
  - Handle global and local context
  - Evaluate expressions
  - Function calls and returns

### CPU architecture specs:
* Use stack to pass variables to function and local variables
* r0-r13, sp, bp, pc  
 bp - frame pointer, point to the beginning of the function's stack

### How to run:  
**python3 main.py m1.c -o out.asm**

### To implement:
- Think about register representation [+]
- Generate code for function call [+]
- Push empty space into stack for local variables in function [+]
- Beeing able to recognize local variables in function body [+]
- Follow current context to know what function should return [+]
- Need somehow check that function will return smth
- Add char type, make it different size
- Count every expression inside if statement
- Else lables same everywhere, need new ones for every if..else [+]
