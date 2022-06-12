Compiler for C-like language   
Generate code straight to assembly  
Main points:
* Use simple techniques for code parsing and generation
* Understand how to implement:
  - Function code generation
  - Handle global and local context
  - Evaluate expressions
  - Function calls and returns

CPU arch:
* Use stack for pass variables to function and local variables
* r0-r13, sp, bp, pc  
 bp - frame pointer, point to the beginning of the function's stack

How to run:  
> python3 main.py m1.c -o out.asm

TODO:
- Think about register representation - [+]
***
- Generate code for function call [+]
- Push empty space into stack for local variables in function [+]
  - Collect local variables when generate code for function  
  to give them an offset from BP register to know where it's located
- Beeing able to recognize local variables in function body [+]
- Follow current context to know what function should return [+]
- Need somehow check that function will return smth
***
- Add char type, make it different size
***
- Count every expression inside if statement
- Else lables same everywhere, need new ones for every if..else [+]
