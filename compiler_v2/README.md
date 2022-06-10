Compiler for C-like language in python

TODO:
- Think about register representation - [+]
***
- Generate code for function call [+-]
- Push empty space into stack for local variables in function [+-]
  - Collect local variables when generate code for function  
  to give them an offset from BP register to know where it's located
  - Problem: for now we don't collect nested declaration
- Beeing able to recognize local variables in function body [+-]
- Follow current context to know what function should return [+]
***
- Add char type, make it different size
***
- Count every expression inside if statement []
- Else lables same everywhere, need new ones for every if..else []
