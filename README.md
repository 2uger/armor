Compiler for C-like language.  
### Main features:
* Global and local variables
* Function calls
* Branching
* Type checking

### CPU architecture specs:
* Use armV7 base arch
* r0 - r11 - free regs
* r12 - as frame pointer

# Example:
```
int r = 2;

int f(int arg_1, int arg_2) {
    return arg_1 + arg_2;
};

int main(int k) {
    int loc = k + 1;
    return f(2, loc);
};

```
Will generate:
```
.data
r: .word 2

.text
.global _start
_start:
        b main
f:
        push {r12}
        mov r12, sp
        ldr r0, [r12, #4]
        ldr r1, [r12, #8]
        add r2, r0, r1
        mov r0, r2
        mov sp, r12
        pop {r12}
        bx lr
main:
        push {r12}
        mov r12, sp
        sub sp, sp, #4
        ldr r0, [r12, #4]
        ldr r1, =1
        add r2, r0, r1
        str r2, [r12, #0]
        ldr r0, =2
        ldr r1, [r12, #0]
        push {r0, r1}
        bl f
        add sp, sp, #8
        mov sp, r12
        pop {r12}
        bx lr

adr_r: .word r
```
### How to run:  
**python3 main.py m1.c -o out.asm**

### To implement:
- More type checking
- Add char type, make it different size
