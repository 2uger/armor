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

### Example:
```
int r = 2;

int f(int arg_1, int arg_2) {
    int ll;
    if (arg_1 > arg_2) {
        ll = 2;
    } else {
        ll = 3;
    }
    return ll;
};

int m = 3;

void main() {
    int loc = f(2, 3) + 1;
    return loc;
};
```
Will generate:
```
.data
r: .word 2
m: .word 3

.text
.global _start
_start:
	b main
f:
	push {r12}
	mov r12, sp
	sub sp, sp, #8
	ldr r0, [r12, #4]
	ldr r1, [r12, #8]
	cmp r0, r1
	ble lable_else_1
	ldr r0, =2
	str r0, [r12, #-4]
	b lable_end_0
lable_else_1:
	ldr r0, =3
	str r0, [r12, #-4]
lable_end_0:
	ldr r0, [r12, #-4]
	mov sp, r12
	pop {r12}
	bx lr
main:
	push {r12}
	mov r12, sp
	sub sp, sp, #8
	ldr r0, =2
	ldr r1, =3
	push {r0, r1}
	bl f
	add sp, sp, #8
	ldr r1, =1
	add r2, r0, r1
	str r2, [r12, #-4]
	ldr r0, [r12, #-4]
	mov sp, r12
	pop {r12}
	bx lr

adr_r: .word r
adr_m: .word m
```
### How to run:  
python3 main.py m1.c -o out.asm
