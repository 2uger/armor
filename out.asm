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
	sub sp, sp, #12
	ldr r0, [r12, #4]
	ldr r1, [r12, #8]
	cmp r0, r1
	blt lable_else_0
	ldr r0, =3
	str r0, [r12, #-4]
lable_else_0:
	ldr r0, =1
	str r0, [r12, #-8]
	ldr r0, [r12, #-8]
	mov sp, r12
	pop {r12}
	bx lr
main:
	push {r12}
	mov r12, sp
	sub sp, sp, #8
	ldr r0, =2
	ldr r1, adr_m
	ldr r2, [r1]
	push {r0, r2}
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
