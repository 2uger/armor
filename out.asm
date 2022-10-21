.text
.global _start
_start:
	b main
f:
	push {r12}
	mov r12, sp
	sub sp, sp, #8
	ldr r0, [r12, #4]
	ldr r1, =12
	add r2, r0, r1
	str r2, [r12, #-4]
	ldr r0, [r12, #-4]
	mov sp, r12
	pop {r12}
	bx lr
main:
	push {r12}
	mov r12, sp
	sub sp, sp, #20
	ldr r0, =2
	str r0, [r12, #-4]
	ldr r0, =2
	push {r0}
	bl f
	add sp, sp, #4
	ldr r1, =0
	cmp r0, r1
	beq lable_else_0
	ldr r0, =2
	ldr r1, [r12, #-4]
	add r2, r0, r1
	str r2, [r12, #-8]
lable_else_0:
	ldr r0, =3
	str r0, [r12, #-12]
	ldr r0, =3
	str r0, [r12, #-16]
	ldr r0, [r12, #-16]
	mov sp, r12
	pop {r12}
	bx lr

