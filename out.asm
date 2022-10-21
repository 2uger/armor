.text
.global _start
_start:
	b main
main:
	push {r12}
	mov r12, sp
	sub sp, sp, #16
	ldr r0, =2
	str r0, [r12, #0]
	ldr r0, =2
	ldr r1, =1
	cmp r0, r1
	bgt lable_else_0
	ldr r0, =2
	ldr r1, =2
	add r2, r0, r1
	str r2, [r12, #4]
lable_else_0:
	ldr r0, =3
	str r0, [r12, #8]
	ldr r0, =3
	str r0, [r12, #12]
	ldr r0, [r12, #12]
	mov sp, r12
	pop {r12}
	bx lr

