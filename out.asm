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
