.data
.word 2
.word 12312

.text
f:
	push {bp}
	mov bp, sp
	sub sp, sp, #-4
	bne lable_else_0
	ldr r0, =3
	str r0, [bp, #-8]
	ldr r0, [4100]
	ldr r1, [4096]
	add r2, r0, r1
	mov r0, r2
	mov sp, bp
	pop {bp}
	bx lr
lable_else_0:
	ldr r0, =2
	push {r0}
	bl f
	sub sp, sp, #4
	ldr r1, [bp, #-8]
	ldr r2, =2
	add r3, r1, r2
	mov r0, r3
	mov sp, bp
	pop {bp}
	bx lr
