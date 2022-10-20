.data
.word 2

.text
f:
	push {bp}
	mov bp, sp
	ldr r0, =2
	ldr r1, [4096]
	add r2, r0, r1
	ldr r0, [bp, #-16]
	add r1, r0, r2
	str r1, [4096]
	ldr r0, [4096]
	mov sp, bp
	pop {bp}
	bx lr
func_2:
	push {bp}
	mov bp, sp
	ldr r0, =2
	ldr r1, =3
	ldr r2, =4
	push {r0, r1, r2}
	bl f
	sub sp, sp, #12
	ldr r3, =2
	mov r0, r3
	mov sp, bp
	pop {bp}
	bx lr
