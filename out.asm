Section: .data:
0x1000	0x3	 // r

Section: .text:
f:
    push {bp}
    mov bp, sp
    sub sp, sp, #0
    sub r1, bp, #16
    ldr r0, [r1]
    ldr r1, =0
    cmp r0, r1
    bgt LBL_0
    sub r1, bp, #16
    ldr r0, [r1]
    ldr r2, =4096
    ldr r1, [r2]
    add r2, r0, r1
    sub r0, bp, #8
    str r2, [r0]
    mov sp, bp
    pop {bp ,r0}
    bx r0
LBL_0:
    sub r1, bp, #16
    ldr r0, [r1]
    ldr r1, =2
    add r2, r0, r1
    sub r0, bp, #8
    str r2, [r0]
    mov sp, bp
    pop {bp ,r0}
    bx r0
    sub r1, bp, #16
    ldr r0, [r1]
    ldr r1, =2
    add r2, r0, r1
    sub r0, bp, #8
    str r2, [r0]
    mov sp, bp
    pop {bp ,r0}
    bx r0
