Section: .data:

Section: .text:
f:
    push {bp}
    mov bp, sp
    sub sp, sp, #4
    sub r1, bp, #16
    ldr r0, [r1]
    ldr r1, =9
    ldr r2, =1
    ldr r3, =4
    add r4, r2, r3
    add r2, r1, r4
    add r1, r0, r2
    add r0, bp, #4
    str r1, [r0]
    sub r1, bp, #4
    ldr r0, [r1]
    ldr r1, =2
    add r2, r0, r1
    sub r0, bp, #8
    str r2, [r0]
    mov sp, bp
    pop {bp ,r0}
    bx r0
func:
    push {bp}
    mov bp, sp
    sub sp, sp, #4
    ldr r0, =2
    push {r0}
    add sp, sp, #4
    bl f
    sub r0, sp, #8
    str r1, [r0]
    sub sp, sp, #16
    add r0, bp, #4
    str r1, [r0]
    sub r1, bp, #4
    ldr r0, [r1]
    sub r1, bp, #8
    str r0, [r1]
    mov sp, bp
    pop {bp ,r1}
    bx r1
