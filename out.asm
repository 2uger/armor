Section: .data:

Section: .text:
f:
    push {bp}
    mov bp, sp
    sub sp, sp, #12
    sub r1, bp, #16
    ldr r0, [r1]
    add r1, bp, #4
    str r0, [r1]
    sub r1, bp, #4
    ldr r0, [r1]
    ldr r1, =1
    add r2, r0, r1
    add r0, bp, #8
    str r2, [r0]
    ldr r0, =1
    sub r1, bp, #4
    str r0, [r1]
    sub r1, bp, #16
    ldr r0, [r1]
    ldr r1, =1
    sub r2, r0, r1
    add r0, bp, #12
    str r2, [r0]
    sub r1, bp, #4
    ldr r0, [r1]
    ldr r1, =2
    add r2, r0, r1
    sub r0, bp, #8
    str r2, [r0]
    mov sp, bp
    pop {bp ,r0}
    bx r0
new_func:
    push {bp}
    mov bp, sp
    sub sp, sp, #4
    ldr r0, =1
    add r1, bp, #4
    str r0, [r1]
