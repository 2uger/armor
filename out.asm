Section: .data:

Section: .text:
f:
    push {bp}
    mov bp, sp
    sub sp, sp, #8
    ldr r0, =2
    sub r2, bp, #20
    ldr r1, [r2]
    add r2, r0, r1
    add r0, bp, #4
    str r2, [r0]
    sub r1, bp, #4
    ldr r0, [r1]
    sub r2, bp, #4
    ldr r1, [r2]
    add r2, r0, r1
    add r0, bp, #8
    str r2, [r0]
