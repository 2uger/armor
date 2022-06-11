    mov r0, #2
    mov r1, #12
    add r2, r0, r1
    mov r0, #4096
    str r2, [r0]
    mov r0, #21
    mov r1, #2
    add r2, r0, r1
    mov r0, #4100
    str r2, [r0]
    

Identifier: function:
    push {bp}
    mov bp, sp
    sub sp, sp, #12
    mov r0, #2
    mov r1, #2
    mul r2, r0, r1
    add r0, bp, #0
    str r2, [r0]
    mov r0, #2
    add r1, bp, #4
    str r0, [r1]
    mov r0, #2
    mov r1, #22
    mov r2, #2
    push {r1 ,r2}
    add sp, sp, #4
    b function
    sub r1, sp, #4
    str r2, [r1]
    sub sp, sp, #16
    cmp r0, r2
    blt l0
    mov r1, #2
    add r3, bp, #8
    str r1, [r3]
    mov r3, #4096
    ldr r1, [r3]
    sub r4, bp, #16
    ldr r3, [r4]
    add r4, r1, r3
    sub r1, bp, #8
    str r4, [r1]
    pop {bp ,r1}
    bx r1
l0:
    mov r1, #324
    sub r3, bp, #8
    str r1, [r3]
    pop {bp ,r3}
    bx r3
    

Identifier: rec:
    push {bp}
    mov bp, sp
    sub sp, sp, #0
    sub r3, bp, #16
    ldr r1, [r3]
    mov r3, #20
    cmp r1, r3
    beq l1
    mov r4, #99
    sub r5, bp, #8
    str r4, [r5]
    pop {bp ,r5}
    bx r5
l1:
    mov r4, #2
    push {r4}
    add sp, sp, #4
    b rec
    sub r4, sp, #4
    str r5, [r4]
    sub sp, sp, #12
    push {r5}
    add sp, sp, #4
    b rec
    sub r4, sp, #4
    str r5, [r4]
    sub sp, sp, #12
    

Identifier: main:
    push {bp}
    mov bp, sp
    sub sp, sp, #0
    mov r4, #2
    mov r6, #2
    push {r4 ,r6}
    add sp, sp, #4
    b function
    sub r4, sp, #4
    str r6, [r4]
    sub sp, sp, #16
    mov r4, #4100
    str r6, [r4]
    mov r4, #2
    sub r6, bp, #8
    str r4, [r6]
    pop {bp ,r6}
    bx r6
