Section: .data:
0x1000	0x2	 // m
0x1004	0x17	 // n

Section: .text:
function:
    push {bp}
    mov bp, sp
    sub sp, sp, #12
    ldr r0, =2
    ldr r1, =2
    mul r2, r0, r1
    add r0, bp, #0
    str r2, [r0]
    ldr r0, =2
    add r1, bp, #4
    str r0, [r1]
    ldr r0, =2
    ldr r1, =22
    ldr r2, =2
    push {r1 ,r2}
    add sp, sp, #4
    bl function
    sub r1, sp, #4
    str r2, [r1]
    sub sp, sp, #16
    cmp r0, r2
    blt l0
    ldr r1, =2
    add r3, bp, #8
    str r1, [r3]
    sub r3, bp, #16
    ldr r1, [r3]
    sub r3, bp, #8
    str r1, [r3]
    pop {bp ,r3}
    bx r3
l0:
    ldr r1, =324
    sub r3, bp, #8
    str r1, [r3]
    pop {bp ,r3}
    bx r3
rec:
    push {bp}
    mov bp, sp
    sub sp, sp, #0
    sub r3, bp, #16
    ldr r1, [r3]
    ldr r3, =20
    cmp r1, r3
    beq l1
    ldr r4, =99
    sub r5, bp, #8
    str r4, [r5]
    pop {bp ,r5}
    bx r5
l1:
    ldr r4, =2
    push {r4}
    add sp, sp, #4
    bl rec
    sub r4, sp, #4
    str r5, [r4]
    sub sp, sp, #12
    push {r5}
    add sp, sp, #4
    bl rec
    sub r4, sp, #4
    str r5, [r4]
    sub sp, sp, #12
main:
    push {bp}
    mov bp, sp
    sub sp, sp, #0
    ldr r4, =2
    ldr r6, =2
    push {r4 ,r6}
    add sp, sp, #4
    bl function
    sub r4, sp, #4
    str r6, [r4]
    sub sp, sp, #16
    ldr r4, =4100
    str r6, [r4]
    ldr r4, =2
    sub r6, bp, #8
    str r4, [r6]
    pop {bp ,r6}
    bx r6
