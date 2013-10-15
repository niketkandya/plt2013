
.global main
.func main
main:
mov r0, #1
str r0, [sp,#-4]!
mov r0, #2
ldr r1, [sp], #4
add r0, r1, r0
str r0, [sp,#-4]!
mov r0, #3
ldr r1, [sp], #4
add r0, r1, r0
mov r1, r0
push {ip, lr}
ldr r0, addr_of_for
bl printf
pop {ip, pc}
mov r0, #77
bx lr


addr_of_for: .word format
format: .asciz "%d\n"

