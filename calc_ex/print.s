.data 
msg: .asciz "Hello World %d"

.text

.globl print
print: 
push {ip, lr} 
ldr r0, addr_of_msg
mov r1, #5 + 6
bl printf
pop {ip, lr}
bx lr  

addr_of_msg: .word msg 

