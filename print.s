.data 
msg: .asciz "Hello World"

.text

.globl print
print: 
push {ip, lr} 
ldr r0, addr_of_msg
bl printf
pop {ip, lr}
bx lr  

addr_of_msg: .word msg 

