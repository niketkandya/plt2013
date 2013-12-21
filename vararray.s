.data

.text
.global main
main:
	 stmfd sp!, {fp, lr}
	 add fp, sp,#4
	 sub sp, sp,#4
	 ldr r0, =10

	 bl  fun
	 str r0, [fp,#-8]
	 ldr r0, [fp,#-8]
	 b main_exit
main_exit:
	 sub sp, fp, #4
	 ldmfd sp!, {fp, pc}

.data
.LC0:
	 .asciz   "a[%d] = %d\t"
.LC1:
	 .asciz   "b[%d] = %c\n"

.text
.global fun
fun:
	 stmfd sp!, {fp, lr}
	 add fp, sp,#4
	 sub sp, sp,#100
	 str r0, [fp,#-24]
	 ldr r0, =4
	 ldr r1, [fp,#-24]
	 muls r3, r0, r1
	 str r3, [fp,#-104]
	 ldr r0, [fp,#-104]
	 lsr r1,r0,#2
	 lsl r1,r1,#2
	 cmp r1,r0
	 movne r0,#4
	 moveq r0,#0
	 add r3,r0,r1
	 sub sp, sp,r3
	 mov r0,sp
	 str r0, [fp,#-12]
	 ldr r0, =1
	 ldr r1, [fp,#-24]
	 muls r3, r0, r1
	 strb r3, [fp,#-97]
	 ldrb r0, [fp,#-97]
	 lsr r1,r0,#2
	 lsl r1,r1,#2
	 cmp r1,r0
	 movne r0,#4
	 moveq r0,#0
	 add r3,r0,r1
	 sub sp, sp,r3
	 mov r0,sp
	 str r0, [fp,#-16]
	 ldr r0, =0
	 str r0, [fp,#-8]
	 b loop1_end
loop1_start:
	 ldr r0, =4
	 ldr r1, [fp,#-8]
	 muls r3, r0, r1
	 str r3, [fp,#-28]
	 ldr r0, [fp,#-12]
	 ldr r1, [fp,#-28]
	 adds r3, r0, r1
	 str r3, [fp,#-32]
	 ldr r0, [fp,#-8]
	 ldr r4, [fp,#-32]
	 str r0, [r4,#0]
	 ldrb r0, =97
	 ldr r1, [fp,#-8]
	 adds r3, r0, r1
	 str r3, [fp,#-36]
	 ldr r0, =1
	 ldr r1, [fp,#-8]
	 muls r3, r0, r1
	 str r3, [fp,#-40]
	 ldr r0, [fp,#-16]
	 ldr r1, [fp,#-40]
	 adds r3, r0, r1
	 str r3, [fp,#-44]
	 ldr r0, [fp,#-36]
	 ldr r4, [fp,#-44]
	 strb r0, [r4,#0]
	 ldr r0, [fp,#-8]
	 ldr r1, =1
	 adds r3, r0, r1
	 str r3, [fp,#-48]
	 ldr r0, [fp,#-48]
	 str r0, [fp,#-8]
loop1_end:
	 ldr r0, [fp,#-8]
	 ldr r1, [fp,#-24]
	 cmp r0, r1
	 movlt r3,#1
	 movge r3,#0
	 uxtb r3,r3
	 str r3, [fp,#-52]
	 ldr r0, [fp,#-52]
	 cmp r0,#0
	 bne loop1_start
	 ldr r0, =0
	 str r0, [fp,#-8]
	 ldr r0, =0
	 str r0, [fp,#-20]
	 b loop2_end
loop2_start:
	 ldr r0, =4
	 ldr r1, [fp,#-8]
	 muls r3, r0, r1
	 str r3, [fp,#-56]
	 ldr r0, [fp,#-12]
	 ldr r1, [fp,#-56]
	 adds r3, r0, r1
	 str r3, [fp,#-60]
	 ldr r0, =.LC0
	 ldr r1, [fp,#-8]
	 ldr r4, [fp,#-60]
	 ldr r2, [r4,#0]

	 bl  printf
	 str r0, [fp,#-64]
	 ldr r0, =1
	 ldr r1, [fp,#-8]
	 muls r3, r0, r1
	 str r3, [fp,#-68]
	 ldr r0, [fp,#-16]
	 ldr r1, [fp,#-68]
	 adds r3, r0, r1
	 str r3, [fp,#-72]
	 ldr r0, =.LC1
	 ldr r1, [fp,#-8]
	 ldr r4, [fp,#-72]
	 ldrb r2, [r4,#0]

	 bl  printf
	 str r0, [fp,#-76]
	 ldr r0, =4
	 ldr r1, [fp,#-8]
	 muls r3, r0, r1
	 str r3, [fp,#-80]
	 ldr r0, [fp,#-12]
	 ldr r1, [fp,#-80]
	 adds r3, r0, r1
	 str r3, [fp,#-84]
	 ldr r0, [fp,#-20]
	 ldr r4, [fp,#-84]
	 ldr r1, [r4,#0]
	 adds r3, r0, r1
	 str r3, [fp,#-88]
	 ldr r0, [fp,#-88]
	 str r0, [fp,#-20]
	 ldr r0, [fp,#-8]
	 ldr r1, =1
	 adds r3, r0, r1
	 str r3, [fp,#-92]
	 ldr r0, [fp,#-92]
	 str r0, [fp,#-8]
loop2_end:
	 ldr r0, [fp,#-8]
	 ldr r1, [fp,#-24]
	 cmp r0, r1
	 movlt r3,#1
	 movge r3,#0
	 uxtb r3,r3
	 str r3, [fp,#-96]
	 ldr r0, [fp,#-96]
	 cmp r0,#0
	 bne loop2_start
	 ldr r0, [fp,#-20]
	 b fun_exit
fun_exit:
	 sub sp, fp, #4
	 ldmfd sp!, {fp, pc}

