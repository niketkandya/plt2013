.data
.LC0:
	 .asciz   "Player 1: 'O'\nPlayer 2: 'X'\n\n"
.LC1:
	 .asciz   "Valid inputs are 0-9\n\n"
.LC2:
	 .asciz   "Player %d, enter your move: "
.LC3:
	 .asciz   "\n"
.LC4:
	 .asciz   "%d"
.LC5:
	 .asciz   "Winner is Player %d!\n"
.LC6:
	 .asciz   "No one wins!\n"

.text
.global main
main:
	 stmfd sp!, {fp, lr}
	 add fp, sp,#4
	 sub sp, sp,#320
	 ldr r0, =1
	 ldr r1, =0
	 muls r3, r0, r1
	 str r3, [fp,#-48]
	 sub r0, fp,#40
	 ldr r1, [fp,#-48]
	 adds r3, r0, r1
	 str r3, [fp,#-60]
	 ldrb r0, =32
	 ldr r4, [fp,#-60]
	 strb r0, [r4,#0]
	 ldr r0, =1
	 ldr r1, =1
	 muls r3, r0, r1
	 str r3, [fp,#-64]
	 sub r0, fp,#40
	 ldr r1, [fp,#-64]
	 adds r3, r0, r1
	 str r3, [fp,#-76]
	 ldrb r0, =32
	 ldr r4, [fp,#-76]
	 strb r0, [r4,#0]
	 ldr r0, =1
	 ldr r1, =2
	 muls r3, r0, r1
	 str r3, [fp,#-80]
	 sub r0, fp,#40
	 ldr r1, [fp,#-80]
	 adds r3, r0, r1
	 str r3, [fp,#-92]
	 ldrb r0, =32
	 ldr r4, [fp,#-92]
	 strb r0, [r4,#0]
	 ldr r0, =1
	 ldr r1, =3
	 muls r3, r0, r1
	 str r3, [fp,#-96]
	 sub r0, fp,#40
	 ldr r1, [fp,#-96]
	 adds r3, r0, r1
	 str r3, [fp,#-108]
	 ldrb r0, =32
	 ldr r4, [fp,#-108]
	 strb r0, [r4,#0]
	 ldr r0, =1
	 ldr r1, =4
	 muls r3, r0, r1
	 str r3, [fp,#-112]
	 sub r0, fp,#40
	 ldr r1, [fp,#-112]
	 adds r3, r0, r1
	 str r3, [fp,#-124]
	 ldrb r0, =32
	 ldr r4, [fp,#-124]
	 strb r0, [r4,#0]
	 ldr r0, =1
	 ldr r1, =5
	 muls r3, r0, r1
	 str r3, [fp,#-128]
	 sub r0, fp,#40
	 ldr r1, [fp,#-128]
	 adds r3, r0, r1
	 str r3, [fp,#-140]
	 ldrb r0, =32
	 ldr r4, [fp,#-140]
	 strb r0, [r4,#0]
	 ldr r0, =1
	 ldr r1, =6
	 muls r3, r0, r1
	 str r3, [fp,#-144]
	 sub r0, fp,#40
	 ldr r1, [fp,#-144]
	 adds r3, r0, r1
	 str r3, [fp,#-156]
	 ldrb r0, =32
	 ldr r4, [fp,#-156]
	 strb r0, [r4,#0]
	 ldr r0, =1
	 ldr r1, =7
	 muls r3, r0, r1
	 str r3, [fp,#-160]
	 sub r0, fp,#40
	 ldr r1, [fp,#-160]
	 adds r3, r0, r1
	 str r3, [fp,#-172]
	 ldrb r0, =32
	 ldr r4, [fp,#-172]
	 strb r0, [r4,#0]
	 ldr r0, =1
	 ldr r1, =8
	 muls r3, r0, r1
	 str r3, [fp,#-176]
	 sub r0, fp,#40
	 ldr r1, [fp,#-176]
	 adds r3, r0, r1
	 str r3, [fp,#-188]
	 ldrb r0, =32
	 ldr r4, [fp,#-188]
	 strb r0, [r4,#0]
	 ldr r0, =1
	 ldr r1, =9
	 muls r3, r0, r1
	 str r3, [fp,#-192]
	 sub r0, fp,#40
	 ldr r1, [fp,#-192]
	 adds r3, r0, r1
	 str r3, [fp,#-204]
	 ldrb r0, =32
	 ldr r4, [fp,#-204]
	 strb r0, [r4,#0]
	 ldr r0, =.LC0

	 bl  printf
	 str r0, [fp,#-208]
	 ldr r0, =.LC1

	 bl  printf
	 str r0, [fp,#-212]
	 ldr r0, =0
	 str r0, [fp,#-28]
	 ldr r0, =0
	 str r0, [fp,#-12]
	 ldr r0, =1
	 str r0, [fp,#-8]
	 b loop2_end
loop2_start:
	 sub r0, fp,#40

	 bl  printboard
	 str r0, [fp,#-216]
	 ldr r0, =0
	 str r0, [fp,#-20]
	 b loop1_end
loop1_start:
	 ldr r0, =.LC2
	 ldr r1, [fp,#-8]

	 bl  printf
	 str r0, [fp,#-220]
	 ldr r0, =.LC3

	 bl  printf
	 str r0, [fp,#-224]
	 ldr r0, =.LC4
	 sub r1, fp,#16

	 bl  scanf
	 str r0, [fp,#-228]
	 ldr r0, =1
	 str r0, [fp,#-20]
	 ldr r0, [fp,#-16]
	 ldr r1, =0
	 cmp r0, r1
	 movlt r3,#1
	 movge r3,#0
	 uxtb r3,r3
	 str r3, [fp,#-232]
	 ldr r0, [fp,#-232]
	 cmp r0,#0
	 beq end1
	 ldr r0, =0
	 str r0, [fp,#-20]
end1:
	 ldr r0, [fp,#-16]
	 ldr r1, =9
	 cmp r0, r1
	 movgt r3,#1
	 movle r3,#0
	 uxtb r3,r3
	 str r3, [fp,#-236]
	 ldr r0, [fp,#-236]
	 cmp r0,#0
	 beq end2
	 ldr r0, =0
	 str r0, [fp,#-20]
end2:
	 ldr r0, [fp,#-20]
	 ldr r1, =1
	 cmp r0, r1
	 moveq r3,#1
	 movne r3,#0
	 uxtb r3,r3
	 str r3, [fp,#-240]
	 ldr r0, [fp,#-240]
	 cmp r0,#0
	 beq end4
	 ldr r0, =1
	 ldr r1, [fp,#-16]
	 muls r3, r0, r1
	 str r3, [fp,#-244]
	 sub r0, fp,#40
	 ldr r1, [fp,#-244]
	 adds r3, r0, r1
	 str r3, [fp,#-256]
	 ldr r4, [fp,#-256]
	 ldrb r0, [r4,#0]
	 ldrb r1, =32
	 cmp r0, r1
	 moveq r3,#0
	 movne r3,#1
	 uxtb r3,r3
	 strb r3, [fp,#-257]
	 ldrb r0, [fp,#-257]
	 cmp r0,#0
	 beq end3
	 ldr r0, =0
	 str r0, [fp,#-20]
end3:
end4:
loop1_end:
	 ldr r0, [fp,#-20]
	 ldr r1, =0
	 cmp r0, r1
	 moveq r3,#1
	 movne r3,#0
	 uxtb r3,r3
	 str r3, [fp,#-264]
	 ldr r0, [fp,#-264]
	 cmp r0,#0
	 bne loop1_start
	 ldr r0, [fp,#-8]

	 bl  getchar
	 strb r0, [fp,#-265]
	 ldrb r0, [fp,#-265]
	 strb r0, [fp,#-41]
	 ldr r0, =1
	 ldr r1, [fp,#-16]
	 muls r3, r0, r1
	 str r3, [fp,#-272]
	 sub r0, fp,#40
	 ldr r1, [fp,#-272]
	 adds r3, r0, r1
	 str r3, [fp,#-284]
	 ldrb r0, [fp,#-41]
	 ldr r4, [fp,#-284]
	 strb r0, [r4,#0]
	 sub r0, fp,#40

	 bl  checkboard
	 str r0, [fp,#-288]
	 ldr r0, [fp,#-288]
	 ldr r1, =0
	 cmp r0, r1
	 movgt r3,#1
	 movle r3,#0
	 uxtb r3,r3
	 str r3, [fp,#-292]
	 ldr r0, [fp,#-292]
	 cmp r0,#0
	 beq end5
	 sub r0, fp,#40

	 bl  printboard
	 str r0, [fp,#-296]
	 ldr r0, =.LC5
	 ldr r1, [fp,#-8]

	 bl  printf
	 str r0, [fp,#-300]
	 ldr r0, [fp,#-8]
	 str r0, [fp,#-12]
end5:
	 ldr r0, [fp,#-8]
	 ldr r1, =1
	 cmp r0, r1
	 moveq r3,#1
	 movne r3,#0
	 uxtb r3,r3
	 str r3, [fp,#-304]
	 ldr r0, [fp,#-304]
	 cmp r0,#0
	 beq else6
	 ldr r0, =2
	 str r0, [fp,#-8]
	 b end6
else6:
	 ldr r0, =1
	 str r0, [fp,#-8]
end6:
	 ldr r0, [fp,#-28]
	 ldr r1, =1
	 adds r3, r0, r1
	 str r3, [fp,#-308]
	 ldr r0, [fp,#-308]
	 str r0, [fp,#-28]
	 ldr r0, [fp,#-28]
	 ldr r1, =9
	 cmp r0, r1
	 movge r3,#1
	 movlt r3,#0
	 uxtb r3,r3
	 str r3, [fp,#-312]
	 ldr r0, [fp,#-312]
	 cmp r0,#0
	 beq end8
	 ldr r0, [fp,#-12]
	 ldr r1, =0
	 cmp r0, r1
	 moveq r3,#1
	 movne r3,#0
	 uxtb r3,r3
	 str r3, [fp,#-316]
	 ldr r0, [fp,#-316]
	 cmp r0,#0
	 beq end7
	 ldr r0, =.LC6

	 bl  printf
	 str r0, [fp,#-320]
	 ldr r0, =-1
	 str r0, [fp,#-12]
end7:
end8:
loop2_end:
	 ldr r0, [fp,#-12]
	 ldr r1, =0
	 cmp r0, r1
	 moveq r3,#1
	 movne r3,#0
	 uxtb r3,r3
	 str r3, [fp,#-324]
	 ldr r0, [fp,#-324]
	 cmp r0,#0
	 bne loop2_start
	 ldr r0, =0
	 b main_exit
main_exit:
	 sub sp, fp, #4
	 ldmfd sp!, {fp, pc}

.data

.text
.global getchar
getchar:
	 stmfd sp!, {fp, lr}
	 add fp, sp,#4
	 sub sp, sp,#8
	 str r0, [fp,#-8]
	 ldr r0, [fp,#-8]
	 ldr r1, =1
	 cmp r0, r1
	 moveq r3,#1
	 movne r3,#0
	 uxtb r3,r3
	 str r3, [fp,#-12]
	 ldr r0, [fp,#-12]
	 cmp r0,#0
	 beq end9
	 ldrb r0, =79
	 b getchar_exit
end9:
	 ldrb r0, =88
	 b getchar_exit
getchar_exit:
	 sub sp, fp, #4
	 ldmfd sp!, {fp, pc}

.data

.text
.global checkboard
checkboard:
	 stmfd sp!, {fp, lr}
	 add fp, sp,#4
	 sub sp, sp,#144
	 str r0, [fp,#-16]
	 ldr r0, =0
	 str r0, [fp,#-8]
	 ldr r0, =0
	 str r0, [fp,#-12]
	 b loop3_end
loop3_start:
	 ldr r0, =3
	 ldr r1, [fp,#-12]
	 muls r3, r0, r1
	 str r3, [fp,#-20]
	 ldr r0, [fp,#-16]
	 ldr r1, [fp,#-20]

	 bl  checkrow
	 str r0, [fp,#-24]
	 ldr r0, [fp,#-8]
	 ldr r1, [fp,#-24]
	 adds r3, r0, r1
	 str r3, [fp,#-28]
	 ldr r0, [fp,#-16]
	 ldr r1, [fp,#-12]

	 bl  checkcol
	 str r0, [fp,#-32]
	 ldr r0, [fp,#-28]
	 ldr r1, [fp,#-32]
	 adds r3, r0, r1
	 str r3, [fp,#-36]
	 ldr r0, [fp,#-36]
	 str r0, [fp,#-8]
	 ldr r0, [fp,#-12]
	 ldr r1, =1
	 adds r3, r0, r1
	 str r3, [fp,#-40]
	 ldr r0, [fp,#-40]
	 str r0, [fp,#-12]
loop3_end:
	 ldr r0, [fp,#-12]
	 ldr r1, =3
	 cmp r0, r1
	 movlt r3,#1
	 movge r3,#0
	 uxtb r3,r3
	 str r3, [fp,#-44]
	 ldr r0, [fp,#-44]
	 cmp r0,#0
	 bne loop3_start
	 ldr r0, =1
	 ldr r1, =0
	 muls r3, r0, r1
	 str r3, [fp,#-48]
	 ldr r0, [fp,#-16]
	 ldr r1, [fp,#-48]
	 adds r3, r0, r1
	 str r3, [fp,#-52]
	 ldr r4, [fp,#-52]
	 ldrb r0, [r4,#0]
	 ldrb r1, =32
	 cmp r0, r1
	 moveq r3,#0
	 movne r3,#1
	 uxtb r3,r3
	 strb r3, [fp,#-53]
	 ldrb r0, [fp,#-53]
	 cmp r0,#0
	 beq end12
	 ldr r0, =1
	 ldr r1, =0
	 muls r3, r0, r1
	 str r3, [fp,#-60]
	 ldr r0, [fp,#-16]
	 ldr r1, [fp,#-60]
	 adds r3, r0, r1
	 str r3, [fp,#-64]
	 ldr r0, =1
	 ldr r1, =4
	 muls r3, r0, r1
	 str r3, [fp,#-68]
	 ldr r0, [fp,#-16]
	 ldr r1, [fp,#-68]
	 adds r3, r0, r1
	 str r3, [fp,#-72]
	 ldr r4, [fp,#-64]
	 ldrb r0, [r4,#0]
	 ldr r4, [fp,#-72]
	 ldrb r1, [r4,#0]
	 cmp r0, r1
	 moveq r3,#1
	 movne r3,#0
	 uxtb r3,r3
	 strb r3, [fp,#-73]
	 ldrb r0, [fp,#-73]
	 cmp r0,#0
	 beq end11
	 ldr r0, =1
	 ldr r1, =4
	 muls r3, r0, r1
	 str r3, [fp,#-80]
	 ldr r0, [fp,#-16]
	 ldr r1, [fp,#-80]
	 adds r3, r0, r1
	 str r3, [fp,#-84]
	 ldr r0, =1
	 ldr r1, =8
	 muls r3, r0, r1
	 str r3, [fp,#-88]
	 ldr r0, [fp,#-16]
	 ldr r1, [fp,#-88]
	 adds r3, r0, r1
	 str r3, [fp,#-92]
	 ldr r4, [fp,#-84]
	 ldrb r0, [r4,#0]
	 ldr r4, [fp,#-92]
	 ldrb r1, [r4,#0]
	 cmp r0, r1
	 moveq r3,#1
	 movne r3,#0
	 uxtb r3,r3
	 strb r3, [fp,#-93]
	 ldrb r0, [fp,#-93]
	 cmp r0,#0
	 beq end10
	 ldr r0, =1
	 str r0, [fp,#-8]
end10:
end11:
end12:
	 ldr r0, =1
	 ldr r1, =2
	 muls r3, r0, r1
	 str r3, [fp,#-100]
	 ldr r0, [fp,#-16]
	 ldr r1, [fp,#-100]
	 adds r3, r0, r1
	 str r3, [fp,#-104]
	 ldr r4, [fp,#-104]
	 ldrb r0, [r4,#0]
	 ldrb r1, =32
	 cmp r0, r1
	 moveq r3,#0
	 movne r3,#1
	 uxtb r3,r3
	 strb r3, [fp,#-105]
	 ldrb r0, [fp,#-105]
	 cmp r0,#0
	 beq end15
	 ldr r0, =1
	 ldr r1, =2
	 muls r3, r0, r1
	 str r3, [fp,#-112]
	 ldr r0, [fp,#-16]
	 ldr r1, [fp,#-112]
	 adds r3, r0, r1
	 str r3, [fp,#-116]
	 ldr r0, =1
	 ldr r1, =4
	 muls r3, r0, r1
	 str r3, [fp,#-120]
	 ldr r0, [fp,#-16]
	 ldr r1, [fp,#-120]
	 adds r3, r0, r1
	 str r3, [fp,#-124]
	 ldr r4, [fp,#-116]
	 ldrb r0, [r4,#0]
	 ldr r4, [fp,#-124]
	 ldrb r1, [r4,#0]
	 cmp r0, r1
	 moveq r3,#1
	 movne r3,#0
	 uxtb r3,r3
	 strb r3, [fp,#-125]
	 ldrb r0, [fp,#-125]
	 cmp r0,#0
	 beq end14
	 ldr r0, =1
	 ldr r1, =4
	 muls r3, r0, r1
	 str r3, [fp,#-132]
	 ldr r0, [fp,#-16]
	 ldr r1, [fp,#-132]
	 adds r3, r0, r1
	 str r3, [fp,#-136]
	 ldr r0, =1
	 ldr r1, =6
	 muls r3, r0, r1
	 str r3, [fp,#-140]
	 ldr r0, [fp,#-16]
	 ldr r1, [fp,#-140]
	 adds r3, r0, r1
	 str r3, [fp,#-144]
	 ldr r4, [fp,#-136]
	 ldrb r0, [r4,#0]
	 ldr r4, [fp,#-144]
	 ldrb r1, [r4,#0]
	 cmp r0, r1
	 moveq r3,#1
	 movne r3,#0
	 uxtb r3,r3
	 strb r3, [fp,#-145]
	 ldrb r0, [fp,#-145]
	 cmp r0,#0
	 beq end13
	 ldr r0, =1
	 str r0, [fp,#-8]
end13:
end14:
end15:
	 ldr r0, [fp,#-8]
	 b checkboard_exit
checkboard_exit:
	 sub sp, fp, #4
	 ldmfd sp!, {fp, pc}

.data
.LC7:
	 .asciz   "Column win!\n"

.text
.global checkcol
checkcol:
	 stmfd sp!, {fp, lr}
	 add fp, sp,#4
	 sub sp, sp,#80
	 str r0, [fp,#-16]
	 str r1, [fp,#-20]
	 ldr r0, [fp,#-20]
	 ldr r1, =3
	 adds r3, r0, r1
	 str r3, [fp,#-24]
	 ldr r0, [fp,#-24]
	 str r0, [fp,#-8]
	 ldr r0, [fp,#-20]
	 ldr r1, =6
	 adds r3, r0, r1
	 str r3, [fp,#-28]
	 ldr r0, [fp,#-28]
	 str r0, [fp,#-12]
	 ldr r0, =1
	 ldr r1, [fp,#-20]
	 muls r3, r0, r1
	 str r3, [fp,#-32]
	 ldr r0, [fp,#-16]
	 ldr r1, [fp,#-32]
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
	 ldr r4, [fp,#-36]
	 ldrb r0, [r4,#0]
	 ldr r4, [fp,#-44]
	 ldrb r1, [r4,#0]
	 cmp r0, r1
	 moveq r3,#1
	 movne r3,#0
	 uxtb r3,r3
	 strb r3, [fp,#-45]
	 ldrb r0, [fp,#-45]
	 cmp r0,#0
	 beq end18
	 ldr r0, =1
	 ldr r1, [fp,#-8]
	 muls r3, r0, r1
	 str r3, [fp,#-52]
	 ldr r0, [fp,#-16]
	 ldr r1, [fp,#-52]
	 adds r3, r0, r1
	 str r3, [fp,#-56]
	 ldr r0, =1
	 ldr r1, [fp,#-12]
	 muls r3, r0, r1
	 str r3, [fp,#-60]
	 ldr r0, [fp,#-16]
	 ldr r1, [fp,#-60]
	 adds r3, r0, r1
	 str r3, [fp,#-64]
	 ldr r4, [fp,#-56]
	 ldrb r0, [r4,#0]
	 ldr r4, [fp,#-64]
	 ldrb r1, [r4,#0]
	 cmp r0, r1
	 moveq r3,#1
	 movne r3,#0
	 uxtb r3,r3
	 strb r3, [fp,#-65]
	 ldrb r0, [fp,#-65]
	 cmp r0,#0
	 beq end17
	 ldr r0, =1
	 ldr r1, [fp,#-20]
	 muls r3, r0, r1
	 str r3, [fp,#-72]
	 ldr r0, [fp,#-16]
	 ldr r1, [fp,#-72]
	 adds r3, r0, r1
	 str r3, [fp,#-76]
	 ldr r4, [fp,#-76]
	 ldrb r0, [r4,#0]
	 ldrb r1, =32
	 cmp r0, r1
	 moveq r3,#0
	 movne r3,#1
	 uxtb r3,r3
	 strb r3, [fp,#-77]
	 ldrb r0, [fp,#-77]
	 cmp r0,#0
	 beq end16
	 ldr r0, =.LC7

	 bl  printf
	 str r0, [fp,#-84]
	 ldr r0, =1
	 b checkcol_exit
end16:
end17:
end18:
	 ldr r0, =0
	 b checkcol_exit
checkcol_exit:
	 sub sp, fp, #4
	 ldmfd sp!, {fp, pc}

.data
.LC8:
	 .asciz   "Row win!\n"

.text
.global checkrow
checkrow:
	 stmfd sp!, {fp, lr}
	 add fp, sp,#4
	 sub sp, sp,#80
	 str r0, [fp,#-16]
	 str r1, [fp,#-20]
	 ldr r0, [fp,#-20]
	 ldr r1, =1
	 adds r3, r0, r1
	 str r3, [fp,#-24]
	 ldr r0, [fp,#-24]
	 str r0, [fp,#-8]
	 ldr r0, [fp,#-20]
	 ldr r1, =2
	 adds r3, r0, r1
	 str r3, [fp,#-28]
	 ldr r0, [fp,#-28]
	 str r0, [fp,#-12]
	 ldr r0, =1
	 ldr r1, [fp,#-20]
	 muls r3, r0, r1
	 str r3, [fp,#-32]
	 ldr r0, [fp,#-16]
	 ldr r1, [fp,#-32]
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
	 ldr r4, [fp,#-36]
	 ldrb r0, [r4,#0]
	 ldr r4, [fp,#-44]
	 ldrb r1, [r4,#0]
	 cmp r0, r1
	 moveq r3,#1
	 movne r3,#0
	 uxtb r3,r3
	 strb r3, [fp,#-45]
	 ldrb r0, [fp,#-45]
	 cmp r0,#0
	 beq end21
	 ldr r0, =1
	 ldr r1, [fp,#-8]
	 muls r3, r0, r1
	 str r3, [fp,#-52]
	 ldr r0, [fp,#-16]
	 ldr r1, [fp,#-52]
	 adds r3, r0, r1
	 str r3, [fp,#-56]
	 ldr r0, =1
	 ldr r1, [fp,#-12]
	 muls r3, r0, r1
	 str r3, [fp,#-60]
	 ldr r0, [fp,#-16]
	 ldr r1, [fp,#-60]
	 adds r3, r0, r1
	 str r3, [fp,#-64]
	 ldr r4, [fp,#-56]
	 ldrb r0, [r4,#0]
	 ldr r4, [fp,#-64]
	 ldrb r1, [r4,#0]
	 cmp r0, r1
	 moveq r3,#1
	 movne r3,#0
	 uxtb r3,r3
	 strb r3, [fp,#-65]
	 ldrb r0, [fp,#-65]
	 cmp r0,#0
	 beq end20
	 ldr r0, =1
	 ldr r1, [fp,#-20]
	 muls r3, r0, r1
	 str r3, [fp,#-72]
	 ldr r0, [fp,#-16]
	 ldr r1, [fp,#-72]
	 adds r3, r0, r1
	 str r3, [fp,#-76]
	 ldr r4, [fp,#-76]
	 ldrb r0, [r4,#0]
	 ldrb r1, =32
	 cmp r0, r1
	 moveq r3,#0
	 movne r3,#1
	 uxtb r3,r3
	 strb r3, [fp,#-77]
	 ldrb r0, [fp,#-77]
	 cmp r0,#0
	 beq end19
	 ldr r0, =.LC8

	 bl  printf
	 str r0, [fp,#-84]
	 ldr r0, =1
	 b checkrow_exit
end19:
end20:
end21:
	 ldr r0, =0
	 b checkrow_exit
checkrow_exit:
	 sub sp, fp, #4
	 ldmfd sp!, {fp, pc}

.data
.LC9:
	 .asciz   "|%c|%c|%c|\n"
.LC10:
	 .asciz   "-------\n"
.LC11:
	 .asciz   "|%c|%c|%c|\n"
.LC12:
	 .asciz   "-------\n"
.LC13:
	 .asciz   "|%c|%c|%c|\n"

.text
.global printboard
printboard:
	 stmfd sp!, {fp, lr}
	 add fp, sp,#4
	 sub sp, sp,#96
	 str r0, [fp,#-8]
	 ldr r0, =1
	 ldr r1, =2
	 muls r3, r0, r1
	 str r3, [fp,#-12]
	 ldr r0, [fp,#-8]
	 ldr r1, [fp,#-12]
	 adds r3, r0, r1
	 str r3, [fp,#-16]
	 ldr r0, =1
	 ldr r1, =1
	 muls r3, r0, r1
	 str r3, [fp,#-20]
	 ldr r0, [fp,#-8]
	 ldr r1, [fp,#-20]
	 adds r3, r0, r1
	 str r3, [fp,#-24]
	 ldr r0, =1
	 ldr r1, =0
	 muls r3, r0, r1
	 str r3, [fp,#-28]
	 ldr r0, [fp,#-8]
	 ldr r1, [fp,#-28]
	 adds r3, r0, r1
	 str r3, [fp,#-32]
	 ldr r0, =.LC9
	 ldr r4, [fp,#-32]
	 ldrb r1, [r4,#0]
	 ldr r4, [fp,#-24]
	 ldrb r2, [r4,#0]
	 ldr r4, [fp,#-16]
	 ldrb r3, [r4,#0]

	 bl  printf
	 str r0, [fp,#-36]
	 ldr r0, =.LC10

	 bl  printf
	 str r0, [fp,#-40]
	 ldr r0, =1
	 ldr r1, =5
	 muls r3, r0, r1
	 str r3, [fp,#-44]
	 ldr r0, [fp,#-8]
	 ldr r1, [fp,#-44]
	 adds r3, r0, r1
	 str r3, [fp,#-48]
	 ldr r0, =1
	 ldr r1, =4
	 muls r3, r0, r1
	 str r3, [fp,#-52]
	 ldr r0, [fp,#-8]
	 ldr r1, [fp,#-52]
	 adds r3, r0, r1
	 str r3, [fp,#-56]
	 ldr r0, =1
	 ldr r1, =3
	 muls r3, r0, r1
	 str r3, [fp,#-60]
	 ldr r0, [fp,#-8]
	 ldr r1, [fp,#-60]
	 adds r3, r0, r1
	 str r3, [fp,#-64]
	 ldr r0, =.LC11
	 ldr r4, [fp,#-64]
	 ldrb r1, [r4,#0]
	 ldr r4, [fp,#-56]
	 ldrb r2, [r4,#0]
	 ldr r4, [fp,#-48]
	 ldrb r3, [r4,#0]

	 bl  printf
	 str r0, [fp,#-68]
	 ldr r0, =.LC12

	 bl  printf
	 str r0, [fp,#-72]
	 ldr r0, =1
	 ldr r1, =8
	 muls r3, r0, r1
	 str r3, [fp,#-76]
	 ldr r0, [fp,#-8]
	 ldr r1, [fp,#-76]
	 adds r3, r0, r1
	 str r3, [fp,#-80]
	 ldr r0, =1
	 ldr r1, =7
	 muls r3, r0, r1
	 str r3, [fp,#-84]
	 ldr r0, [fp,#-8]
	 ldr r1, [fp,#-84]
	 adds r3, r0, r1
	 str r3, [fp,#-88]
	 ldr r0, =1
	 ldr r1, =6
	 muls r3, r0, r1
	 str r3, [fp,#-92]
	 ldr r0, [fp,#-8]
	 ldr r1, [fp,#-92]
	 adds r3, r0, r1
	 str r3, [fp,#-96]
	 ldr r0, =.LC13
	 ldr r4, [fp,#-96]
	 ldrb r1, [r4,#0]
	 ldr r4, [fp,#-88]
	 ldrb r2, [r4,#0]
	 ldr r4, [fp,#-80]
	 ldrb r3, [r4,#0]

	 bl  printf
	 str r0, [fp,#-100]
	 ldr r0, =0
	 b printboard_exit
printboard_exit:
	 sub sp, fp, #4
	 ldmfd sp!, {fp, pc}

