.data
.LC0:
	 .asciz   "%d"
.LC1:
	 .asciz   "%s"
.LC2:
	 .asciz   "len: %d, source: %s\n"
.LC3:
	 .asciz   "\n"

.text
.global main
main:
	 stmfd sp!, {fp, lr}
	 add fp, sp,#4
	 sub sp, sp,#1264
	 ldr r0, =0
	 str r0, [fp,#-628]
	 sub r0, fp,#628
	 str r0, [fp,#-632]
	 ldr r0, =.LC0
	 sub r1, fp,#12

	 bl  scanf
	 str r0, [fp,#-636]
	 ldr r0, =.LC1
	 sub r1, fp,#512

	 bl  scanf
	 str r0, [fp,#-640]
	 ldr r0, =.LC2
	 ldr r1, [fp,#-12]
	 sub r2, fp,#512

	 bl  printf
	 str r0, [fp,#-644]
	 ldr r0, =0
	 str r0, [fp,#-620]
	 ldr r0, =1
	 ldr r1, =0
	 muls r3, r0, r1
	 str r3, [fp,#-648]
	 sub r0, fp,#612
	 ldr r1, [fp,#-648]
	 adds r3, r0, r1
	 str r3, [fp,#-748]
	 ldr r0, [fp,#-748]
	 str r0, [fp,#-616]
	 b loop1_end
loop1_start:
	 ldr r0, =1
	 ldr r1, [fp,#-620]
	 muls r3, r0, r1
	 str r3, [fp,#-752]
	 sub r0, fp,#512
	 ldr r1, [fp,#-752]
	 adds r3, r0, r1
	 str r3, [fp,#-1252]
	 ldr r4, [fp,#-1252]
	 ldrb r0, [r4,#0]
	 strb r0, [fp,#-5]
	 sub r0, fp,#612
	 ldrb r1, [fp,#-5]
	 sub r2, fp,#512
	 ldr r3, [fp,#-632]
	 ldr r4, [fp,#-620]

	 bl  do_command
	 str r0, [fp,#-1256]
	 ldr r0, [fp,#-1256]
	 str r0, [fp,#-624]
	 ldr r0, [fp,#-624]
	 ldr r1, =1
	 adds r3, r0, r1
	 str r3, [fp,#-1260]
	 ldr r0, [fp,#-1260]
	 str r0, [fp,#-620]
loop1_end:
	 ldr r0, [fp,#-620]
	 ldr r1, [fp,#-12]
	 cmp r0, r1
	 movlt r3,#1
	 movge r3,#0
	 uxtb r3,r3
	 str r3, [fp,#-1264]
	 ldr r0, [fp,#-1264]
	 cmp r0,#0
	 bne loop1_start
	 ldr r0, =.LC3

	 bl  printf
	 str r0, [fp,#-1268]
	 ldr r0, =0
	 b main_exit
main_exit:
	 sub sp, fp, #4
	 ldmfd sp!, {fp, pc}

.data
.LC4:
	 .asciz   "%c"
.LC5:
	 .asciz   " %c"

.text
.global do_command
do_command:
	 stmfd sp!, {fp, lr}
	 add fp, sp,#4
	 sub sp, sp,#184
	 str r0, [fp,#-28]
	 strb r1, [fp,#-29]
	 str r2, [fp,#-36]
	 str r3, [fp,#-40]
	 str r4, [fp,#-44]
	 ldr r0, =1
	 ldr r4, [fp,#-40]
	 ldr r1, [r4,#0]
	 muls r3, r0, r1
	 str r3, [fp,#-48]
	 ldr r0, [fp,#-28]
	 ldr r1, [fp,#-48]
	 adds r3, r0, r1
	 str r3, [fp,#-52]
	 ldr r0, [fp,#-52]
	 str r0, [fp,#-16]
	 ldrb r0, [fp,#-29]
	 ldrb r1, =62
	 cmp r0, r1
	 moveq r3,#1
	 movne r3,#0
	 uxtb r3,r3
	 strb r3, [fp,#-53]
	 ldrb r0, [fp,#-53]
	 cmp r0,#0
	 beq end1
	 ldr r4, [fp,#-40]
	 ldr r0, [r4,#0]
	 ldr r1, =1
	 adds r3, r0, r1
	 str r3, [fp,#-60]
	 ldr r0, [fp,#-60]
	 ldr r4, [fp,#-40]
	 str r0, [r4,#0]
	 ldr r0, [fp,#-44]
	 b do_command_exit
end1:
	 ldrb r0, [fp,#-29]
	 ldrb r1, =60
	 cmp r0, r1
	 moveq r3,#1
	 movne r3,#0
	 uxtb r3,r3
	 strb r3, [fp,#-61]
	 ldrb r0, [fp,#-61]
	 cmp r0,#0
	 beq end2
	 ldr r4, [fp,#-40]
	 ldr r0, [r4,#0]
	 ldr r1, =1
	 subs r3, r0, r1
	 str r3, [fp,#-68]
	 ldr r0, [fp,#-68]
	 ldr r4, [fp,#-40]
	 str r0, [r4,#0]
	 ldr r0, [fp,#-44]
	 b do_command_exit
end2:
	 ldrb r0, [fp,#-29]
	 ldrb r1, =43
	 cmp r0, r1
	 moveq r3,#1
	 movne r3,#0
	 uxtb r3,r3
	 strb r3, [fp,#-69]
	 ldrb r0, [fp,#-69]
	 cmp r0,#0
	 beq end3
	 ldr r4, [fp,#-16]
	 ldrb r0, [r4,#0]
	 ldr r1, =1
	 adds r3, r0, r1
	 str r3, [fp,#-76]
	 ldr r0, [fp,#-76]
	 ldr r4, [fp,#-16]
	 strb r0, [r4,#0]
	 ldr r0, [fp,#-44]
	 b do_command_exit
end3:
	 ldrb r0, [fp,#-29]
	 ldrb r1, =45
	 cmp r0, r1
	 moveq r3,#1
	 movne r3,#0
	 uxtb r3,r3
	 strb r3, [fp,#-77]
	 ldrb r0, [fp,#-77]
	 cmp r0,#0
	 beq end4
	 ldr r4, [fp,#-16]
	 ldrb r0, [r4,#0]
	 ldr r1, =1
	 subs r3, r0, r1
	 str r3, [fp,#-84]
	 ldr r0, [fp,#-84]
	 ldr r4, [fp,#-16]
	 strb r0, [r4,#0]
	 ldr r0, [fp,#-44]
	 b do_command_exit
end4:
	 ldrb r0, [fp,#-29]
	 ldrb r1, =46
	 cmp r0, r1
	 moveq r3,#1
	 movne r3,#0
	 uxtb r3,r3
	 strb r3, [fp,#-85]
	 ldrb r0, [fp,#-85]
	 cmp r0,#0
	 beq end5
	 ldr r0, =.LC4
	 ldr r4, [fp,#-16]
	 ldrb r1, [r4,#0]

	 bl  printf
	 str r0, [fp,#-92]
	 ldr r0, [fp,#-44]
	 b do_command_exit
end5:
	 ldrb r0, [fp,#-29]
	 ldrb r1, =44
	 cmp r0, r1
	 moveq r3,#1
	 movne r3,#0
	 uxtb r3,r3
	 strb r3, [fp,#-93]
	 ldrb r0, [fp,#-93]
	 cmp r0,#0
	 beq end6
	 ldr r0, =.LC5
	 ldr r1, [fp,#-16]

	 bl  scanf
	 str r0, [fp,#-100]
	 ldr r0, [fp,#-44]
	 b do_command_exit
end6:
	 ldrb r0, [fp,#-29]
	 ldrb r1, =91
	 cmp r0, r1
	 moveq r3,#1
	 movne r3,#0
	 uxtb r3,r3
	 strb r3, [fp,#-101]
	 ldrb r0, [fp,#-101]
	 cmp r0,#0
	 beq end13
	 ldr r0, [fp,#-44]
	 str r0, [fp,#-12]
	 ldr r4, [fp,#-16]
	 ldrb r0, [r4,#0]
	 ldr r1, =0
	 cmp r0, r1
	 moveq r3,#1
	 movne r3,#0
	 uxtb r3,r3
	 str r3, [fp,#-108]
	 ldr r0, [fp,#-108]
	 cmp r0,#0
	 beq else12
	 ldr r0, =0
	 str r0, [fp,#-24]
	 ldr r0, =0
	 str r0, [fp,#-20]
	 b loop2_end
loop2_start:
	 ldr r0, [fp,#-44]
	 ldr r1, =1
	 adds r3, r0, r1
	 str r3, [fp,#-112]
	 ldr r0, [fp,#-112]
	 str r0, [fp,#-44]
	 ldr r0, =1
	 ldr r1, [fp,#-44]
	 muls r3, r0, r1
	 str r3, [fp,#-116]
	 ldr r0, [fp,#-36]
	 ldr r1, [fp,#-116]
	 adds r3, r0, r1
	 str r3, [fp,#-120]
	 ldr r4, [fp,#-120]
	 ldrb r0, [r4,#0]
	 strb r0, [fp,#-5]
	 ldr r0, [fp,#-24]
	 ldr r1, =0
	 cmp r0, r1
	 moveq r3,#1
	 movne r3,#0
	 uxtb r3,r3
	 str r3, [fp,#-124]
	 ldr r0, [fp,#-124]
	 cmp r0,#0
	 beq end8
	 ldrb r0, [fp,#-5]
	 ldrb r1, =93
	 cmp r0, r1
	 moveq r3,#1
	 movne r3,#0
	 uxtb r3,r3
	 strb r3, [fp,#-125]
	 ldrb r0, [fp,#-125]
	 cmp r0,#0
	 beq end7
	 ldr r0, =1
	 str r0, [fp,#-20]
end7:
end8:
	 ldr r0, [fp,#-20]
	 ldr r1, =0
	 cmp r0, r1
	 moveq r3,#1
	 movne r3,#0
	 uxtb r3,r3
	 str r3, [fp,#-132]
	 ldr r0, [fp,#-132]
	 cmp r0,#0
	 beq end11
	 ldrb r0, [fp,#-5]
	 ldrb r1, =93
	 cmp r0, r1
	 moveq r3,#1
	 movne r3,#0
	 uxtb r3,r3
	 strb r3, [fp,#-133]
	 ldrb r0, [fp,#-133]
	 cmp r0,#0
	 beq end9
	 ldr r0, [fp,#-24]
	 ldr r1, =1
	 subs r3, r0, r1
	 str r3, [fp,#-140]
	 ldr r0, [fp,#-140]
	 str r0, [fp,#-24]
end9:
	 ldrb r0, [fp,#-5]
	 ldrb r1, =91
	 cmp r0, r1
	 moveq r3,#1
	 movne r3,#0
	 uxtb r3,r3
	 strb r3, [fp,#-141]
	 ldrb r0, [fp,#-141]
	 cmp r0,#0
	 beq end10
	 ldr r0, [fp,#-24]
	 ldr r1, =1
	 adds r3, r0, r1
	 str r3, [fp,#-148]
	 ldr r0, [fp,#-148]
	 str r0, [fp,#-24]
end10:
end11:
loop2_end:
	 ldr r0, [fp,#-20]
	 ldr r1, =0
	 cmp r0, r1
	 moveq r3,#1
	 movne r3,#0
	 uxtb r3,r3
	 str r3, [fp,#-152]
	 ldr r0, [fp,#-152]
	 cmp r0,#0
	 bne loop2_start
	 b end12
else12:
	 b loop4_end
loop4_start:
	 ldr r0, [fp,#-12]
	 str r0, [fp,#-44]
	 ldr r0, [fp,#-44]
	 ldr r1, =1
	 adds r3, r0, r1
	 str r3, [fp,#-156]
	 ldr r0, [fp,#-156]
	 str r0, [fp,#-44]
	 ldr r0, =1
	 ldr r1, [fp,#-44]
	 muls r3, r0, r1
	 str r3, [fp,#-160]
	 ldr r0, [fp,#-36]
	 ldr r1, [fp,#-160]
	 adds r3, r0, r1
	 str r3, [fp,#-164]
	 ldr r4, [fp,#-164]
	 ldrb r0, [r4,#0]
	 strb r0, [fp,#-5]
	 b loop3_end
loop3_start:
	 ldr r0, [fp,#-28]
	 ldrb r1, [fp,#-5]
	 ldr r2, [fp,#-36]
	 ldr r3, [fp,#-40]
	 ldr r4, [fp,#-44]

	 bl  do_command
	 str r0, [fp,#-168]
	 ldr r0, [fp,#-168]
	 str r0, [fp,#-44]
	 ldr r0, [fp,#-44]
	 ldr r1, =1
	 adds r3, r0, r1
	 str r3, [fp,#-172]
	 ldr r0, [fp,#-172]
	 str r0, [fp,#-44]
	 ldr r0, =1
	 ldr r1, [fp,#-44]
	 muls r3, r0, r1
	 str r3, [fp,#-176]
	 ldr r0, [fp,#-36]
	 ldr r1, [fp,#-176]
	 adds r3, r0, r1
	 str r3, [fp,#-180]
	 ldr r4, [fp,#-180]
	 ldrb r0, [r4,#0]
	 strb r0, [fp,#-5]
loop3_end:
	 ldrb r0, [fp,#-5]
	 ldrb r1, =93
	 cmp r0, r1
	 moveq r3,#0
	 movne r3,#1
	 uxtb r3,r3
	 strb r3, [fp,#-181]
	 ldrb r0, [fp,#-181]
	 cmp r0,#0
	 bne loop3_start
loop4_end:
	 ldr r4, [fp,#-16]
	 ldrb r0, [r4,#0]
	 ldr r1, =0
	 cmp r0, r1
	 moveq r3,#0
	 movne r3,#1
	 uxtb r3,r3
	 str r3, [fp,#-188]
	 ldr r0, [fp,#-188]
	 cmp r0,#0
	 bne loop4_start
end12:
end13:
	 ldr r0, [fp,#-44]
	 b do_command_exit
do_command_exit:
	 sub sp, fp, #4
	 ldmfd sp!, {fp, pc}

