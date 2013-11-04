.global main
main:
	 stmfd sp!, {fp, lr}
	 add fp, sp,#4
	 sub sp, sp,#8

	 ldr  r0, =5
	 str r0, [fp,#-8]

	 ldr  r0, =0
	 str r0, [fp,#-12]

	bl loop1_end
loop1_start:
	 ldr  r0, [fp,#-12]
	 ldr  r1, [fp,#-8]
	 adds r3, r0, r1
	 str r3, [fp,#-16]

	 ldr  r0, [fp,#-16]
	 str r0, [fp,#-12]

	 ldr  r0, [fp,#-8]
	 ldr  r1, =1
	 subs r3, r0, r1
	 str r3, [fp,#-20]

	 ldr  r0, [fp,#-20]
	 str r0, [fp,#-8]

loop1_end:
	 ldr  r0, [fp,#-8]
	 ldr  r1, =0
	 cmp r0, r1
        moveq r3,#0
        movne r3,#1
        uxtb r3,r3
	 str r3, [fp,#-24]

	 ldr  r0, [fp,#-24]
	 cmp r0,#1
	 beq loop1_start

	 ldr  r0, [fp,#-12]

	 sub sp, fp, #4
	 ldmfd sp!, {fp, pc}

.global fun
fun:
	 stmfd sp!, {fp, lr}
	 add fp, sp,#4
	 sub sp, sp,#8
	 str r0, [fp,#-8]
	 str r1, [fp,#-12]

	 ldr  r0, [fp,#-8]
	 ldr  r1, [fp,#-12]
	 adds r3, r0, r1
	 str r3, [fp,#-16]

	 ldr  r0, [fp,#-16]

	 sub sp, fp, #4
	 ldmfd sp!, {fp, pc}

