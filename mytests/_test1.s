.global main
main:
	 stmfd sp!, {fp, lr}
	 add fp, sp,#4
	 sub sp, sp,#8

	 ldr  r0, =7
	 str r0, [fp,#-8]

	 ldr  r0, =9
	 str r0, [fp,#-12]

	 ldr  r0, [fp,#-12]
	 ldr  r1, [fp,#-8]
	 adds r3, r0, r1
	 str r3, [fp,#-16]

	 ldr  r0, [fp,#-16]
	 ldr  r1, =5
	 adds r3, r0, r1
	 str r3, [fp,#-20]

	 ldr  r0, [fp,#-20]
	 str r0, [fp,#-8]

	 ldr  r0, [fp,#-8]
	 ldr  r1, [fp,#-12]

	 bl  fun
	 str r0, [fp,#-24]

	 ldr  r0, [fp,#-24]
	 str r0, [fp,#-12]

	 ldr  r0, [fp,#-8]
	 ldr  r1, [fp,#-12]
	 subs r3, r0, r1
	 str r3, [fp,#-28]

	 ldr  r0, [fp,#-8]
	 ldr  r1, [fp,#-12]
	 adds r3, r0, r1
	 str r3, [fp,#-32]

	 ldr  r0, [fp,#-32]
	 ldr  r1, [fp,#-28]

	 bl  fun
	 str r0, [fp,#-36]

	 ldr  r0, [fp,#-36]
	 str r0, [fp,#-12]

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

