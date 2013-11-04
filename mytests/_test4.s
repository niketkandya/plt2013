.global main
main:
	 stmfd sp!, {fp, lr}
	 add fp, sp,#4
	 sub sp, sp,#8

	 ldr  r0, =7
	 str r0, [fp,#-8]

	 ldr  r0, =9
	 str r0, [fp,#-12]

	 ldr  r0, [fp,#-8]
	 ldr  r1, [fp,#-12]
	 adds r3, r0, r1
	 str r3, [fp,#-16]

	 ldr  r0, [fp,#-8]
	 ldr  r1, [fp,#-12]
	 subs r3, r0, r1
	 str r3, [fp,#-20]

	 ldr  r0, [fp,#-16]
	 ldr  r1, [fp,#-20]
	 cmp r0, r1
         movgt r3,#1
         movle r3,#0
         uxtb r3,r3
	 str r3, [fp,#-24]

	 ldr  r0, [fp,#-24]
	 cmp r0,#1
	 beq else1

	 ldr  r0, [fp,#-12]

	bl end1
else1:
	 ldr  r0, [fp,#-8]

end1:
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

