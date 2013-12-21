.data
.LC0:
	 .asciz   "Select the choice of operation on link list"
.LC1:
	 .asciz   "\n1.) insert at beginning\n"
.LC2:
	 .asciz   "2.) delete from end\n"
.LC3:
	 .asciz   "3.) display list\n"
.LC4:
	 .asciz   "4.) Exit\n"
.LC5:
	 .asciz   "\n\nenter the choice of operation you want to do "
.LC6:
	 .asciz   "%d"
.LC7:
	 .asciz   "enter the value you want to insert in node "
.LC8:
	 .asciz   "%d"

.text
.global main
main:
	 stmfd sp!, {fp, lr}
	 add fp, sp,#4
	 sub sp, sp,#76
	 ldr r0, =0
	 str r0, [fp,#-20]
	 ldr r0, =.LC0

	 bl  printf
	 str r0, [fp,#-28]
	 ldr r0, =.LC1

	 bl  printf
	 str r0, [fp,#-32]
	 ldr r0, =.LC2

	 bl  printf
	 str r0, [fp,#-36]
	 ldr r0, =.LC3

	 bl  printf
	 str r0, [fp,#-40]
	 ldr r0, =.LC4

	 bl  printf
	 str r0, [fp,#-44]
	 b loop1_end
loop1_start:
	 ldr r0, =.LC5

	 bl  printf
	 str r0, [fp,#-48]
	 ldr r0, =.LC6
	 sub r1, fp,#12

	 bl  scanf
	 str r0, [fp,#-52]
	 ldr r0, [fp,#-12]
	 ldr r1, =1
	 cmp r0, r1
	 moveq r3,#1
	 movne r3,#0
	 uxtb r3,r3
	 str r3, [fp,#-56]
	 ldr r0, [fp,#-56]
	 cmp r0,#0
	 beq else4
	 ldr r0, =.LC7

	 bl  printf
	 str r0, [fp,#-60]
	 ldr r0, =.LC8
	 sub r1, fp,#8

	 bl  scanf
	 str r0, [fp,#-64]
	 ldr r0, [fp,#-8]
	 sub r1, fp,#20
	 sub r2, fp,#24

	 bl  insert_beginning
	 sub r0, fp,#20
	 sub r1, fp,#24

	 bl  display
	 b end4
else4:
	 ldr r0, [fp,#-12]
	 ldr r1, =2
	 cmp r0, r1
	 moveq r3,#1
	 movne r3,#0
	 uxtb r3,r3
	 str r3, [fp,#-68]
	 ldr r0, [fp,#-68]
	 cmp r0,#0
	 beq else3
	 sub r0, fp,#20
	 sub r1, fp,#24

	 bl  delete_from_end
	 str r0, [fp,#-72]
	 sub r0, fp,#20
	 sub r1, fp,#24

	 bl  display
	 b end3
else3:
	 ldr r0, [fp,#-12]
	 ldr r1, =3
	 cmp r0, r1
	 moveq r3,#1
	 movne r3,#0
	 uxtb r3,r3
	 str r3, [fp,#-76]
	 ldr r0, [fp,#-76]
	 cmp r0,#0
	 beq else2
	 sub r0, fp,#20
	 sub r1, fp,#24

	 bl  display
	 b end2
else2:
	 ldr r0, [fp,#-12]
	 ldr r1, =4
	 cmp r0, r1
	 moveq r3,#1
	 movne r3,#0
	 uxtb r3,r3
	 str r3, [fp,#-80]
	 ldr r0, [fp,#-80]
	 cmp r0,#0
	 beq else1
	 ldr r0, =0
	 b main_exit
	 b end1
else1:
	 ldr r0, =0
	 b main_exit
end1:
end2:
end3:
end4:
loop1_end:
	 ldr r0, =1
	 cmp r0,#0
	 bne loop1_start
main_exit:
	 sub sp, fp, #4
	 ldmfd sp!, {fp, pc}

.data
.LC9:
	 .asciz   "List is Empty!"
.LC10:
	 .asciz   "-> %d "

.text
.global display
display:
	 stmfd sp!, {fp, lr}
	 add fp, sp,#4
	 sub sp, sp,#44
	 str r0, [fp,#-12]
	 str r1, [fp,#-16]
	 ldr r4, [fp,#-12]
	 ldr r0, [r4,#0]
	 str r0, [fp,#-8]
	 ldr r0, =12
	 ldr r1, =0
	 muls r3, r0, r1
	 str r3, [fp,#-24]
	 ldr r0, [fp,#-8]
	 ldr r1, [fp,#-24]
	 cmp r0, r1
	 moveq r3,#1
	 movne r3,#0
	 uxtb r3,r3
	 str r3, [fp,#-20]
	 ldr r0, [fp,#-20]
	 cmp r0,#0
	 beq end5
	 ldr r0, =.LC9

	 bl  printf
	 str r0, [fp,#-28]
end5:
	 b loop2_end
loop2_start:
	 ldr r0, [fp,#-8]
	 ldr r1, =4
	 adds r3, r0, r1
	 str r3, [fp,#-32]
	 ldr r0, =.LC10
	 ldr r4, [fp,#-32]
	 ldr r1, [r4,#0]

	 bl  printf
	 str r0, [fp,#-36]
	 ldr r0, [fp,#-8]
	 ldr r1, =8
	 adds r3, r0, r1
	 str r3, [fp,#-40]
	 ldr r4, [fp,#-40]
	 ldr r0, [r4,#0]
	 str r0, [fp,#-8]
loop2_end:
	 ldr r0, =12
	 ldr r1, =0
	 muls r3, r0, r1
	 str r3, [fp,#-48]
	 ldr r0, [fp,#-8]
	 ldr r1, [fp,#-48]
	 cmp r0, r1
	 moveq r3,#0
	 movne r3,#1
	 uxtb r3,r3
	 str r3, [fp,#-44]
	 ldr r0, [fp,#-44]
	 cmp r0,#0
	 bne loop2_start
display_exit:
	 sub sp, fp, #4
	 ldmfd sp!, {fp, pc}

.data
.LC11:
	 .asciz   "Cannot Delete: "
.LC12:
	 .asciz   "\nData deleted from list is %d \n"
.LC13:
	 .asciz   "\nData deleted from list is %d \n"

.text
.global delete_from_end
delete_from_end:
	 stmfd sp!, {fp, lr}
	 add fp, sp,#4
	 sub sp, sp,#68
	 str r0, [fp,#-12]
	 str r1, [fp,#-16]
	 ldr r4, [fp,#-12]
	 ldr r0, [r4,#0]
	 str r0, [fp,#-8]
	 ldr r0, =12
	 ldr r1, =0
	 muls r3, r0, r1
	 str r3, [fp,#-24]
	 ldr r0, [fp,#-8]
	 ldr r1, [fp,#-24]
	 cmp r0, r1
	 moveq r3,#1
	 movne r3,#0
	 uxtb r3,r3
	 str r3, [fp,#-20]
	 ldr r0, [fp,#-20]
	 cmp r0,#0
	 beq end6
	 ldr r0, =.LC11

	 bl  printf
	 str r0, [fp,#-28]
	 ldr r0, =0
	 b delete_from_end_exit
end6:
	 ldr r4, [fp,#-16]
	 ldr r0, [r4,#0]
	 str r0, [fp,#-8]
	 ldr r0, [fp,#-8]
	 ldr r1, =0
	 adds r3, r0, r1
	 str r3, [fp,#-32]
	 ldr r0, =12
	 ldr r1, =0
	 muls r3, r0, r1
	 str r3, [fp,#-40]
	 ldr r4, [fp,#-32]
	 ldr r0, [r4,#0]
	 ldr r1, [fp,#-40]
	 cmp r0, r1
	 moveq r3,#1
	 movne r3,#0
	 uxtb r3,r3
	 str r3, [fp,#-36]
	 ldr r0, [fp,#-36]
	 cmp r0,#0
	 beq end7
	 ldr r4, [fp,#-16]
	 ldr r0, [r4,#0]
	 ldr r1, =4
	 adds r3, r0, r1
	 str r3, [fp,#-44]
	 ldr r0, =.LC12
	 ldr r4, [fp,#-44]
	 ldr r1, [r4,#0]

	 bl  printf
	 str r0, [fp,#-48]
	 ldr r0, [fp,#-8]

	 bl  free
	 str r0, [fp,#-52]
	 ldr r0, =0
	 ldr r4, [fp,#-12]
	 str r0, [r4,#0]
	 ldr r0, =0
	 ldr r4, [fp,#-16]
	 str r0, [r4,#0]
	 ldr r0, =0
	 b delete_from_end_exit
end7:
	 ldr r4, [fp,#-16]
	 ldr r0, [r4,#0]
	 ldr r1, =4
	 adds r3, r0, r1
	 str r3, [fp,#-56]
	 ldr r0, =.LC13
	 ldr r4, [fp,#-56]
	 ldr r1, [r4,#0]

	 bl  printf
	 str r0, [fp,#-60]
	 ldr r0, [fp,#-8]
	 ldr r1, =0
	 adds r3, r0, r1
	 str r3, [fp,#-64]
	 ldr r4, [fp,#-64]
	 ldr r0, [r4,#0]
	 ldr r4, [fp,#-16]
	 str r0, [r4,#0]
	 ldr r4, [fp,#-16]
	 ldr r0, [r4,#0]
	 ldr r1, =8
	 adds r3, r0, r1
	 str r3, [fp,#-68]
	 ldr r0, =0
	 ldr r4, [fp,#-68]
	 str r0, [r4,#0]
	 ldr r0, [fp,#-8]

	 bl  free
	 str r0, [fp,#-72]
	 ldr r0, =0
	 b delete_from_end_exit
delete_from_end_exit:
	 sub sp, fp, #4
	 ldmfd sp!, {fp, pc}

.data
.LC14:
	 .asciz   "Adding to Empty List\n"
.LC15:
	 .asciz   "Adding to List\n"

.text
.global insert_beginning
insert_beginning:
	 stmfd sp!, {fp, lr}
	 add fp, sp,#4
	 sub sp, sp,#68
	 str r0, [fp,#-20]
	 str r1, [fp,#-24]
	 str r2, [fp,#-28]
	 ldr r0, =24

	 bl  malloc
	 str r0, [fp,#-32]
	 ldr r0, [fp,#-32]
	 str r0, [fp,#-8]
	 ldr r0, [fp,#-8]
	 ldr r1, =4
	 adds r3, r0, r1
	 str r3, [fp,#-36]
	 ldr r0, [fp,#-20]
	 ldr r4, [fp,#-36]
	 str r0, [r4,#0]
	 ldr r0, =12
	 ldr r1, =0
	 muls r3, r0, r1
	 str r3, [fp,#-44]
	 ldr r4, [fp,#-24]
	 ldr r0, [r4,#0]
	 ldr r1, [fp,#-44]
	 cmp r0, r1
	 moveq r3,#1
	 movne r3,#0
	 uxtb r3,r3
	 str r3, [fp,#-40]
	 ldr r0, [fp,#-40]
	 cmp r0,#0
	 beq else8
	 ldr r0, =.LC14

	 bl  printf
	 str r0, [fp,#-48]
	 ldr r0, [fp,#-8]
	 ldr r1, =0
	 adds r3, r0, r1
	 str r3, [fp,#-52]
	 ldr r0, =0
	 ldr r4, [fp,#-52]
	 str r0, [r4,#0]
	 ldr r0, [fp,#-8]
	 ldr r1, =8
	 adds r3, r0, r1
	 str r3, [fp,#-56]
	 ldr r0, =0
	 ldr r4, [fp,#-56]
	 str r0, [r4,#0]
	 ldr r0, [fp,#-8]
	 ldr r4, [fp,#-24]
	 str r0, [r4,#0]
	 ldr r4, [fp,#-24]
	 ldr r0, [r4,#0]
	 ldr r4, [fp,#-28]
	 str r0, [r4,#0]
	 b end8
else8:
	 ldr r0, =.LC15

	 bl  printf
	 str r0, [fp,#-60]
	 ldr r0, [fp,#-8]
	 str r0, [fp,#-12]
	 ldr r0, [fp,#-12]
	 ldr r1, =0
	 adds r3, r0, r1
	 str r3, [fp,#-64]
	 ldr r0, =0
	 ldr r4, [fp,#-64]
	 str r0, [r4,#0]
	 ldr r0, [fp,#-12]
	 ldr r1, =8
	 adds r3, r0, r1
	 str r3, [fp,#-68]
	 ldr r4, [fp,#-24]
	 ldr r0, [r4,#0]
	 ldr r4, [fp,#-68]
	 str r0, [r4,#0]
	 ldr r4, [fp,#-24]
	 ldr r0, [r4,#0]
	 ldr r1, =0
	 adds r3, r0, r1
	 str r3, [fp,#-72]
	 ldr r0, [fp,#-12]
	 ldr r4, [fp,#-72]
	 str r0, [r4,#0]
	 ldr r0, [fp,#-12]
	 ldr r4, [fp,#-24]
	 str r0, [r4,#0]
end8:
insert_beginning_exit:
	 sub sp, fp, #4
	 ldmfd sp!, {fp, pc}

