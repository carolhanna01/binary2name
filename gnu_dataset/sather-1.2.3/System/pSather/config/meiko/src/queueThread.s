	.file	"queueThread.c"
	.align 32
	.section	".text"
	.align 4
	.global Queue_Thread
	.type	 Queue_Thread,#function
	.proc	020
Queue_Thread:
	!#PROLOGUE# 0
	save %sp,-96,%sp
	!#PROLOGUE# 1
	sethi %hi(K_S),%i4
	or %i4,%lo(K_S),%i4 
        mov 0,%i3       /*set K_S to 0*/
	ld [%i0+4],%i5  /*Load queue size*/
	add %i5,-1,%i5  /*-1 for the queue_size mask*/
	mov 0,%o4       /*Clear array_tail*/
	mov 1,%o2       /*Set bc to 1*/
	sethi %hi(done+4),%i2
	st %g0,[%i2+%lo(done+4)]     /*Clear done event*/
	ld [%i0+8],%o3   /*message_ptr is o3*/
	ld [%i0],%o5     /*queue_base_ptr on remote end*/
	mov -1,%o0       /*-1 indicates null node, etc*/
	b .LL29
	mov %o3,%o7 
	/*Seems to make it with 1 instruction to spare?*/
.LL14:
        sethi %hi(done),%i2
        or %i2,%lo(done),%i2
	waitevent [%i2]
	st %o0,[%o3+28] 
	add %o4,1,%o4  
	and %o4,%i5,%o4  
	sll %o4,5,%i2  
	b .LL29
	add %o7,%i2,%o3  
.LL15:
	ew_break:
	cmp %o1, 1024 /*Are the control bits still set?*/
	be,a .LL14  /*DMA_PKT*/
	dma [%o3]
.LL16:
	break
.LL29:
	ld [%o3+28],%o1  /*remote node is in 7th slot now*/
	cmp %o1,1024  /*DMA PKT or -1 */
	bgeu .LL15
.LL17:
	and %o1,3583,%i1
.LL31:  
        ew_part2:
	open %i1
	sendtrans 4120, %g0,%o5
	ew_cb_se_begin:
	sendtrans 47104, %o3, %o5
        close %i2
	ew_cb_se_end:
        cmp %i2,1
	bne .LL27  
	and %o1,512,%i2 
	cmp %i2,0
	be .LL21
 	nop  
	add %i3,1,%i3
	st %i3,[%i4] 
.LL21:
	st %o0,[%o3+28] 
	mov 1,%o2    
	add %o4,1,%o4  
	and %o4,%i5,%o4  
	sll %o4,5,%i2  
	b .LL29
	add %o7,%i2,%o3  
.LL27:
        mov  0,%i2
.LL20: 
	cmp %i2,%o2
	bge,a .LL30
	sll %o2,1,%o2
.LL26:
	break
	add %i2,1,%i2
	cmp %i2,%o2
	bl .LL26
	nop
	sll %o2,1,%o2
.LL30:
	sethi %hi(524287),%i2
	or %i2,%lo(524287),%i2
	cmp %o2,%i2
	bg,a .LL17
	mov %i2,%o2
	b .LL31
	nop
	ret
	restore
.LLfe1:
	.size	 Queue_Thread,.LLfe1-Queue_Thread
                .common done,8,8
	.ident	"GCC: (GNU) 2.5.8"
