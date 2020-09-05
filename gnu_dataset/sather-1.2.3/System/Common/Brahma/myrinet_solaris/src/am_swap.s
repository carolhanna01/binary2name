	.file	"am_swap.c"
gcc2_compiled.:
.section	".text"
	.align 4
	.global am_swap
	.type	 am_swap,#function
	.proc	04
am_swap:
	!#PROLOGUE# 0
	!#PROLOGUE# 1
	swap [%o1],%o0
	retl
	nop
.LLfe1:
	.size	 am_swap,.LLfe1-am_swap
	.ident	"GCC: (GNU) 2.7.0"
