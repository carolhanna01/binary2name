	.globl _big_context_resume_user
	.globl _big_context_resume_system
_big_context_resume_user:
_big_context_resume_system:
	punt
	mov %rca,%pc
	add %sp,4,%sp
