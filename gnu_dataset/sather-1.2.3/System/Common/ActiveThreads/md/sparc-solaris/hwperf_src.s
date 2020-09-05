/*
 * Copyright (c) 1995 by Sun Microsystems, Inc.
 */

#pragma	ident	"%Z%%M% %I%	%E% SMI"


/*#include	"/sys/asm_linkage.h"*/
#include	"hwperf.h"



#define	MCOUNT(x) 

/*
 * ENTRY provides the standard procedure entry code and an easy way to
 * insert the calls to mcount for profiling. ENTRY_NP is identical, but
 * never calls mcount.
 */	
#define	ENTRY(x) \
	.section	".text"; \
	.align	4; \
	.global	x; \
	.type	x, #function; \
x:	MCOUNT(x)
	
/*
 * SET_SIZE trails a function and set the size for the ELF symbol table.
 */
#define	SET_SIZE(x) \
	.size	x, (.-x)



	
	/*
	 * Get functions
	 */
	.global	get_spf_pcr
	.global	get_spf_pic0
	.global	get_spf_pic0
	.global	get_spf_pic01
	.global	sub_spf_pic01	

	ENTRY(get_spf_pcr)
	rd	SPF_PCR_ASR, %g1
	retl
	mov	%g1, %o0
	SET_SIZE(get_spf_pcr)


	ENTRY(get_spf_pic0)
	rd	SPF_PIC_ASR, %g1
	retl
	mov	%g1, %o0
	SET_SIZE(get_spf_pic0)

	ENTRY(get_spf_pic1)
	rd	SPF_PIC_ASR, %g1
	retl
	srlx	%g1, 32, %o0
	SET_SIZE(get_spf_pic1)


	/*
	 * Return the difference between pic1 and pic0 
	 * (useful, for instance, to ge the number of E-cache misses
	 */ 
	ENTRY(sub_spf_pic01)
	rd	SPF_PIC_ASR, %g1
	srlx	%g1, 32, %o0
	retl
	sub	%g1, %o0, %o0
	SET_SIZE(sub_spf_pic01)	

	/*
	 * Store contents of pic0 and pic1 in location pointed by %o0 and %o1.
	 */
	ENTRY(get_spf_pic01)
	rd	SPF_PIC_ASR, %g1
	st	%g1, [%o0]
	srlx	%g1, 32, %g1
	retl
	st	%g1, [%o1]
	SET_SIZE(get_spf_pic01)


	/*
	 * Some set/clr fucntions
	 */

	.global	set_spf_pcr
	.global	set_spf_pic01
	.global	clr_spf_pic01

	ENTRY(set_spf_pcr)
	clr	%g1			/* clear the upper 32 bits */
	mov	%o0, %g1
	retl
	wr	%g1, SPF_PCR_ASR
	SET_SIZE(set_spf_pcr)

	ENTRY(set_spf_pic01)
	clr	%g1
	mov	%o1, %g1
	sllx	%g1, 32, %g1
	or	%o0, %g1, %g1
	retl
	wr	%g1, SPF_PIC_ASR
	SET_SIZE(set_spf_pic01)

	ENTRY(clr_spf_pic01)
	clr	%g1
	retl
	wr	%g1, SPF_PIC_ASR
	SET_SIZE(clr_spf_pic01)



