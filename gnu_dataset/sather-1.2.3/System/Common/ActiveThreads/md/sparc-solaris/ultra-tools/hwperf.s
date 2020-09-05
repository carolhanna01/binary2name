# 1 "hwperf_src.s" 




#ident	"%Z%%M% %I%	%E% SMI"



# 1 "./hwperf.h" 1
#ident   "%Z%%M% %I%     %E% SMI"

































































# 10 "hwperf_src.s" 2









	






	








	
	


	.global	get_spf_pcr
	.global	get_spf_pic0
	.global	get_spf_pic0
	.global	get_spf_pic01
	.global	sub_spf_pic01	

	.section ".text"; .align 4; .global get_spf_pcr; .type get_spf_pcr, #function; get_spf_pcr: 
	rd	%asr16, %g1
	retl
	mov	%g1, %o0
	.size get_spf_pcr, (.-get_spf_pcr)


	.section ".text"; .align 4; .global get_spf_pic0; .type get_spf_pic0, #function; get_spf_pic0: 
	rd	%asr17, %g1
	retl
	mov	%g1, %o0
	.size get_spf_pic0, (.-get_spf_pic0)

	.section ".text"; .align 4; .global get_spf_pic1; .type get_spf_pic1, #function; get_spf_pic1: 
	rd	%asr17, %g1
	retl
	srlx	%g1, 32, %o0
	.size get_spf_pic1, (.-get_spf_pic1)


	


 
	.section ".text"; .align 4; .global sub_spf_pic01; .type sub_spf_pic01, #function; sub_spf_pic01: 
	rd	%asr17, %g1
	srlx	%g1, 32, %o0
	retl
	sub	%g1, %o0, %o0
	.size sub_spf_pic01, (.-sub_spf_pic01)	

	


	.section ".text"; .align 4; .global get_spf_pic01; .type get_spf_pic01, #function; get_spf_pic01: 
	rd	%asr17, %g1
	st	%g1, [%o0]
	srlx	%g1, 32, %g1
	retl
	st	%g1, [%o1]
	.size get_spf_pic01, (.-get_spf_pic01)


	



	.global	set_spf_pcr
	.global	set_spf_pic01
	.global	clr_spf_pic01

	.section ".text"; .align 4; .global set_spf_pcr; .type set_spf_pcr, #function; set_spf_pcr: 
	clr	%g1			
	mov	%o0, %g1
	retl
	wr	%g1, %asr16
	.size set_spf_pcr, (.-set_spf_pcr)

	.section ".text"; .align 4; .global set_spf_pic01; .type set_spf_pic01, #function; set_spf_pic01: 
	clr	%g1
	mov	%o1, %g1
	sllx	%g1, 32, %g1
	or	%o0, %g1, %g1
	retl
	wr	%g1, %asr17
	.size set_spf_pic01, (.-set_spf_pic01)

	.section ".text"; .align 4; .global clr_spf_pic01; .type clr_spf_pic01, #function; clr_spf_pic01: 
	clr	%g1
	retl
	wr	%g1, %asr17
	.size clr_spf_pic01, (.-clr_spf_pic01)



