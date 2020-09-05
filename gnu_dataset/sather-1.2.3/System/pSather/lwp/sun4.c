/*------------------------->  ANSI C - sourcefile  <-------------------------*/
/* Copyright (C) 1991-93 by Stephen Crane                                    */
/* This file is part of the GNU Sather library. It is free software; you may */
/* redistribute  and/or modify it under the terms of the GNU Library General */
/* Public  License (LGPL)  as published  by the  Free  Software  Foundation; */
/* either version 3 of the license, or (at your option) any later version.   */
/* This  library  is distributed  in the  hope that it will  be  useful, but */
/* WITHOUT ANY WARRANTY without even the implied warranty of MERCHANTABILITY */
/* or FITNESS FOR A PARTICULAR PURPOSE. See Doc/LGPL for more details.       */
/* The license text is also available from:  Free Software Foundation, Inc., */
/* 59 Temple Place, Suite 330, Boston, MA 02111-1307 USA                     */
/*------------>  Please email comments to <bug-sather@gnu.org>  <------------*/

/*
 * sun4.c -- lightweight process initialisation for sun4.
 *
 * author: Stephen Crane, (jsc@doc.ic.ac.uk), Department of Computing,
 * Imperial College of Science, Technology and Medicine, 180 Queen's
 * Gate, London SW7 2BZ, England.
 */

#include "lwp.h"

/*
 * initp -- initialise a new process's context.
 */
void initp (volatile struct pcb *volatile newp, void *sp)
{
	static jmp_buf *cpp;
	extern struct pcb *currp;
	
	newp->context[0] = (int)sp;

	/* preserve cpp for new context */
	cpp = (jmp_buf *) &newp->context;
	if (!savep (currp->context)) {
		/* create new context */		
		/* flush registers */
		asm volatile ("ta	0x03");
		/* %o0 <- newp */
		asm volatile ("ld	[%fp+0x44], %o0");
		/* %o1 <- newp->context[0] */
		asm volatile ("ld	[%o0], %o1");
		/* create min frame on new stack */
		asm volatile ("save	%o1,-96, %sp");
		if (!savep (*cpp))
			restorep (currp->context);
		wrapp ();
	}
}
