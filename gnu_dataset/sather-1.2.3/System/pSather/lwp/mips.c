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
 * mips.c -- lightweight process initialisation for mips.
 *
 * author: Stephen Crane, (jsc@doc.ic.ac.uk), Department of Computing,
 * Imperial College of Science, Technology and Medicine, 180 Queen's
 * Gate, London SW7 2BZ, England.
 */

#include "lwp.h"

/*
 * fake setjmp() and longjmp() because builtin longjmp() checks the
 * stack.
 */
static int jmpresult;

int savep (jmp_buf jb)
{
	jmpresult = 0;
	/* save context */
	asm ("sw	$31, 8($4)");	/* return address */
	asm ("sw	$30, 4($4)");	/* frame pointer */
	asm ("sw	$sp, 0($4)");	/* stack pointer */
	asm ("sd	$16, 12($4)");	/* callee save registers */
	asm ("sd	$18, 20($4)");
	asm ("sd	$20, 28($4)");
	asm ("sd	$22, 36($4)");
	return (jmpresult);
}

void restorep (jmp_buf jb)
{
	jmpresult = 1;
	/* restore context */
	asm ("lw	$sp, 0($4)");
	asm ("lw	$30, 4($4)");
	asm ("lw	$31, 8($4)");

	asm ("ld	$16, 12($4)");
	asm ("ld	$18, 20($4)");
	asm ("ld	$20, 28($4)");
	asm ("ld	$22, 36($4)");
}

/*
 * initp -- initialise a new process's context.
 */
void initp (struct pcb *newp, void *sp)
{
	newp->context[1] = (int)sp;
	newp->context[2] = (int)wrapp;
}
