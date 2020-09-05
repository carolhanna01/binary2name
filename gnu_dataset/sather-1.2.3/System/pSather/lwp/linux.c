/*------------------------->  ANSI C - sourcefile  <-------------------------*/
/* Copyright (C) 1992/93 by Stephen Crane                                    */
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
 * linux.c -- lightweight process initialisation for linux
 *
 * author: Stephen Crane, (jsc@doc.ic.ac.uk), Department of Computing,
 * Imperial College of Science, Technology and Medicine, 180 Queen's
 * Gate, London SW7 2BZ, England.
 *
 * Modified to run on 386/486 machines running linux by:
 * Mark Little (M.C.Little@newcastle.ac.uk), Department of Computing
 * Science, The University, Newcastle upon Tyne, NE1 7RU, England.
 * 7/7/93
 */
#include <sys/param.h>

#include "lwp.h"

/*
 * initp -- initialise a new process's context.
 */
void initp (struct pcb *newp, void *sp)
{
	newp->context->__sp = sp;
	newp->context->__bp = sp;
	newp->context->__pc = (void *)wrapp;
}

/*
 * getdtablesize -- linux hasn't got this
 */
int getdtablesize (void)
{
	return (NOFILE);
}
