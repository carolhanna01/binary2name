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
 * queue.c -- queue manipulation routines.
 *
 * author: Stephen Crane, (jsc@doc.ic.ac.uk), Department of Computing,
 * Imperial College of Science, Technology and Medicine, 180 Queen's
 * Gate, London SW7 2BZ, England.
 */
#include "lwp.h"

/*
 * hoq -- remove the head of the queue and return it
 */
struct pcb *hoq (struct lpq *q)
{
	struct pcb *head;

	if ((head=q->head) && !(q->head=head->next))
		q->tail = 0;
	return (head);
}

/*
 * toq -- add element to the tail of the queue
 */
void toq (struct lpq *q, struct pcb *p)
{
	if (!q->tail)
		q->head = p;
	else
		q->tail->next = p;
	q->tail = p;
	p->next = 0;
}
