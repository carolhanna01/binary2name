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
 * sem.c -- semaphore manipulation.
 *
 * author: Stephen Crane, (jsc@doc.ic.ac.uk), Department of Computing,
 * Imperial College of Science, Technology and Medicine, 180 Queen's
 * Gate, London SW7 2BZ, England.
 *
 * This version of lwp has been significantly changed by Claudio Fleiner
 * (fleiner@icsi.berkeley.edu) in 1995. New features include locks, 
 * the possibility to interrupt threads, thread local memory, definitions of
 * functions to execute whenever a thread is rescheduled or killed, and more.
 * These features have been tested under sun 4.1.3 and linux 1.1.59.
 */
#include <stdio.h>
#include <malloc.h>
#include <assert.h>

#include "lwp.h"

/*
 * creats -- create a semaphore.
 */
struct sem *creats (int count)
{
	struct sem *new;

	if (!(new = (struct sem *)malloc (sizeof(struct sem))))
		return (0);
	new->count = count;
	new->q.head = new->q.tail = 0;
	return (new);
}

/*
 * signals -- signal a semaphore.  We only yield here if
 * the blocked process has a higher priority than ours'.
 */
void signals (struct sem *s)
{
	extern struct pcb *currp;
	struct pcb *p = hoq (&s->q);

	if (p!=NULL) {
		assert(s->count==0);
		readyp (p);
		if (currp->pri < p->pri)
			yieldp ();
	} else s->count++;
}

/*
 * tests -- if the counter is zero, it returns zero,
 * otherwise it decrements it and returns one
 */
int tests(struct sem *s)
{
	if(s->count>0) {
		s->count--;
		return 1;
	} 
	return 0;
}

/*
 * waits -- wait on a semaphore
 */
int waits (struct sem *s)
{
	extern struct pcb *currp;

	if (s->count == 0) {
		toq (&s->q, currp);
		currp->lock_queue= &s->q;
		currp->lock_intr=0;
		reschedp ();
		currp->lock_queue= NULL;
		if(currp->lock_intr) return -1;
	} else s->count--;
	return 0;
}

/*
 * creatl -- create a lock.
 */
struct lock *creatl ()
{
	struct lock *new;

	if (!(new = (struct lock *)malloc (sizeof(struct lock))))
		return (0);
	new->locked = 0;
	new->q.head = new->q.tail = 0;
	return (new);
}

/*
 * unlockl -- unlock a lock.  We only yield here if
 * the blocked process has a higher priority than ours'.
 */
void unlockl (struct lock *s)
{
	extern struct pcb *currp;
	struct pcb *p = hoq (&s->q);

	if(p!=NULL) {
		readyp (p);
		if (currp->pri < p->pri)
			yieldp ();
	} else s->locked=0;
#ifdef DEADLOCK_DETECTION
	s->cur=NULL;
#endif
}

/*
 * lockl -- block on a lock
 */
int lockl (struct lock *s)
{
	extern struct pcb *currp;

#ifdef DEADLOCK_DETECTION
	if(s->cur==currp) {
		fprintf(stderr,"Potential Deadlock Situation\n");
		fflush(stderr);
	}
#endif
	if (s->locked) {
		toq (&s->q, currp);
		if(currp->intr_lock) currp->lock_queue= &s->q;
		currp->lock_intr=0;
		reschedp ();
		currp->lock_queue= NULL;
		if(currp->lock_intr) return -1;
	} else s->locked=1;
#ifdef DEADLOCK_DETECTION
	s->cur=currp;
#endif
	return 0;
}

/*
 * tryl -- try to block on a lock, returns 1 on success, 0 on failure
 */
int tryl (struct lock *s)
{
	if (s->locked) return 0;
	s->locked=1;
	return 1;
}


/*
 * intrp() interrupts a thread. If the thread is waiting in either 
 * a lock-queue or sem-queue, it is removed and rescheduled. It will
 * return from the lockl/waits with an error
 */
void intrp(p_id_t id)
{
	if(id->ignore_intr) return;
	id->got_intr=1;
	if(id->block_intr) return;
	if(id->lock_queue!=NULL && id->lock_queue->head!=NULL) {
		struct pcb *cur;
		if(id->lock_queue->head==id) {
			id->lock_queue->head=id->next;
			if(id->next==NULL) id->lock_queue->tail=NULL;
		} else {
			cur=id->lock_queue->head;
			while(cur->next!=id) cur=cur->next;
			cur->next=id->next;
			if(id->next==NULL) id->lock_queue->tail=cur;
		}
		id->next=NULL;
		id->lock_queue=NULL;
		id->lock_intr=1;
		readyp (id);
		if (currp->pri < id->pri) yieldp();
	} else if(id==currp) {
		id->got_intr=0;
		if(id->intr_handler!=NULL) (*(id->intr_handler))();
	}
}
