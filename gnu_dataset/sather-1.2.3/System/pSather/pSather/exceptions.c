/*------------------------->  ANSI C - sourcefile  <-------------------------*/
/* Copyright (C) 1995/96 by International Computer Science Institute         */
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
 * Exception handling in pSather
 *
 * Version 1.0 (released for 1.0.9) by Claudio Fleiner (fleiner@icsi.berkeley.edu)
 */
#include <stddef.h>
#include <setjmp.h>
#include <assert.h>
#include "pSather.h"
#include "local.h"
#include "locks.h"


FOB p_ex_exception(void)
{
	assert(EXCEPTION_STACK!=NULL);
	return(EXCEPTION_STACK->exception);
}

void p_ex_init_thread(void)
{
	EXCEPTION_STACK=NULL;
}

void *p_ex_top()
{
	return EXCEPTION_STACK;
}

void p_ex_set(void *n)
{
	EXCEPTION_STACK=n;
}

void p_ex_push_first(void *nn)
{
	struct EXCEPT_ELEMENT_COMMON *n=nn;
	n->prev_frame=NULL;
	n->next_frame=NULL;
	EXCEPTION_STACK=n;
}

void p_ex_push(void *nn)
{
	struct EXCEPT_ELEMENT_COMMON *n=nn,*p;
	p=EXCEPTION_STACK;
	n->prev_frame=p;
	if(p->next_frame!=NULL && p->type==EXCEPT_LOOP) {
		int i;
		EXCEPT_LOOP_ELEMENT(1) *l;
		l=(void *)p;
		for(i=0;i<l->slots;i++)
			if(l->slot[i]==NULL) {
				l->slot[i]=n;
				break;
			}
	} else p->next_frame=n;
	EXCEPTION_STACK=n;
}

static void kill_tree(struct EXCEPT_ELEMENT_COMMON *n)
{
	EXCEPT_LOOP_ELEMENT(1) *lp;
	EXCEPT_LOCK_ELEMENT(1) *ln;
	int i;
	if(n!=NULL) { 
		kill_tree(n->next_frame);
		switch(n->type) {
		case EXCEPT_LOCK:
			ln=(void *)n;
			if(ln->branches) LM_RELEASE_LOCKS(ln);
			break;
		case EXCEPT_PROTECT:
			break;
		case EXCEPT_LOOP:
			lp=(void *)n;
			for(i=0;i<lp->slots;i++) 
				kill_tree(lp->slot[i]);
		}
		if(n->heap) free(n);
	}
}

static void kill_exception(struct EXCEPT_ELEMENT_COMMON *n)
{
	EXCEPT_LOOP_ELEMENT(1) *lp;
	int i;
	if(n->prev_frame->next_frame==n) {
		n->prev_frame->next_frame=NULL;
	} else {
		assert(n->prev_frame->type=EXCEPT_LOOP);
		lp=(void *)n->prev_frame;
		for(i=0;i<lp->slots;i++)
			if(lp->slot[i]==n) {
				lp->slot[i]=NULL;
				break;
			}
		assert(i!=lp->slots);
	}
	kill_tree(n);
}

void p_ex_pop(int num)
{
	struct EXCEPT_ELEMENT_COMMON *n=EXCEPTION_STACK;
	while(num-->1) n=n->prev_frame;
	EXCEPTION_STACK=n->prev_frame;
	kill_exception(n);
}


void p_ex_raise(FOB ex)
{
	struct EXCEPT_ELEMENT_COMMON *n;
	struct EXCEPT_PROTECT_ELEMENT *pn;
	n=EXCEPTION_STACK;

	if(n->type!=EXCEPT_PROTECT) {
		while(n->prev_frame->type!=EXCEPT_PROTECT){
		  n=n->prev_frame;
		}
		pn=(void *)n->prev_frame;
		kill_exception(n);
	} else {
		pn=(void *)n;
	}

	if (!(pn->c.prev_frame)){
	  RFATAL("Uncought exception. Aborting program.");
	}

	EXCEPTION_STACK=pn->c.prev_frame;
	pn->c.prev_frame->exception=ex;
	pn->c.prev_frame->next_frame=NULL;
	longjmp(pn->jmp,1);
}

