/*------------------------->  ANSI C - headerfile  <-------------------------*/
/* Copyright (C) 199x by International Computer Science Institute            */
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

#ifndef _BARRIER_H_
#define _BARRIER_H_

#include "spinlock.h"
#include "thread.h"
/*------------------------------------------------------------------*/
/*                         Barrier                                  */
/*------------------------------------------------------------------*/
/*------------------------------------------------------------------*/


typedef struct at_barrier {
  struct at_barrier *next;
  at_thread_t*  sleepers;  /* Threads waiting on the barrier */
  int      counter;        /* Barrier current counter */
  int      total_size;     /* Target size, specified at creation */ 
  at_spinlock_t slck;      /* Base spinlock: guards all operations 
			      on barriers */
} at_barrier_t;

/* External interface */
at_barrier_t* at_barrier_create(int count);
void at_barrier_init(at_barrier_t *s, int counter);
void at_barrier_destroy(at_barrier_t *);
void at_barrier_enter(at_barrier_t *);

/* Debugging/internal help */
void at_barrier_print(at_barrier_t *);
void at_barrier_elt_init(void *d);

#endif /* _BARRIER_H_ */






