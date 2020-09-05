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

#ifndef _SEMA_H_
#define _SEMA_H_

#include "spinlock.h"
#include "thread.h"
/*------------------------------------------------------------------*/
/*                         Semafors                                 */
/*------------------------------------------------------------------*/
/* This is a simple implementation of a blocking semaphore.         */
/* at_sema_wait(s) blocks if the counter value associated with s    */
/* Becomes 0. at_sema_signal increases the counter value by one,    */
/* at_sema_wait decrements it by one.                               */
/* at_sema_trywait is a non-blocking version of at_sema_wait. It    */
/* returns 0 if succeded, 1 otherwise.                              */
/* at_sema_create and at_sema_destory use a pool to keep the        */
/* latency down. They are pretty cheap and it's to advantage to call*/
/* at_sema_destry when done with a at_sema_tphore.                  */ 
/*------------------------------------------------------------------*/

/* 
 * Volatile is necessary to guarantee sequential consistency
 * with respect to updates of semaphore internal representation.
 * Basically, it is used to prevent the compiler from reordering
 * of modifications of internal state and operations on the 
 * guarding spinlock.
 * For instance, without "volatile", gcc 2.7.2 used with -O3 reorders 
 * the increment of "counter" with unlocking of "slck" - a very nasty race
 * condition. 
 */

typedef struct at_sema {
  struct at_sema *next;
  at_thread_t*  sleepers;  /* Threads waiting on the semaphore */
  int      counter;        /* Semaphore counter */
  at_spinlock_t slck;      /* Base spinlock: guards all operations 
			      on Semaphores */
} at_sema_t;

/* External interface */
at_sema_t* at_sema_create(int count);
void at_sema_init(at_sema_t *s, int counter);
void at_sema_destroy(at_sema_t *);
void at_sema_wait(at_sema_t *);
void at_sema_signal(at_sema_t *);
int at_sema_trywait(at_sema_t *);

/* Debugging/internal help */
void at_sema_print(at_sema_t *);
void at_sema_elt_init(void *d);

#endif /* _SEMA_H_ */






