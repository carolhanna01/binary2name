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

#ifndef _MUTEX_H_
#define _MUTEX_H_

#include "spinlock.h"
#include "thread.h"
/*------------------------------------------------------------------*/
/*                         Mutex                                    */
/*------------------------------------------------------------------*/
/* Unlike spinlocks, mutexes are *blocking* locks. Mutexes are      */
/* acquired by calling at_mutex_lock(at_mutex_t *) and released by  */
/* at_mutex_unlock(at_mutex_t *). Before use, mutexes must be       */
/* created with  at_mutex_t *at_mutex_create(). It returns an       */
/* unlocked mutex                                                   */
/*------------------------------------------------------------------*/

/* 
 * Volatile is necessary to guarantee sequential consistency
 * with respect to updates of mutex internal representation.
 * Basically, it is used to prevent the compiler from reordering
 * of modifications of internal state and operations on the 
 * guarding spinlock (gcc with -O3 in fact tries to do it!)
 */

/* For pooling, it is important for next to be the first attribute */
typedef struct at_mutex {
  struct at_mutex *next;
  at_thread_t*  sleepers; /* Threads waiting on the mutex */
  at_thread_t*  owner;    /* NULL, if the mutex is free. Otherwise the owner */
  at_spinlock_t slck;     /* Base spinlock: guards all operations on Mutexes */
} at_mutex_t;

/* External interface */
at_mutex_t *at_mutex_create();
void at_mutex_destroy(at_mutex_t *);
void at_mutex_lock(at_mutex_t *);
void at_mutex_unlock(at_mutex_t *);
int at_mutex_trylock(at_mutex_t *m);
void at_mutex_init(at_mutex_t *m);


/* Debugging/internal help */
void at_mutex_print(at_mutex_t *m);
void at_mutex_elt_init(void *d);

#endif /* _MUTEX_H_ */

