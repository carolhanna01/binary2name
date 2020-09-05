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

/*
** This is the header for Active Thread implementation of Brahma 
** synchronization and thread manipulation primitives. 
** his file could be used by various Active Threads based Brahma platforms
** (TCP/IP, Myrinet, Meiko, etc.)
*/

#ifndef _AT_SYNC_H_ 
#define _AT_SYNC_H_


#include <unistd.h>
#include <limits.h>
#include <time.h>

#include "at_types.h"
#include "at.h"


/* A unit of delay */
typedef struct {
   unsigned long sec;
   unsigned long nsec;
} BR_delay_t;

/* Handler for delayed function calls. */
typedef void (*BR_delay_handler_t)(void *);

/* Prototypes and definitions... */

/*EXTERNAL thread_key_t BR_here_key; */

/* ID of executing thread */
EXTERNAL BR_thread_t BR_THREAD_ID();

/* Returns 1 if two threads are equal, otherwise 0. */
/* EXTERNAL int BR_SAME_THREAD(BR_thread_t id1, BR_thread_t id2); */
#define BR_THREAD_SAME(x,y) ((x).t==(y).t)

/* Defines an order for all threads */
#define BR_THREAD_LT(x,y) ((x).t<(y).t)

/* Returns a value which can never be an id of an actual thread, to use as
a sentinel value. */
EXTERNAL BR_thread_t BR_INVALID_ID();

#define BR_THREAD_HASH(x) ((unsigned int) (x).t)
/*((unsigned int)((x).t)|((x).loc)) */

#define BR_THREAD_YIELD() at_yield()

/*EXTERNAL thread_key_t BR_key; */

#define BR_SET_THREAD_LOCAL(x) at_setlocal(x)
#define BR_GET_THREAD_LOCAL()  at_getlocal()

EXTERNAL void BR_delay_function(BR_delay_t, BR_delay_handler_t, void *);
EXTERNAL void BR_unlock_handler(BR_cluster_t, BR_word_t);
EXTERNAL void BR_signal_handler(BR_cluster_t, BR_word_t);
EXTERNAL void BR_signal_mem_handler(BR_cluster_t, caddr_t , size_t, caddr_t);


#define BR_LOCK_CREATE() at_mutex_create()
#define BR_LOCK_DELETE(l) at_mutex_destroy(l)
#define BR_SEMA_CREATE(count) at_sema_create(count)
#define BR_SEMA_DELETE(s) at_sema_destroy(s)

#define BR_LOCK(l) at_mutex_lock(l)
#define BR_UNLOCK(l) at_mutex_unlock(l)

#define BR_WAIT(s) at_sema_wait(s)
#define BR_SIGNAL(s) at_sema_signal(s)

#define BR_TRY_LOCK(l) (at_mutex_trylock(l))
#define BR_TRY_WAIT(s) (at_sema_trywait(s))

#define BR_RW_CREATE()       at_rw_create()
#define BR_RW_RD_LOCK(rw)    at_rw_rd_lock(rw)
#define BR_RW_RD_TRYLOCK(rw) at_rw_rd_trylock(rw)
#define BR_RW_WR_LOCK(rw)    at_rw_wr_lock(rw)
#define BR_RW_WR_TRYLOCK(rw) at_rw_wr_trylock(rw)
#define BR_RW_RD_UNLOCK(rw)  at_rw_rd_unlock(rw)
#define BR_RW_WR_UNLOCK(rw)  at_rw_wr_unlock(rw)
#define BR_RW_DELETE(rw)     at_rw_destroy(rw)

/* 
 * Brahma spinlocks are actually hybrid locks. Eventually, the name 
 * will need to change accordingly.
 */
#define BR_SPINLOCK_DEC(s)    AT_HYBRIDLOCK_DEC(s)
#define BR_SPINLOCK_INIT(s)   AT_HYBRIDLOCK_INIT(s)

#define BR_SPINLOCK_LOCK(x)   AT_HYBRIDLOCK_LOCK(x)
#define BR_SPINLOCK_UNLOCK(x) AT_HYBRIDLOCK_UNLOCK(x)
#define BR_SPINLOCK_TRY(x)    AT_HYBRIDLOCK_TRY(x)

#define BR_REAL_SPINLOCK_DEC(s)    AT_SPINLOCK_DEC(s)
#define BR_REAL_SPINLOCK_INIT(s)   AT_SPINLOCK_INIT(s)

#define BR_REAL_SPINLOCK_LOCK(x)   AT_SPINLOCK_LOCK(x)
#define BR_REAL_SPINLOCK_UNLOCK(x) AT_SPINLOCK_UNLOCK(x)
#define BR_REAL_SPINLOCK_TRY(x)    AT_SPINLOCK_TRY(x)

#define BR_PROCESSORS()       at_ncpus()
#endif 





