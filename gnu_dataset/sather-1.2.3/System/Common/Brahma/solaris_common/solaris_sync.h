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
** This is the header for Solaris implementation of Brahma synchronization
** and thread manipulation primitives. It uses Sparc V7 spinlocks and Solaris
** threads. This file could be used by various Solaris based Brahma platforms
** (TCP/IP, Myrinet, Meiko, etc.)
*/

#ifndef _SOLARIS_SYNC_H_ 
#define _SOLARIS_SYNC_H_

#ifndef _REENTRANT
#define _REENTRANT
#endif

#include <synch.h>
#include <thread.h>
#include <unistd.h>
#include <limits.h>
#include <time.h>

#include "solaris_types.h"

/* Synchronization types */
typedef mutex_t *BR_lock_t;
typedef sema_t *BR_sema_t;
typedef rwlock_t *BR_rw_t;
typedef volatile unsigned char BR_spinlock_t;

/* A unit of delay */
typedef struct {
   unsigned long sec;
   unsigned long nsec;
} BR_delay_t;

/* Handler for delayed function calls. */
typedef void (*BR_delay_handler_t)(void *);

/* Prototypes and definitions... */

EXTERNAL thread_key_t BR_here_key;

/* ID of executing thread */
EXTERNAL BR_thread_t BR_THREAD_ID();

/* Returns 1 if two threads are equal, otherwise 0. */
/* EXTERNAL int BR_THREAD_SAME(BR_thread_t id1, BR_thread_t id2); */
#define BR_THREAD_SAME(x,y) (((x).t==(y).t)) 

/* Defines an order for all threads */
#define BR_THREAD_LT(x,y) ((x).t<(y).t)

/* Returns a value which can never be an id of an actual thread, to use as
a sentinel value. */
EXTERNAL BR_thread_t BR_INVALID_ID();

#define BR_THREAD_HASH(x) ((unsigned int)(x).t)

#define BR_THREAD_YIELD() thr_yield()

EXTERNAL thread_key_t BR_key;

#define BR_SET_THREAD_LOCAL(x) thr_setspecific(BR_key, (void *)(x))
EXTERNAL caddr_t BR_GET_THREAD_LOCAL();

EXTERNAL void BR_delay_function(BR_delay_t, BR_delay_handler_t, void *);
EXTERNAL void BR_unlock_handler(BR_cluster_t, BR_word_t);
EXTERNAL void BR_signal_handler(BR_cluster_t, BR_word_t);
EXTERNAL void BR_signal_mem_handler(BR_cluster_t, caddr_t , size_t, caddr_t);


EXTERNAL BR_lock_t BR_LOCK_CREATE();
#define BR_LOCK_DELETE(l) mutex_destroy(l); free(l)
EXTERNAL BR_sema_t BR_SEMA_CREATE(unsigned int count);
#define BR_SEMA_DELETE(s) sema_destroy(s); free(s)

#define BR_LOCK(l) mutex_lock(l)
#define BR_UNLOCK(l) mutex_unlock(l)

#define BR_WAIT(s) sema_wait(s)
#define BR_SIGNAL(s) sema_post(s)

#define BR_TRY_LOCK(l) (!mutex_trylock(l))
#define BR_TRY_WAIT(s) (!sema_trywait(s))

EXTERNAL BR_rw_t BR_RW_CREATE();
#define BR_RW_RD_LOCK(rw)    rw_rdlock(rw)
#define BR_RW_RD_TRYLOCK(rw) rw_tryrdlock(rw)
#define BR_RW_WR_LOCK(rw)    rw_wrlock(rw)
#define BR_RW_WR_TRYLOCK(rw) rw_trywrlock(rw)
#define BR_RW_RD_UNLOCK(rw)     rw_unlock(rw)
#define BR_RW_WR_UNLOCK(rw)     rw_unlock(rw)
#define BR_RW_DELETE(rw)     rwlock_destroy(rw); free(rw);

#define BR_SPINLOCK_DEC(s) BR_spinlock_t s
#define BR_SPINLOCK_INIT(s) (s)=0

#if defined(_REENTRANT) && defined(__GNUC__) && defined(__sparc__)
# define INLINE inline

/* x must be an address of a word, and y must be a register local */
# define TEST_AND_SET(x,y) asm volatile \
        ("ldstub [%1], %0" : "=r" (y) : "r" (x))

# define BR_SPINLOCK_TRY(x) BR_spinlock_try(&x)              
EXTERNAL int BR_spinlock_try(BR_spinlock_t* x);


# define BR_SPINLOCK_LOCK(x)           	\
{  register int XyZ, XyZd;              \
   TEST_AND_SET(&(x),XyZ);              \
   if (XyZ != 0) {                      \
      XyZd=1;                           \
      while (1) {                       \
         if (XyZd>100000) thr_yield();  \
         else {                         \
            for (XyZ=XyZd; XyZ--;);     \
            XyZd*=2;                    \
         }                              \
         TEST_AND_SET(&(x),XyZ);        \
         if (XyZ == 0) break;           \
      }                                 \
   }                                    \
}
# define BR_SPINLOCK_UNLOCK(x) (x)=0
#else
# error Spinlocks need Sparc, gcc, and _REENTRANT in this implementation.
#endif


/* Real Spinlock that never yields */
#define BR_REAL_SPINLOCK_DEC(x)       BR_SPINLOCK_DEC(x)
#define BR_REAL_SPINLOCK_INIT(x)      BR_SPINLOCK_INIT(x)
#define BR_REAL_SPINLOCK_LOCK(x)      BR_SPINLOCK_LOCK(x)
#define BR_REAL_SPINLOCK_UNLOCK(x)    BR_SPINLOCK_UNLOCK(x)
#define BR_REAL_SPINLOCK_TRY(x)       BR_SPINLOCK_TRY(x)

#endif _SOLARIS_SYNC_H_








