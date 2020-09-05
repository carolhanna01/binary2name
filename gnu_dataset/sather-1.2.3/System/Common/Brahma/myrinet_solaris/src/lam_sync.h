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

#ifndef _LAM_SYNC_H_
#define _LAM_SYNC_H_
#ifndef _REENTRANT
#define _REENTRANT
#endif

#ifdef BR_MYRINET_SOLARIS_IMPL
# define EXTERNAL
#else
# define EXTERNAL extern
#endif

#ifdef __GNUG__ 
    #define CFUNC "C"
#else
    #define CFUNC  
#endif

/* Synchronization stuff for thread safety */
#if defined(BR_MYRINET_SOLARIS)
  #include "solaris_common/solaris_sync.h"
#elif defined(BR_MYRINET_SOLARIS_AT)
  #include "at_common/at_sync.h"
#else
  #error "Undefined platform in lam_sync.h"
#endif

/* Polling lock */
#  define BR_POLL_LOCK BR_REAL_SPINLOCK_LOCK(poll_lck)
#  define BR_POLL_UNLOCK BR_REAL_SPINLOCK_UNLOCK(poll_lck)
#  define BR_POLL_TRY BR_REAL_SPINLOCK_TRY(poll_lck)
#  define BR_POLL_INIT BR_REAL_SPINLOCK_INIT(poll_lck)
#  define BR_POLL_DEC BR_REAL_SPINLOCK_DEC(poll_lck)

/* outgoing buffer lock */
#  define BR_OUT_LOCK BR_REAL_SPINLOCK_LOCK(out_lck)
#  define BR_OUT_UNLOCK BR_REAL_SPINLOCK_UNLOCK(out_lck)
#  define BR_OUT_INIT BR_REAL_SPINLOCK_INIT(out_lck)
#  define BR_OUT_DEC BR_REAL_SPINLOCK_DEC(out_lck)

/* request/dest/credit locks - one for each destination */
#  define BR_CREDITS_DEC(n)   BR_spinlock_t credit_lcks[n]
#  define BR_CREDITS_INIT(n)  {int _i;                                  \
                            for(_i=0;_i<n;_i++){                     \
                              BR_SPINLOCK_INIT(credit_lcks[_i]);     \
	       		    }                                        \
                           }             
#  define BR_CREDITS_LOCK(_i)   BR_REAL_SPINLOCK_LOCK(credit_lcks[_i])
#  define BR_CREDITS_UNLOCK(_i) BR_REAL_SPINLOCK_UNLOCK(credit_lcks[_i])
#  define BR_CREDITS_TRY(_i)    BR_REAL_SPINLOCK_TRY(credit_lcks[_i])

/* A lock that protects lam_sync_all */
#  define BR_SYNC_ALL_LOCK BR_SPINLOCK_LOCK(sync_all_lck)
#  define BR_SYNC_ALL_UNLOCK BR_SPINLOCK_UNLOCK(sync_all_lck)
#  define BR_SYNC_ALL_INIT BR_SPINLOCK_INIT(sync_all_lck)
#  define BR_SYNC_ALL_DEC BR_SPINLOCK_DEC(sync_all_lck)

/* 
 * A lock protecting the thread creation request queue
 * necessary to minimize the total number of created threads 
 */ 

/*#  define THR_QUEUE_LOCK BR_SPINLOCK_LOCK(thr_queue_lck)
#  define BR_THR_QUEUE_UNLOCK BR_SPINLOCK_UNLOCK(thr_queue_lck)
#  define BR_THR_QUEUE_INIT BR_SPINLOCK_INIT(thr_queue_lck)
#  define BR_THR_QUEUE_DEC BR_SPINLOCK_DEC(thr_queue_lck)
*/

#  define BR_THR_QUEUE_LOCK BR_LOCK(thr_queue_lck)
#  define BR_THR_QUEUE_UNLOCK BR_UNLOCK(thr_queue_lck)
#  define BR_THR_QUEUE_INIT thr_queue_lck=BR_LOCK_CREATE()
#  define BR_THR_QUEUE_DEC BR_lock_t thr_queue_lck

void init_locks();

#endif








