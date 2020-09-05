/*------------------------->  ANSI C - headerfile  <-------------------------*/
/* Copyright (C) 199x by International Computer Science Institute            */
/* Copyright (C) 1999 by Free Software Foundation, Inc.                      */
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
** This is the header for Brahma - LWP-Version
*/

#include <lwp.h>
#include <sys/types.h>
#include <limits.h>

#define BR_THREAD_STACK_SIZE 0x10000

/* Basic types. */
typedef unsigned long BR_word_t;
typedef unsigned long long BR_doubleword_t;

/* Integral cluster id type. */
typedef unsigned long BR_cluster_t;

/* Standard Active Message handler types */
typedef void (*BR_handler_0_t)(BR_cluster_t);
typedef void (*BR_handler_1_t)(BR_cluster_t,BR_word_t);
typedef void (*BR_handler_2_t)(BR_cluster_t,BR_word_t,BR_word_t);
typedef void (*BR_handler_3_t)(BR_cluster_t,BR_word_t,BR_word_t,BR_word_t);
typedef void (*BR_handler_4_t)(BR_cluster_t,BR_word_t,BR_word_t,BR_word_t,BR_word_t);
typedef void (*BR_handler_5_t)(BR_cluster_t,BR_word_t,BR_word_t,BR_word_t,BR_word_t,BR_word_t);

/* Bulk xfer */
typedef void (*BR_handler_mem_t)(BR_cluster_t,caddr_t,size_t,BR_word_t);
typedef void (*BR_signal_handler_t)(void);
typedef void (*BR_handler_t)(BR_cluster_t,...);

/* ID of a thread */
typedef struct pcb *BR_thread_t;

/* Synchronization types */
typedef struct lock *BR_lock_t;
typedef struct sem *BR_sema_t;
typedef unsigned char BR_spinlock_t;

/* A unit of delay */
typedef long BR_delay_t;

/* Handler for delayed function calls. */
typedef void (*BR_delay_handler_t)(void *);


/* Prototypes and definitions... */

#define BR_ASCII_PLATFORM "linux_lwp"

extern void BR_init(int clusters, int argc, char *argv[]);
extern void BR_exit();
#define BR_start()

extern unsigned int BR_clusters;
#define BR_CLUSTERS() BR_clusters

/*
** Get the local memory associated with this cluster.  For truly
** distributed systems, this may just be a pointer to a static
** region at the same address everywhere.  For systems which emulate
** distribution in a single address space, this will be a zeroed
** region allocated in BR_init(...).
*/

/* Size of the cluster local memory in bytes, at least 1KB. */
#define BR_CLUSTER_LOCAL_SIZE() 1024
extern caddr_t *BR_cluster_local_arr;

#define BR_CLUSTER_LOCAL() (BR_cluster_local_arr[BR_HERE()])


#define BR_PROCESSORS() 1

/*#define BR_here ((BR_cluster_t)currp->argc)*/
#define BR_here (currp->argc)
#define BR_HERE() BR_here

#define BR_SET_THREAD_LOCAL(x) (set_thread_mem(x))
#define BR_GET_THREAD_LOCAL() (get_thread_mem())

#define BR_MAX_XFER() INT_MAX

/* In this implementation, there is no difference between messages and forks */

#define BR_REQUEST_0 BR_FORK_0
#define BR_REQUEST_1 BR_FORK_1
#define BR_REQUEST_2 BR_FORK_2
#define BR_REQUEST_3 BR_FORK_3
#define BR_REQUEST_4 BR_FORK_4
#define BR_REQUEST_5 BR_FORK_5

#define BR_REPLY_0 BR_FORK_0
#define BR_REPLY_1 BR_FORK_1
#define BR_REPLY_2 BR_FORK_2
#define BR_REPLY_3 BR_FORK_3
#define BR_REPLY_4 BR_FORK_4
#define BR_REPLY_5 BR_FORK_5

extern void BR_STORE(BR_cluster_t, caddr_t,caddr_t,size_t,BR_handler_mem_t,BR_word_t);

extern void BR_ASYNC_STORE(BR_cluster_t, caddr_t, caddr_t, size_t,
		  BR_handler_mem_t, BR_word_t, BR_handler_mem_t, BR_word_t);

extern void BR_GET(BR_cluster_t, caddr_t, caddr_t, size_t,BR_handler_mem_t,BR_word_t);

extern void BR_dummy();

#define BR_freeze() prisetp(MAXTPRI-1)
#define BR_thaw()   prisetp(1)

extern void BR_FORK_0(BR_cluster_t, BR_handler_0_t);
extern void BR_FORK_1(BR_cluster_t, BR_handler_1_t, BR_word_t);
extern void BR_FORK_2(BR_cluster_t, BR_handler_2_t, BR_word_t, BR_word_t);
extern void BR_FORK_3(BR_cluster_t, BR_handler_3_t, BR_word_t, BR_word_t, BR_word_t);
extern void BR_FORK_4(BR_cluster_t, BR_handler_4_t, BR_word_t, BR_word_t, BR_word_t, BR_word_t);
extern void BR_FORK_5(BR_cluster_t, BR_handler_5_t, BR_word_t, BR_word_t, BR_word_t, BR_word_t, BR_word_t);

extern void BR_delay_function(BR_delay_t, BR_delay_handler_t, void *);

#define BR_THREAD_ID() currp
#define BR_SAME_THREAD(x,y) ((x==0)?(y==0):(y!=0 && (x->id == y->id)))
#define BR_INVALID_ID() ((BR_thread_t)0)
extern char *BR_ascii_id(BR_thread_t, char *, size_t);
#define BR_THREAD_HASH(x) (x->id)


#define BR_LOCK_CREATE() (BR_lock_t)creatl()
#define BR_LOCK_DELETE(l) deletel((struct lock*)l)
#define BR_SEMA_CREATE(c) (BR_sema_t)creats(c)
#define BR_SEMA_DELETE(s) deletes((struct sem*)s)

#define BR_LOCK(l) while(lockl(l)==-1)
#define BR_UNLOCK(l) unlockl(l)

#define BR_WAIT(s) while(waits(s)==-1)
#define BR_SIGNAL(s) signals(s)

#define BR_TRY_LOCK(l) tryl(l)
#define BR_TRY_WAIT(s) tests(s)

extern void BR_unlock_handler(BR_cluster_t, BR_word_t);
extern void BR_signal_handler(BR_cluster_t, BR_word_t);
extern void BR_signal_mem_handler(BR_cluster_t, caddr_t, size_t, BR_word_t);

#define BR_SPINLOCK_DEC(s) BR_spinlock_t s
#define BR_SPINLOCK_INIT(s) (s=0)

#define BR_SPINLOCK_LOCK(s) while(s) yieldp(); s=1
#define BR_SPINLOCK_UNLOCK(s) s=0
#define BR_SPINLOCK_TRY(s) (s?0:(s=1))

/* not sure whether there is really no difference... */
#define BR_REAL_SPINLOCK_LOCK(s)   BR_SPINLOCK_LOCK(s)
#define BR_REAL_SPINLOCK_UNLOCK(s) BR_SPINLOCK_UNLOCK(s)


#define BR_POLL()
#define BR_THREAD_YIELD() yieldp()

/* nothing useful defined here yet */
#define BR_BARRIER() 

/* With the following, I was not sure how to implement them
#define BR_SPINSEMA_DEC(s) unsigned char s
#define BR_SPINSEMA_INIT(s, c)

#define BR_SPINSEMA_WAIT(s)
#define BR_SPINSEMA_SIGNAL(s)


*/

