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

#ifndef _THREAD_H_
#define _THREAD_H_

#include "qt.h"
#include "p_pool.h"
#include "const.h"

#ifndef NULL
#define NULL	0
#endif

typedef void (at_userf_t)(void*);

/* 
 * If there exists at least one word of user acessible read/write
 * per processor memory (for example, a dedicated Sparc register %g7,
 * thread base address needs to conform only to the standard acrhitecture
 * specific alignment. 
 * However, if per-processor memory is not available,
 * we will use the base stack address of the current thread to figure
 * out where the lwp datastructure (and hence current thread structures)
 * reside. For this to be possible, the thread stack base pointer must
 * be aligned on the thread stack size alignment. This way we can quickly
 * figure out where the stack base is given any current sp.
 */

#define AT_STKSIZE at_stack_size

#define AT_STKBASE_ALIGNMENT QT_STKALIGN

/* `alignment' must be a power of 2. */
#define AT_STKALIGN(sp, alignment) \
  ((void *)((((qt_word_t)(sp)) + (alignment) - 1) & ~((alignment)-1)))



/* 
 * Affinity of an unbound thread. Such threads could be run by any
 * available virtual CPUs as long as they are in the runnable state.
 */
#define AT_UNBOUND -1
     
/* This may be used to signify the "urgency" of the thread by 
   some bundles. These things should go directly to the head of
   whatever data structures are used. Used in distributed environments
   to make remotely originating threads of higher priority... */

#define AT_URGENT -2     


/* 
 * Possible thread states:
 * AT_INITIALIZED - initialized, but has not started to run
 * AT_RUNNING     - currently running
 * AT_READY       - ready to run and waiting in some runable queue
 * AT_BLOCKED     - blocked on the synchronization object 
 * AT_ZOMBIE      - after termination or before initialization
 *
 * Note that these all have somewhat relaxed semantics as to know
 * exactly what state the thread is in it must be stopped if it runs
 * or removed from the queue it is in to make sure it does not change its
 * state. The "state" is guaranteed to be correct only under these
 * circumstances. For instance, the thread could be removed from 
 * the queue and examined to see if it has been run or not.
 */
#define AT_INITIALIZED 0
#define AT_RUNNING     1
#define AT_READY       2
#define AT_BLOCKED     3
#define AT_ZOMBIE      4

/* The notion of a thread is merged with the notion of a queue.
   Thread stuff: thread status (sp) and stuff to use during
   (re)initialization.  Queue stuff: next thread in the queue
   (next). */

#define AT_MAXDEGREE 3
/* It is important that next is the first attribute - for pooling */
typedef struct at_thread {
  struct at_thread *next;     /* Next thread in the queue. */
  struct at_thread *prev;     /* Next thread in the queue. */
  qt_t *sp;                   /* QuickThreads handle. */
  void *sto;                  /* `malloc'-allocated stack. */
  int id;                     /* Thread id: 0.. */
  void* thread_local;         /* Pointer to local storage, set by client */
  int affinity;               /* Thread affinity: virtual proc# or AT_UNBOUND*/
  struct at_bundle *bundle;        /* bundle this thread belongs to */
  int state;                  /* Current State */
  at_userf_t* func;           /* Function the thread is executing */
  unsigned nargs;             /* Number of args for this function */
  at_word_t   args[6];        /* temporary buffer for args */
  int   data;                 /* Could be used by clients (also pads things
				 to double word boundary */
  float   priority;
  float   misses;             /* global # of misses when a thread blocks */
  int     last_cpu;           /* cpu # where the thread ran last */
  float   F;                  /* footprint size */
  struct  at_thread *incoming[AT_MAXDEGREE];
  struct  at_thread *outgoing[AT_MAXDEGREE];
  float   out_q[AT_MAXDEGREE];  /* Overlap "q" for outgoing edges */
  float   in_q[AT_MAXDEGREE];   /* Overlap "q" for incoming edges */
  int     indegree;
  int     outdegree;
  int     heap_index;         /* If in a heap, position index */
} at_thread_t;

at_thread_t *at_thread_create();
void at_thread_elt_init(void *d);
void at_thread_elt_print(void *d);
void at_thread_pool_print(at_pool_t *p);

extern AT_SPINLOCK_DEC(thr_num_lck);


#endif /* _THREAD_H_ */




