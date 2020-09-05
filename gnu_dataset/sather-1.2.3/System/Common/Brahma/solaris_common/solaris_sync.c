/*------------------------->  ANSI C - sourcefile  <-------------------------*/
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
** This is the Solaris implementation for Brahma synchronization and 
** thread manipulation primitives. Uses Solaris threads and Sparc V7 spinlocks.
** This file could be used by various Solaris based Brahma platforms
** (TCP/IP, Myrinet, Meiko, etc.)
*/

#include "solaris_sync.h"
#include <stdio.h>


/* Things necessary to determine the number of procs for a cluster */

#include <assert.h>
#include <kstat.h>
#include <fcntl.h>
#include <sys/systeminfo.h>
#include <stdlib.h>
#include <string.h>

unsigned int BR_PROCESSORS() {
  kstat_t *k;
  kstat_ctl_t *kctl;
  int ncpu = 0;

  /* Open the kstat interface */
  if(NULL==(kctl=kstat_open())){
    perror("kstat_open");
    exit(1);
  }

  /*
   * Follow the kstat chain, making note of each useful module we
   * come across. Usefulness = disk if diskflag, cpu if cpuflag.
   * Ripped out of xcpustate.  Why isn't this easier?
   */
  for(k=kctl->kc_chain;k;k=k->ks_next){
      if(0==(strncmp(k->ks_name, "cpu_stat", 8))){
        ncpu++;
      }
  }

  /* Close the kstat interface */
  if(kstat_close(kctl)!=0){
    perror("kstat_close");
    exit(1);
  }

  return ncpu;
}

typedef struct delay_args_struct {
   hrtime_t awake;
   BR_delay_handler_t func;
   void *arg;
} *delay_args;

/* Don't execute until given time arrives */
static void BR_sleep_then_exec(delay_args a) {
   while (gethrtime() < a->awake) thr_yield();
   ((BR_delay_handler_t) a->func)(a->arg);
   free(a);
}

/*
** Asyncronously call a function after at least n.sec seconds and
** n.nsec nano seconds, with arg as argument.  May or may not be
** executed by the thread that called "BR_delay_function".
*/
void BR_delay_function(BR_delay_t n, BR_delay_handler_t func, void *arg) {
   int res;
   hrtime_t awake = gethrtime() 
		    + ((hrtime_t) n.nsec) 
		    + (((hrtime_t) n.sec) * 1000000000);
   delay_args a = malloc(sizeof(struct delay_args_struct));
   a->awake = awake;
   a->func = func;
   a->arg = arg;
   while (1) {
      /* If overloading the system, slow down and try again. */
      res = thr_create(NULL, 0, (void *(*)(void*)) BR_sleep_then_exec,
				      a, THR_DETACHED, NULL);
      if (res==0) break;
      thr_yield();
   }
}


#define BR_ID_CLUSTER_SIZE 6
#define BR_ID_CLUSTER_MASK 0xFC000000
#define BR_ID_THREAD_MASK (~BR_ID_CLUSTER_MASK)
#define BR_ID_CLUSTER_SHIFT (32-BR_ID_CLUSTER_SIZE)
#define BR_ID_CLUSTER_BITS(n) ((unsigned int)(n)<<BR_ID_CLUSTER_SHIFT)
#define BR_ID_CLUSTER(p) (((unsigned int)(p)&BR_ID_CLUSTER_MASK)>>BR_ID_CLUSTER_SHIFT)
#define BR_ID_THREAD(p) ((unsigned int)(p)&BR_ID_THREAD_MASK)
#define BR_MAKE_THREAD_ID(cl,p) (unsigned int)(((cl)<<BR_ID_CLUSTER_SHIFT)|(unsigned  int)p)

/*
** Create human-readable form of the thread id in the buffer "buf",
** for debugging.
*/
char *BR_ascii_id(BR_thread_t id, char *buf, size_t maxlen) {
  sprintf(buf,"%02d-%-4d",BR_ID_THREAD(id.t), BR_ID_CLUSTER(id.t));
  return buf;
}


/* Get the local memory set with "BR_SET_THREAD_LOCAL". */
caddr_t BR_GET_THREAD_LOCAL() {
   caddr_t res;
   thr_getspecific(BR_key, (void **) &res);
   return res;
}


/*
** There are four kind of synchronization provided by Brahma: there are
** mutual exclusions and semaphores (lock_t and sema_t), and lightweight
** versions of each (spinlock_t and spinsema_t).  The heavy versions are
** equivalent to the synchronization functionality in Solaris threads -
** they may block, and the critical regions they are used with may be of
** arbitrary length and be nested.  The lightweight versions try to use
** inline atomic instructions to build very fast spinlock-style mutual
** exclusion.  These should only be used for very small, nonblocking,
** non-nested exclusion.  The heavy versions can always replace the light
** versions, with some performance penalty.
** 
** Using locks and semaphores in request or reply handlers should be
** avoided.
*/

/* Create or delete an instance of the synchronization object. */
BR_lock_t BR_LOCK_CREATE() {
   BR_lock_t res = (BR_lock_t) malloc(sizeof(lock_t));
   (void) mutex_init(res, USYNC_THREAD, NULL);
   return res;
}

BR_sema_t BR_SEMA_CREATE(unsigned int count) {
   BR_sema_t res = (BR_sema_t) malloc(sizeof(sema_t));
   (void) sema_init(res, count, USYNC_THREAD, NULL);
   return res;
}

BR_rw_t BR_RW_CREATE() {
   BR_rw_t res = (BR_rw_t) malloc(sizeof(rwlock_t));
   (void) rwlock_init(res, USYNC_THREAD, NULL);
   return res;
}

/* ID of executing thread */
BR_thread_t BR_THREAD_ID() {
   BR_thread_t t;
   t.t = BR_MAKE_THREAD_ID(BR_HERE(),thr_self());
   return t;
}

/* Returns a value which can never be an id of an actual thread, to use as
a sentinel value. */

BR_thread_t BR_INVALID_ID() {
   BR_thread_t t;
   t.t = (unsigned int)-1;
   return t;
}

/*
** Useful handler for unlocking and signalling on remote nodes.
** (The second arg will have to be cast to word to use.)
*/
void BR_unlock_handler(BR_cluster_t ignored, BR_word_t lock) {
   BR_UNLOCK((BR_lock_t)lock);
}

void BR_signal_handler(BR_cluster_t ignored, BR_word_t sema) {
   BR_SIGNAL((BR_sema_t)sema);
}

void BR_signal_mem_handler(BR_cluster_t from, caddr_t a, size_t size, caddr_t sema) {
   BR_SIGNAL((BR_sema_t)sema);
}

INLINE int BR_spinlock_try(BR_spinlock_t* x) {
  register int XyZ;            
  TEST_AND_SET(x, XyZ);
  return XyZ==0;
}












