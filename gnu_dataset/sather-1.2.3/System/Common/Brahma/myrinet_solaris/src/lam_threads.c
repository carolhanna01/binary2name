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
 * This files contains the implementation of Brahma threads for Myrinet
 * based Brahma. It implements "thread caching" in a limited way.
 * The number of physical threads created is minimized - before finishing
 * up, a thread checks if there are pending thread creation requests,
 * and if so, the current running thread is reused. Remote forks
 * work in a similar way. A request to destination that enqueues
 * a thread creation request is made. This is then handled the same
 * way as other forks on the remote cluster.
 *
 * Much of the code is based on David Stoutamire's thread caching code
 * for SMPs/Solaris. Unfortunately, there are a few differences and I
 * can't see how the same code can be reused in a straightforward way.
 * Sather would be very handy here!
 */



/*
** Instead of forking immediately, fork requests are put on a queue.  When
** the queue gets too large, forks do happen immediately, but the
** enqueueing thread defers.  The lower priority background thread can
** only wake up when there is unused CPU power, and it dequeues and forks
** threads in a tight loop.  This background thread is automatically
** preempted by the new thread it forks.
*/

#include <stdio.h>
#include <stdlib.h>
#include "lam_sync.h"
#include "myrinet_solaris.h"

extern BR_THR_QUEUE_DEC; /* guards the access to the thrtead creation queue 
			 * is declared and initialized in lam.c */
extern BR_lock_t BR_background_lck;


#define MAX_FORK_QUEUE 64
typedef struct fork_elem_struct {
   BR_cluster_t src;
   BR_handler_t func;
   BR_word_t arg0, arg1, arg2, arg3;
   } BR_fork_elem_t;
static BR_fork_elem_t BR_fork_queue[MAX_FORK_QUEUE];

static unsigned int BR_fork_head = 0;
static unsigned int BR_fork_tail = 0;

#define next_fork_head ((BR_fork_head==MAX_FORK_QUEUE)?0:(BR_fork_head+1))
#define next_fork_tail ((BR_fork_tail==MAX_FORK_QUEUE)?0:(BR_fork_tail+1))
#define fork_queue_empty (BR_fork_head==BR_fork_tail)
#define fork_queue_full (next_fork_head==BR_fork_tail)

extern int lam_myproc;

/* Start the thread using args in a queue element */
static void BR_fork_wrapper(BR_fork_elem_t *e) {
   while (1) {

      /* Copy out the args and unlock, quickly. */
      BR_cluster_t src = e->src;
      BR_handler_t func = e->func;
      BR_word_t arg0 = e->arg0;
      BR_word_t arg1 = e->arg1;
      BR_word_t arg2 = e->arg2;
      BR_word_t arg3 = e->arg3;

      /*------------------------------------------------------------------*/
      BR_THR_QUEUE_UNLOCK;
      /*------------------------------------------------------------------*/

      func(src, arg0, arg1, arg2, arg3);

      /* After thread finishes, we get here.  Try to start next thread. */
      /*------------------------------------------------------------------*/
      BR_THR_QUEUE_LOCK;
      /*------------------------------------------------------------------*/

      if (!fork_queue_empty) {
	 BR_fork_tail = next_fork_tail;
	 e = &(BR_fork_queue[BR_fork_tail]);
      } else {
	/*----------------------------------------------------------------*/
	 BR_THR_QUEUE_UNLOCK;
	/*----------------------------------------------------------------*/
	 return;
	 /* The thread terminates when nothing left in queue. */
	 /* Right now there is no pooling of idle threads.    */
      }
   }
}

void BR_FORK_4(BR_cluster_t dest, BR_handler_t func,
	 BR_word_t arg0, BR_word_t arg1, BR_word_t arg2, BR_word_t arg3) {
  if (dest != lam_myproc) {
    /* Remote fork */
    BR_REQUEST_5(dest, (BR_handler_5_t)BR_enqueue_fork, (BR_word_t)func, 
		 arg0, arg1, arg2, arg3);
  }
  else {
    /* Local fork */
    BR_enqueue_fork(dest, func, arg0, arg1, arg2, arg3);
  }
}


/* Enqueue the local fork ... */
/* This can be used a remote handler as well to enqueue remote fork requests*/
void BR_enqueue_fork(BR_cluster_t src, BR_handler_t func,
	 BR_word_t arg0, BR_word_t arg1, BR_word_t arg2, BR_word_t arg3) {

   BR_fork_elem_t *e;
   TNF_PROBE_0(BR_enqueue_fork_start, "BRAHMA BR_enqueue_fork BR_enqueue_fork_start", "");

   /*------------------------------------------------------------------*/
   BR_THR_QUEUE_LOCK;
   /*------------------------------------------------------------------*/
   
   /* If queue is full, create next thread but defer to slow down creation */
   if (fork_queue_full) {
     
     /* Insert new thread args while we have the spinlock */
     BR_fork_head = next_fork_head;
     e = &(BR_fork_queue[BR_fork_head]);
     e->src = src;
     e->func = func;
     e->arg0 = arg0;
     e->arg1 = arg1;
     e->arg2 = arg2;
     e->arg3 = arg3;
     
     /* Dequeue and create thread */
     BR_fork_tail = next_fork_tail;
     if(thr_create(NULL, 0, (void *(*)(void*)) BR_fork_wrapper, 
		   &(BR_fork_queue[BR_fork_tail]), THR_DETACHED, NULL)){
       printf("thr_create() failed (1)\n");
     }
     /* Unlock will be done by created thread */
     BR_THREAD_YIELD();
   }
   else {
     BR_fork_head = next_fork_head;
     e = &(BR_fork_queue[BR_fork_head]);
     e->src = src;
     e->func = func;
     e->arg0 = arg0;
     e->arg1 = arg1;
     e->arg2 = arg2;
     e->arg3 = arg3;
     /*----------------------------------------------------------------*/
     BR_THR_QUEUE_UNLOCK;
     /*----------------------------------------------------------------*/
   }

   TNF_PROBE_0(BR_enqueue_fork_end, "BRAHMA BR_enqueue_fork BR_enqueue_fork_end", "");
}


/* This gets executed at low priority */
void BR_background() {
  /* Signal the main thread that started */
  BR_UNLOCK(BR_background_lck);
   while (1) {
     /*------------------------------------------------------------------*/
     BR_THR_QUEUE_LOCK;
     /*------------------------------------------------------------------*/
     if (!fork_queue_empty) {
	 thread_t thr;
	 BR_fork_tail = next_fork_tail;
	 if (thr_create(NULL, 0, (void *(*)(void*)) BR_fork_wrapper,
			&(BR_fork_queue[BR_fork_tail]), THR_DETACHED, &thr)){
	   printf("thr_create failed (2)\n");
	 }
	 thr_setprio(thr, 1);
	 /* The created thread is responsible for unlocking qlock. */
      } else {
	/*----------------------------------------------------------------*/
	BR_THR_QUEUE_UNLOCK;
	/*----------------------------------------------------------------*/
      }
      /* Make sure any created threads get to run as soon as possible. */
     BR_THREAD_YIELD();
     BR_POLL();  /* poll to possibly process remote thread forks, etc */
   }
}





