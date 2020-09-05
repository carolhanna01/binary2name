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

#include "at-int.h"
#include "bundle.h"
#include "state.h"


/* 
 * Flat model with respect to bundles, Supports memory-consicius scheduling
 * for threads (MCS).
 * Arriving threads are pushed immediately on the corersponding physical
 * processor local queue. Threads are enqueued at the head of local queues
 * which will essentially simulate LIFO scheduling for non-blocking threads.
 * Some applications can exploit this.
 * Remove and schedule operations do nothing
 */

/* LIFO */
/*#define at_queue_put at_queue_put_at_head*/

void mcs_lazy_thread_created(at_bundle_t *b, at_thread_t *t){
  /* Immediately push onto the corresponding processor local queue */
  /* - this is a flat model, so there is only a single bundle */
  at_bundle_state_t *bs;
  int pnum;

  /* We do not allocate the stack and thread local storage on thread
     creation. instead, it happens lazily on thread's startup */

  if(at_vproc() == t->affinity){
    pnum = at_cpu();
  }
  else {
    /* A very simple virtual -> physical processor mapping */
    /* It is ok, because there is load balancing below */
    if(t->affinity==AT_UNBOUND) {
      /* enqueue on the same cpu as parent */
      pnum = at_cpu();
    }
    else{
      pnum = t->affinity % at_ncpus();
    }
  }
  at_queue_put_at_head(at_local_run_queues[pnum], t);
}

void mcs_lazy_thread_started(at_bundle_t *b, at_thread_t *t){
   /* Allocate storage for the stack and thread's local space */
  at_create_stack(t);            /* Get a stack from the pool */
  at_create_local(t);            /* Create thread local storage if needed */ 
}

at_bundle_t *at_mcs_lazy_bundle_create(at_bundle_t *p){
  at_bundle_t *bundle, *current;
  at_scheduler_t *sched;
  at_bundle_state_t *state;
  int i;

  bundle = at_malloc(sizeof(at_bundle_t));
  state = at_malloc(sizeof(at_bundle_state_t));
  
  /* Queues are not necessary here ??? Why */
  
  at_thread_queue_init(&(state->threads));
  at_bundle_queue_init(&(state->bundles));

  state->parent = p;
  state->thread_counter = 0;
  state->bundle_counter = 0;
  bundle->state = (void *)state;
  AT_SPINLOCK_INIT(bundle->slck);  

  state->misses = (float *) at_malloc(sizeof(float)*at_ncpus());
  for(i=0; i<at_ncpus(); i++){
    state->misses[i] = 0.0;
  }

  sched = at_malloc(sizeof(at_scheduler_t));
  
  /* Initialize the scheduler */
  sched->thread_created = mcs_lazy_thread_created;
  sched->thread_started = mcs_lazy_thread_started;
  sched->thread_terminated = mcs_thread_terminated;
  sched->thread_blocked = mcs_thread_blocked;
  sched->thread_unblocked = mcs_thread_unblocked;
  sched->processor_idle = mcs_processor_idle;
  /* These two are recycled from other flat models */
  sched->bundle_created = bundle_created;
  sched->bundle_terminated = bundle_terminated;
  
  bundle->scheduler = sched;
  /* add as a child to the current bundle */
  /* The current bundle could be NULL for the very first root bundle */
  current = at_get_focus();
  if(current){
    /* trigger a bundle created event */
    (*current->scheduler->bundle_created)(current, bundle);
  }
  return bundle;
}







