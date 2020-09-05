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
#include "lifo_lazy.h"
/*
 * Flat with respect to bundles: no nested bundles allowed. LIFO
 * with respect to threads. A single thread is returned in response to a
 * single request. Thread affinity information is ingnored.
 */


void lifo_lazy_thread_created(at_bundle_t *b, at_thread_t *t){
  /* Simply enqueue at the end */
  at_bundle_state_t *bs;

  /* We do not allocate the stack and thread local storage on thread
     creation. instead, it happens lazily on thread's startup */

  /* If the current bundle has the focus, we can push the new thread
     directly onto the proc queue */
  if(t->bundle==at_focus){
    at_queue_put_at_head(&at_global_run_queue, t);
  }
  else{
    AT_SPINLOCK_LOCK(b->slck);
    bs = (at_bundle_state_t *)b->state;
    at_thread_queue_put_at_head(&(bs->threads), t);
    AT_SPINLOCK_UNLOCK(b->slck);
  }
}

void lifo_lazy_thread_started(at_bundle_t *b, at_thread_t *t){
  /* Allocate storage for the stack and thread's local space */
  at_create_stack(t);            /* Get a stack from the pool */
  at_create_local(t);            /* Create thread local storage if needed */
}

at_bundle_t *at_lifo_lazy_bundle_create(at_bundle_t *p){
  at_bundle_t *bundle, *current;
  at_scheduler_t *sched;
  at_bundle_state_t *state;

  bundle = at_malloc(sizeof(at_bundle_t));
  state = at_malloc(sizeof(at_bundle_state_t));
  at_thread_queue_init(&(state->threads));
  at_bundle_queue_init(&(state->bundles));
  state->parent = p;
  state->thread_counter = 0;
  state->bundle_counter = 0;
  bundle->state = (void *)state;
  AT_SPINLOCK_INIT(bundle->slck);
  
  sched = at_malloc(sizeof(at_scheduler_t));
  
  /* Initialize the scheduler */
  sched->thread_created = lifo_lazy_thread_created;
  sched->thread_started = lifo_lazy_thread_started;

  /* Other event handlers are recycled from the standard LIFO scheduler */
  sched->thread_terminated = lifo_thread_terminated;
  sched->thread_blocked = lifo_thread_blocked;
  sched->thread_unblocked = lifo_thread_unblocked;
  sched->processor_idle = lifo_processor_idle;
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







