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
#include "lifo.h"

/*
 * LIFO scheduling with respect to this bundle's threads, depth-first with 
 * respect to bundles if the bundle with a focus is out of threads. 
 * Thread affinity information is ingnored.
 */

/* Scheduling event handlers */

void lifo_thread_created(at_bundle_t *b, at_thread_t *t){
  /* Simply enqueue at the end */
  at_bundle_state_t *bs;

  at_create_stack(t);            /* Get a stack from the pool */
  at_create_local(t);            /* Create thread local storage if needed */

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

void lifo_thread_started(at_bundle_t *b, at_thread_t *t){
}

void lifo_thread_terminated(at_bundle_t *b, at_thread_t *t){
  /* This exploits locality by returning the stack to a local buffer */
  at_destroy_stack(t);
  at_destroy_local(t);
}

void lifo_thread_blocked(at_bundle_t *b, at_thread_t *t){
  /* Nothing to do */
}

void lifo_thread_unblocked(at_bundle_t *b, at_thread_t *t){
  at_bundle_state_t *bs;

  /* If this bundle has focus, can push the thread onto the
     processor buffer directly */

  if(b==at_focus){
    at_queue_put_at_head(&at_global_run_queue, t);
  }
  else {
    AT_SPINLOCK_LOCK(b->slck);
    bs = (at_bundle_state_t *)b->state;
    at_thread_queue_put_at_head(&(bs->threads), t);
    AT_SPINLOCK_UNLOCK(b->slck);
  }
}


void lifo_processor_idle(at_bundle_t *b, int proc){
  /* If the thread queue is not empty, push one thread on the
     processor buffer. If it is empty, forward the request 
     to the furst child bundle that can satisfy it. If no
     such children exists, forward it to the parent bundle.
     The order: FIFO within a bundle, depth-first if the
     current bundle is out of work */
  at_bundle_state_t *bs, *child_state;
  at_thread_t *t;
  at_bundle_t *parent, *child, *tb;
  int done=0;
 
  AT_SPINLOCK_LOCK(b->slck);
  bs = (at_bundle_state_t *)b->state;
  
  t = at_thread_queue_get(&(bs->threads));
  AT_SPINLOCK_UNLOCK(b->slck);
  
  if(t) {
    /* put on the corresponding queue */
    at_queue_put_at_head(at_local_run_queues[proc], t);
  }else {
    /* check the child bundles and forward the request, if possible */
    child = bs->bundles.head;
    while(child){
      AT_SPINLOCK_LOCK(child->slck);
      child_state = (at_bundle_state_t *)child->state;
      if(child_state->threads.head){
	(*(child->scheduler->processor_idle))(child, proc);
	done=1;
      }
      tb = child;
      child=child->next;
      AT_SPINLOCK_UNLOCK(tb->slck);
      if(done) break;
    }
    if(done==0){
      /* forward the request to the parent bundle */
      parent = bs->parent;
      if (parent){
	(*(parent->scheduler->processor_idle))(parent, proc);
      }
    }
  }
}

				     
at_bundle_t *at_lifo_bundle_create(at_bundle_t *p){
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
  sched->thread_created = lifo_thread_created;
  sched->thread_terminated = lifo_thread_terminated;
  sched->thread_started = lifo_thread_started;
  sched->thread_blocked = lifo_thread_blocked;
  sched->thread_unblocked = lifo_thread_unblocked;
  sched->processor_idle = lifo_processor_idle;
  sched->bundle_created = bundle_created;  /* reused across all */
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







