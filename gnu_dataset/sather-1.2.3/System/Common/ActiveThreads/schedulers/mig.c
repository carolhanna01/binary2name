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
#include "mig.h"

/*
 * MIG scheduling with respect to this bundle's threads, depth-first with 
 * respect to bundles if the bundle with a focus is out of threads. 
 * Thread affinity information is ingnored.
 */

extern void BR_mig_create_stack(at_thread_t *t);
extern void BR_mig_destroy_stack(at_thread_t *t);
extern void BR_push_handler(int dest, at_thread_t *t);

at_bundle_t *at_mig_bundle=(void *)0xFF;  /* The *global* migration bundle */

/* Scheduling event handlers */

void mig_thread_created(at_bundle_t *b, at_thread_t *t){
  /* Simply enqueue at the end */
  at_mig_bundle_state_t *bs;

  /* Not allowed to migrate yet */
  t->data = AT_MIG_DISABLE;

  BR_mig_create_stack(t);            /* Get a stack from the pool */
  at_create_local(t);            /* Create thread local storage if needed */


  /* If the *urgent* thread, put in the head of the local queue */
  if(t->affinity == AT_URGENT){
    at_queue_put_at_head(at_local_run_queues[at_cpu()],t);
    return;
  }
  /* If the current bundle has the focus, we can push the new thread
     directly onto the proc queue */
  if(t->bundle==at_focus){
    at_queue_put(&at_global_run_queue, t);
  }
  else {
    /* enqueue on the bundle queue */
    AT_SPINLOCK_LOCK(b->slck);
    bs = (at_mig_bundle_state_t *)b->state;
    /* When just created, not allowed to migrate yet (for now)*/
    at_thread_queue_put(&(bs->local_threads), t);
    AT_SPINLOCK_UNLOCK(b->slck);
  }
}

void mig_thread_started(at_bundle_t *b, at_thread_t *t){
}

/* 
 * This is a hack - will have to think how to do this through 
 * standard event mechanism
 */
at_thread_t  *mig_thread_get(at_bundle_t *b){
   at_mig_bundle_state_t *bs;
   at_thread_t *t;

   AT_SPINLOCK_LOCK(b->slck);
   bs = (at_mig_bundle_state_t *)b->state;
   t = at_thread_queue_get(&(bs->global_threads));
   AT_SPINLOCK_UNLOCK(b->slck);
   return t;
}

void mig_thread_terminated(at_bundle_t *b, at_thread_t *t){
  BR_mig_destroy_stack(t);
  at_destroy_local(t);
}

void mig_thread_blocked(at_bundle_t *b, at_thread_t *t){
  /* Nothing to do */
}

void mig_thread_unblocked(at_bundle_t *b, at_thread_t *t){
  at_mig_bundle_state_t *bs;

  /* If the *urgent* thread, put in the head of the local queue */
  if(t->affinity == AT_URGENT){
    at_queue_put_at_head(at_local_run_queues[at_cpu()],t);
    return;
  }
  /* If this bundle has focus, can push the thread onto the
     processor buffer directly */

  if((b==at_focus) && (t->data == AT_MIG_DISABLE)){
    at_queue_put(&at_global_run_queue, t);
  }
  else {
    AT_SPINLOCK_LOCK(b->slck);
    bs = (at_mig_bundle_state_t *)b->state;
    if(t->data == AT_MIG_DISABLE){
      at_thread_queue_put(&(bs->local_threads), t);
    }
    else {
      at_thread_queue_put(&(bs->global_threads), t);      
    }
    AT_SPINLOCK_UNLOCK(b->slck);
  }
}

/* A handler for the bundle-specific event #1 */
/* This actually calls the migration code */
void mig_bundle_event1(at_bundle_t *b, at_thread_t *t, at_word_t dest){
  BR_push_handler(dest, t);
}


/* Currently, must be a leaf (terminating) bundle */
/* First check the local queue, if empty, then the global one */

void  mig_processor_idle(at_bundle_t *b, int proc){
  at_mig_bundle_state_t *bs, *child_state;
  at_thread_t *t;
  at_bundle_t *parent, *child, *tb;
  int done=0;

  bs = (at_mig_bundle_state_t *)b->state;
  
  
  t = at_thread_queue_get(&(bs->local_threads));
  if(t==NULL){
    t = at_thread_queue_get(&(bs->global_threads));    
  }

  if(t) {
    /* put on the corresponding queue */
    at_queue_put(at_local_run_queues[proc], t);
  }
}
				     
at_bundle_t *at_mig_bundle_create(at_bundle_t *parent){
  at_bundle_t *bundle, *current;
  at_scheduler_t *sched;
  at_mig_bundle_state_t *state;

  bundle = at_malloc(sizeof(at_bundle_t));
  state = at_malloc(sizeof(at_mig_bundle_state_t));
  at_thread_queue_init(&(state->local_threads));
  at_thread_queue_init(&(state->global_threads));
  at_bundle_queue_init(&(state->bundles));
  state->parent = parent;
  state->thread_counter = 0;
  state->bundle_counter = 0;
  bundle->state = (void *)state;
  AT_SPINLOCK_INIT(bundle->slck);
  
  sched = at_malloc(sizeof(at_scheduler_t));
  
  /* Initialize the scheduler */
  sched->thread_created = mig_thread_created;
  sched->thread_terminated = mig_thread_terminated;
  sched->thread_started = mig_thread_started;
  sched->thread_blocked = mig_thread_blocked;
  sched->thread_unblocked = mig_thread_unblocked;
  
  sched->processor_idle = mig_processor_idle;
  sched->bundle_created = bundle_created;
  sched->bundle_terminated = bundle_terminated;

  /* Bundle-specific events section */
  sched->bundle_event1 = mig_bundle_event1;

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

void at_mig_enable(){
  at_thread_t *t;

  t = at_self();
  t->data = AT_MIG_ENABLE;
  return;
}

void at_mig_disable(){
  at_thread_t *t;

  t = at_self();
  t->data = AT_MIG_DISABLE;
  return;
}


