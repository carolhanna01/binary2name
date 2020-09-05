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

/*#include <assert.h>*/
#include <math.h>
#include <sys/time.h>
#include "at-int.h"
#include "bundle.h"
#include "state.h"
#include "heap.h"
#include "lff.h"


/* 
 * No nested bundles allowed 
 * Largest Footprint First policy 
*/


/* 
 * We ignore the dependencies for now. Will have to fix this later
 */

/* 
 * Will need to have an array of priorities per thread (for each cpu)
 */

/* Tirn off assertions */
#define assert(x)

#define AT_POWBUFSIZE 120000
/* Lookup the log value from the bundle table */
#define LOG(bs,x) (bs)->logs[(int)(x)]
#define KPOW(bs,x) (((x)<AT_POWBUFSIZE)?((bs)->pows[(int)(x)]):0.0)
#define LOCAL_CHILDREN 5

/*#define AT_PROFILE*/

#ifdef AT_PROFILE
hrtime_t at_prof_starts[32], at_prof_runtimes[32];
#endif

#ifdef AT_PROFILE
#define LFF_ENTER()  at_prof_starts[at_cpu()]=gethrtime()
#define LFF_EXIT()   at_prof_runtimes[at_cpu()] += (gethrtime()-at_prof_starts[at_cpu()])
#else
#define LFF_ENTER()
#define LFF_EXIT()
#endif

void lff_thread_created(at_bundle_t *b, at_thread_t *t){
  /* Simply enqueue at the end */
  at_lff_bundle_state_t *bs;

  LFF_ENTER();
  AT_SPINLOCK_LOCK(b->slck);
  
  /* We do not allocate the stack and thread local storage on thread
     creation. instead, it happens lazily on thread's startup */
  bs = (at_lff_bundle_state_t *)b->state;

  t->indegree = 0;
  t->outdegree = 0;
  t->priority = 0.0;
  t->heap_index = AT_HEAP_ABSENT;  /* not added to the heap yet */
  t->F = 0.0;
  t->last_cpu = at_cpu();
  t->next = NULL;
  t->prev = NULL;
  t->misses = 0.0;

  /* Buffer the child */
  bs->children[at_cpu()][bs->num_children[at_cpu()]] = t;
  bs->num_children[at_cpu()]++;

  AT_SPINLOCK_UNLOCK(b->slck);
  LFF_EXIT();
}

void lff_thread_started(at_bundle_t *b, at_thread_t *t){
  at_lff_bundle_state_t *bs = (at_lff_bundle_state_t *)b->state;

  LFF_ENTER();
  /* Allocate storage for the stack and thread's local space */
  at_create_stack(t);            /* Get a stack from the pool */
  at_create_local(t);            /* Create thread local storage if needed */
  t->misses = bs->misses[at_cpu()];
  t->last_cpu = at_cpu();
  LFF_EXIT();
}

/* Set the edge/degree stuff */
void at_share(at_thread_t *t1, at_thread_t *t2, float q){
  at_bundle_t *b;

  LFF_ENTER();
  b = t1->bundle;
  /* Must protect -- will do in the future*/
  AT_SPINLOCK_LOCK(b->slck);
  
  t1->outgoing[t1->outdegree] = t2;
  t1->out_q[t1->outdegree] = q;
  t1->outdegree++;

  t2->incoming[t2->indegree] = t1;
  t2->in_q[t2->indegree] = q;
  t2->indegree++;

  AT_SPINLOCK_UNLOCK(b->slck);

  LFF_EXIT();
}



/* May be called to "flush" created threads immediately, without
   waiting for the parent to block */
void at_lff_flush(at_bundle_t *b){
  LFF_ENTER();
  AT_SPINLOCK_LOCK(b->slck);
  lff_update(b, at_self());
  AT_SPINLOCK_UNLOCK(b->slck);
  LFF_EXIT();
}
/* 
 * Update the footrpint and priority of running "t". Also goes throgh
 * dependencies and does the same, if necessary (live on the same cpu).
 * If dependencies are updated, the current heap is informed.
 */

/* This one is protected on the outside */
void lff_update(at_bundle_t *b, at_thread_t *t){
  int m, n, i, j;
  at_thread_t *dt;
  float kpn, q, delta, new_p;
  at_lff_bundle_state_t *bs = (at_lff_bundle_state_t *)b->state;
  float new_F;
  int delay;
  
  m = bs->misses[at_cpu()];
  n = sub_spf_pic01();

  /*kpn = pow(AT_k, (double)n);*/
  kpn = KPOW(bs,n);
  delta = (m+n)*AT_logk;
  
  new_F = AT_N - (AT_N - t->F)*kpn;
  assert(new_F <= AT_N);
  /*t->priority = log(t->F) - delta;*/
  t->priority = LOG(bs,new_F) - delta;
  t->F = new_F;
  bs->misses[at_cpu()] += n;
  
  /* Handle dependencies here. For now, do just one level.
     May have to extend this in the future. Also, note that
     the newly created thread are not handler here.
     To avoid a simple common case */
  
  for(i=0; i<t->outdegree; i++){
    dt = t->outgoing[i];
    
    if(!AT_HEAP_IS_ABSENT(dt) &&
       ((dt->last_cpu == at_cpu()) || (AT_HEAP_IS_ON_QUEUE(dt)))){
      /* First, need to compute what the footprint was before "t" started */
      /* "dt" was inactive until that time. This is the init. footprint */
      /*
	if(t->misses < dt->misses){
	at_printf("[%d] ERROR: t->misses %e  dt->misses  %e\n", at_cpu(),
		  t->misses, dt->misses);
	at_printf("******************* E-CACHE MISSES *****************\n");
	for(j=0; j<at_ncpus(); j++){
	  at_printf("CPU %d:      %e\n", j, bs->misses[j]);
	}
	at_printf("****************************************************\n");	
	abort();
      }
      */
      delay = (int) (t->misses - dt->misses);
      if(delay>0){
	dt->F *= KPOW(bs,delay);
      }
      
      /* Now stamp it with current misses */
      dt->misses = bs->misses[at_cpu()];
      
      q =  t->out_q[i];
      dt->F = q*AT_N - (q*AT_N - dt->F)*kpn;

      assert(dt->F <= AT_N);	
      
      /* If alreday in a runnable state, must update its place in the heap*/
      /* If running, or blocked, no need to do that (and would be wrong) */
      if(!AT_HEAP_IS_ABSENT(dt)){
	new_p = LOG(bs,dt->F) - delta;
	/* Inform the heap about priority change */
	at_heap_change_p(bs->heaps[at_cpu()], dt, new_p);
      }
    }
  }

  /* The the miss count of the last update */
  t->misses = bs->misses[at_cpu()];

  /* Clear the counters */
  (void) clr_spf_pic01();

  /* FLush the children, if needed */
  if(bs->num_children[at_cpu()]){
    lff_flush_children(bs);
  }
}

void lff_flush_children(at_lff_bundle_state_t *bs){
  at_thread_t *in_t;
  float q, m;
  int i, k, dest, local_children;
  at_bundle_t *b;
  at_thread_t *t;
  int num_children;
  
  /* the parent has been updated already */
  
  m = bs->misses[at_cpu()];
  num_children = bs->num_children[at_cpu()];
  
  if(num_children>LOCAL_CHILDREN){
    /* number of new children assigned to each processor */
    local_children = ceil( (double)num_children / at_ncpus());
  }
  else {
    local_children = num_children;
  }

     
  for(k=0; k<num_children; k++){
    /* 
       Compute the initial footprint by iteraring over the
       incoming edges of "t"
       */
    
    /* We should have update all incoming edges, not just the
       parent (will probably do this when have time) */

    t = bs->children[at_cpu()][k];
    for(i=0; i<t->indegree; i++){
      in_t = t->incoming[i];
      if(in_t->last_cpu == at_cpu()){
	q = t->in_q[i];
	t->F += in_t->F * q;
	assert(t->F <= AT_N);      
      }
    }
    /* Compute initial priority */
    t->priority = LOG(bs,t->F) - m*AT_logk;
    t->misses = m;
    /* Now insert in the heap */
    
    /*  Turning this off for now ... (need it for photo)*/
    dest = at_cpu();
    /*If lot's of children created, perform initial distribution */
    /*    if((num_children>LOCAL_CHILDREN) && (t->F==0.0)){
      t->priority = 0.1;
      dest = k/local_children;
    }*/
    at_heap_insert(bs->heaps[dest], t);

    /*at_heap_insert(bs->heaps[at_cpu()], t);*/
  }
  /* All children have been taken care off */
  bs->num_children[at_cpu()] = 0;
}


void lff_thread_terminated(at_bundle_t *b, at_thread_t *t){
  at_lff_bundle_state_t *bs;
  unsigned int n;
  int i, j, k;
  int found;
  at_thread_t *other_t;

  LFF_ENTER();
  AT_SPINLOCK_LOCK(b->slck);
  
  bs = (at_lff_bundle_state_t *)b->state;

  /* Update the thread/dependencies */
  /*lff_update(b, t);*/

  /* Remove from the "outgoing" dependency lists of related threads*/
  for(i=0; i<t->indegree; i++){
    other_t = t->incoming[i];
    found = 0;
    
    for(j=0; j<other_t->outdegree; j++){
      if(other_t->outgoing[j]==t){
	/* Shift left */
	found = 1;
	for(k=j; k<other_t->outdegree-1; k++){
	  other_t->outgoing[k] = other_t->outgoing[k+1];
	  other_t->out_q[k] = other_t->out_q[k+1];
	}
	other_t->outdegree--;
	break;
      }
    }
    /*
    if(!found){
      at_printf("Error in lff_terminated (1)\n");
      abort();
    }
    */
  }

  /* Remove from the "incoming" dependency lists of related threads*/
  for(i=0; i<t->outdegree; i++){
    other_t = t->outgoing[i];
    found = 0;
    
    for(j=0; j<other_t->indegree; j++){
      if(other_t->incoming[j]==t){
	/* Shift left */
	found = 1;
	for(k=j; k<other_t->indegree-1; k++){
	  other_t->incoming[k] = other_t->incoming[k+1];
	  other_t->in_q[k] = other_t->in_q[k+1];
	}
	other_t->indegree--;
	break;
      }
    }
    /*
    if(!found){
      at_printf("Error in lff_terminated (2)\n");
      abort();
    }
    */
  }  

  AT_SPINLOCK_UNLOCK(b->slck);

  at_destroy_stack(t);
  at_destroy_local(t);

  LFF_EXIT();
}

void lff_thread_blocked(at_bundle_t *b, at_thread_t *t){
}

/* For an unblocking threads, we do not really need to recompute the
   footprint because its priority does not change */
void lff_thread_unblocked(at_bundle_t *b, at_thread_t *t){
  at_lff_bundle_state_t *bs;

  LFF_ENTER();
  AT_SPINLOCK_LOCK(b->slck);
  
  bs = (at_lff_bundle_state_t *)b->state;

  /* Place on the proper heap */
  /* For now, simply place on the heap of the last cpu... */
  at_heap_insert(bs->heaps[t->last_cpu], t);

  AT_SPINLOCK_UNLOCK(b->slck);
  LFF_EXIT();
}

/* Ignore child bundles - we do not support them here anyway */
void lff_processor_idle(at_bundle_t *b, int proc){
  at_lff_bundle_state_t *bs, *child_state;
  at_thread_t *t=NULL;
  at_bundle_t *parent, *child, *tb;
  int p, i, neighbor;
  int n;

  LFF_ENTER();
  
  /*at_printf("IDLE Event:  %d\n", proc);*/
  bs = (at_lff_bundle_state_t *)b->state;

  /* Check if not busy first */
  if(!AT_SPINLOCK_TRY(b->slck)) {
    /* Wait a bit, then return */
    for(i=0; i<5000; i++){(volatile)n++;}
    return;
  }

    /*  AT_SPINLOCK_LOCK(b->slck);  */
  t = at_heap_top(bs->heaps[at_cpu()]);
  /* If null, try to steal from a neighbor */
  if(t==NULL){
    neighbor = at_cpu();
    for(i=0; i<at_ncpus()-1; i++){
      neighbor = (neighbor+1)%at_ncpus();
      t = at_heap_bottom(bs->heaps[neighbor]);
      if (t) break;
    }
  }
  AT_SPINLOCK_UNLOCK(b->slck);

  /* Clear the counters */
  (void) clr_spf_pic01();

  if(t){

    /* 
     * put on the requesting proc 
     * But first, recompute the *real* expected footprint 
     * The footprint decayed while the thread was inactive, even
     * though priorities have not changed. 
     */

    if(t->last_cpu == at_cpu()){
      n = (int) (bs->misses[at_cpu()] - t->misses);
      t->F = t->F * KPOW(bs, n);
      assert(t->F <= AT_N);
    }
    else {
      /* Just migrated to this cpu */
      t->F = 0.0;
    }
    t->misses = bs->misses[at_cpu()];
    
    at_queue_put(at_local_run_queues[proc], t);
  }
  else {
    /* forward the request to the parent bundle */
    /* A hack - do it only for proc 0 */
    parent = bs->parent;
    (*(parent->scheduler->processor_idle))(parent, proc);
  }
  LFF_EXIT();
}

void lff_bundle_terminated(at_bundle_t *b, at_bundle_t *child){
  /*  LFF_ENTER();*/
  AT_SPINLOCK_LOCK(b->slck);
  at_bundle_queue_remove(&(((at_bundle_state_t *)b->state)->bundles), child);
  AT_SPINLOCK_UNLOCK(b->slck);
#ifdef AT_ULTRA_PIC  
  lff_stats(child);
#endif
  /*LFF_EXIT();*/
}

void lff_stats(at_bundle_t *b){
  at_lff_bundle_state_t *bs;
  double total=0;
  int i;
  
  AT_SPINLOCK_LOCK(b->slck);
  bs = (at_lff_bundle_state_t *)b->state;
  at_printf("\n\n******************* E-CACHE MISSES *****************\n");
  for(i=0; i<at_ncpus(); i++){
    at_printf("CPU %d:      %e\n", i, bs->misses[i]);
    total += bs->misses[i];
  }
  at_printf("****************************************************\n");
  at_printf("Total misses:  %e\n", total);
  at_printf("****************************************************\n");

#ifdef AT_PROFILE  
  at_printf("\n******************* TIME INSIDE LFF ****************\n");
  for(i=0; i<at_ncpus(); i++){
    at_printf("CPU %d:      %e\n", i, at_prof_runtimes[i]/1.0e9);
  }
  at_printf("****************************************************\n");
#endif
  AT_SPINLOCK_UNLOCK(b->slck);
}

at_bundle_t *at_lff_bundle_create(at_bundle_t *p){
  at_bundle_t *bundle, *current;
  at_scheduler_t *sched;
  at_lff_bundle_state_t *state;
  int i;

  bundle = at_malloc(sizeof(at_bundle_t));
  state = (at_lff_bundle_state_t *)at_malloc(sizeof(at_lff_bundle_state_t));
  
  /* Create per-processor heaps, etc. */
  state->heaps = (at_heap_t **)at_malloc(sizeof(at_heap_t *)*at_ncpus());
  state->misses = (float *)at_malloc(sizeof(float)*at_ncpus());
  state->logs = (float *)at_malloc(sizeof(double) * AT_N);
  state->pows = (float *)at_malloc(sizeof(double) * AT_POWBUFSIZE);

  at_bundle_queue_init(&(state->bundles));
  at_thread_dqueue_init(&(state->global_threads));

  for(i=0; i<at_ncpus(); i++){
    state->heaps[i] = at_heap_create(&(state->global_threads), AT_HEAPSIZE);
    state->misses[i] = 0.0;
    state->num_children[i] = 0;
    
  }

  /* Initialize the logarithm table */
  for(i=1; i<AT_N; i++){
    state->logs[i] = (float) log((double) i);
  }
  state->logs[0] = 0;

  /* Initialize the powers table */
  for(i=0; i<AT_POWBUFSIZE; i++){
    state->pows[i] = (float) pow((double) AT_k, (double)i);
  }
  
  state->parent = p;
  state->thread_counter = 0;
  state->bundle_counter = 0;

  bundle->state = (void *)state;
  AT_SPINLOCK_INIT(bundle->slck);
  
  sched = at_malloc(sizeof(at_scheduler_t));
  
  /* Initialize the scheduler */
  sched->thread_created = lff_thread_created;
  sched->thread_started = lff_thread_started;

  sched->thread_terminated = lff_thread_terminated;
  sched->thread_blocked = lff_thread_blocked;
  sched->thread_unblocked = lff_thread_unblocked;
  sched->processor_idle = lff_processor_idle;
  sched->bundle_created = bundle_created;
  sched->bundle_terminated = lff_bundle_terminated;

  bundle->scheduler = sched;

  /* add as a child to the current bundle */
  /* The current bundle could be NULL for the very first root bundle */
  current = at_get_focus();
  if(current){
    /* trigger a bundle created event */
    (*current->scheduler->bundle_created)(current, bundle);
  }

#ifdef AT_PROFILE
  for(i=0; i<at_ncpus(); i++){
    at_prof_runtimes[i] = 0;
  }
#endif
  /* Clear performance counters */
  (void) clr_spf_pic01();
  return bundle;
}







