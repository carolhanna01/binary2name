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
#include "crt.h"


/* 
 * No nested bundles allowed 
 * Smallest Cache Reload Transient Ratio First Policy
*/


/* Turn off assertions */
#define assert(x)

#define AT_POWBUFSIZE 120000
/* Lookup the log value from the bundle table */
#define LOG(bs,x) (bs)->logs[(int)(x)]
#define KPOW(bs,x) (((x)<AT_POWBUFSIZE)?((bs)->pows[(int)(x)]):0.0)
#define LOCAL_CHILDREN 5

/*#define AT_PROFILE*/


#ifdef AT_PROFILE
extern hrtime_t at_prof_starts[32], at_prof_runtimes[32];
#define CRT_ENTER()  at_prof_starts[at_cpu()]=gethrtime()
#define CRT_EXIT()   at_prof_runtimes[at_cpu()] += (gethrtime()-at_prof_starts[at_cpu()])
#else
#define CRT_ENTER()
#define CRT_EXIT()
#endif



/* May be called to "flush" created threads immediately, without
   waiting for the parent to block */

#ifdef AT_ULTRA_PIC
void at_crt_flush(at_bundle_t *b){
  CRT_ENTER();
  AT_SPINLOCK_LOCK(b->slck);
  crt_update(b, at_self());
  AT_SPINLOCK_UNLOCK(b->slck);
  CRT_EXIT();
}
#endif
/* 
 * Update the footrpint and priority of running "t". Also goes throgh
 * dependencies and does the same, if necessary (live on the same cpu).
 * If dependencies are updated, the current heap is informed.
 *
 * Calculates a new priority based on the cache reload transient
 * for the thread itself and its dependencies
 */

/* This one is protected on the outside */
void crt_update(at_bundle_t *b, at_thread_t *t){
  int m, n, i, j;
  at_thread_t *dt;
  float kpn, q, delta, new_p;
  at_lff_bundle_state_t *bs = (at_lff_bundle_state_t *)b->state;
  float new_F;
  float mlogk;
  int delay;
  
  m = bs->misses[at_cpu()];
  n = sub_spf_pic01();

  /*kpn = pow(AT_k, (double)n);*/
  kpn = KPOW(bs,n);
  new_F = AT_N - (AT_N - t->F)*kpn;
  assert(new_F <= AT_N);
  
  bs->misses[at_cpu()] += n;
  m = bs->misses[at_cpu()];
  mlogk=m*AT_logk;
  t->priority = -mlogk;
  t->F = new_F;

  /* Handle dependencies here. For now, do just one level.
     May have to extend this in the future. Also, note that
     the newly created thread are not handled here.
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
      new_F = dt->F;
      if(delay>0){
	new_F *= KPOW(bs,delay);
      }
      
      /* Now stamp it with current misses */
      dt->misses = bs->misses[at_cpu()];
      q =  t->out_q[i];
      new_F = q*AT_N - (q*AT_N - new_F)*kpn;

      assert(new_F <= AT_N);	
      
      new_p = LOG(bs,new_F)-LOG(bs,dt->F)-mlogk;

      /* If a dependent thread is blocked, update its priority
	 without adding it to a heap */
      if((dt->state == AT_BLOCKED) && (dt->last_cpu == at_cpu())) {
	dt->priority = new_p;
      }

      /* If already on the heap, move up or down as needed */
      if(!AT_HEAP_IS_ABSENT(dt)){
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
    crt_flush_children(bs);
  }
}

void crt_flush_children(at_lff_bundle_state_t *bs){
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
    t->priority = - m*AT_logk;
    t->misses = m;
    /* Now insert in the heap */
    
    /*  Turning this off for now ... (need it for photo)*/
    dest = at_cpu();
    /*If lot's of children created, perform initial distribution */
    /*    if((num_children>LOCAL_CHILDREN) && (t->F==0.0)){
      t->priority = 0.1;
      dest = k/local_children;
    }
    */
    at_heap_insert(bs->heaps[dest], t);

    /*at_heap_insert(bs->heaps[at_cpu()], t);*/
  }
  /* All children have been taken care off */
  bs->num_children[at_cpu()] = 0;
}



at_bundle_t *at_crt_bundle_create(at_bundle_t *p){
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
    lff_runtimes[i] = 0;
  }
#endif
  /* Clear performance counters */
  (void) clr_spf_pic01();
  return bundle;
}







