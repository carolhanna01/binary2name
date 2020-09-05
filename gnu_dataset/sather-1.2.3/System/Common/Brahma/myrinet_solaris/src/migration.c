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

#include "at.h"
#include "brahma.h"
#include "migration.h"


char  stacks_buf[(3*MIG_STACK_SIZE*MIG_STACKS_PER_CLUSTER)+4000];
void  *stacks;
void       *stacks_free_list[MIG_STACKS_PER_CLUSTER];
at_spinlock_t stacks_slck;
int        stacks_free_list_top;
/* Pointers to the first and last local stacks */
void       *stacks_local_first, *stacks_local_last;
at_mutex_t mig_started;
at_mutex_t mig_finished;
int        mig_result;
volatile   int        BR_mig_from=0;
volatile   int        BR_mig_to=0;
volatile int BR_stack_stolen=0;  /* Total # bytes migrated */
volatile int BR_stack_received=0;  /* Total # bytes migrated */
volatile int BR_stack_pushed=0;  /* Total # bytes migrated */

BR_mig_stat_t BR_push_self_stat;
BR_mig_stat_t overhead;
int overhead_times=0;


at_spinlock_t BR_stat_slck;

at_bundle_t *at_mig_bundle;

volatile int BR_num_service_forks=0;
at_spinlock_t BR_service_forks_slck;

extern volatile int BR_started;


/* This will need to be intergrated more closely at a later point */
void BR_mig_poll(){
#ifdef AT_MIGRATION
  if(at_thread_count()<BR_PROCESSORS()+1){
    if(BR_steal()==BR_MIG_TERMINATION){
      BR_exit();  
    }
  }
#else
  BR_POLL();
#endif
}


void BR_inc_service_forks(){
  AT_SPINLOCK_LOCK(BR_service_forks_slck);
  BR_num_service_forks++;
  AT_SPINLOCK_UNLOCK(BR_service_forks_slck);
}

void BR_dec_service_forks(){
  AT_SPINLOCK_LOCK(BR_service_forks_slck);
  BR_num_service_forks--;
  AT_SPINLOCK_UNLOCK(BR_service_forks_slck);
}

int BR_service_forks(){
  int i;
  AT_SPINLOCK_LOCK(BR_service_forks_slck);
  i = BR_num_service_forks;
  AT_SPINLOCK_UNLOCK(BR_service_forks_slck);
  return i;
}

void BR_sema_signal_h_1(BR_cluster_t from, BR_word_t s){
  at_sema_t *sema;
  
  sema = (at_sema_t *)s;
  at_sema_signal(sema);
}

void BR_mig_stacks_init(){
  void *stack_origin;
  int i, j;

  AT_SPINLOCK_INIT(stacks_slck);
  
  stacks = AT_STKALIGN((void *)stacks_buf,AT_STKBASE_ALIGNMENT);
  /* Initialize the free list */
  stack_origin = stacks + BR_HERE()*MIG_STACK_SIZE*MIG_STACKS_PER_CLUSTER;
  for(i=0; i < MIG_STACKS_PER_CLUSTER; i++){
    stacks_free_list[i] = stack_origin;
    stack_origin += MIG_STACK_SIZE;
  }
  stacks_free_list_top = 0;

  stacks_local_first = stacks + BR_HERE()*MIG_STACK_SIZE*MIG_STACKS_PER_CLUSTER;
  stacks_local_last  = stacks_local_first + MIG_STACK_SIZE*(MIG_STACKS_PER_CLUSTER-1);

  at_mutex_init(&mig_started);
  at_mutex_init(&mig_finished);
  AT_SPINLOCK_UNLOCK(stacks_slck);

  AT_SPINLOCK_INIT(BR_stat_slck);
  AT_SPINLOCK_INIT(BR_service_forks_slck);
  memset((void *)&BR_push_self_stat, 0, sizeof(BR_mig_stat_t));
  memset((void *)&overhead, 0, sizeof(BR_mig_stat_t));

  /* Create a bundle for migration */
  /* And set focus */
  at_mig_bundle = at_mig_bundle_create(at_get_focus());
  BR_BARRIER();
  /* Disable automatic termination */
  /*at_do_when_idle(BR_mig_poll);*/
}

void BR_mig_create_stack(at_thread_t *t){
  void *sto;

  AT_SPINLOCK_LOCK(stacks_slck);
  sto = stacks_free_list[stacks_free_list_top];
  stacks_free_list_top++;
  if(stacks_free_list_top==MIG_STACKS_PER_CLUSTER){
    at_printf("OUT of stacks, cluster %d\n", BR_HERE());
    BR_exit();
  }
  AT_SPINLOCK_UNLOCK(stacks_slck);
  t->sto = sto;
  t->sto = AT_STKALIGN(t->sto,AT_STKBASE_ALIGNMENT);
  t->sp = QT_SP (t->sto, MIG_STACK_SIZE - QT_STKALIGN);
  return;
}

/* Return stacks to the free list only for local stacks */
void BR_mig_destroy_stack(at_thread_t *t){
  void* local_first, *local_last;
  void *sto;
  sto = t->sto;

  AT_SPINLOCK_LOCK(stacks_slck);
  if((stacks_local_first <= sto) && (sto <= stacks_local_last)){
    stacks_free_list_top--;
    stacks_free_list[stacks_free_list_top]=sto;
  }
  AT_SPINLOCK_UNLOCK(stacks_slck);
}



/*----------------------------------------------------------------------*/

/* 
 * Steal me a thread from cluster. As a sideffect ;-) performs termination
 * detection... 
 * Return values: 
 * BR_MIG_SUCCESS     - successful migration - a thread has been deposited
 * BR_MIG_IN_PROGRESS - another migration is already in progress
 * BR_MIG_FAILURE     - no threads could be obtained
 * BR_MIG_TERMINATION - no threads could be obtained, and  all 
 *                      clusters are idle (including the calling)
 */
int BR_steal(){
  at_thread_t *t;
  BR_cluster_t from;
  int res=BR_MIG_FAILURE;
  int i;
  

  if(at_mutex_trylock(&mig_started)==0){
    return BR_MIG_IN_PROGRESS;
  }
  
  for(i=1; i<BR_CLUSTERS(); i++){
    from =  (BR_HERE()+BR_CLUSTERS()-i)%BR_CLUSTERS();
    if((res=BR_steal_from(from))==BR_MIG_SUCCESS){
      /*      at_printf("[%d] Successful migration from [%d]\n", BR_HERE(), from);*/
      break;
    }
  }
  
  /* May have to trigger termination detection */
  /* Only one cluster does so... */
  if((res!=BR_MIG_SUCCESS) && (BR_HERE()==1)){
    if(BR_term_detection()){
      res = BR_MIG_TERMINATION;
    }
  }
  at_mutex_unlock(&mig_started);
  return res;
}

void BR_steal_term_loop(){
  int res;
  /* Entering a service section */
  BR_inc_service_forks();
  while(1){
    while((at_thread_count()-BR_service_forks())>=BR_PROCESSORS()){
      BR_POLL();
      at_yield();
    }
    
    res = BR_steal();
    if(res==BR_MIG_TERMINATION){
      BR_mig_finish_time();      
      BR_exit();
    }
    else {
      if (res==BR_MIG_SUCCESS){
	at_yield();
      }
    }
  }
}

void BR_is_idle_rep_h_2(BR_cluster_t dest, BR_word_t p, BR_word_t idle){
  int *ip = (int *)p;
  
  *ip = idle;
}

void BR_is_idle_req_h_1(BR_cluster_t src, BR_word_t addr){
  int idle;
  int work_threads;


  work_threads = at_thread_count() - BR_service_forks();
  idle = (work_threads==0) || ((work_threads==1) && BR_exit_reached());
  BR_REPLY_2(src, BR_is_idle_rep_h_2, addr, idle);
}

int BR_term_detection(){
  /* For now, go around the ring for a few times ;-) ... */
  int try, times, i;
  int work_threads;
  int remote_idle;
  int local_idle;

  times = 1;
  for(try=0; try<times; try++){
    for(i=1; i<BR_CLUSTERS(); i++){
      work_threads = at_thread_count() - BR_service_forks();
      local_idle = (work_threads==0) || 
	((work_threads==1) && BR_exit_reached());
      if(local_idle==0) {
	break;
      }
	
      remote_idle = -1;
      BR_REQUEST_1((BR_HERE()+i)%BR_CLUSTERS(), BR_is_idle_req_h_1, 
		   (BR_word_t)&remote_idle);
      while(remote_idle == -1){
	BR_POLL();
      }
		   
      if(!remote_idle) {
	break;
      }
      
      BR_POLL();
    }
    if(!(remote_idle&&local_idle)) {
      break;
    }
  }
  
  if(remote_idle&&local_idle){
    return 1;
  }
  else {
    return 0;
  }
}

/*--------------------------------------------------------------------*/
/* Directed steals                                                    */

/* Returns:
   BR_MIG_SUCCESS
   BR_MIG_IDLE  
   BR_MIG_FAILURE
   */
int BR_steal_from(BR_cluster_t from){
  at_mutex_t mx;
  int res;

  BR_POLL();
  at_mutex_init(&mx);
  at_mutex_lock(&mx);
  
  BR_FORK_2(from, BR_steal_req_h_2, (BR_word_t)&mx, (BR_word_t)&res);
  
  at_mutex_lock(&mx);
  return res;
}

void BR_steal_req_h_2(BR_cluster_t src, BR_word_t mx, BR_word_t addr){
  int res;
  int  local_idle, work_threads;

  BR_inc_service_forks();
  /* Push the thing to the source */
  if(BR_push_to(src)){

    work_threads = at_thread_count() - BR_service_forks();
    local_idle = (work_threads<=1) || 
      ((work_threads==2) && BR_exit_reached());    
    if(local_idle){
      res = BR_MIG_IDLE;
    }
    else {
      res = BR_MIG_FAILURE;
    }
  }
  else {
    /* Successful push */
    res = BR_MIG_SUCCESS;
  }
  BR_REQUEST_3(src, BR_steal_done_h_3, mx, addr, res);

  BR_dec_service_forks();
}

void BR_steal_done_h_3(BR_cluster_t from, BR_word_t m, BR_word_t p, 
		       BR_word_t res){
  at_mutex_t *mx;
  int *rp;

  mx = (at_mutex_t *)m;
  rp = (int *)p;
  
  *rp = res;
  at_mutex_unlock(mx);
}


/*--------------------------------------------------------------------*/
int BR_migrated_from(){
  return BR_mig_from;
}

int BR_migrated_to(){
  return BR_mig_to;
}

void BR_push_dir_mem_h(BR_cluster_t from, caddr_t addr, size_t s, void* arg){
  at_thread_t *t;

  /* First, get the new TCB */
  t = (at_thread_t *)at_p_pool_get((at_p_pool_t *)at_thread_pool);
  /* Initialize it propoperly */
  memcpy(t,addr, sizeof(at_thread_t));
  
  BR_mig_to++;
  BR_stack_received+=s;

  at_thread_count_inc();
  /* Add to the migration bundle by triggering the unblocked event */
  t->bundle = at_mig_bundle;
  /* Trigger the "unblocked" event explicitly */
  (*(at_mig_bundle->scheduler->thread_unblocked))(at_mig_bundle, 
						  (at_thread_t *)t);
}

/* 
 * This is called either directly (by BR_push_to()), or, it is registered
 * as a callback for BR_push_self_to() and is called by the Active Threads
 * on raising a certain scheduling event ...
 * 
 * Use asynchronous store to minimize overhead
 *
 */
void BR_push_handler(int to, at_thread_t *lt){
  void *first_stack_entry;
  int  current_stack_size; 
  void *where;
  
  /*----------------------------------------------------------------*/

  at_thread_count_dec();
  first_stack_entry = (lt->sto+MIG_STACK_SIZE);
  current_stack_size = (((int)first_stack_entry)-((int)lt->sp));
  
  where = lt->sp-sizeof(at_thread_t);
  memcpy(where, lt, sizeof(at_thread_t));
  BR_ASYNC_STORE(to, (caddr_t)where, (caddr_t)where, 
		 current_stack_size + sizeof(at_thread_t), 
		 (BR_handler_mem_t)BR_push_dir_mem_h, (BR_word_t)NULL,
		 (BR_handler_mem_t)BR_push_on_completion_h, (BR_word_t)NULL);

  at_p_pool_put((at_p_pool_t *)at_thread_pool, lt);
  BR_stack_pushed += current_stack_size;
  /*at_fprintf(stderr,"NULL THREAD SIZE %d\n",  current_stack_size + sizeof(at_thread_t)); */
  BR_push_self_stat.finish=gethrtime();
  BR_push_self_stat.total += (BR_push_self_stat.finish-
			      BR_push_self_stat.start);

  /* The protecting mutex is unlocked by the on_completion handler  */
  /*----------------------------------------------------------------*/  
}


void BR_push_on_completion_h(BR_cluster_t from, caddr_t addr, size_t s, 
			     void* arg){
  /* Return the local thread to the pool */
  BR_mig_from++;
  /*----------------------------------------------------------------*/  
}


int BR_push_to(BR_cluster_t to){
  at_thread_t *lt;

  /* Get a thread to push */
  lt = mig_thread_get(at_mig_bundle);
  if(lt){
    BR_push_handler(to, lt);
    return 0;
  }
  else {
    return 1;  /* Nothing to push... */
  }
}

void  BR_push_self_to(BR_cluster_t to){
  /* Trigger the bundle-specific event 1 that deal with migration */
  

  BR_push_self_stat.start=gethrtime();
  at_bundle_event1(to);  
}


static hrtime_t BR_start_time;
void BR_mig_start_time(){
  BR_start_time = gethrtime();
}

void BR_mig_finish_time(){
  hrtime_t finish;

  finish = gethrtime();
  at_printf("RUN TIME: %e\n", ((double)finish-BR_start_time)/1.0e+9);
}




