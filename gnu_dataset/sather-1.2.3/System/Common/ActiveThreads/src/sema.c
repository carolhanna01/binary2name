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

/* ***************************************************************************
 * Implementation of blocking semaphores for Active Threads                  *
 * written by:    Boris Weissman                                             *
 * on:            Feb 20, 1997                                               *
 *****************************************************************************/

#include "at-int.h"

extern volatile at_p_pool_t *at_sema_pool;

void *at_sema_block_help(qt_t *sp, void *old, void *blockq);

/* 
 * This is a very simple implementation of locking. The spinlock is kept over
 * the context switch...
 */

void at_sema_wait(at_sema_t *s){
  at_thread_t *old, *next;

  AT_SPINLOCK_LOCK(s->slck);

  while(s->counter==0) {
    /* The current thread goes to sleep. Find the successor */
    next = at_get_next();
    
    old = AT_GET_CURRENT;
    old->state = AT_BLOCKED;
    
    /* s->slck is unlocked by at_sema_block_help */
    at_block(at_sema_block_help, old, (void *)s, next);

    /* 
     * The threads is waken up here by Sema signal. It tries to
     * repeat the sequence and needs to reaquire the base spinlock
     */
    AT_SPINLOCK_LOCK(s->slck);
  }
  
  /* OK, the sema is ours! */
  s->counter--;
  AT_SPINLOCK_UNLOCK(s->slck);  
}

/* 
 * A nonblocking version of at_sema_wait. Returns 1 if the sema is 
 * taken, 0 otherwise.
 */

int at_sema_trywait(at_sema_t *s){

  AT_SPINLOCK_LOCK(s->slck);

  if(s->counter==0) {
    /* The sema is already locked */
    AT_SPINLOCK_UNLOCK(s->slck);
    return 0;
  }
  
  /* OK, the sema is ours! */
  s->counter--;
  AT_SPINLOCK_UNLOCK(s->slck);
  return 1;
}

void* at_sema_block_help(qt_t *sp, void *old, void *sema){
  /* 
   * Put the blocking thread at the end of the queue to get FIFO
   * behavior
   *
   * For now, simply scan to the end of the queue
   */
  at_sema_t *s;
  at_thread_t **t;   			
  at_thread_t *old_t = (at_thread_t *)old;
  at_bundle_t *old_b;

  old_t->sp = sp;  /* Record the current sp */

  /* Put at the tail of the sleepers queue */
  old_t->next = (at_thread_t *)NULL;
  s = (at_sema_t *)sema;
  t = &s->sleepers;
  while(*t){
    t = &(*t)->next;
  }
  *t = old_t;

  /* Unlock the base sema */
  AT_SPINLOCK_UNLOCK(s->slck);

  /* Signal a "thread blocked" event to the appropriate bundle */
  old_b = old_t->bundle;
  (*(old_b->scheduler->thread_blocked))(old_b, old_t);

  /* return garbage */
}


void at_sema_signal(at_sema_t *s){
  at_thread_t *t=NULL;
  at_bundle_t *b;

  AT_SPINLOCK_LOCK(s->slck);
  s->counter++;

  if((s->sleepers)){
    t = s->sleepers;
    s->sleepers = t->next;
  }
  /* Unlock the base sema */
  AT_SPINLOCK_UNLOCK(s->slck);

  /* Put the thread on the proper run queue, if necessary */
  if(t){
    b = t->bundle;
    (*(b->scheduler->thread_unblocked))(b, (at_thread_t *)t);
  }
  return;
}

at_sema_t* at_sema_create(int counter){
  at_sema_t *s;
  
  s = (at_sema_t *) at_p_pool_get((at_p_pool_t *)at_sema_pool);

  s->counter = counter;
  s->sleepers = (at_thread_t *)NULL;
  AT_SPINLOCK_INIT(s->slck);
  
  return s;
}

void at_sema_init(at_sema_t *s, int counter){
  s->counter = counter;
  s->sleepers = (at_thread_t *)NULL;
  AT_SPINLOCK_INIT(s->slck);
}

void at_sema_destroy(at_sema_t *s){
  at_p_pool_put((at_p_pool_t *)at_sema_pool, (void *)s);
}

void at_sema_print(at_sema_t *s){
  at_thread_t *t;
  
  at_printf("Sema id:%p :  ", s);
  t = s->sleepers;
  while(t){
    at_printf("[%d] ", t->id);
    t = t->next;
  }
  at_printf("\n");
}



