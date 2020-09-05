/*------------------------->  ANSI C - sourcefile  <-------------------------*/
/* Copyright (C) 1997 by International Computer Science Institute            */
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
 * Implementation of blocking mutexes for Active Threads                     *
 * written by:    Boris Weissman                                             *
 * on:            Feb 20, 1997                                               *
 *****************************************************************************/

#include "at-int.h"

extern volatile at_p_pool_t *at_mutex_pool;

void *at_mutex_block_help(qt_t *sp, void *old, void *blockq);

/* 
 * This is a very simple implementation of locking. The spinlock is kept over
 * the context switch...
 */

void at_mutex_lock(at_mutex_t *m){
  at_thread_t *old, *next;

  AT_SPINLOCK_LOCK(m->slck);

  while(m->owner) {
    /* The current thread goes to sleep. Find the successor */
    next = at_get_next();
    
    old = AT_GET_CURRENT;
    old->state = AT_BLOCKED;
    
    /* m->slck is unlocked by at_mutex_block_help */
    at_block(at_mutex_block_help, old, (void *)m, next);

    /* 
     * The threads is waken up here by Mutex unlock. It tries to
     * repeat the sequence and needs to reaquire the base spinlock
     */
    AT_SPINLOCK_LOCK(m->slck);
  }
  
  /* OK, the mutex is ours! */
  m->owner = AT_GET_CURRENT;
  AT_SPINLOCK_UNLOCK(m->slck);  
}

/* 
 * A nonblocking version of at_mutex_lock. Returns 1 if the lock is 
 * taken, 0 otherwise.
 */

int at_mutex_trylock(at_mutex_t *m){

  AT_SPINLOCK_LOCK(m->slck);

  if(m->owner) {
    /* The lock is already locked */
    AT_SPINLOCK_UNLOCK(m->slck);
    return 0;
  }
  
  /* OK, the mutex is ours! */
  m->owner = AT_GET_CURRENT;
  AT_SPINLOCK_UNLOCK(m->slck);
  return 1;
}

void* at_mutex_block_help(qt_t *sp, void *old, void *mx){
  /* 
   * Put the blocking thread at the end of the queue to get FIFO
   * behavior
   *
   * For now, simply scan to the end of the queue
   */
  at_mutex_t *m;
  at_thread_t * volatile *t;
  at_thread_t *old_t = (at_thread_t *)old;
  at_bundle_t *old_b;

  old_t->sp = sp;  /* Record the current sp */

  /* Put at the tail of the sleepers queue */
  old_t->next = (at_thread_t *)NULL;
  m = (at_mutex_t *)mx;
  t = &m->sleepers;
  while(*t){
    t = &(*t)->next;
  }
  *t = old_t;

  /* Unlock the base mutex */
  AT_SPINLOCK_UNLOCK(m->slck);

  /* Signal a "thread blocked" event to the appropriate bundle */
  old_b = old_t->bundle;
  (*(old_b->scheduler->thread_blocked))(old_b, old_t);
  /* return garbage */
}


void at_mutex_unlock(at_mutex_t *m){
  volatile at_thread_t *t=NULL;
  at_bundle_t *b;

  AT_SPINLOCK_LOCK(m->slck);
  /* clear the lock */
  m->owner = NULL;
  if(m->sleepers){
    t = m->sleepers;
    m->sleepers = t->next;
  }
  AT_SPINLOCK_UNLOCK(m->slck);

  /* Put the thread on the proper run queue, if necessary */
  if(t){
    b = t->bundle;
    (*(b->scheduler->thread_unblocked))(b, (at_thread_t *)t);
  }
  return;
}

at_mutex_t* at_mutex_create(){
  at_mutex_t *m;
  
  m = (at_mutex_t *) at_p_pool_get((at_p_pool_t *)at_mutex_pool);

  m->owner = (at_thread_t *)NULL;
  m->sleepers = (at_thread_t *)NULL;
  AT_SPINLOCK_INIT(m->slck);
  
  return m;
}

void at_mutex_init(at_mutex_t *m){
  m->owner = (at_thread_t *)NULL;
  m->sleepers = (at_thread_t *)NULL;
  AT_SPINLOCK_INIT(m->slck);
}

void at_mutex_destroy(at_mutex_t *m){
  at_p_pool_put((at_p_pool_t *)at_mutex_pool, (void *)m);
}

void at_mutex_print(at_mutex_t *m){
  at_thread_t *t;
  
  at_printf("Mutex id:%p :  ", m);
  t = m->sleepers;
  while(t){
    at_printf("[%d] ", t->id);
    t = t->next;
  }
  at_printf("\n");
}

