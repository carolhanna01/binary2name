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
 * Implementation of condition variables for Active Threads                  *
 * written by:    Boris Weissman                                             *
 * on:            Sep 2, 1997                                                *
 *****************************************************************************/

#include "cond.h"
#include "at-int.h"

extern volatile at_p_pool_t *at_cond_pool;

void *at_cond_block_help(qt_t *sp, void *old, void *blockq);

/* 
 * This is a very simple implementation of locking. The spinlock is kept over
 * the context switch...
 */

void at_cond_wait(at_cond_t *c, at_mutex_t *mx){
  at_thread_t *old, *next;

  /* No need to acquire the mutex - it is already held by the thread */
  /* This must be ensured by the user - such is the semantics of cond. vars*/

  /* The current thread goes to sleep. Find the successor */
  next = at_get_next();
  
  old = AT_GET_CURRENT;
  old->state = AT_BLOCKED;
  
  c->mx = mx;
  /* c->mx is unlocked by at_mutex_block_help */
  at_block(at_cond_block_help, old, (void *)c, next);
  
  /* 
   * The threads is waken up here by cond signal. It tries to
   * repeat the sequence and needs to reaquire the base mutex
   */
  at_mutex_lock(mx);
}

void* at_cond_block_help(qt_t *sp, void *old, void *cond){
  /* 
   * Put the blocking thread at the end of the queue to get FIFO
   * behavior
   *
   * For now, simply scan to the end of the queue
   *
   * After we are done, we need to unlock the base mutex for our 
   * condition variable
   */
  at_cond_t *c;
  at_thread_t * volatile *t;
  at_thread_t *old_t = (at_thread_t *)old;
  at_bundle_t *old_b;

  old_t->sp = sp;  /* Record the current sp */

  /* Put at the tail of the sleepers queue */
  old_t->next = (at_thread_t *)NULL;
  c = (at_cond_t *)cond;
  t = &c->sleepers;
  while(*t){
    t = &(*t)->next;
  }
  *t = old_t;

  /* Unlock the base mutex */
  at_mutex_unlock(c->mx);

  /* Signal a "thread blocked" event to the appropriate bundle */
  old_b = old_t->bundle;
  (*(old_b->scheduler->thread_blocked))(old_b, old_t);
  /* return garbage */
}


void at_cond_signal(at_cond_t *c){
  volatile at_thread_t *t=NULL;
  at_bundle_t *b;

  if(c->sleepers){
    t = c->sleepers;
    c->sleepers = t->next;
  }

  /* Put the thread on the proper run queue, if necessary */
  if(t){
    b = t->bundle;
    (*(b->scheduler->thread_unblocked))(b, (at_thread_t *)t);
  }
  return;
}

void at_cond_broadcast(at_cond_t *c){
  volatile at_thread_t *t=NULL;
  at_bundle_t *b;

  t = c->sleepers;

  while(t){
    b = t->bundle;
    (*(b->scheduler->thread_unblocked))(b, (at_thread_t *)t);
    t = t->next;
  }

  return;
}

at_cond_t* at_cond_create(){
  at_cond_t *c;
  
  c = (at_cond_t *) at_p_pool_get((at_p_pool_t *)at_cond_pool);

  c->sleepers = (at_thread_t *)NULL;
  c->mx = (at_mutex_t *)NULL;
  
  return c;
}

void at_cond_init(at_cond_t *c){
  c->sleepers = (at_thread_t *)NULL;
  c->mx = (at_mutex_t *)NULL;
}

void at_cond_destroy(at_cond_t *c){
  at_p_pool_put((at_p_pool_t *)at_cond_pool, (void *)c);
}

