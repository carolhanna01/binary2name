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
 * Implementation of general pool data structure for Active Threads          *
 * written by:    Boris Weissman                                             *
 * on:            Feb 20, 1997                                               *
 *****************************************************************************/

#include <stdio.h>
#include <stdlib.h> 

#include "pool.h"
#include "wrappers.h"

/* 
 * A very simple implementation of a pool orginized as a FIFO queue.
 * If the queue is empty, a new chunk is of queue elements is allocated.
 *
 * The queue guarantees to call a client supplied function on all
 * newly created queue elements in the order in which they are
 * eventually given to the client. It could be used to initialize
 * client datastructures (the data field in at_pool_elt_t)
 *
 * Fifo behavior is beneficial, for instance, in implementing pools of
 * threads. Address space for stacks could be reserved without 
 * allocating any swap memory and will stay this way unless the threads
 * are actually used. Thus, we can afford fairly large pools pf threads.
 *
 * Size passed at creating is also used as a block size for following
 * allocations if necessary.
 *
 * Some of the Pool interface functions are inline and could be found in
 * pool.h (need this fpr proper inlining)
 *
 * get, put operations are thread safe. Spinlocks are used since the
 * the critical sections are extremely small.
 *
 *
 * For inlining to work properly with GCC, we need to use at least -O
 * 
 */



/* Allocate the block of elements and set links properly */
/* size must be > 0. The pool must be already locked. */
at_pool_elt_t *at_pool_create_chunk(int size, int elt_size){
  at_pool_elt_t *h, *e;
  int i;

  h = (at_pool_elt_t *)at_malloc(elt_size*size);
  e = h;
  /* Link the queue elements together */
  for(i=0; i<size-1; i++){
    e->next = (at_pool_elt_t *)((char *)e+elt_size);
    e = e->next;
  }
  e->next = NULL;
  return h;
}


at_pool_t* at_pool_create(int size, int elt_size){
  at_pool_t *p;

  p = (at_pool_t *)at_malloc(sizeof(at_pool_t));
  p->head = at_pool_create_chunk(size, elt_size);
  p->chunk_size = size;
  p->size = size;
  p->elt_size = elt_size;
  AT_POOL_LOCK_INIT(p);

  return p;
}


/* Since it may be called only by at_pool_get, the at_pool lock is taken already */
void at_pool_expand(at_pool_t *p){
  p->head = at_pool_create_chunk(p->chunk_size, p->elt_size);
  p->size += p->chunk_size;
}


void at_pool_destroy(at_pool_t *p){
  at_free(p);
}












