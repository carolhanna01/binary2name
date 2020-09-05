/*------------------------->  ANSI C - headerfile  <-------------------------*/
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

#ifndef _POOL_H_
#define _POOL_H_

#include <stdio.h>
#include <stdlib.h>
#ifdef AT_DEBUG
#include <assert.h>
#endif

#include "spinlock.h"  /* Needed for locks */
#include "wrappers.h"

#if defined(__GNUC__)
  #ifndef AT_INLINE
     #ifndef AT_NO_INLINE
        #define AT_INLINE inline extern 
     #else
        /* This can be used for debugging - eliminates inlining */
        #define AT_INLINE static
     #endif
  #endif
#else
  #define AT_INLINE 
#endif

/* All structures that want to be pooled must have "next" as first field */
typedef struct at_pool_elt{
  struct at_pool_elt *next;
  /* Element-specific stuff will be here */
} at_pool_elt_t;

typedef struct {
  at_spinlock_t slck;         /* Used for internal synchronization */
  at_pool_elt_t *head;        /* points to available elements */
  int size;                   /* current queue size */
  int chunk_size;             /* block size for doubling */
  int elt_size;               /* element size */
} at_pool_t;


/* 
 * Pool external interface, includes all inline functions 
 * All interface functions are properly synchronized 
 */

at_pool_t* at_pool_create(int size, int elt_size);
void at_pool_destroy(at_pool_t *p);


/* Some stuff used internally */
at_pool_elt_t *at_pool_create_chunk(int size, int elt_size);
void at_pool_expand(at_pool_t *p);


#define AT_POOL_LOCK_INIT(p)  AT_SPINLOCK_INIT(p->slck)
#define AT_POOL_LOCK(p)       AT_SPINLOCK_LOCK(p->slck) 
#define AT_POOL_UNLOCK(p)     AT_SPINLOCK_UNLOCK(p->slck) 

/* 
 * The following functions are inline and must be in the header file for
 * proper inline substitution.
 */

/*
 * Aply the func to every element in the queue starting with current
 * elt must not be NULL 
 * This one is not sychnronized, Proper sycnronization must be assured
 * at the outer level(s)
 *
 * This one is normally called from within functions holding the pool lock
 * If it is used, for example, for a client-defined printing routine,
 * proper pool sycnronization must be ensured on the outside.
 */

AT_INLINE void at_pool_apply(at_pool_t *p, void (*func)(void *d)) {
  at_pool_elt_t *e;
  if(func){
    e = p->head;
    while (e) {
      func((void *)e);
      e = e->next;
    } 
  }
}

AT_INLINE void *at_pool_get(at_pool_t *pool){
  at_pool_elt_t *e;

  AT_POOL_LOCK(pool);

  e = pool->head;
  if (!e) {
    /*    at_printf("Need a new chunk!\n");*/
    at_pool_expand(pool);
  }
#ifdef AT_DEBUG
  assert(pool->head);
#endif
  e = pool->head;
  pool->head = e->next;
  pool->size--;
  e->next = NULL;

  AT_POOL_UNLOCK(pool);

  return (void *)e;
}

/* synhcronized */
AT_INLINE void at_pool_put(at_pool_t *p, void *elt){
  at_pool_elt_t *e;

  e = (at_pool_elt_t *)elt;

  AT_POOL_LOCK(p);

  e->next = p->head;
  p->head = e;
  p->size++;

  AT_POOL_UNLOCK(p);
}

/* Append a list "e" of size s to a pool */
AT_INLINE void at_pool_init(at_pool_t *p, at_pool_elt_t *e, int s){
#ifdef AT_DEBUG
/*  assert(pool->head==NULL); */
#endif
  p->head = e;
  p->size = s;
}

/* If the pool is empty, return void instead of allocating */
AT_INLINE void *at_pool_get_no_alloc(at_pool_t *pool){
  at_pool_elt_t *e;

  AT_POOL_LOCK(pool);
  e = pool->head;
  if(e){
    pool->head = e->next;
    pool->size--;
    e->next = NULL;
  }
  AT_POOL_UNLOCK(pool);
  return e;
}

/* Split the pool if the current number of elements is at least 
   twice larger than chunk size */
AT_INLINE void *at_pool_split(at_pool_t *p, int *res_size){
  at_pool_elt_t *e;
  at_pool_elt_t *r=NULL;
  int i, s;
  
  AT_POOL_LOCK(p);
  if(p->size >= p->chunk_size*2){
    e = p->head;
    s = p->size/2;
    for(i=0; i<s; i++){
      e = e->next;
    }
    r = e->next;
    e->next = NULL;
    *res_size = p->size-s;
    p->size = s;
  }
  AT_POOL_UNLOCK(p);
  return r;
}
/* 
 * These are not syncronized since the size may change after the function
 * is returned, but before it is used. Use the locking macros
 * on the outside and call at_pool_size if you want to make sure that size
 * does not change
 */
AT_INLINE int at_pool_size(at_pool_t *p){
  return p->size;
}

AT_INLINE int at_pool_is_empty(at_pool_t *p){
  return p->size=0;
}


#endif  /* _POOL_H_ */







