/*------------------------->  ANSI C - sourcefile  <-------------------------*/
/* Copyright (C) 1998 by International Computer Science Institute            */
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
 * thread_queue.c                                                            *
 * written by:    Boris Weissman                                             *
 * on:            Feb 28, 1997                                               *
 *****************************************************************************/

#include <math.h>
#include "at-int.h"
#include "thread_dqueue.h"

#define DQUEUE_NOCHECKS

#ifdef DQUEUE_NOCHECKS
#define at_dqueue_sanity_check(x,y,z)
#endif

/* Doubly linked queue */
void at_thread_dqueue_put (at_thread_dqueue_t *dq, at_thread_t *t)
{
  at_dqueue_sanity_check(dq, t, "put start");

  t->next = NULL;
  
  if (dq->tail==NULL){
    dq->tail = t;
    dq->head = t;
    t->prev = NULL;
  }
  else {
    t->prev = dq->tail;
    dq->tail->next = t;
    dq->tail = t;
  }

  dq->size++;
  at_dqueue_sanity_check(dq, t, "put end");
}

void at_thread_dqueue_put_at_head (at_thread_dqueue_t *dq, at_thread_t *t)
{
  at_dqueue_sanity_check(dq, t, "put_at_head start");

  if(dq->tail==NULL){
    dq->tail=t;
  }
  t->next = dq->head;
  t->prev = NULL;
  if(dq->head){
    dq->head->prev = t;
  }
  dq->head = t;

  dq->size++;
  at_dqueue_sanity_check(dq, t, "put_at_head end");
}

/* This is not thread safe. Must be called in a safe setting */
void at_thread_dqueue_init (at_thread_dqueue_t *dq)
{
  dq->head = dq->tail = NULL;
  dq->size = 0;
}


at_thread_t *at_thread_dqueue_get (at_thread_dqueue_t *dq)
{
  at_thread_t *t;

  at_dqueue_sanity_check(dq, NULL, "get start");

  if(dq->head==NULL){
    t = NULL;
  }
  else{
    t = dq->head;
    dq->head = t->next;
    /* is empty now? */
    if(dq->head==NULL){ 
      dq->tail=NULL;
    }
    else {
      dq->head->prev = NULL;
    }
    dq->size--;
  }
  at_dqueue_sanity_check(dq, t, "get end");
  return t;
}

/* Get a random queue element */
at_thread_t *at_thread_dqueue_get_rand(at_thread_dqueue_t *dq)
{
  int i, index;
  at_thread_t *t;

  if(dq->head== NULL){
    return NULL;
  }
  index = floor(drand48()*dq->size);
  t = dq->head;
  for(i=0; i<index; i++){
    t = t->next;
  }
  at_thread_dqueue_delete(dq, t);
  return t;
}

/* Delete a specified thread from the queue */
void at_thread_dqueue_delete(at_thread_dqueue_t *dq, at_thread_t *t){
  at_dqueue_sanity_check(dq, t, "delete start");
  if(dq->head == t){
    dq->head = t->next;
  }
  if(dq->tail == t){
    dq->tail = t->prev;
  }
  if(t->prev != NULL){
    t->prev->next = t->next;
  }
  if(t->next != NULL){
    t->next->prev = t->prev;
  }

  at_dqueue_sanity_check(dq, t, "delete end");
  t->next = t->prev = NULL;
  dq->size--;
  return;
}

#ifndef DQUEUE_NOCHECKS
/* A sanity check - walk the queue forwards and backwards 
   and compare things */
void at_dqueue_sanity_check(at_thread_dqueue_t *dq, at_thread_t *tt, char *str){
  at_thread_t *t, *pt;
  int n, m;

  if(dq->tail && (dq->tail->next != NULL)){
    at_printf("(%s) DOUBLE QUEUE BROKEN: dq->tail->next != NULL\n", str);
    abort();
  }

  if(dq->head && (dq->head->prev != NULL)){
    at_printf("(%s) DOUBLE QUEUE BROKEN: dq->head->prev != NULL\n", str);
    abort();
  }

  t = dq->head;
  pt = t;
  n = 0;

  while(t){
    pt = t;
    t=t->next;
    n++;
  }
  if(pt != dq->tail){
    at_printf("(%s) DOUBLE QUEUE BROKEN: dq->tail is wrong\n", str);
    abort();
  }


  t = dq->tail;
  pt = t;
  m = 0;

  while(t){
    pt = t;
    t=t->prev;
    m++;
  }
  if(pt != dq->head){
    at_printf("(%s) DOUBLE QUEUE BROKEN: dq->head is wrong\n", str);
    abort();
  }

  if(n!=m){
    at_printf("(%s) DOUBLE QUEUE BROKEN: n != m\n", str);
    abort();
  }

  at_printf("%20s   ok,   size: %d    t: %x\n", str, n, tt);
  
}

#endif
