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
 * Active Threads inline functions                                           *
 * written by:    Boris Weissman                                             *
 * on:            May 15, 1997                                               *
 *****************************************************************************/

#include "at-int.h"
#include "thread_queue.h"

void at_thread_queue_put (at_thread_queue_t *q, at_thread_t *t)
{
  AT_SPINLOCK_LOCK(q->lck); 
  t->next = NULL;
  
  if (q->tail==NULL){
    q->tail = t;
    q->head = t;
  }
  else {
    q->tail->next = t;
    q->tail = t;
  }
  AT_SPINLOCK_UNLOCK(q->lck); 
}

void at_thread_queue_put_at_head (at_thread_queue_t *q, at_thread_t *t)
{
  AT_SPINLOCK_LOCK(q->lck);
  if(q->tail==NULL){
    q->tail=t;
  }
  t->next = q->head;
  q->head = t;
  AT_SPINLOCK_UNLOCK(q->lck); 
}

/* This is not thread safe. Must be called in a safe setting */
void at_thread_queue_init (at_thread_queue_t *q)
{
  q->head = q->tail = NULL;
  AT_SPINLOCK_INIT(q->lck);

}


at_thread_t *at_thread_queue_get (at_thread_queue_t *q)
{
  at_thread_t *t;

  AT_SPINLOCK_LOCK(q->lck);
  if(q->head==NULL){
    t = NULL;
  }
  else{
    t = q->head;
    q->head = t->next;
    /* is empty now? */
    if(q->head==NULL) q->tail=NULL;
  }
  AT_SPINLOCK_UNLOCK(q->lck);
  return t;
}


void at_thread_schedule_all(at_thread_queue_t *q)
{
  /* Schedule all threads on the queue to run on corresponding processors */
  at_thread_t *t;

  AT_SPINLOCK_LOCK(q->lck);
  t = q->head;
  while(t){
    at_queue_put(AT_WHICH_QUEUE(t), t);
    t = t->next;
  }
  q->head = q->tail = NULL;
  AT_SPINLOCK_UNLOCK(q->lck);
}



