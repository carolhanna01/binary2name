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

#include "at-int.h"
#include "bundle_queue.h"

void at_bundle_queue_put (at_bundle_queue_t *q, at_bundle_t *t)
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

void at_bundle_queue_put_at_head (at_bundle_queue_t *q, at_bundle_t *t)
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
void at_bundle_queue_init (at_bundle_queue_t *q)
{
  q->head = q->tail = NULL;
  AT_SPINLOCK_INIT(q->lck);
}


at_bundle_t *at_bundle_queue_get (at_bundle_queue_t *q)
{
  at_bundle_t *t;

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

void at_bundle_queue_remove(at_bundle_queue_t *q, at_bundle_t *b){
  at_bundle_t *c, *p;
  AT_SPINLOCK_LOCK(q->lck);
  
  c = q->head;
  p = c;
  if(c==b){
    q->head = c->next;
  }
  else {
    while(c){
      if(c==b){
	break;
      }
      c=c->next;
      p=c;
    }
    /* remove it */
    p=b->next;
  }
  AT_SPINLOCK_UNLOCK(q->lck);
}
