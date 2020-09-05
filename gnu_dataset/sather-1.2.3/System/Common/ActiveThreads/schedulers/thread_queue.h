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

#ifndef _THREAD_QUEUE_H_
#define _THREAD_QUEUE_H_

#include "thread.h"
#include "spinlock.h"

typedef struct at_thread_queue_t {
  at_thread_t *head;
  at_thread_t *tail;
  
  at_spinlock_t lck;  /* Protects the queue */
} at_thread_queue_t;

extern void at_thread_queue_init(at_thread_queue_t *q);
extern void at_thread_queue_put(at_thread_queue_t *q, at_thread_t *t);
extern void at_thread_queue_put_at_head(at_thread_queue_t *q,at_thread_t *t);
extern at_thread_t *at_thread_queue_get(at_thread_queue_t *q);

#endif /* _THREAD_QUEUE_H_ */
