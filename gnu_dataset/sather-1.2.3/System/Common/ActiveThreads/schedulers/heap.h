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

#ifndef _HEAP_H_
#define _HEAP_H_

#include <limits.h>
#include "thread.h"
#include "spinlock.h"
#include "thread_dqueue.h"

#define AT_HEAPSIZE 40

#define AT_HEAP_ON_QUEUE -1
#define AT_HEAP_ABSENT   -2

#define AT_HEAP_IS_ON_HEAP(t) ((t)->heap_index>=1)
#define AT_HEAP_IS_ON_QUEUE(t) ((t)->heap_index==AT_HEAP_ON_QUEUE)
#define AT_HEAP_IS_ABSENT(t) ((t)->heap_index==AT_HEAP_ABSENT)


typedef struct at_theap {
  at_spinlock_t lck;  /* heap guard */
  at_thread_t  **heap_threads;
  int size;
  int asize;
  int threshold;
  at_thread_dqueue_t *global_threads; /* A doubly linked overflow queue */
} at_heap_t;

at_heap_t *at_heap_create();
int at_heap_insert(at_heap_t *h,  at_thread_t *t);
int at_heap_up(at_heap_t *h, int k);
at_thread_t *at_heap_top(at_heap_t *h);
int at_heap_down(at_heap_t *h, int k);
int at_heap_change_p(at_heap_t *h, at_thread_t *t, float new_p);
at_thread_t *at_heap_bottom(at_heap_t *h);

#endif
