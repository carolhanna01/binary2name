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

#ifndef _BUNDLE_H_
#define _BUNDLE_H_

#include "at-int.h"

typedef struct at_bundle {
  struct at_scheduler *scheduler;
  struct at_bundle *next;    /* Next bundle - eases linking */
  at_spinlock_t slck;  /* multi-purpose lock */
  void *state;
} at_bundle_t;


/* Each scheduler must provide event handler for all scheduling events */
typedef struct at_scheduler {
  void (*thread_created)(at_bundle_t *b, at_thread_t *t);
  void (*thread_terminated)(at_bundle_t *b, at_thread_t *t);
  void (*thread_started)(at_bundle_t *b, at_thread_t *t);
  void (*thread_blocked)(at_bundle_t *b, at_thread_t *t);
  void (*thread_unblocked)(at_bundle_t *b, at_thread_t *t);
  void (*bundle_created)(at_bundle_t *parent, at_bundle_t *b);
  void (*bundle_terminated)(at_bundle_t *parent, at_bundle_t *b);
  void (*processor_idle)(at_bundle_t *b, int proc);
  void (*bundle_event1)(at_bundle_t *b, at_thread_t *t, at_word_t arg);
} at_scheduler_t;




#endif /* _BUNDLE_H */
