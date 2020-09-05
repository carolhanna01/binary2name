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

#ifndef _COND_H_
#define _COND_H_

#include "spinlock.h"
#include "mutex.h"
#include "thread.h"
/*------------------------------------------------------------------*/
/*                       Condition Variable                         */
/*------------------------------------------------------------------*/
/*------------------------------------------------------------------*/

/* For pooling, it is important for next to be the first attribute */
typedef struct at_cond {
  struct at_cond *next;   /* misc. linking */
  at_mutex_t *mx;         /* base mutex - stored here every now and then ... */
  at_thread_t*  sleepers; /* Threads waiting on the cond. variable */
  int pad;
} at_cond_t;

/* External interface */
at_cond_t *at_cond_create();
void at_cond_destroy(at_cond_t *);
void at_cond_wait(at_cond_t *c, at_mutex_t *mx);
void at_cond_signal(at_cond_t *);
void at_cond_broadcast(at_cond_t *);
void at_cond_init(at_cond_t *c);

#endif /* _COND_H_ */

