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

#ifndef _RW_H_
#define _RW_H_

#include "spinlock.h"
#include "thread.h"
/*------------------------------------------------------------------*/
/*                         Reader/Write Locks                       */
/*------------------------------------------------------------------*/

/* For pooling, it is important for next to be the first attribute */
typedef struct at_rw {
  struct at_rw *next;
  at_thread_t*  rd_sleepers; /* Reader threads waiting on the lock */
  at_thread_t*  wr_sleepers; /* Writer Threads waiting on the lock */
  at_thread_t*  wr_owner;  /* NULL, if the lock is free. Otherwise the owner */
  int           rd_counter;
  int           wr_counter;
  at_spinlock_t slck;     /* Base spinlock: guards all operations on Mutexes */
} at_rw_t;

/* External interface */
at_rw_t *at_rw_create();
void at_rw_destroy(at_rw_t *);
void at_rw_rd_lock(at_rw_t *);
void at_rw_rd_unlock(at_rw_t *);
void at_rw_lock(at_rw_t *);
int at_rw_rd_trylock(at_rw_t *rw);
int at_rw_wr_trylock(at_rw_t *rw);
void at_rw_init(at_rw_t *rq);


/* Debugging/internal help */
void at_rw_print(at_rw_t *rw);


#endif /* _RW_H_ */

