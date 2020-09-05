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

/*
** This is the Active Thread implementation for Brahma synchronization and 
** thread manipulation primitives. 
** This file could be used by various Active Thread based Brahma platforms
** (TCP/IP, Myrinet, Meiko, etc.)
*/

#include "at.h"
#include "at_sync.h"
#include <stdio.h>


/* Things necessary to determine the number of procs for a cluster */

#include <sys/time.h>
#include <assert.h>
#include <stdlib.h>
#include <string.h>




#define BR_ID_CLUSTER_SIZE 6
#define BR_ID_CLUSTER_MASK 0xFC000000
#define BR_ID_THREAD_MASK (~BR_ID_CLUSTER_MASK)
#define BR_ID_CLUSTER_SHIFT (32-BR_ID_CLUSTER_SIZE)
#define BR_ID_CLUSTER_BITS(n) ((unsigned int)(n)<<BR_ID_CLUSTER_SHIFT)
#define BR_ID_CLUSTER(p) (((unsigned int)(p)&BR_ID_CLUSTER_MASK)>>BR_ID_CLUSTER_SHIFT)
#define BR_ID_THREAD(p) ((unsigned int)(p)&BR_ID_THREAD_MASK)
#define BR_MAKE_THREAD_ID(cl,p) (unsigned int)(((cl)<<BR_ID_CLUSTER_SHIFT)|(unsigned  int)p)

/*
** Create human-readable form of the thread id in the buffer "buf",
** for debugging.
*/
char *BR_ascii_id(BR_thread_t id, char *buf, size_t maxlen) {
  sprintf(buf,"%02d-%-4d",BR_ID_THREAD(id.t), BR_ID_CLUSTER(id.t));
  return buf;
}


/* ID of executing thread */
BR_thread_t BR_THREAD_ID() {
   BR_thread_t t;
   at_thread_t *at = at_self();
   t.t = BR_MAKE_THREAD_ID(BR_HERE(), at->id);
   return t;
}

/* Returns a value which can never be an id of an actual thread, to use as
a sentinel value. */

BR_thread_t BR_INVALID_ID() {
   BR_thread_t t;
   t.t = (unsigned int )-1;
   return t;
}

/*
** Useful handler for unlocking and signalling on remote nodes.
** (The second arg will have to be cast to word to use.)
*/
void BR_unlock_handler(BR_cluster_t ignored, BR_word_t lock) {
   BR_UNLOCK((BR_lock_t)lock);
}

void BR_signal_handler(BR_cluster_t ignored, BR_word_t sema) {
   BR_SIGNAL((BR_sema_t)sema);
}

void BR_signal_mem_handler(BR_cluster_t from, caddr_t a, size_t size, caddr_t sema) {
   BR_SIGNAL((BR_sema_t)sema);
}

typedef struct delay_args_struct {
   clock_t awake;
   BR_delay_handler_t func;
   void *arg;
} *delay_args;

/* Don't execute until given time arrives */
void BR_sleep_then_exec(at_word_t arg) {
   delay_args a = (delay_args) arg;
   while (clock() < a->awake) at_yield();
   ((BR_delay_handler_t) a->func)(a->arg);
   at_free(a);
}

/*
** Asyncronously call a function after at least n.sec seconds and
** n.nsec nano seconds, with arg as argument.  May or may not be
** executed by the thread that called "BR_delay_function".
*/
void BR_delay_function(BR_delay_t n, BR_delay_handler_t func, void *arg) {
   at_thread_t *res;
   delay_args a;
   clock_t awake = clock() 
		    + ((clock_t) n.nsec/1000.0) 
		    + (((clock_t) n.sec) * 1000000);
   a = at_malloc(sizeof(struct delay_args_struct));

   a->awake = awake;
   a->func = func;
   a->arg = arg;
   while (1) {
     /* If overloading the system, slow down and try again. */
     res = at_create_1(at_get_focus(), AT_UNBOUND, BR_sleep_then_exec, (at_word_t)a);
     if (res) break;
     at_yield();
   }
}












