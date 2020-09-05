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

#include<stdio.h>
#include<malloc.h>
#include "at.h"
#include "at_c_locks.h"

/* Prototypes of static functions */

/* Mutex */
static int at_mutex_c_lock_acquirable_at_all(at_generic_c_lock_t*);
static int at_mutex_c_lock_acquirable(at_generic_c_lock_t*,at_thread_id);
static void at_mutex_c_lock_acquire(at_generic_c_lock_t*,at_thread_id);
static void at_mutex_c_lock_release(at_generic_c_lock_t*,at_thread_id);

/* Reader */
static at_reader_c_lock_t* at_reader_c_lock_create(at_writer_c_lock_t*);
static void at_reader_c_lock_init(at_reader_c_lock_t*,at_writer_c_lock_t*);
static int at_reader_c_lock_acquirable_at_all(at_generic_c_lock_t*);
static int at_reader_c_lock_acquirable(at_generic_c_lock_t*,at_thread_id);
static void at_reader_c_lock_acquire(at_generic_c_lock_t*, at_thread_id);
static void at_reader_c_lock_release(at_generic_c_lock_t*, at_thread_id);

/* Writer */
static at_writer_c_lock_t* at_writer_c_lock_create();
static void at_writer_c_lock_init(at_writer_c_lock_t*);
static int at_writer_c_lock_acquirable_at_all(at_generic_c_lock_t*);
static int at_writer_c_lock_acquirable(at_generic_c_lock_t*,at_thread_id);
static void at_writer_c_lock_acquire(at_generic_c_lock_t*, at_thread_id);
static void at_writer_c_lock_release(at_generic_c_lock_t*, at_thread_id);
static int at_writer_c_lock_acquirable_at_all_reader(at_writer_c_lock_t*);
static int at_writer_c_lock_acquirable_reader(at_writer_c_lock_t*,at_thread_id);
static void at_writer_c_lock_acquire_reader(at_writer_c_lock_t*,at_thread_id);
static void at_writer_c_lock_release_reader(at_writer_c_lock_t*,at_thread_id);

/* Mutex: create */
at_mutex_c_lock_t* at_mutex_c_lock_create() {
   /* Allocate memory */
   at_mutex_c_lock_t* new = (at_mutex_c_lock_t*) malloc(
                           sizeof(struct at_mutex_c_lock_struct));
#ifdef AT_CL_TRACE_LOCKS
   printf("Mutex %p: created by thread %d.\n",new,at_self()->id);fflush(stdout);
#endif
   /* Initialise struct */
   at_mutex_c_lock_init(new);
   return new;
}

/* Mutex: init */
void at_mutex_c_lock_init(at_mutex_c_lock_t* self) {
   at_generic_c_lock_t* me = (at_generic_c_lock_t*)self;
#ifdef AT_CL_TRACE_LOCKS
   printf("Mutex %p: init called.\n", self);
#endif
   /* Set Methods */
   me->acquirable_at_all = at_mutex_c_lock_acquirable_at_all;
   me->acquirable = at_mutex_c_lock_acquirable;
   me->acquire = at_mutex_c_lock_acquire;
   me->release = at_mutex_c_lock_release;
   me->reservation = NULL;
   me->cancel_reservation = NULL;
   /* It's a single lock */
   me->next_lock = NULL;
   /* Create manager. It sets the manager pointer of all locks passed */
   at_c_lock_manager_create(&me,1);
   /* Initialise Attributes */
   self->locked = 0;
}

/* Mutex: acquirable at all */
static int at_mutex_c_lock_acquirable_at_all(at_generic_c_lock_t* self) {
   at_mutex_c_lock_t* me = (at_mutex_c_lock_t*)self;
#ifdef AT_CL_TRACE_LOCKS
   printf("Mutex %p: acquirable at all called by thread %d.\n",
                                                        self, at_self()->id);
#endif
   /* Yes, if no other thread is holding it */
   if (me->locked == 0) {
#ifdef AT_CL_TRACE_LOCKS
      printf("Mutex %p: acquirable at all.\n", self);
#endif
      return (1);
   }
#ifdef AT_CL_TRACE_LOCKS
      printf("Mutex %p: not acquirable at all.\n", self);
#endif
   /* No otherwise */
   return 0;
}

/* Mutex: acquirable? */
static int at_mutex_c_lock_acquirable(at_generic_c_lock_t* self,
                                             at_thread_id tid) {
   at_mutex_c_lock_t* me = (at_mutex_c_lock_t*)self;
#ifdef AT_CL_TRACE_LOCKS
   printf("Mutex %p: acquirable called for thread %d.\n", self, tid);
#endif
   /* Yes, if no other thread is holding it */
   if (me->locked == 0) {
#ifdef AT_CL_TRACE_LOCKS
      printf("Mutex %p: acquirable for only thread %d.\n", self, tid);
#endif
      return (1);
   }
   /* Yes, if the calling thread is holding it */
   if (me->locked_by == tid) {
#ifdef AT_CL_TRACE_LOCKS
      printf("Mutex %p: acquirable for same thread %d.\n", self, tid);
#endif
      return (1);
   }
#ifdef AT_CL_TRACE_LOCKS
      printf("Mutex %p: not acquirable for thread %d.\n", self, tid);
#endif
   /* No otherwise */
   return 0;
}

/* Mutex: acquire */
static void at_mutex_c_lock_acquire(at_generic_c_lock_t* self,at_thread_id tid)
{
   at_mutex_c_lock_t* me = (at_mutex_c_lock_t*)self;
#ifdef AT_CL_TRACE_LOCKS
   printf("Mutex %p: acquire called for thread %d.\n", self, tid);
#endif
   /* Increase lock counter */
   me->locked++;
   /* store holding thread */
   me->locked_by = tid;
}

/* Mutex: release */
static void at_mutex_c_lock_release(at_generic_c_lock_t* self,at_thread_id tid)
{
   at_mutex_c_lock_t* me = (at_mutex_c_lock_t*)self;
#ifdef AT_CL_TRACE_LOCKS
   printf("Mutex %p: release called for thread %d.\n", self, tid);
#endif
   /* Decrease lock counter */
   me->locked--;
   /* me->locked_by remains unchanged. It is irrelevant, if me->locked == 0 */
}

/* RW: create */
at_rw_c_lock_t* at_rw_c_lock_create() {
   /* Allocate memory */
   at_rw_c_lock_t* new = (at_rw_c_lock_t*) malloc(
                           sizeof(struct at_rw_c_lock_struct));
#ifdef AT_CL_TRACE_LOCKS
   printf("RW %p: created by thread %d.\n", new, at_self()->id);
#endif
   /* Initialise struct */
   at_rw_c_lock_init(new);
   return new;
}

/* RW: init */
void at_rw_c_lock_init(at_rw_c_lock_t* self) {
#ifdef AT_CL_TRACE_LOCKS
   printf("RW %p: init called.\n", self);
#endif
   /* Initialize Attributes */
   self->writer = (at_generic_c_lock_t*)at_writer_c_lock_create();
   self->reader = (at_generic_c_lock_t*)at_reader_c_lock_create(
                                      (at_writer_c_lock_t*)(self->writer));
#ifdef AT_CL_TRACE_LOCKS
   printf("RW %p: Reader is %p.\n", self, self->reader);
   printf("RW %p: Writer is %p.\n", self, self->writer);
#endif
}

/* Reader: create */
static at_reader_c_lock_t* at_reader_c_lock_create(at_writer_c_lock_t* wrt) {
   /* Allocate memory */
   at_reader_c_lock_t* new = (at_reader_c_lock_t*) malloc(
                           sizeof(struct at_reader_c_lock_struct));
#ifdef AT_CL_TRACE_LOCKS
   printf("Reader %p: created by thread %d.\n", new, at_self()->id);
#endif
   /* Initialise struct */
   at_reader_c_lock_init(new,wrt);
   return new;
}

/* Reader: init */
static void at_reader_c_lock_init(at_reader_c_lock_t* self,
                              at_writer_c_lock_t* writer) {
   at_generic_c_lock_t* me = (at_generic_c_lock_t*)self;
   at_generic_c_lock_t* lock_array[] = { (at_generic_c_lock_t*)self,
                                            (at_generic_c_lock_t*)writer };
#ifdef AT_CL_TRACE_LOCKS
   printf("Reader %p: init called.\n", self);
#endif
   /* Set Methods */
   me->acquirable_at_all = at_reader_c_lock_acquirable_at_all;
   me->acquirable = at_reader_c_lock_acquirable;
   me->acquire = at_reader_c_lock_acquire;
   me->release = at_reader_c_lock_release;
   me->reservation = NULL;
   me->cancel_reservation = NULL;
   /* Link reader and writer to be managed by a single manager */
   me->next_lock = NULL;
   me->manager = at_c_lock_manager_create(lock_array,2);
   /* Initialise Attributes */
   self->writer = writer;
}

/* Reader: acquirable at all */
static int at_reader_c_lock_acquirable_at_all(at_generic_c_lock_t* self) {
   at_reader_c_lock_t* me = (at_reader_c_lock_t*)self;
   int ret_val;
#ifdef AT_CL_TRACE_LOCKS
   printf("Writer %p: acquireable at all called by thread %d.\n",
                                                      self, at_self()->id);
#endif
   /* Pass request to the writer */
   ret_val = at_writer_c_lock_acquirable_at_all_reader(me->writer);
#ifdef AT_CL_TRACE_LOCKS
   if (ret_val)
      printf("Reader %p: acquirable at all.\n", self);
   else
      printf("Reader %p: not acquirable at all.\n", self);
#endif
   return (ret_val);
}

/* Reader: acquirable */
static int at_reader_c_lock_acquirable(at_generic_c_lock_t* self,
                                              at_thread_id tid) {
   at_reader_c_lock_t* me = (at_reader_c_lock_t*)self;
   int ret_val;
#ifdef AT_CL_TRACE_LOCKS
   printf("Reader %p: acquireable called for thread %d.\n", self, tid);
#endif
   /* Pass request to the writer */
   ret_val = at_writer_c_lock_acquirable_reader(me->writer,tid);
#ifdef AT_CL_TRACE_LOCKS
   if (ret_val)
      printf("Reader %p: acquirable for thread %d.\n", self, tid);
   else
      printf("Reader %p: not acquirable for thread %d.\n", self, tid);
#endif
   return (ret_val);
}

/* Reader: acquire */
static void at_reader_c_lock_acquire(at_generic_c_lock_t* self,at_thread_id tid)
{
   at_reader_c_lock_t* me = (at_reader_c_lock_t*)self;
#ifdef AT_CL_TRACE_LOCKS
   printf("Reader %p: acquire called for thread %d.\n", self, tid);
#endif
   /* Pass request to the writer */
   at_writer_c_lock_acquire_reader(me->writer,tid);
}

/* Reader: release */
static void at_reader_c_lock_release(at_generic_c_lock_t* self,at_thread_id tid)
{
   at_reader_c_lock_t* me = (at_reader_c_lock_t*)self;
#ifdef AT_CL_TRACE_LOCKS
   printf("Reader %p: release called for thread %d.\n", self, tid);
#endif
   /* Pass request to the writer */
   at_writer_c_lock_release_reader(me->writer,tid);
}

/* Writer: create */
static at_writer_c_lock_t* at_writer_c_lock_create() {
   /* Allocate memory */
   at_writer_c_lock_t* new = (at_writer_c_lock_t*) malloc(
                           sizeof(struct at_writer_c_lock_struct));
#ifdef AT_CL_TRACE_LOCKS
   printf("Writer %p: created by thread %d.\n", new, at_self()->id);
#endif
   /* Initialise struct */
   at_writer_c_lock_init(new);
   return new;
}

/* Writer: init */
static void at_writer_c_lock_init(at_writer_c_lock_t* self) {
   at_generic_c_lock_t* me = (at_generic_c_lock_t*)self;
#ifdef AT_CL_TRACE_LOCKS
   printf("Writer %p: init called.\n", self);
#endif
   /* Set Methods */
   me->acquirable_at_all = at_writer_c_lock_acquirable_at_all;
   me->acquirable = at_writer_c_lock_acquirable;
   me->acquire = at_writer_c_lock_acquire;
   me->release = at_writer_c_lock_release;
   me->reservation = NULL;
   me->cancel_reservation = NULL;
   /* The common manager is creater by at_reader_c_lock_init() */
   me->next_lock = NULL;
   /* Initialise Attributes */
   self->locked = 0;
   self->readers = 0;
}

/* Writer: acquirable at all */
static int at_writer_c_lock_acquirable_at_all(at_generic_c_lock_t* self) {
   at_writer_c_lock_t* me = (at_writer_c_lock_t*)self;
#ifdef AT_CL_TRACE_LOCKS
   printf("Writer %p: acquirable at all called by thread %d.\n",
                                                         self, at_self()->id);
#endif
   /* Yes, if no reader and no writer is holding it */
   if (me->readers == 0  &&  me->locked == 0) {
#ifdef AT_CL_TRACE_LOCKS
      printf("Writer %p: acquirable at all.\n", self);
#endif
      return (1);
   }
#ifdef AT_CL_TRACE_LOCKS
   printf("Writer %p: not acquirable at all.\n", self);
#endif
   return (0);
}

/* Writer: acquirable */
static int at_writer_c_lock_acquirable(at_generic_c_lock_t* self,
                                              at_thread_id tid) {
   at_writer_c_lock_t* me = (at_writer_c_lock_t*)self;
#ifdef AT_CL_TRACE_LOCKS
   printf("Writer %p: acquirable called for thread %d.\n", self, tid);
#endif
   /* No, if a reader is holding it */
   if (me->readers != 0) {
#ifdef AT_CL_TRACE_LOCKS
      printf("Writer %p: not acquirable for thread %d.\n", self, tid);
#endif
      return (0);
   }
   /* Yes, if no other writer is holding it */
   if (me->locked == 0) {
#ifdef AT_CL_TRACE_LOCKS
      printf("Writer %p: acquirable for thread %d.\n", self, tid);
#endif
      return (1);
   }
   /* Yes, if the calling thread is holding it */
   if (me->locked_by_writer == tid) {
#ifdef AT_CL_TRACE_LOCKS
      printf("Writer %p: acquirable for thread %d.\n", self, tid);
#endif
      return (1);
   }
#ifdef AT_CL_TRACE_LOCKS
   printf("Writer %p: not acquirable for thread %d.\n", self, tid);
#endif
   return (0);
}

/* Writer: acquire */
static void at_writer_c_lock_acquire(at_generic_c_lock_t* self,at_thread_id tid)
{
   at_writer_c_lock_t* me = (at_writer_c_lock_t*)self;
#ifdef AT_CL_TRACE_LOCKS
   printf("Writer %p: acquire called for thread %d.\n", self, tid);
#endif
   /* Increase lock counter */
   me->locked++;
   /* store holding thread */
   me->locked_by_writer = tid;
}

/* Writer: release */
static void at_writer_c_lock_release(at_generic_c_lock_t* self,at_thread_id tid)
{
   at_writer_c_lock_t* me = (at_writer_c_lock_t*)self;
#ifdef AT_CL_TRACE_LOCKS
   printf("Writer %p: release called for thread %d.\n", self, tid);
#endif
   /* Decrease lock counter */
   me->locked--;
   /* me->locked_by_writer unchanged. It is irrelevant, if me->locked == 0 */
}

/* Writer: reader acquirable at all */
static int at_writer_c_lock_acquirable_at_all_reader(at_writer_c_lock_t* self) {
   at_writer_c_lock_t* me = (at_writer_c_lock_t*)self;
   /* Yes, if no writer is holding it */
   if (me->locked == 0) return (1);
   return (0);
}

/* Writer: reader acquirable */
static int at_writer_c_lock_acquirable_reader(at_writer_c_lock_t* self,
                                                     at_thread_id tid) {
   at_writer_c_lock_t* me = (at_writer_c_lock_t*)self;
   /* Yes, if no writer is holding it */
   if (me->locked == 0) return (1);
   /* Yes, if the calling thread is holding it (as writer) */
   if (me->locked_by_writer == tid) return (1);
   return (0);
}

/* Writer: reader acquire */
static void at_writer_c_lock_acquire_reader(at_writer_c_lock_t* self,
                                                  at_thread_id tid) {
   at_writer_c_lock_t* me = (at_writer_c_lock_t*)self;
   /* Increase reaqder counter */
   me->readers++;
}

/* Writer: reader release */
static void at_writer_c_lock_release_reader(at_writer_c_lock_t* self,
                                                  at_thread_id tid) {
   at_writer_c_lock_t* me = (at_writer_c_lock_t*)self;
   /* Decrease reaqder counter */
   me->readers--;
}
