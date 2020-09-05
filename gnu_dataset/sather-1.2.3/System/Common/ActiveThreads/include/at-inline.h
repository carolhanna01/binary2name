/*------------------------->  ANSI C - headerfile  <-------------------------*/
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

#ifndef _AT_INLINE_H_
#define _AT_INLINE_H_

/* ***************************************************************************
 * Active Threads inline functions                                           *
 * written by:    Boris Weissman                                             *
 * on:            Feb 20, 1997                                               *
 *****************************************************************************/

#ifdef AT_DEBUG
#include <assert.h>
#endif

#include "qt.h"
#include "thread.h"
#include "pool.h"
#include "p_pool.h"
#include "utils.h"
#include "at-lwp.h"
#include "bundle.h"
#include "mutex.h"

/* A queue is a circular list of threads.  The queue head is a
   designated list element.  If this is a uniprocessor-only
   implementation we can store the `main' thread in this, but in a
   multiprocessor there are several `heavy' threads but only one run
   queue.  A fancier implementation might have private run queues,
   which would lead to a simpler (trivial) implementation */

typedef struct at_queue_t
{
	at_thread_t *head;
	at_thread_t *tail;

	at_spinlock_t lck;  /* Protects the queue */
}
at_queue_t;

extern at_queue_t at_global_run_queue;
extern at_queue_t **at_local_run_queues;
extern at_thread_t *at_queue_get ( at_queue_t *q );
extern void at_queue_put ( at_queue_t *q, at_thread_t *t );
extern void at_queue_put_at_head ( at_queue_t *q, at_thread_t *t );
extern void *at_yieldhelp ( qt_t *sp, void *old, void *blockq );
extern void *at_bundle_event1_help ( qt_t *sp, void *old, void *arg );
extern void at_only ( void *pu, void *pt, qt_userf_t *f );
extern volatile at_bundle_t* at_focus;
extern at_spinlock_t at_focus_slck;

extern volatile at_p_pool_t *at_thread_pool;
extern volatile at_p_pool_t *at_stack_pool;
extern volatile at_p_pool_t *at_thread_local_pool;

extern void at_create_wrapper_0( void* arg );
extern void at_create_wrapper_1( void* arg );
extern void at_create_wrapper_2( void* arg );
extern void at_create_wrapper_3( void* arg );
extern void at_create_wrapper_4( void* arg );
extern void at_create_wrapper_5( void* arg );
extern void at_create_wrapper_6( void* arg );

extern AT_SPINLOCK_DEC( thr_num_lck );
extern volatile int at_thr_num;

/* Stuff from at-inline.h */
AT_INLINE void at_set_focus( at_bundle_t *b );
AT_INLINE at_bundle_t* at_get_focus();
AT_INLINE void at_yield();
AT_INLINE at_thread_t* at_create_0( at_bundle_t *b, int affinity, at_userf_0_t f );
AT_INLINE at_thread_t* at_create_1 ( at_bundle_t *b, int affinity,
                                     at_userf_1_t *f, at_word_t a0 );
AT_INLINE at_thread_t* at_create_2( at_bundle_t *b, int affinity,
                                    at_userf_2_t *f, at_word_t a0, at_word_t a1 );
AT_INLINE at_thread_t* at_create_3( at_bundle_t *b, int affinity,
                                    at_userf_3_t *f, at_word_t a0,
                                    at_word_t a1, at_word_t a2 );
AT_INLINE at_thread_t* at_create_4( at_bundle_t *b, int affinity, at_userf_4_t *f,
                                    at_word_t a0, at_word_t a1, at_word_t a2,
                                    at_word_t a3 );
AT_INLINE at_thread_t* at_create_5( at_bundle_t *b, int affinity,
                                    at_userf_5_t *f, at_word_t a0, at_word_t a1,
                                    at_word_t a2, at_word_t a3, at_word_t a4 );
AT_INLINE at_thread_t* at_create_6( at_bundle_t *b, int affinity,
                                    at_userf_6_t *f, at_word_t a0,
                                    at_word_t a1, at_word_t a2,
                                    at_word_t a3, at_word_t a4, at_word_t a5 );

AT_INLINE void at_setlocal( void* addr );
AT_INLINE void* at_getlocal( void );
AT_INLINE void at_setdata( int data );
AT_INLINE int at_getdata( void );
AT_INLINE at_thread_t *at_self( void );
AT_INLINE void at_set_affinity( int vproc );
AT_INLINE int at_get_affinity( void );
AT_INLINE int at_ncpus( void );
AT_INLINE int at_spinlock_try( at_spinlock_t* x );
AT_INLINE void at_create_stack( at_thread_t *t );
AT_INLINE void at_destroy_stack( at_thread_t *t );
AT_INLINE void at_create_local( at_thread_t *t );
AT_INLINE void at_destroy_local( at_thread_t *t );


AT_INLINE at_thread_t *at_get_next();
AT_INLINE void at_thread_set_args( at_thread_t *new_t );
AT_INLINE void *at_block ( qt_helper_t *helper, at_thread_t *old_t, void *a,
                           at_thread_t *new_t );
AT_INLINE void *at_abort ( qt_helper_t *helper, at_thread_t *old_t, void *a,
                           at_thread_t *new_t );
AT_INLINE int at_thread_next_id();


#define AT_WHICH_QUEUE(t) (((t)->affinity==AT_UNBOUND)?(&at_global_run_queue):at_local_run_queues[(t)->affinity])

static at_thread_t* last_next;

AT_INLINE void at_set_focus( at_bundle_t *b )
{
	AT_SPINLOCK_LOCK( at_focus_slck );
	at_focus = b;
	AT_SPINLOCK_UNLOCK( at_focus_slck );
}

AT_INLINE at_bundle_t* at_get_focus()
{
	at_bundle_t * b;
	AT_SPINLOCK_LOCK( at_focus_slck );
	b = ( at_bundle_t * ) at_focus;
	AT_SPINLOCK_UNLOCK( at_focus_slck );
	return b;
}

extern void at_flush( at_bundle_t *b );


/* Code that checks other processors' local queues will be added here
   to avoid an etxra context switch */
AT_INLINE at_thread_t *at_get_next()
{
	/* Get the next thread to be scheduled */
	at_thread_t * next;
	at_bundle_t *bundle;

	/* This is needed for hardware feedback */
#ifdef AT_ULTRA_PIC

	at_flush( at_get_focus() );
	* /
#endif
	/* First, check local queue */
	if ( ( next = at_queue_get( at_local_run_queues[ at_cpu() ] ) ) == NULL ) {
		next = at_queue_get( &at_global_run_queue );
	}

	if ( next == NULL ) {
		bundle = at_get_focus();
		/* Dispatch a "processor idle" event */
		( * bundle->scheduler->processor_idle ) ( bundle, at_cpu() );

		if ( ( next = at_queue_get( at_local_run_queues[ at_cpu() ] ) ) == NULL ) {
			next = at_queue_get ( &at_global_run_queue );
		}

		/* If the runable queue is empty, switch to the main thread that
		 * simply spins until there are threads on the run queue */
		if ( next == NULL ) {
			next = AT_GET_MAIN;
		}
	}
	last_next = next;
	return next;
}


/*
 * Similar to above, but only checks the global and local queues.
 * It does not generate the idle event and never returns main.
 * This could be used by the other nodes, for instance to implement
 * thread migration.
 */
AT_INLINE at_thread_t *at_get_work_thread()
{
	/* Get the next thread to be scheduled */
	at_thread_t * next = NULL;

	/* First, check local queue, the global */
	if ( ( next = at_queue_get( at_local_run_queues[ at_cpu() ] ) ) == NULL ) {
		next = at_queue_get( &at_global_run_queue );
	}
	return next;
}

AT_INLINE void at_thread_set_args( at_thread_t *new_t )
{
	switch ( new_t->nargs ) {
		case 0:
			new_t->sp = QT_ARGS ( new_t->sp, ( void * ) new_t, new_t,
			                      at_create_wrapper_0, at_only );
			break;
		case 1:
			new_t->sp = QT_ARGS ( new_t->sp, ( void * ) new_t, new_t,
			                      at_create_wrapper_1, at_only );
			break;
		case 2:
			new_t->sp = QT_ARGS ( new_t->sp, ( void * ) new_t, new_t,
			                      at_create_wrapper_2, at_only );
			break;
		case 3:
			new_t->sp = QT_ARGS ( new_t->sp, ( void * ) new_t, new_t,
			                      at_create_wrapper_3, at_only );
			break;
		case 4:
			new_t->sp = QT_ARGS ( new_t->sp, ( void * ) new_t, new_t,
			                      at_create_wrapper_4, at_only );
			break;
		case 5:
			new_t->sp = QT_ARGS ( new_t->sp, ( void * ) new_t, new_t,
			                      at_create_wrapper_5, at_only );
			break;
		case 6:
			new_t->sp = QT_ARGS ( new_t->sp, ( void * ) new_t, new_t,
			                      at_create_wrapper_6, at_only );
			break;
	}
}


AT_INLINE void *at_block ( qt_helper_t *helper, at_thread_t *old_t, void *a,
                           at_thread_t *new_t )
{
	if ( new_t->state == AT_INITIALIZED ) {
		/* Have to upcall into the bundle scheduler (for instance,
		   to create the stack, and then set the stack up */
		( *( new_t->bundle->scheduler->thread_started ) ) ( new_t->bundle, new_t );
		/* Set the args properly */
		at_thread_set_args( new_t );
	}

	AT_SET_CURRENT( new_t );
	new_t->state = AT_RUNNING;

	QT_BLOCK ( helper, old_t, a, new_t->sp );
}

AT_INLINE void *at_abort ( qt_helper_t *helper, at_thread_t *old_t, void *a,
                           at_thread_t *new_t )
{
	if ( new_t->state == AT_INITIALIZED ) {
		/* Have to upcall into the bundle scheduler (for instance,
		   to create the stack, and then set the stack up */
		( *( new_t->bundle->scheduler->thread_started ) ) ( new_t->bundle, new_t );
		/* Set the args properly */
		at_thread_set_args( new_t );
	}

	AT_SET_CURRENT( new_t );
	new_t->state = AT_RUNNING;

	QT_ABORT ( helper, old_t, a, new_t->sp );
}


/* Note that the "main" threads never yields and we do not need to introduce
 * any extra checks to see if it is a main thread or not */

/* This implementation may be optimized for a case when there is no threads
   to yield to */
AT_INLINE void at_yield()
{
	at_thread_t * old, *newthread;
	at_bundle_t *bundle;
	void ( *processor_idle ) ( at_bundle_t * b, int proc );

	newthread = at_get_next();

	old = AT_GET_CURRENT;
	old->state = AT_READY;
	at_block( at_yieldhelp, old, ( void * ) AT_WHICH_QUEUE( old ), newthread );
}

/* This blocks a thread and vectors a bundle-specific event. The bundle
 * obtains the thread TCB and can deal with it any way it wants.
 * The semantics of bundle-specific events are different for different
 * bundles. For instance, a bundle that supports thread migration over
 * the network, may place a migration code in this event handler*/
AT_INLINE void at_bundle_event1( at_word_t arg )
{
	at_thread_t * old, *newthread;
	at_bundle_t *bundle;
	void ( *processor_idle ) ( at_bundle_t * b, int proc );

	newthread = at_get_next();

	old = AT_GET_CURRENT;
	old->state = AT_READY;
	at_block( at_bundle_event1_help, old, ( void * ) arg, newthread );
}


AT_INLINE int at_thread_next_id()
{
	int id;

	AT_SPINLOCK_LOCK( thr_num_lck );
	at_thr_num++;
	id = at_thr_num;
	AT_SPINLOCK_UNLOCK( thr_num_lck );
	return id;
}

extern volatile int at_thr_count;
extern at_mutex_t at_join_all_mutex;

AT_INLINE void at_thread_count_inc()
{
	AT_SPINLOCK_LOCK( thr_num_lck );
	if ( at_thr_count == 1 ) {
		at_mutex_lock( &at_join_all_mutex );
	}
	at_thr_count++;
	AT_SPINLOCK_UNLOCK( thr_num_lck );
}

AT_INLINE void at_thread_count_dec()
{
	AT_SPINLOCK_LOCK( thr_num_lck );
	at_thr_count--;
	if ( at_thr_count == 1 ) {
		at_mutex_unlock( &at_join_all_mutex );
	}
	AT_SPINLOCK_UNLOCK( thr_num_lck );
}

AT_INLINE void at_thread_count_init( int count )
{
	AT_SPINLOCK_LOCK( thr_num_lck );
	at_thr_count = count;
	AT_SPINLOCK_UNLOCK( thr_num_lck );
}

AT_INLINE int at_thread_count()
{
	return at_thr_count;
}

AT_INLINE at_thread_t* at_create_0( at_bundle_t *b, int affinity, at_userf_0_t f )
{
	at_thread_t * t;

	t = ( at_thread_t * ) at_p_pool_get( ( at_p_pool_t * ) at_thread_pool );

	t->func = ( at_userf_t * ) f;
	t->nargs = 0;

	t->affinity = affinity;
	t->state = AT_INITIALIZED;
	t->bundle = b;
	t->id = at_thread_next_id();

	at_thread_count_inc();
	( *( b->scheduler->thread_created ) ) ( b, t );
	return t;
}


AT_INLINE at_thread_t* at_create_1 ( at_bundle_t *b, int affinity,
                                     at_userf_1_t *f, at_word_t a0 )
{
	at_thread_t * t;

	t = ( at_thread_t * ) at_p_pool_get( ( at_p_pool_t * ) at_thread_pool );

	t->func = ( at_userf_t * ) f;
	t->nargs = 1;
	t->args[ 0 ] = a0;

	t->affinity = affinity;
	t->state = AT_INITIALIZED;
	t->bundle = b;
	t->id = at_thread_next_id();

	at_thread_count_inc();
	( *( b->scheduler->thread_created ) ) ( b, t );

	return t;
}


AT_INLINE at_thread_t* at_create_2( at_bundle_t *b, int affinity,
                                    at_userf_2_t *f, at_word_t a0, at_word_t a1 )
{
	at_thread_t * t;

	t = ( at_thread_t * ) at_p_pool_get( ( at_p_pool_t * ) at_thread_pool );

	/* Initialize the args in the thread structure */
	t->func = ( at_userf_t * ) f;
	t->nargs = 2;
	t->args[ 0 ] = a0;
	t->args[ 1 ] = a1;

	t->affinity = affinity;
	t->state = AT_INITIALIZED;
	t->bundle = b;
	t->id = at_thread_next_id();

	at_thread_count_inc();
	( *( b->scheduler->thread_created ) ) ( b, t );

	return t;
}


AT_INLINE at_thread_t* at_create_3( at_bundle_t *b, int affinity,
                                    at_userf_3_t *f, at_word_t a0,
                                    at_word_t a1, at_word_t a2 )
{
	at_thread_t * t;

	t = ( at_thread_t * ) at_p_pool_get( ( at_p_pool_t * ) at_thread_pool );

	/* Initialize the args in the thread structure */
	t->func = ( at_userf_t * ) f;
	t->nargs = 3;
	t->args[ 0 ] = a0;
	t->args[ 1 ] = a1;
	t->args[ 2 ] = a2;

	t->affinity = affinity;
	t->state = AT_INITIALIZED;
	t->bundle = b;
	t->id = at_thread_next_id();

	at_thread_count_inc();
	( *( b->scheduler->thread_created ) ) ( b, t );

	return t;
}

AT_INLINE at_thread_t* at_create_4( at_bundle_t *b, int affinity, at_userf_4_t *f,
                                    at_word_t a0, at_word_t a1, at_word_t a2,
                                    at_word_t a3 )
{
	at_thread_t * t;

	t = ( at_thread_t * ) at_p_pool_get( ( at_p_pool_t * ) at_thread_pool );

	/* Initialize the args in the thread structure */
	t->func = ( at_userf_t * ) f;
	t->nargs = 4;
	t->args[ 0 ] = a0;
	t->args[ 1 ] = a1;
	t->args[ 2 ] = a2;
	t->args[ 3 ] = a3;


	t->affinity = affinity;
	t->state = AT_INITIALIZED;
	t->bundle = b;
	t->id = at_thread_next_id();

	at_thread_count_inc();
	( *( b->scheduler->thread_created ) ) ( b, t );

	return t;
}

AT_INLINE at_thread_t* at_create_5( at_bundle_t *b, int affinity,
                                    at_userf_5_t *f, at_word_t a0, at_word_t a1,
                                    at_word_t a2, at_word_t a3, at_word_t a4 )
{
	at_thread_t * t;

	t = ( at_thread_t * ) at_p_pool_get( ( at_p_pool_t * ) at_thread_pool );

	/* Initialize the args in the thread structure */
	t->func = ( at_userf_t * ) f;
	t->nargs = 5;
	t->args[ 0 ] = a0;
	t->args[ 1 ] = a1;
	t->args[ 2 ] = a2;
	t->args[ 3 ] = a3;
	t->args[ 4 ] = a4;

	t->affinity = affinity;
	t->state = AT_INITIALIZED;
	t->bundle = b;
	t->id = at_thread_next_id();

	at_thread_count_inc();
	( *( b->scheduler->thread_created ) ) ( b, t );

	return t;
}

AT_INLINE at_thread_t* at_create_6( at_bundle_t *b, int affinity,
                                    at_userf_6_t *f, at_word_t a0,
                                    at_word_t a1, at_word_t a2,
                                    at_word_t a3, at_word_t a4, at_word_t a5 )
{
	at_thread_t * t;

	t = ( at_thread_t * ) at_p_pool_get( ( at_p_pool_t * ) at_thread_pool );

	/* Initialize the args in the thread structure */
	t->func = ( at_userf_t * ) f;
	t->nargs = 6;
	t->args[ 0 ] = a0;
	t->args[ 1 ] = a1;
	t->args[ 2 ] = a2;
	t->args[ 3 ] = a3;
	t->args[ 4 ] = a4;
	t->args[ 5 ] = a5;

	t->affinity = affinity;
	t->state = AT_INITIALIZED;
	t->bundle = b;
	t->id = at_thread_next_id();

	at_thread_count_inc();
	( *( b->scheduler->thread_created ) ) ( b, t );

	return t;
}


/* Set thread_local field of the currently executing thread to addr */
AT_INLINE void at_setlocal( void* addr )
{
	at_thread_t * t;
	t = AT_GET_CURRENT;
	t->thread_local = addr;
}

/* Get thread_local field of the currently executing thread */
AT_INLINE void* at_getlocal( void )
{
	at_thread_t * t;
	t = AT_GET_CURRENT;
	return t->thread_local;
}

/* Set thread_local field of the currently executing thread to addr */
AT_INLINE void at_setdata( int data )
{
	at_thread_t * t;
	t = AT_GET_CURRENT;
	t->data = data;
}

/* Get thread_local field of the currently executing thread */
AT_INLINE int at_getdata( void )
{
	at_thread_t * t;
	t = AT_GET_CURRENT;
	return t->data;
}


/* Return a pointer to the currentry running thread */
AT_INLINE at_thread_t *at_self( void )
{
	return AT_GET_CURRENT;
}
/*
 * Changes the affinity for the calling thread. The thread is given
 * the affinity for the virtual processor vproc. If vproc is
 * AT_UNBOUND, thread t will not be bound to any virtual
 * processor. Upon perfrorming the call, the thread blocks, and
 * is later restarted at the destination processor when it becomes
 * available
 * 
 * This can never be called by the *main* scheduling threads
 */
AT_INLINE void at_set_affinity( int vproc )
{
	at_thread_t * old_thread, *new_thread;

	old_thread = AT_GET_CURRENT;

	/* Check if need to block */
	if ( old_thread->affinity == vproc ) {
		return ;
	}

	old_thread->affinity = vproc;
	new_thread = at_get_next();
	at_block( at_yieldhelp, old_thread, ( void * ) AT_WHICH_QUEUE( old_thread ), new_thread );
}

/* Returns the affinity of the calling thread */
AT_INLINE int at_get_affinity( void )
{
	at_thread_t * t = AT_GET_CURRENT;
	return t->affinity;
}

extern volatile unsigned int at_concurrency;

AT_INLINE int at_ncpus( void )
{
	return at_concurrency;
}


AT_INLINE int at_spinlock_try( at_spinlock_t* x )
{
	register int XyZ;
	MD_READ_AND_MODIFY( x, XyZ );
	return MD_OBTAINED_LOCK( XyZ );
}

AT_INLINE void at_create_stack( at_thread_t *t )
{
	t->sto = ( void * ) at_p_pool_get( ( at_p_pool_t * ) at_stack_pool );
	t->sto = AT_STKALIGN( t->sto, AT_STKBASE_ALIGNMENT );
	t->sp = QT_SP ( t->sto, AT_STKSIZE - QT_STKALIGN );
}

AT_INLINE void at_destroy_stack( at_thread_t *t )
{
	at_p_pool_put( ( at_p_pool_t * ) at_stack_pool, t->sto );
}

/* Creates thread's local storage */
AT_INLINE void at_create_local( at_thread_t *t )
{
	if ( at_local_size ) {
		t->thread_local = ( void * )
		                  at_p_pool_get( ( at_p_pool_t * ) at_thread_local_pool );
	}
}

AT_INLINE void at_destroy_local( at_thread_t *t )
{
	if ( at_local_size ) {
		at_p_pool_put( ( at_p_pool_t * ) at_thread_local_pool,
		               t->thread_local );
	}
}



#endif /* _AT_INLINE_H_ */






