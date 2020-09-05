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
 * Active Threads implementation. See also at-inline.h for inline funcs      *
 * written by:    Boris Weissman                                             *
 * on:            Feb 20, 1997                                               *
 *****************************************************************************/

#ifdef AT_DEBUG
#include <assert.h>
#endif

#include "at-int.h"


/* Local pools for things indexed by proc number */
volatile at_p_pool_t *at_thread_pool;
volatile at_p_pool_t *at_mutex_pool;
volatile at_p_pool_t *at_cond_pool;
volatile at_p_pool_t *at_rw_pool;
volatile at_p_pool_t *at_sema_pool;
volatile at_p_pool_t *at_barrier_pool;
volatile at_p_pool_t *at_stack_pool;
volatile at_p_pool_t *at_thread_local_pool;

extern void perror ( char const *msg );
extern int main( int argc, char **argv );

volatile unsigned int at_concurrency;
volatile unsigned int at_stack_size = 0x2000;  /* Default stack size */
volatile unsigned int at_local_size = 0;       /* No default local storage */

volatile at_bundle_t *at_focus;                /* current focus bundle */
volatile at_bundle_t *at_bundle;               /* original bundle */
at_spinlock_t at_focus_slck;                   /* associated lock */

/* Set to 1 to avoid executing new threads */
static volatile unsigned int at_stopped = 0;
/* This is unlocked only when there is a single *user* thread in the system */
at_mutex_t at_join_all_mutex;
volatile int at_thr_count = 0;

/* A corresponding word is set if "main" thread is in progress */

#define AT_QUEUE_LOCK(q) AT_SPINLOCK_LOCK(q->lck)
#define AT_QUEUE_UNLOCK(q) AT_SPINLOCK_UNLOCK(q->lck)

void at_queue_put ( at_queue_t *q, at_thread_t *t )
{
	AT_QUEUE_LOCK( q );
	t->next = NULL;

	if ( q->tail == NULL ) {
		q->tail = t;
		q->head = t;
	} else {
		q->tail->next = t;
		q->tail = t;
	}
	AT_QUEUE_UNLOCK( q );
}

void at_queue_put_at_head ( at_queue_t *q, at_thread_t *t )
{
	AT_QUEUE_LOCK( q );
	if ( q->tail == NULL ) {
		q->tail = t;
	}
	t->next = q->head;
	q->head = t;
	AT_QUEUE_UNLOCK( q );
}

void at_queue_init ( at_queue_t *q )
{
	q->head = q->tail = NULL;
	AT_SPINLOCK_INIT( q->lck );

}


at_thread_t *at_queue_get ( at_queue_t *q )
{
	at_thread_t * t;

	AT_QUEUE_LOCK( q );
	if ( q->head == NULL ) {
		t = NULL;
	} else {
		t = q->head;
		q->head = t->next;
		/* is empty now? */
		if ( q->head == NULL )
			q->tail = NULL;
	}
	AT_QUEUE_UNLOCK( q );
	return t;
}



 /* Thread routines. */

at_queue_t at_global_run_queue;	/* A queue of runable threads. */
at_queue_t **at_local_run_queues;    /* An array of per virtual cpu queues */


static void *at_mainhelp ( qt_t *old, void *ignore0, void *ignore1 );
void at_only ( void *pu, void *pt, qt_userf_t *f );
static void *at_aborthelp ( qt_t *sp, void *old, void *null );
void *at_yieldhelp ( qt_t *sp, void *old, void *blockq );
void *at_bundle_event1_help ( qt_t *sp, void *old, void *arg );
static void main_thread( void *arg );

static void at_start( int concurrency );

static volatile int at_lwp_started;

void at_init( unsigned int concurrency, unsigned int stack_size,
              unsigned int local_size )
{
	int i;
	at_queue_init( &at_global_run_queue );
	AT_SPINLOCK_INIT( kernel_lck );
	AT_SPINLOCK_INIT( at_focus_slck );
	AT_SPINLOCK_INIT( thr_num_lck );

	if ( stack_size != 0 )
		at_stack_size = stack_size;
	if ( local_size != 0 )
		at_local_size = local_size;

	if ( getenv( "AT_STACK_SIZE" ) != NULL ) {
		sscanf( getenv( "AT_STACK_SIZE" ), "%d", &at_stack_size );
	}

	if ( getenv( "AT_PROCESSORS" ) != NULL ) {
		sscanf( getenv( "AT_PROCESSORS" ), "%d", &concurrency );
	}
	/* Ready to start ... */

#ifdef AT_ULTRA_PIC
	/* Clear the counters */
	( void ) clr_spf_pic01();
#endif

	at_start( concurrency );
}

static void at_init_pools()
{
	at_thread_pool = at_p_pool_create( AT_THREAD_POOL_SIZE, sizeof( at_thread_t ) );
	at_stack_pool = at_p_pool_create( AT_THREAD_POOL_SIZE, at_stack_size );
	at_mutex_pool = at_p_pool_create( AT_SYNC_POOL_SIZE, sizeof( at_mutex_t ) );
	at_sema_pool = at_p_pool_create( AT_SYNC_POOL_SIZE, sizeof( at_sema_t ) );
	at_barrier_pool = at_p_pool_create( AT_SYNC_POOL_SIZE, sizeof( at_barrier_t ) );
	at_cond_pool = at_p_pool_create( AT_SYNC_POOL_SIZE, sizeof( at_cond_t ) );
	at_rw_pool = at_p_pool_create( AT_SYNC_POOL_SIZE, sizeof( at_rw_t ) );
	if ( at_local_size ) {
		at_thread_local_pool = at_p_pool_create( AT_THREAD_POOL_SIZE, at_local_size );
	}
}
/* Start threads with a specified level of concurrency */
/* This code is just a bit tricky. We want the thread that initially
 * calls at_start() to keep executing in its LWP util it explicitly
 * yields or aborts. We also want the "main" thread to be initialized
 * and ready to run in case we complete all "work" threads. 
 * Here is what happens step by step:
 * 1) concurrency-1 LWPs are created (we already had one - the one from
 *    which at_start() is called. The new LWPs are passed the "main"
 *    thread that simply seats there busy waiting for work. When the
 *    "main" thread sees a "work" thread on the runable queue, it
 *    deschedules itself in favor of the "work" thread
 * 
 * The following steps are only done for the original LWP:
 *
 * 2) Data structures for the original thread are created and the lwp's
 *    private structure's current thread field is set to the currently
 *    running thread.
 * 3) Data structures for the original LWP are initialized: the stack
 *    space for the main thread is created and recordede in LWP's private
 *    structure. This is special treatment of the "main" thread on the
 *    starting LWP is necessary, since we want to continue running the
 *    calling thread, rather than switch to the "main" thread.
 *
 */
volatile lwp_private_t* privates[ AT_MAX_PROCS ];

static void at_start( int concurrency )
{
	lwp_private_t * orig_private;
	int i;
	caddr_t stackbase;
	lwp_private_t *private;
	void *sto;
	at_thread_t *t;

	/* Initialize machine-dependent stuff */
	if ( LWP_INIT( concurrency ) ) {
		fprintf( stderr, "LWP_INIT() failed\n" );
		exit( 1 );
	}

	if ( concurrency > at_nprocs() ) {
		fprintf( stderr, "Specified concurrency level %d is greated than number of CPUs %d\n", concurrency, at_nprocs() );
		exit( 1 );
	}


	if ( concurrency <= 0 ) {
		/* Use default concurrency level equal to the number of CPUs */
		concurrency = at_nprocs();
	}
	at_concurrency = concurrency;

	/* Allocate local per virtual processor run queues */
	at_local_run_queues = ( at_queue_t ** )
	                      at_malloc( sizeof( at_queue_t * ) * at_concurrency );
	/* Initialize local queues */
	for ( i = 0; i < at_concurrency; i++ ) {
		at_local_run_queues[ i ] = ( at_queue_t * ) at_malloc( sizeof( at_queue_t ) );
		at_queue_init( at_local_run_queues[ i ] );
	}


	/* Deal with the orinial LWP here */

	orig_private = ( lwp_private_t * ) at_malloc( sizeof( lwp_private_t ) );
	orig_private->id = 0;
	LWP_SET_PRIVATE( orig_private );

	/* Initialize all pools */
	at_init_pools();

	/* Initialize the thread count to 1 - the current thread */
	at_thread_count_init( 1 );
	/*
	 * Initialize the "join all threads mutex". Originally, 
	 * there is a single thread, so it is unlocked
	 */
	at_mutex_init( &at_join_all_mutex );

	/* Set the "current" thread field in the lwp's private storage */
	/* Get storage for the current thread */
	t = ( at_thread_t * ) at_p_pool_get( ( at_p_pool_t * ) at_thread_pool );
	t->sto = ( void * ) at_p_pool_get( ( at_p_pool_t * ) at_stack_pool );
	t->sto = AT_STKALIGN( t->sto, AT_STKBASE_ALIGNMENT );
	if ( at_local_size ) {
		t->thread_local = ( void * )
		                  at_p_pool_get( ( at_p_pool_t * ) at_thread_local_pool );
	}
	/* The "initial" thread is not bound to virtual processor 0 */
	t->affinity = 0;
	/* Create the "original" bundle */
	at_bundle = at_mcs_lazy_bundle_create( NULL );
	/*at_bundle = at_mcs_bundle_create(NULL);*/

	t->bundle = ( at_bundle_t * ) at_bundle;
	t->state = AT_RUNNING;
	t->func = ( at_userf_t * ) main;
	/* Set the focus appropriately */
	at_set_focus( ( at_bundle_t * ) at_bundle );
	AT_SET_CURRENT( t );


	/* Initialize the "main" thread in the lwp private storage */
	orig_private->at_main = at_p_pool_get( ( at_p_pool_t * ) at_thread_pool );
	orig_private->at_main->sto =
	    ( void * ) at_p_pool_get( ( at_p_pool_t * ) at_stack_pool );
	orig_private->at_main->sto =
	    AT_STKALIGN( orig_private->at_main->sto, AT_STKBASE_ALIGNMENT );
	if ( at_local_size ) {
		orig_private->at_main->thread_local = ( void * )
		                                      at_p_pool_get( ( at_p_pool_t * ) at_thread_local_pool );
	}
	orig_private->at_main->sp = QT_SP ( orig_private->at_main->sto,
	                                    AT_STKSIZE - QT_STKALIGN );
	orig_private->at_main->sp = QT_ARGS ( orig_private->at_main->sp,
	                                      orig_private, orig_private->at_main,
	                                      main_thread, at_only );
	orig_private->at_main->state = AT_READY;
	orig_private->at_main->func = ( at_userf_t * ) main_thread;

	/* Main threads have fixed affinities... */
	orig_private->at_main->affinity = orig_private->id;
	orig_private->at_main->bundle = at_get_focus();

	/* Note that the "main" thread does not go on to the runable queue */
	/* or any other sleep queues because the main scheduling thread    */
	/* cannot block on any synchronization objects

	/* 
	 * Create a needed number of LWPs and pass the "main" thread to each
	 * We already have one LWP (calling this function), so we need only
	 * concurrency-1 more
	 *
	 * The are all passed "main" threads as initial.
	 */

	privates[ 0 ] = orig_private;
	for ( i = 1; i < concurrency; i++ ) {

		stackbase = ( caddr_t ) at_malloc( at_stack_size );
		private = ( lwp_private_t * ) at_malloc( sizeof( lwp_private_t ) );
		privates[ i ] = private;

		private->at_main = at_p_pool_get( ( at_p_pool_t * ) at_thread_pool );
		private->at_current = private->at_main;
		private->id = i;
		private->at_main->affinity = private->id;
		private->at_main->bundle = at_get_focus();
		private->at_main->state = AT_READY;
		private->at_main->func = ( at_userf_t * ) main_thread;

		at_lwp_started = 0;

#if defined(__linux__)

		private->at_main->thread_local = ( void * )
		                                 at_p_pool_get( ( at_p_pool_t * ) at_thread_local_pool );
		LWP_START( ( void * ) main_thread, private, stackbase, at_stack_size,
		           private );
#else

		LWP_START( main_thread, private, stackbase, at_stack_size, private );
#endif

		while ( !at_lwp_started ) {}
		;
	}
	return ;
}

/*
 * A function that gets executed when there is nothing to do 
 * This could be used for example, to poll the network or do some other
 * not critical or unpredictable work
 */

static struct
{
	void ( *func ) ();
}
at_when_idle = {NULL};

void at_do_when_idle( void ( *func ) () )
{
	at_when_idle.func = func;
	/* at_when_idle.arg = arg;  No args for now to avoid synchronization */
}

/*
 * This is the "main" thread that runs on LWPs when there is no other
 * work. It is not placed on the runnable queue. Instead, when it is 
 * swaped out, it is saved in the LWP private area. It is swaped in again
 * only when the last runnable thread terminates (see at_exit)
 *
 * If the runable queue is empty, the main thread will simply busy wait
 * for work.
 */

#if defined(__linux__)
# include <pthread.h>
# include <unistd.h>

extern void set_pid( lwp_private_t * ) ;

#endif  /* __linux__ */

#if defined(__linux__) || defined(__CYGWIN32__)
void at_finish( void )
{
	/*  LWP_STOP(); */
}
#endif  /* __linux__ || __CYGWIN32__ */

void main_thread( void *arg )
{
	at_thread_t * next;
	lwp_private_t *private;
	int i;
	at_bundle_t *bundle;
	at_scheduler_t *sched;
	void ( *processor_idle ) ( at_bundle_t * b, int proc );
	int counter;


#ifdef AT_ULTRA_PIC
	/* Clear the counters */
	( void ) clr_spf_pic01();
#endif

	private = ( lwp_private_t * ) arg;

	/* if this is not the initial lwp, need to set the its private storage */
	if ( private->id > 0 ) {
		LWP_SET_PRIVATE( private );
#if defined(__linux__)

		set_pid( private ) ;
#endif  /* __linux__ */

	}

	at_lwp_started = 1;

	/*#ifdef AT_DEBUG */
	/*at_printf("LWP #%d started, g7: %x\n", LWP_GET_PRIVATE->id, (int)LWP_GET_PRIVATE );*/
	/*#endif */

	while ( 1 ) {
#if defined(__linux__)
		/* kill thread if externally told to do so */
		pthread_testcancel() ;
#endif  /* __linux__ */

		if ( !at_stopped ) {
			/* First check the local queue */
			if ( ( next = at_queue_get( at_local_run_queues[ at_cpu() ] ) ) == NULL ) {
				if ( at_global_run_queue.head ) {
					next = at_queue_get ( &at_global_run_queue );
				}
			}

			if ( next == NULL ) {
				bundle = at_get_focus();
				processor_idle = bundle->scheduler->processor_idle;
				/* Dispatch a "processor idle" event */
				( *processor_idle ) ( bundle, at_cpu() );

				if ( ( next = at_queue_get( at_local_run_queues[ at_cpu() ] ) ) == NULL ) {
					if ( at_global_run_queue.head ) {
						next = at_queue_get ( &at_global_run_queue );
					}
				}


				/* Check other processors's queues */
#ifndef AT_ULTRA_PIC
				if ( next == NULL ) {
					for ( i = 1; i < at_concurrency; i++ ) {
						next = at_queue_get( at_local_run_queues[ ( at_cpu() + i ) % at_concurrency ] );
						if ( next )
							break;
					}
				}
#endif

				/* Ok, wait for a while, and repeat the sequence */
				for ( i = 0; i < 2000; i++ ) {
					( volatile ) counter += 3;
				}
			}

			if ( next != NULL ) {

#ifdef AT_DEBUG
				/* This should never happen: main may not be on the run queue */
				assert( next != AT_GET_MAIN );
#endif

#ifdef AT_DEBUG

				at_printf( "main thread found work [%d], g7: %x\n", LWP_GET_PRIVATE->id, ( int ) LWP_GET_PRIVATE );
#endif

				AT_GET_CURRENT->state = AT_READY;
				at_block( at_mainhelp, NULL, NULL, next );
			} else {
#ifdef AT_DEBUG
				at_printf( "main thread NO work [%d]\n", LWP_GET_PRIVATE->id );
#endif

				if ( at_when_idle.func ) {
					at_when_idle.func();
				}
			}
		}
	}
}


static void *at_mainhelp ( qt_t *old, void *ignore0, void *ignore1 )
{
	/* Save the "main" thread in the designated place in LWP private storage */
	AT_SET_MAIN_STACK( old );
	/* return (garbage); */
}



void at_only ( void *pu, void *pt, qt_userf_t *f )
{

#ifdef AT_DEBUG
	assert( pu && pt );
#endif
	/*  AT_SET_CURRENT(pt); */
	/*(*(at_userf_t *)f)(pu);*/

	/* Experimenting ... */
	( *( at_userf_t * ) f ) ( pu );
	at_exit();
	/* NOTREACHED */
}


void at_exit ( void )
{
	at_thread_t * old, *newthread;

	newthread = at_get_next();

	old = AT_GET_CURRENT;
	old->state = AT_ZOMBIE;

	at_abort( at_aborthelp, old, ( void * ) NULL, newthread );
}


/* Return the terminating thread to the thread pool */
static void *
at_aborthelp ( qt_t *sp, void *old, void *null )
{
	at_thread_t * t = ( at_thread_t * ) old;
	at_bundle_t *b;
	b = t->bundle;
	( *( b->scheduler->thread_terminated ) ) ( b, t );
	t->state = AT_ZOMBIE;
	t->sp = t->sto = t->thread_local = NULL;
	at_p_pool_put( ( at_p_pool_t * ) at_thread_pool, old );
	at_thread_count_dec();
	/* return (garbage); */
}



void *at_yieldhelp ( qt_t *sp, void *old, void *blockq )
{
	at_bundle_t * b;
	( ( at_thread_t * ) old ) ->sp = sp;
	b = ( ( at_thread_t * ) old ) ->bundle;
	( *( b->scheduler->thread_unblocked ) ) ( b, ( at_thread_t * ) old );
	/*at_queue_put ((at_queue_t *)blockq, (at_thread_t *)old);*/
	/* return (garbage); */
}

void *at_bundle_event1_help ( qt_t *sp, void *old, void *arg )
{
	at_bundle_t * b;
	( ( at_thread_t * ) old ) ->sp = sp;
	b = ( ( at_thread_t * ) old ) ->bundle;
	( *( b->scheduler->bundle_event1 ) ) ( b, ( at_thread_t * ) old, ( at_word_t ) arg );
	/* return (garbage); */
}

void at_create_wrapper_0( void* t )
{
	at_word_t * args;
	at_userf_0_t *func;

	args = ( ( at_thread_t * ) t ) ->args;
	func = ( at_userf_0_t * ) ( ( at_thread_t * ) t ) ->func;
	func();
}

void at_create_wrapper_1( void* t )
{
	at_word_t * args;
	at_userf_1_t *func;

	args = ( ( at_thread_t * ) t ) ->args;
	func = ( at_userf_1_t * ) ( ( at_thread_t * ) t ) ->func;
	func( args[ 0 ] );
}

void at_create_wrapper_2( void* t )
{
	at_word_t * args;
	at_userf_2_t *func;

	args = ( ( at_thread_t * ) t ) ->args;
	func = ( at_userf_2_t * ) ( ( at_thread_t * ) t ) ->func;
	func( args[ 0 ], args[ 1 ] );
}

void at_create_wrapper_3( void* t )
{
	at_word_t * args;
	at_userf_3_t *func;

	args = ( ( at_thread_t * ) t ) ->args;
	func = ( at_userf_3_t * ) ( ( at_thread_t * ) t ) ->func;
	func( args[ 0 ], args[ 1 ], args[ 2 ] );
}

void at_create_wrapper_4( void* t )
{
	at_word_t * args;
	at_userf_4_t *func;

	args = ( ( at_thread_t * ) t ) ->args;
	func = ( at_userf_4_t * ) ( ( at_thread_t * ) t ) ->func;
	func( args[ 0 ], args[ 1 ], args[ 2 ], args[ 3 ] );
}

void at_create_wrapper_5( void* t )
{
	at_word_t * args;
	at_userf_5_t *func;

#ifdef AT_DEBUG

	assert( t );
#endif

	args = ( ( at_thread_t * ) t ) ->args;
	func = ( at_userf_5_t * ) ( ( at_thread_t * ) t ) ->func;
	func( args[ 0 ], args[ 1 ], args[ 2 ], args[ 3 ], args[ 4 ] );
}

void at_create_wrapper_6( void* t )
{
	at_word_t * args;
	at_userf_6_t *func;

	args = ( ( at_thread_t * ) t ) ->args;
	func = ( at_userf_6_t * ) ( ( at_thread_t * ) t ) ->func;
	func( args[ 0 ], args[ 1 ], args[ 2 ], args[ 3 ], args[ 4 ], args[ 5 ] );
}

void at_stop()
{
	at_stopped = 1;
}

void at_continue()
{
	at_stopped = 0;
}

void at_join_all()
{
	at_mutex_lock( &at_join_all_mutex );
}




