/*------------------------->  ANSI C - sourcefile  <-------------------------*/
/* Copyright (C) 1995/96 by International Computer Science Institute         */
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
 * Code to attach new threads to an object, and for remote execution
 *
 * Version 1.0 (released for 1.0.9) by Claudio Fleiner (fleiner@icsi.berkeley.edu)
 */
#include <stdio.h>
#include <setjmp.h>
#include <signal.h>
#include <assert.h>

#include "pSather.h"
#include "locks.h"
#include "local.h"
#include "trace.h"
#include "memory.h"
#include "stat.h"
#undef DEBUG
#include "debug.h"

/*
 * standard signal handler, we don't use signals in 
 * pSather, so there should never arrive one !
 */
void standard_signal_handler()
{
	RFATAL( "a kill signal arrived ?!?" );
}

/*
 * Memory strukture needed when forking pSather threads
 * on another cluster (or the same one)
 */
struct FORK_struct
{
	FOB self;
	FOB attach;
	FOB arg;
};

/*
 * init_thread sets up the different structure needed
 * for a thread. It assumes that it has to initialize
 * a new thread. It creates some thread local memory,
 * used to store the pSather thread ID, the current
 * file and line number (those are the file/line number
 * from the last pSather command), and its import 
 * counter.
 */

BR_lock_t thread_count_lock = NULL;
BR_lock_t psather_dead = NULL;
static void change_thread_count( vnn_t from, BR_word_t val, BR_word_t ta )
{
	BR_LOCK( thread_count_lock );
	thread_count += val;
	if ( HERE == 0 && thread_count == 0 )
		BR_UNLOCK( psather_dead );
	BR_UNLOCK( thread_count_lock );
	BR_REPLY_1( from, r_ta_sema_signal, ta );
}

void init_thread_count()
{
	thread_count_lock = BR_LOCK_CREATE();
	psather_dead = BR_LOCK_CREATE();
}

void init_psather_end_detection()
{
	if ( HERE == 0 ) {
		BR_LOCK( psather_dead );
	}
}

int psather_prio;
void init_psather_prio()
{
	if ( getenv( "THREAD_PRIO" ) ) {
		psather_prio = atoi( getenv( "THREAD_PRIO" ) );
	}
#ifdef POLLING
	if ( psather_prio <= 1 )
		psather_prio = 2;
#endif
}

void init_thread_memory()
{
	LOCAL_MEM m;

	/* We do no need to allocate space for Active Threads */
	/* AT Threads come with needed local memory already allocated */

#ifdef AT_THREADS

	m = BR_GET_THREAD_LOCAL();
#else

	CALLOC( m, sizeof( *m ), 1 );
#endif

	m->psather_tid = BR_THREAD_ID();
	m->got_tid = 1;
	m->file = "INIT";
	m->line = 0;
	m->attach = NULL;
#ifndef AT_THREADS

	BR_SET_THREAD_LOCAL( ( void* ) m );
#endif

	SYS_IMPORT;
}

void init_remote_exec_thread()
{
	extern void gdb_signal_handler( int );
	extern void remote_gdb( int );
	init_thread_memory();
	signal( SIGQUIT, remote_gdb );
	signal( SIGILL, gdb_signal_handler );
	signal( SIGEMT, gdb_signal_handler );
	signal( SIGFPE, gdb_signal_handler );
	signal( SIGBUS, gdb_signal_handler );
	signal( SIGSEGV, gdb_signal_handler );
	signal( SIGSYS, gdb_signal_handler );
}

void init_thread()
{
	init_remote_exec_thread();
	BR_LOCK( thread_count_lock );
	thread_count++;
	if ( HERE != 0 && ( long ) thread_count == 1 ) {
		TA_SEMAPHORE( ta );
		BR_UNLOCK( thread_count_lock );
		BR_REQUEST_2( 0, change_thread_count, 1, ( long ) ta );
		TA_SEMA_WAIT( ta );
	} else {
		BR_UNLOCK( thread_count_lock );
	}
}

/*
 * Deamon threads are ignored when finding out if all threads
 * have finished. This has to be called by threads after they
 * call init_thread(). Such a thread may never call end_thread().
 */
void deamon_thread()
{
	BR_LOCK( thread_count_lock );
	thread_count--;
	if ( thread_count == 0 ) {
		BR_UNLOCK( thread_count_lock );
		if ( HERE == 0 ) {
			BR_UNLOCK( psather_dead );
		} else {
			TA_SEMAPHORE( ta );
			BR_REQUEST_2( 0, change_thread_count, -1, ( long ) ta );
			TA_SEMA_WAIT( ta );
		}
	} else {
		BR_UNLOCK( thread_count_lock );
	}
}

/*
 * end of the thread.
 * Before the local memory can be freed, it has
 * to wait for an eventual active message with the
 * pSather thread id.
 */
void end_thread_memory()
{
	/* No need to free anything for Active Threads */
#ifndef AT_THREADS
	FREE( BR_GET_THREAD_LOCAL() );
#endif

}
void end_remote_exec_thread()
{
	/* TRACE("Entering end_remote_exec_thread.\n"); */
	assert( ( BR_GET_THREAD_LOCAL() ) != NULL );
	/* TRACE("Calling thr_ps_id in end_remote_exec_thread.\n"); */
	thr_ps_id(); /* wait for the pSather thread id */
	/* TRACE("Leaving end_remote_exec_thread.\n"); */
}
void end_thread()
{
	end_remote_exec_thread();
	deamon_thread();
	end_thread_memory();
}

/*
 * returns the pSather thread id, (ev. it has to
 * wait for it, see execp_done_w_ex for the
 * corresponding BR_GET())
 */
BR_thread_t thr_ps_id()
{
	LOCAL_MEM m;
	m = ( LOCAL_MEM ) ( BR_GET_THREAD_LOCAL() );


	if ( !m->got_tid ) {
		am_wait_for( m->got_tid );
	}
	return m->psather_tid;
}


/*
 * call back for the BR_GET in execp_done_w_ex
 */
static void execp_got_ps_id( vnn_t from, void *l, int size, LOCAL_MEM m )
{
	m->got_tid = 1;
}

/*
 * This function is called for each new pSather thread
 * It attached the thread to the attach and calls the startup
 * function. Unhandled exceptions are caught here (and abort the
 * program)
 */
static void start_thread( vnn_t from, FORK_FUNC func, struct FORK_struct *r, long sem )
{
	extern void EXT_ATTACH_birth( FOB );
	LOCAL_MEM m;
	init_thread();
	INIT_PROTECT_BEGIN
	PROTECT_BEGIN
	TRACE( "new thread" );
	m = ( LOCAL_MEM ) ( BR_GET_THREAD_LOCAL() );
	m->attach = r->attach;
	SYS_IMPORT;
	EXT_ATTACH_birth( r->attach );
	SYS_EXPORT;
#ifdef AM_SOLARIS_THREADS

	thr_setprio( thr_self(), psather_prio );
#endif

	BR_REQUEST_1( from, BR_signal_handler, sem );
	( *func ) ( r->self, r->arg, r->attach, BR_HERE() );
	TRACE( "thread dies" );
	PROTECT_WHEN
	RFATAL( "unhandled exception, aborting program" );
	PROTECT_END
	INIT_PROTECT_END
	end_thread();
	FREE( r );
}


/* r is a local pointer (it is never converted to FOB) */
static void store_arguments_r( vnn_t from, void **r, void *result )
{
	( *r ) = result;
}
/*
 * some arguments are sent first (and stored in heap memory)
 */
static void store_arguments( vnn_t from, FOB self, FOB arg, FOB attach, void **r )
{
	struct FORK_struct * result;
	MALLOC( result, sizeof( *result ) );
	result->self = self;
	result->arg = arg;
	result->attach = attach;
	BR_REPLY_2( from, ( BR_handler_2_t ) store_arguments_r, ( long ) r, ( long ) result );
}

/* Added to get sema_signal dependency out of old am library - DPS */
void sema_signal( BR_sema_t sem )
{
	BR_SIGNAL( sem );
}

/*
 * Used to attach a new pSather thread to a attach. 
 */
void p_attach( FORK_FUNC func, FOB self, FOB arg, FOB attach, int pos )
{
	BR_sema_t s = BR_SEMA_CREATE( 0 );
	struct FORK_struct *r;
	if ( pos < 0 || pos >= CLUSTERS )
		RFATAL( "you tried to create a thread on a non existent cluster\n" );

	STAT( THREAD_STARTED );
	DECLARE_LOCK( 1, 1, 0 );
	ADD_LOCK( 0, 0, attach );
	LM_ACQUIRE_SINGLE_LOCK
	LM_PUSH_ON_EXEPT_STACK
	/* collect the arguments in one memory place */
	if ( pos != BR_HERE() ) {
		/* we cannot send all the arguments in one thr_create, so
		 * we send them separatly 
		 */
		r = NULL;
		BR_REQUEST_4( pos, ( BR_handler_4_t ) store_arguments, ( long ) SENDFOB( self, pos ),
		              ( long ) SENDFOB( arg, pos ), ( long ) SENDFOB( attach, pos ), ( long ) & r );
		am_wait_for( r != NULL );
	} else {
		MALLOC( r, sizeof( *r ) );
		r->self = self;
		r->arg = arg;
		r->attach = attach;
	}
	BR_FORK_3( pos, ( BR_handler_3_t ) start_thread, ( long ) func, ( long ) SENDFOBHOME( r ), ( long ) s );
	REG( sema_signal, s );
	BR_WAIT( s );
	UNREG;
	LM_RELEASE_SINGLE_LOCK
	LM_POP_OFF_EXEPT_STACK
	LM_CLOSE_LOCK_STATEMENT
	BR_SEMA_DELETE( s );
}

/*
 * This function is called for each new pSather thread created through fork
 * It attached the thread to the attach and calls the startup
 * function. Unhandled exceptions are caught here (and abort the
 * program)
 */
static void start_fork_thread( vnn_t from, FORK_FUNC func, FOB self, FOB arg, FOB attach )
{
	LOCAL_MEM m;
	init_remote_exec_thread();
	INIT_PROTECT_BEGIN
	PROTECT_BEGIN
	TRACE( "new thread" );
	m = ( LOCAL_MEM ) ( BR_GET_THREAD_LOCAL() );
	m->attach = attach;
	SYS_IMPORT;
#ifdef AM_SOLARIS_THREADS

	thr_setprio( thr_self(), psather_prio );
#endif

	( *func ) ( self, arg, attach, BR_HERE() );
	TRACE( "thread dies" );
	PROTECT_WHEN
	RFATAL( "unhandled exception, aborting program" );
	PROTECT_END
	INIT_PROTECT_END
	end_remote_exec_thread();
}

/*
 * Used to attach a new forked pSather thread
 * No need for serialization and waiting
 * No need to lock the gate (PAR_ATTACH)
 */
void p_fork_attach( FORK_FUNC func, FOB self, FOB arg, FOB attach, int pos )
{
	extern void EXT_ATTACH_birth( FOB );
	/* TRACE("Entering p_fork_attach.\n"); */
	if ( pos < 0 || pos >= CLUSTERS )
		RFATAL( "you tried to create a thread on a non existent cluster\n" );

	STAT( THREAD_STARTED );
	EXT_ATTACH_birth( attach );
	/* TRACE("p_fork_attach forks thread.\n"); */
	BR_FORK_4( pos, ( BR_handler_4_t ) start_fork_thread, ( long ) func,
	           ( long ) SENDFOB( self, pos ), ( long ) SENDFOB( arg, pos ), ( long ) SENDFOB( attach, pos ) );
	/* TRACE("Leaving p_fork_attach.\n"); */
}

/*
 * structure used to execute a function
 * on an other cluster
 */
struct exec_struct
{
	BR_lock_t ctr;
	FOB exc;
	/* attach, psather_tid and prev_FF have to be in the same order as in LOCAL_MEM_struct */
	FOB attach;
	struct _func_frame *prev_FF;
	BR_thread_t psather_tid;
unsigned got_exception:
	1;
};

/*
 * executed if the function returns normaly
 * (note: out arguments are currently not supported)
 */
static void execp_done( vnn_t from, struct exec_struct *info )
{
	/* TRACE("execp_done called unlock.\n"); */
	BR_UNLOCK( info->ctr );
	/* TRACE("execp_done executed unlock.\n"); */
}

/*
 * executed if the function returns with an exception
 * (note: out arguments are currently not supported)
 */
static void execp_done_w_ex( vnn_t from, struct exec_struct *info, FOB exception )
{
	info->exc = exception;
	info->got_exception = 1;
	DEBUG0( "execp_mem thread done" );
	/* TRACE("execp_done called unlock.\n"); */
	BR_UNLOCK( info->ctr );
	/* TRACE("execp_done executed unlock.\n"); */
	DEBUG0( "execp_mem thread done and signaled" );
}

/*
 * this function is started on the remote cluster.
 * It gets a memory position as argument and reads
 * this memory via BR_GET.
 */
static void start_execp_mem( vnn_t from, void ( *func ) ( vnn_t, void *, long ), void *ptr, long size, struct exec_struct *info )
{
	BR_lock_t sem = BR_LOCK_CREATE();
	char * volatile p = NULL;
#ifndef USE_ALLOCA

	char msg[ ATTACH_LOCAL_MEM ];
#endif

	LOCAL_MEM m;

	init_remote_exec_thread();
	DEBUG2( "start_execp_mem  size=%ld (a), from %d", size, from );
	INIT_PROTECT_BEGIN
	PROTECT_BEGIN
	m = ( LOCAL_MEM ) ( BR_GET_THREAD_LOCAL() );
	m->prev_cluster = from;
	/* invalidate pSather thread id, as this thread runs under
	 * the same id as the one that created it. Ask the other
	 * thread to send its id. We don't wait for the id, as this
	 * is done in thr_ps_id().
	 */
#ifdef AM_SOLARIS_THREADS

	thr_setprio( thr_self(), psather_prio );
#endif

	m->got_tid = 0;
	BR_GET( from, ( void * ) &info->attach, ( void * ) &m->attach, sizeof( BR_thread_t ) + sizeof( FOB ) + sizeof( struct _func_frame * ), ( BR_handler_mem_t ) execp_got_ps_id, ( BR_word_t ) m );

	if ( size > 0 )
	{
#ifdef USE_ALLOCA
		p = alloca( size );
#else

		if ( size < ATTACH_LOCAL_MEM )
			p = msg;
		else
			MALLOC( p, size );
#endif

		BR_LOCK( sem );
		BR_GET( from, ptr, p, size, r_lck_unlock_mem, ( BR_word_t ) sem );
		BR_LOCK( sem );
	}
	DEBUG0( "starting function" );
	SYS_IMPORT;
	( *func ) ( from, p, size );
	SYS_EXPORT;
	DEBUG0( "function done" );

	/* function finished without exception */
	BR_REQUEST_1( from, ( BR_handler_1_t ) execp_done, ( long ) info );
	PROTECT_WHEN
	/* oops, we got an exception .... */
	SYS_EXPORT;
	BR_REQUEST_2( from, ( BR_handler_2_t ) execp_done_w_ex, ( long ) info, ( long ) EXCEPTION );
	PROTECT_END
	DEBUG0( "PROTECT_END done" );
#ifndef USE_ALLOCA

	if ( size >= ATTACH_LOCAL_MEM )
		FREE( p );
#endif

	DEBUG0( "free done" );
	INIT_PROTECT_END
	DEBUG0( "INIT_PROTECT_DONE" );
	end_remote_exec_thread();
	BR_LOCK_DELETE( sem );
}

void execp_mem( vnn_t where, void ( *func ) ( vnn_t, void *, long ), void *ptr, long size )
{
	struct exec_struct info;
	BR_lock_t ctr = BR_LOCK_CREATE();
	LOCAL_MEM m;

	m = ( LOCAL_MEM ) ( BR_GET_THREAD_LOCAL() );
	STAT( REMOTE_EXEC );

	BR_LOCK( ctr );
	info.ctr = ctr;
	info.got_exception = 0;
	info.psather_tid = thr_ps_id();
	info.attach = m->attach;
	info.prev_FF = m->pFF;
	DEBUG2( "execp_mem: thr_create(size=%ld) on %d", size, where );
	SYS_EXPORT;
	BR_FORK_4( where, ( BR_handler_4_t ) start_execp_mem, ( long ) func, ( long ) ptr, size, ( long ) & info );
	DEBUG0( "execp_mem: waiting for start" );
	REG( lck_unlock, ctr );
	BR_LOCK( ctr );
	UNREG;
	DEBUG0( "execp_mem: thr_create done" );
	BR_LOCK_DELETE( ctr );
	SYS_IMPORT;
	if ( info.got_exception )
		RAISE( info.exc );
}

static void set_local_zero( vnn_t from, void *bnd, int size, void *arg )
{
	struct {
		OB_HEADER header;
		FOB local;
		void ( *funcptr ) ();
	}
	*b = bnd;
	b->local = NULL;
}

void p_remote_exec( vnn_t from, void *bnd, int size )
{
	struct {
#ifdef PSATHER
		OB_HEADER header;
#endif

		FOB local;
		void ( *funcptr ) ();
	}
	*b = bnd;
	SYS_IMPORT;
#ifdef PRINT_PO
	/* instead of the function frame we pass a pointer to
	 * the local memory. To make this visible, we set the lowest
	 * bit to one.
	 */
	( *b->funcptr ) ( b, ( void * ) ( ( long ) ( BR_GET_THREAD_LOCAL() ) | 1 ) );
#else

	( *b->funcptr ) ( b );
#endif

	if ( b->local != NULL ) {
		BR_STORE( from, ( void* ) b, SENDFOBHOME( b->local ), size, ( BR_handler_mem_t ) set_local_zero, ( BR_word_t ) NULL );
	}
	SYS_EXPORT;
}


void pd_sync()
{
	extern void EXT_ATTACH_synchronize();
	LOCAL_MEM m;
	thr_ps_id(); /* this call makes sure that the attach is stored
				      *	in the local memory (important if the current thread
			    	      *	has been moved to another cluster)
				      */
	m = ( LOCAL_MEM ) ( BR_GET_THREAD_LOCAL() );
	if ( m->attach == NULL )
		RFATAL( "you cannot call sync in the main thread" );
	EXT_ATTACH_synchronize( m->attach );
}
