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

#include "at.h"
#include "at_c_locks.h"

#define MUTEXES 8

void thread_body_1( at_word_t, at_word_t );
void thread_body_a( at_word_t, at_word_t );
void thread_body_b( at_word_t, at_word_t );
void thread_body_c( at_word_t, at_word_t );
void thread_body_d( at_word_t, at_word_t );
void thread_body_e( at_word_t, at_word_t );

int main( int argc, char **argv )
{
	int i;
	int thread_number = 5;
	at_mutex_t **mutexes;
	at_thread_t **threads;
	at_generic_c_lock_t* c_mutexes[ MUTEXES ];

	if ( argc > 1 ) {
		thread_number = atoi( argv[ 1 ] );
		if ( thread_number < 1 )
			thread_number = 5;
	}

	at_printf( "Start %d threads.\n", thread_number );

	mutexes = malloc( thread_number * sizeof( at_mutex_t* ) );
	threads = malloc( thread_number * sizeof( at_thread_t* ) );

	at_init( 0, 0x2000, 0 );
	for ( i = 0; i < thread_number; i++ ) {
		mutexes[ i ] = at_mutex_create();
		at_mutex_lock( mutexes[ i ] );
	}
	for ( i = 0; i < MUTEXES; i++ )
		c_mutexes[ i ] = ( at_generic_c_lock_t* ) at_mutex_c_lock_create();

	for ( i = 0; i < thread_number; i++ ) {
		threads[ i ] = at_create_2( at_get_focus(), AT_UNBOUND, thread_body_e,
		                            ( at_word_t ) c_mutexes,
		                            ( at_word_t ) mutexes[ i ] );
		at_printf( "Created thread %d.\n", threads[ i ] ->id );
		if ( ++i < thread_number ) {
			threads[ i ] = at_create_2( at_get_focus(), AT_UNBOUND,
			                            thread_body_a,
			                            ( at_word_t ) c_mutexes,
			                            ( at_word_t ) mutexes[ i ] );
			at_printf( "Created thread %d.\n", threads[ i ] ->id );
		}
		if ( ++i < thread_number ) {
			threads[ i ] = at_create_2( at_get_focus(), AT_UNBOUND,
			                            thread_body_b,
			                            ( at_word_t ) c_mutexes,
			                            ( at_word_t ) mutexes[ i ] );
			at_printf( "Created thread %d.\n", threads[ i ] ->id );
		}
		if ( ++i < thread_number ) {
			threads[ i ] = at_create_2( at_get_focus(), AT_UNBOUND,
			                            thread_body_c,
			                            ( at_word_t ) c_mutexes,
			                            ( at_word_t ) mutexes[ i ] );
			at_printf( "Created thread %d.\n", threads[ i ] ->id );
		}
		if ( ++i < thread_number ) {
			threads[ i ] = at_create_2( at_get_focus(), AT_UNBOUND,
			                            thread_body_d,
			                            ( at_word_t ) c_mutexes,
			                            ( at_word_t ) mutexes[ i ] );
			at_printf( "Created thread %d.\n", threads[ i ] ->id );
		}
	}

	at_printf( "Waiting for threads to terminate.\n" );
	for ( i = 0; i < thread_number; i++ ) {
		at_mutex_lock( mutexes[ i ] );
		at_printf( "Thread %d has terminated.\n", threads[ i ] ->id );
	}

	at_printf( "Finished.\n" );
	/* Test OK */
	return ( 0 );
}

void thread_body_1( at_word_t c_mutex, at_word_t mutex )
{
	at_printf( "Thread %d started.\n", at_self() ->id );
	at_acquire_single_c_lock( ( at_generic_c_lock_t* ) c_mutex );
	at_printf( "Thread %d acquired conjunctive mutex.\n", at_self() ->id );
	at_yield();
	at_printf( "Thread %d releases conjunctive mutex.\n", at_self() ->id );
	at_release_single_c_lock( ( at_generic_c_lock_t* ) c_mutex );
	at_printf( "Thread %d signals termination.\n", at_self() ->id );
	at_mutex_unlock( ( at_mutex_t* ) mutex );
	at_printf( "Thread %d terminates.\n", at_self() ->id );
}

void thread_body_a( at_word_t c_mutexes, at_word_t mutex )
{
	at_generic_c_lock_t** mxs = ( at_generic_c_lock_t** ) c_mutexes;
	at_printf( "Thread %d started.\n", at_self() ->id );
	AT_ACQUIRE_C_LOCKS_4( mxs[ 1 ], mxs[ 2 ], mxs[ 4 ], mxs[ 5 ] );
	at_printf( "Thread %d acquired conjunctive mutex.\n", at_self() ->id );
	at_yield();
	at_printf( "Thread %d releases conjunctive mutex.\n", at_self() ->id );
	AT_RELEASE_C_LOCKS_4( mxs[ 1 ], mxs[ 2 ], mxs[ 4 ], mxs[ 5 ] );
	at_printf( "Thread %d signals termination.\n", at_self() ->id );
	at_mutex_unlock( ( at_mutex_t* ) mutex );
	at_printf( "Thread %d terminates.\n", at_self() ->id );
}

void thread_body_b( at_word_t c_mutexes, at_word_t mutex )
{
	at_generic_c_lock_t** mxs = ( at_generic_c_lock_t** ) c_mutexes;
	at_printf( "Thread %d started.\n", at_self() ->id );
	AT_ACQUIRE_C_LOCKS_5( mxs[ 6 ], mxs[ 2 ], mxs[ 7 ], mxs[ 3 ], mxs[ 5 ] );
	at_printf( "Thread %d acquired conjunctive mutex.\n", at_self() ->id );
	at_yield();
	at_printf( "Thread %d releases conjunctive mutex.\n", at_self() ->id );
	AT_RELEASE_C_LOCKS_5( mxs[ 6 ], mxs[ 2 ], mxs[ 7 ], mxs[ 3 ], mxs[ 5 ] );
	at_printf( "Thread %d signals termination.\n", at_self() ->id );
	at_mutex_unlock( ( at_mutex_t* ) mutex );
	at_printf( "Thread %d terminates.\n", at_self() ->id );
}

void thread_body_c( at_word_t c_mutexes, at_word_t mutex )
{
	at_generic_c_lock_t** mxs = ( at_generic_c_lock_t** ) c_mutexes;
	at_printf( "Thread %d started.\n", at_self() ->id );
	AT_ACQUIRE_C_LOCKS_2( mxs[ 3 ], mxs[ 0 ] );
	at_printf( "Thread %d acquired conjunctive mutex.\n", at_self() ->id );
	at_yield();
	at_printf( "Thread %d releases conjunctive mutex.\n", at_self() ->id );
	AT_RELEASE_C_LOCKS_2( mxs[ 3 ], mxs[ 0 ] );
	at_printf( "Thread %d signals termination.\n", at_self() ->id );
	at_mutex_unlock( ( at_mutex_t* ) mutex );
	at_printf( "Thread %d terminates.\n", at_self() ->id );
}

void thread_body_d( at_word_t c_mutexes, at_word_t mutex )
{
	at_generic_c_lock_t** mxs = ( at_generic_c_lock_t** ) c_mutexes;
	at_printf( "Thread %d started.\n", at_self() ->id );
	AT_ACQUIRE_C_LOCKS_8( mxs[ 0 ], mxs[ 2 ], mxs[ 4 ], mxs[ 5 ], mxs[ 6 ],
	                      mxs[ 7 ], mxs[ 3 ], mxs[ 1 ] );
	at_printf( "Thread %d acquired conjunctive mutex.\n", at_self() ->id );
	at_yield();
	at_printf( "Thread %d releases conjunctive mutex.\n", at_self() ->id );
	AT_RELEASE_C_LOCKS_8( mxs[ 0 ], mxs[ 2 ], mxs[ 4 ], mxs[ 5 ], mxs[ 6 ],
	                      mxs[ 7 ], mxs[ 3 ], mxs[ 1 ] );
	at_printf( "Thread %d signals termination.\n", at_self() ->id );
	at_mutex_unlock( ( at_mutex_t* ) mutex );
	at_printf( "Thread %d terminates.\n", at_self() ->id );
}

void thread_body_e( at_word_t c_mutexes, at_word_t mutex )
{
	at_generic_c_lock_t** mxs = ( at_generic_c_lock_t** ) c_mutexes;
	at_printf( "Thread %d started.\n", at_self() ->id );
	AT_ACQUIRE_C_LOCKS_2( mxs[ 1 ], mxs[ 0 ] );
	at_printf( "Thread %d acquired conjunctive mutex.\n", at_self() ->id );
	at_yield();
	at_printf( "Thread %d releases conjunctive mutex.\n", at_self() ->id );
	AT_RELEASE_C_LOCKS_2( mxs[ 1 ], mxs[ 0 ] );
	at_printf( "Thread %d signals termination.\n", at_self() ->id );
	at_mutex_unlock( ( at_mutex_t* ) mutex );
	at_printf( "Thread %d terminates.\n", at_self() ->id );
}

