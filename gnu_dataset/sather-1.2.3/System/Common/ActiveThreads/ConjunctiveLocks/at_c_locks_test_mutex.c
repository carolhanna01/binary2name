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

/* Testing the conjunctive reader/writer locks */

#include "at.h"
#include "at_c_locks.h"

void single_test();
void holder( at_word_t, at_word_t, at_word_t, at_word_t, at_word_t );
void tester( at_word_t, at_word_t, at_word_t, at_word_t, at_word_t );

int main()
{
	/* Initialize Active threads */
	at_init( 0, 0x2000, 0 );
	/* Single lock tests */
	single_test();
	/* Test OK */
	return ( 0 );
}

/* Single lock tests */
void single_test()
{
	at_mutex_t * holder_lock = at_mutex_create();
	at_mutex_t* tester0_lock = at_mutex_create();
	at_mutex_t* tester1_lock = at_mutex_create();
	at_mutex_t* tester2_lock = at_mutex_create();

	at_generic_c_lock_t* c_mutex =
	    ( at_generic_c_lock_t* ) at_mutex_c_lock_create();

	/* Initially, lock the mutexes */
	at_mutex_lock( holder_lock );
	at_mutex_lock( tester0_lock );
	at_mutex_lock( tester1_lock );
	at_mutex_lock( tester2_lock );

	/* Create a holder */
	at_create_5( at_get_focus(), AT_UNBOUND, holder, ( at_word_t ) c_mutex,
	             ( at_word_t ) holder_lock, ( at_word_t ) tester0_lock,
	             ( at_word_t ) tester1_lock, ( at_word_t ) tester2_lock );

	/* Create a tester */
	at_create_5( at_get_focus(), AT_UNBOUND, tester, ( at_word_t ) c_mutex,
	             ( at_word_t ) holder_lock, ( at_word_t ) tester0_lock,
	             ( at_word_t ) tester1_lock, ( at_word_t ) tester2_lock );

	/* Wait for the tester to terminate */
	at_mutex_lock( tester0_lock );
	at_mutex_unlock( tester0_lock );
}

void holder( at_word_t clock_,
             at_word_t holder_lock_, at_word_t tester0_lock_,
             at_word_t tester1_lock_, at_word_t tester2_lock_ )
{
	at_generic_c_lock_t * clock = ( at_generic_c_lock_t* ) clock_;
	at_mutex_t* holder_lock = ( at_mutex_t* ) holder_lock_;
	at_mutex_t* tester0_lock = ( at_mutex_t* ) tester0_lock_;
	at_mutex_t* tester1_lock = ( at_mutex_t* ) tester1_lock_;
	at_mutex_t* tester2_lock = ( at_mutex_t* ) tester2_lock_;

	/* The holder waits for the tester's signal */
	at_mutex_lock( tester1_lock );

	/* Then lock the clock */
	at_acquire_single_c_lock( clock );

	/* ... and signal the tester */
	at_mutex_unlock( holder_lock );

	/* wait for the tester's second signal */
	at_mutex_lock( tester2_lock );

	/* ... and release the rw lock */
	at_release_single_c_lock( clock );

	/* release the signalling locks */
	at_mutex_unlock( tester1_lock );
	at_mutex_unlock( tester2_lock );
}

void tester( at_word_t clock_,
             at_word_t holder_lock_, at_word_t tester0_lock_,
             at_word_t tester1_lock_, at_word_t tester2_lock_ )
{
	at_generic_c_lock_t * clock = ( at_generic_c_lock_t* ) clock_;
	at_mutex_t* holder_lock = ( at_mutex_t* ) holder_lock_;
	at_mutex_t* tester0_lock = ( at_mutex_t* ) tester0_lock_;
	at_mutex_t* tester1_lock = ( at_mutex_t* ) tester1_lock_;
	at_mutex_t* tester2_lock = ( at_mutex_t* ) tester2_lock_;

	/* The tester starts */

	/* Try to get the rw lock as reader */
	if ( at_tryacquire_single_c_lock( clock ) ) {
		printf( "Mutex Test 01 OK.\n" );
		at_release_single_c_lock( clock );
	} else
		printf( "Mutex Error 01.\n" );

	/* Try to get the rw lock as reader */
	if ( at_tryacquire_single_c_lock( clock ) ) {
		printf( "Mutex Test 02 OK.\n" );
		at_release_single_c_lock( clock );
	} else
		printf( "Mutex Error 02.\n" );

	/* Signal the holder */
	at_mutex_unlock( tester1_lock );

	/* Wait for the holder's signal */
	at_mutex_lock( holder_lock );

	/* Try to get the locked mutex */
	if ( at_tryacquire_single_c_lock( clock ) ) {
		printf( "Mutex Error 03.\n" );
		at_release_single_c_lock( clock );
	} else
		printf( "Mutex Test 03 OK.\n" );

	/* Try to get the locked mutex */
	if ( at_tryacquire_single_c_lock( clock ) ) {
		printf( "Mutex Error 04.\n" );
		at_release_single_c_lock( clock );
	} else
		printf( "Mutex Test 04 OK.\n" );

	/* Signal the holder */
	at_mutex_unlock( tester2_lock );

	/* Enqueue as reader */
	at_acquire_single_c_lock( clock );

	/* Try to get the mutex again */
	if ( at_tryacquire_single_c_lock( clock ) ) {
		printf( "Mutex Test 05 OK.\n" );
		at_release_single_c_lock( clock );
	} else
		printf( "Mutex Error 05.\n" );

	/* Try to get the mutex again */
	if ( at_tryacquire_single_c_lock( clock ) ) {
		printf( "Mutex Test 05 OK.\n" );
		at_release_single_c_lock( clock );
	} else
		printf( "Mutex Error 05.\n" );

	/* Release as writer */
	at_release_single_c_lock( clock );

	/* release the signalling locks */
	at_mutex_unlock( holder_lock );

	/* Signal the main process */
	at_mutex_unlock( tester0_lock );

}
