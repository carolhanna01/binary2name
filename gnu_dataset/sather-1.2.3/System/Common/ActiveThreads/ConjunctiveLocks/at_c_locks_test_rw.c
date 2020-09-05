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

typedef enum { READER, WRITER } WHO;

void single_test();
void locker( at_word_t, at_word_t, at_word_t, at_word_t, at_word_t );
void holder( at_word_t, at_word_t, at_word_t, at_word_t, at_word_t );
void tester( at_word_t, at_word_t, at_word_t, at_word_t, at_word_t,
             at_word_t );

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
	at_rw_c_lock_t * rwlock = at_rw_c_lock_create();
	at_generic_c_lock_t* holder_lock =
	    ( at_generic_c_lock_t* ) at_mutex_c_lock_create();
	at_generic_c_lock_t* tester0_lock =
	    ( at_generic_c_lock_t* ) at_mutex_c_lock_create();
	at_generic_c_lock_t* tester1_lock =
	    ( at_generic_c_lock_t* ) at_mutex_c_lock_create();
	at_generic_c_lock_t* tester2_lock =
	    ( at_generic_c_lock_t* ) at_mutex_c_lock_create();

	WHO who = WRITER;
	do {
		/* Create a thread that holds the initial locks */
		at_mutex_t* mutex = at_mutex_create();
		at_mutex_lock( mutex );
		at_create_5( at_get_focus(), AT_UNBOUND, locker, ( at_word_t ) mutex,
		             ( at_word_t ) holder_lock, ( at_word_t ) tester0_lock,
		             ( at_word_t ) tester1_lock, ( at_word_t ) tester2_lock );
		/* wait for the thread to terminate */
		at_mutex_lock( mutex );

		/* Create a holder */
		at_create_5( at_get_focus(), AT_UNBOUND, holder,
		             ( at_word_t ) ( ( who == WRITER )
		                             ? rwlock->writer : rwlock->reader ),
		             ( at_word_t ) holder_lock, ( at_word_t ) tester0_lock,
		             ( at_word_t ) tester1_lock, ( at_word_t ) tester2_lock );

		/* Create a tester */
		at_create_6( at_get_focus(), AT_UNBOUND, tester, ( at_word_t ) who,
		             ( at_word_t ) rwlock,
		             ( at_word_t ) holder_lock, ( at_word_t ) tester0_lock,
		             ( at_word_t ) tester1_lock, ( at_word_t ) tester2_lock );

		/* Wait for the tester to terminate */
		at_acquire_single_c_lock( tester0_lock );
		at_release_single_c_lock( tester0_lock );

		/* Swap the tests */
		if ( who == WRITER )
			who = READER;
		else
			who = WRITER;

	} while ( who == READER );
}

void locker( at_word_t mutex_,
             at_word_t holder_lock_, at_word_t tester0_lock_,
             at_word_t tester1_lock_, at_word_t tester2_lock_ )
{
	at_mutex_t * mutex = ( at_mutex_t* ) mutex_;
	at_generic_c_lock_t* holder_lock = ( at_generic_c_lock_t* ) holder_lock_;
	at_generic_c_lock_t* tester0_lock = ( at_generic_c_lock_t* ) tester0_lock_;
	at_generic_c_lock_t* tester1_lock = ( at_generic_c_lock_t* ) tester1_lock_;
	at_generic_c_lock_t* tester2_lock = ( at_generic_c_lock_t* ) tester2_lock_;

	/* Initially lock the locks */
	at_acquire_single_c_lock( holder_lock );
	at_acquire_single_c_lock( tester0_lock );
	at_acquire_single_c_lock( tester1_lock );
	at_acquire_single_c_lock( tester2_lock );

	at_mutex_unlock( mutex );
}

void holder( at_word_t rwlock_,
             at_word_t holder_lock_, at_word_t tester0_lock_,
             at_word_t tester1_lock_, at_word_t tester2_lock_ )
{
	at_generic_c_lock_t * rwlock = ( at_generic_c_lock_t* ) rwlock_;
	at_generic_c_lock_t* holder_lock = ( at_generic_c_lock_t* ) holder_lock_;
	at_generic_c_lock_t* tester0_lock = ( at_generic_c_lock_t* ) tester0_lock_;
	at_generic_c_lock_t* tester1_lock = ( at_generic_c_lock_t* ) tester1_lock_;
	at_generic_c_lock_t* tester2_lock = ( at_generic_c_lock_t* ) tester2_lock_;

	/* The holder waits for the tester's signal */
	at_acquire_single_c_lock( tester1_lock );

	/* Then lock the rw lock ... */
	at_acquire_single_c_lock( rwlock );

	/* ... and signal the tester */
	at_release_single_c_lock( holder_lock );

	/* wait for the tester's second signal */
	at_acquire_single_c_lock( tester2_lock );

	/* ... and release the rw lock */
	at_release_single_c_lock( rwlock );

	/* release the signalling locks */
	at_release_single_c_lock( tester1_lock );
	at_release_single_c_lock( tester2_lock );
}

void tester( at_word_t who_, at_word_t rwlock_,
             at_word_t holder_lock_, at_word_t tester0_lock_,
             at_word_t tester1_lock_, at_word_t tester2_lock_ )
{
	WHO who = who_;
	at_rw_c_lock_t* rwlock = ( at_rw_c_lock_t* ) rwlock_;
	at_generic_c_lock_t* holder_lock = ( at_generic_c_lock_t* ) holder_lock_;
	at_generic_c_lock_t* tester0_lock = ( at_generic_c_lock_t* ) tester0_lock_;
	at_generic_c_lock_t* tester1_lock = ( at_generic_c_lock_t* ) tester1_lock_;
	at_generic_c_lock_t* tester2_lock = ( at_generic_c_lock_t* ) tester2_lock_;

	/* The tester starts */
	/* The rw lock is not held by any other thread */

	/* Try to get the rw lock as reader */
	if ( at_tryacquire_single_c_lock( rwlock->reader ) ) {
		printf( "Reader Test 01 OK.\n" );
		at_release_single_c_lock( rwlock->reader );
	} else
		printf( "Reader Error 01.\n" );

	/* Try to get the rw lock as writer */
	if ( at_tryacquire_single_c_lock( rwlock->writer ) ) {
		printf( "Writer Test 01 OK.\n" );
		at_release_single_c_lock( rwlock->writer );
	} else
		printf( "Writer Error 01.\n" );

	/* Try to get the rw lock as reader */
	if ( at_tryacquire_single_c_lock( rwlock->reader ) ) {
		printf( "Reader Test 02 OK.\n" );
		at_release_single_c_lock( rwlock->reader );
	} else
		printf( "Reader Error 02.\n" );

	/* Try to get the rw lock as writer */
	if ( at_tryacquire_single_c_lock( rwlock->writer ) ) {
		printf( "Writer Test 02 OK.\n" );
		at_release_single_c_lock( rwlock->writer );
	} else
		printf( "Writer Error 02.\n" );

	/* Signal the holder */
	at_release_single_c_lock( tester1_lock );

	/* Wait for the holder's signal */
	at_acquire_single_c_lock( holder_lock );

	/* Now, the rw lock is held by either a reader or a writer,
	 * 'who' tells us which */

	/* Try to get the rw lock as reader */
	if ( at_tryacquire_single_c_lock( rwlock->reader ) ) {
		( who == WRITER )
		? printf( "Reader Error 03.\n" )
		: printf( "Reader Test 03 OK.\n" );
		at_release_single_c_lock( rwlock->reader );
	} else
		( who == WRITER )
		? printf( "Reader Test 03 OK.\n" )
		: printf( "Reader Error 03.\n" );


	/* Try to get the rw lock as writer */
	if ( at_tryacquire_single_c_lock( rwlock->writer ) ) {
		printf( "Writer Error 03.\n" );
		at_release_single_c_lock( rwlock->writer );
	} else
		printf( "Writer Test 03 OK.\n" );

	/* Try to get the rw lock as reader */
	if ( at_tryacquire_single_c_lock( rwlock->reader ) ) {
		( who == WRITER ) 
		? printf( "Reader Error 04.\n" ) 
		: printf( "Reader Test 04 OK.\n" );
		at_release_single_c_lock( rwlock->reader );
	} else
		( who == WRITER ) 
		? printf( "Reader Test 04 OK.\n" ) 
		: printf( "Reader Error 04.\n" );

	/* Try to get the rw lock as writer */
	if ( at_tryacquire_single_c_lock( rwlock->writer ) ) {
		printf( "Writer Error 04.\n" );
		at_release_single_c_lock( rwlock->writer );
	} else
		printf( "Writer Test 04 OK.\n" );

	/* Signal the holder */
	at_release_single_c_lock( tester2_lock );

	/* Enqueue as reader */
	at_acquire_single_c_lock( rwlock->reader );

	/* Try to get the rw lock as reader again */
	if ( at_tryacquire_single_c_lock( rwlock->reader ) ) {
		printf( "Reader Test 05 OK.\n" );
		at_release_single_c_lock( rwlock->reader );
	} else
		printf( "Reader Error 05.\n" );

	/* Try to upgrade from reader to writer */
	if ( at_tryacquire_single_c_lock( rwlock->writer ) ) {
		printf( "Writer Error 05.\n" );
		at_release_single_c_lock( rwlock->writer );
	} else
		printf( "Writer Test 05 OK.\n" );

	/* Release as reader */
	at_release_single_c_lock( rwlock->reader );

	/* Enqueue as writer */
	at_acquire_single_c_lock( rwlock->writer );

	/* Try to get the rw lock as reader */
	if ( at_tryacquire_single_c_lock( rwlock->reader ) ) {
		printf( "Reader Test 06 OK.\n" );
		at_release_single_c_lock( rwlock->reader );
	} else
		printf( "Reader Error 06.\n" );

	/* Try to get the rw lock as writer */
	if ( at_tryacquire_single_c_lock( rwlock->writer ) ) {
		printf( "Writer Test 06 OK.\n" );
		at_release_single_c_lock( rwlock->writer );
	} else
		printf( "Writer Error 06.\n" );

	/* Release as writer */
	at_release_single_c_lock( rwlock->writer );

	/* Enqueue as reader */
	at_acquire_single_c_lock( rwlock->reader );

	/* Try to get the rw lock as reader again */
	if ( at_tryacquire_single_c_lock( rwlock->reader ) ) {
		printf( "Reader Test 07 OK.\n" );
		at_release_single_c_lock( rwlock->reader );
	} else
		printf( "Reader Error 07.\n" );

	/* Try to upgrade from reader to writer */
	if ( at_tryacquire_single_c_lock( rwlock->writer ) ) {
		printf( "Writer Error 07.\n" );
		at_release_single_c_lock( rwlock->writer );
	} else
		printf( "Writer Test 07 OK.\n" );

	/* Release as reader */
	at_release_single_c_lock( rwlock->reader );

	/* Enqueue as writer */
	at_acquire_single_c_lock( rwlock->writer );

	/* Try to get the rw lock as reader */
	if ( at_tryacquire_single_c_lock( rwlock->reader ) ) {
		printf( "Reader Test 08 OK.\n" );
		at_release_single_c_lock( rwlock->reader );
	} else
		printf( "Reader Error 08.\n" );

	/* Try to get the rw lock as writer */
	if ( at_tryacquire_single_c_lock( rwlock->writer ) ) {
		printf( "Writer Test 08 OK.\n" );
		at_release_single_c_lock( rwlock->writer );
	} else
		printf( "Writer Error 08.\n" );

	/* Release as writer */
	at_release_single_c_lock( rwlock->writer );

	/* release the signalling locks */
	at_release_single_c_lock( holder_lock );

	/* Signal the main process */
	at_release_single_c_lock( tester0_lock );

}
