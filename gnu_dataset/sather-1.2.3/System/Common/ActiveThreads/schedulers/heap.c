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

#include <stdio.h>
#include <time.h> 
/*#include <sys/time.h>*/
#include <assert.h>
#include <float.h>
#include "heap.h"

/*
 * A simple implementation of a thread heap
 * This has not been optimized at all.
 * For 10000 threads, mean insert time 0.8 us, mean delete 5us
 *
 * Well, it is not just a heap any more. Only up to "threshold"
 * threads are actually kept in a heap. The rest are placed
 * directly to a global thread queue. The queue is consulted
 * when the heap is empty.
 */

/* This data structure must be protected on the outside */

static at_thread_t at_maxthread;

/*
 * q is a "secondary" storage queue in case the heap size 
 * grows above the threshold...
 */
at_heap_t *at_heap_create( at_thread_dqueue_t *dq, int threshold )
{
	at_heap_t * h;
	h = ( at_heap_t * ) malloc( sizeof( at_heap_t ) );
	h->asize = AT_HEAPSIZE;
	h->threshold = threshold;
	h->global_threads = dq;
	h->heap_threads =
	    ( at_thread_t ** ) malloc( sizeof( at_thread_t * ) * AT_HEAPSIZE );
	at_maxthread.priority = FLT_MAX;
	h->heap_threads[ 0 ] = &at_maxthread;
	h->size = 0;
	return h;
}

/* Insert a new thread with priority already set */
/* returns the heap index of the argument thread */
int at_heap_insert( at_heap_t *h, at_thread_t *t )
{
	int res;
	/* For 0 priority, (just created), go directly to the global queue */
	if ( ( t->priority == 0 ) ) {
		t->heap_index = AT_HEAP_ON_QUEUE;
		at_thread_dqueue_put_at_head( h->global_threads, t );
		res = t->heap_index;
	} else {
		h->size++;

		if ( h->size == ( h->asize ) ) {
			h->asize = h->asize << 1;
			h->heap_threads = ( at_thread_t ** ) realloc(
			                      h->heap_threads,
			                      h->asize * sizeof( at_thread_t * ) );
		}
		h->heap_threads[ h->size ] = t;
		t->heap_index = h->size;

		res = at_heap_up( h, h->size );

		/*
		 * Now, if we are over the threshold, bump off an element from
		 * the heap onto a global queue 
		 */
		if ( h->size == h->threshold ) {
			t = h->heap_threads[ h->size ];
			t->heap_index = AT_HEAP_ON_QUEUE;
			t->priority = 0.0;
			t->F = 0.0;
			at_thread_dqueue_put_at_head( h->global_threads, t );
			if ( res == h->size )
				res = -1;
			h->size--;
		}
	}

	return res;
}

int at_heap_up( at_heap_t *h, int k )
{
	float p; /* Priority */
	at_thread_t **threads, *t;

	assert( k >= 1 );
	threads = h->heap_threads;

	t = threads[ k ];
	p = t->priority;

	while ( threads[ k / 2 ] ->priority < p ) {
		threads[ k ] = threads[ k / 2 ];
		threads[ k ] ->heap_index = k;
		k = k >> 1;
	}
	threads[ k ] = t;
	threads[ k ] ->heap_index = k;     /* Set the index to the pos in heap */

	return k;
}

/*
 * A new (greater) priority of t has been already computed
 * Now, simply fix up the heap
 */
int at_heap_change_p( at_heap_t *h, at_thread_t *t, float new_p )
{
	int res;

	/* If t was previously on the global queue, move it to the heap */
	/* The same if the thread was not yet on a heap or queue - initialized */
	if ( AT_HEAP_IS_ON_QUEUE( t ) ) {
		at_thread_dqueue_delete( h->global_threads, t );
		t->priority = new_p;
		t->heap_index = AT_HEAP_ABSENT;
		res = at_heap_insert( h, t );
	} else {
		if ( new_p >= t->priority ) {
			t->priority = new_p;
			res = at_heap_up( h, t->heap_index );
		} else {
			t->priority = new_p;
			res = at_heap_down( h, t->heap_index );
		}
	}
	return res;
}

/* Returns the thread with the largest priority */
at_thread_t *at_heap_top( at_heap_t *h )
{
	at_thread_t * t = NULL, **threads;

	threads = h->heap_threads;
	if ( h->size == 0 ) {
		/* Still, need to try the overflow dqueue */
		/*t = at_thread_dqueue_get(h->global_threads); */
		/* Random element from a queue instead?? */
		t = at_thread_dqueue_get_rand( h->global_threads );
	} else {
		/* The heap proper is non-empty */
		t = threads[ 1 ];
		threads[ 1 ] = threads[ h->size ];
		threads[ 1 ] ->heap_index = 1;
		h->size--;
		at_heap_down( h, 1 );
	}
	if ( t ) {
		t->heap_index = AT_HEAP_ABSENT;
	}
	return t;
}

/* Returns the bottom element of a specified heap (proper)
   Does not look in the overflow queue if the heap itself is empty */

at_thread_t *at_heap_bottom( at_heap_t *h )
{
	at_thread_t * t;

	if ( h->size == 0 ) {
		t = NULL;
	} else {
		t = h->heap_threads[ h->size ];
		h->size--;
		t->heap_index = AT_HEAP_ABSENT;
	}
	return t;
}

int at_heap_down( at_heap_t *h, int k )
{
	at_thread_t **threads, *t;
	int i, j;
	float p;

	threads = h->heap_threads;
	t = threads[ k ];
	p = t->priority;

	while ( k <= h->size / 2 ) {
		j = k << 1;
		if ( j < h->size ) {
			if ( threads[ j ] ->priority < threads[ j + 1 ] ->priority ) {
				j++;
			}
		}
		if ( p >= threads[ j ] ->priority )
			break;
		threads[ k ] = threads[ j ];
		threads[ k ] ->heap_index = k;
		k = j;
	}
	threads[ k ] = t;
	threads[ k ] ->heap_index = k;
	return k;
}

void at_heap_print( at_heap_t *h )
{
	at_thread_t * t;
	printf( "------------------------ HEAP ----------------------\n" );
	while ( t = at_heap_top( h ) ) {
		printf( "%f  ", t->priority );
	}
	printf( "\n" );
}

/*
 * A little drive program that reads priorities from stdio
 * and adds threads to a heap. On reading 0, it ptints 
 * the heap and terminate 
 */
driver()
{
	/*  hrtime_t start, stop;*/
	clock_t start, stop;
	at_heap_t *h;
	float p = -1;
	at_thread_t *t;
	int i;
	at_thread_dqueue_t dq;

	at_thread_dqueue_init( &dq );

	h = at_heap_create( &dq, AT_HEAPSIZE );

	/*  while(p!=0.0){
	  printf("Enter priortity: ");
	  scanf("%f", &p);
	  t = (at_thread_t *)malloc(sizeof(at_thread_t));
	  t->priority = p;
	  i = at_heap_insert(h, t);
	}

	for(i=1; i<=h->size; i++){
	  printf("[%d]  %f\n", i, h->heap_threads[i]->priority);
	}


	printf("Enter [index, delta] ");
	scanf("%i%f", &i, &p);
	h->heap_threads[i]->priority += p;
	at_heap_up(h, i);

	at_heap_print(h); */

	/*start = gethrtime();*/
	start = clock();
	for ( i = 0; i < AT_HEAPSIZE; i++ ) {
		p = ( float ) rand();
		t = ( at_thread_t * ) malloc( sizeof( at_thread_t ) );
		t->priority = p;
		at_heap_insert( h, t );
	}
	/*stop = gethrtime();*/
	stop = clock();
	printf( "MEAN INSERT TIME:  %e\n",
	        ( double ) ( stop - start ) / ( 1000 * AT_HEAPSIZE ) );

	i = 0;
	start = gethrtime();
	while ( t = at_heap_top( h ) ) {
		i++;
	}
	stop = gethrtime();
	printf( "MEAN DELETE TIME:  %e\n", ( double ) ( stop - start ) / ( i ) );
	printf( "DELETED: %d\n", i );
}








