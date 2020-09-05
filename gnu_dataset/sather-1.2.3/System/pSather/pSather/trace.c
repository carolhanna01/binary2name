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
 * Trace functions
 *
 * Version 1.0 (released for 1.0.9) by Claudio Fleiner (fleiner@icsi.berkeley.edu)
 */
#include <stdio.h>
#include <string.h>
#include <unistd.h>
#include <sys/types.h>
#include <sys/stat.h>
#include <sys/time.h>
#include "pSather.h"

static FILE *trf = NULL;
#ifdef SPINLOCK_LOCK
static spinlock_t lck;
#else
static BR_lock_t lck;
#endif
extern char *bin_prog_name;
extern int fflush( FILE * );

static int do_trace = 1;
void use_trace( int i )
{
	do_trace = i;
}
static void init_trace()
{
	char c[ 1000 ];
	strcpy( c, bin_prog_name );
	strcat( c, ".tr" );
	mkdir( c, 0755 );
	sprintf( c + strlen( c ), "/CL%03d.trace", HERE );
	trf = fopen( c, "w" );
	if ( trf == NULL ) {
		fprintf( stderr, "cannot open trace file %s\n", c );
		abort();

	}
#ifdef SPINLOCK_LOCK
	lck = 0;
#else

	lck = BR_LOCK_CREATE();
#endif
}

void trace( const char *s, long a, long b, long c, long d, long e, long f )
{
	struct timeval now;
	char p[ 30 ];
	int i;
	/*extern int gettimeofday(struct timeval *,void *);*/
	extern int start_sec;
	extern void init_thread();

	if ( trf == NULL )
		init_trace();
	if ( !do_trace )
		return ;
	if ( ( BR_GET_THREAD_LOCAL() ) == NULL )
		init_thread();
#ifdef SPINLOCK_LOCK

	SPINLOCK_LOCK( lck );
#else

	BR_LOCK( lck );
#endif

	gettimeofday( &now, NULL );
	i = fprintf( trf, "%03ld.%06ld ", now.tv_sec - start_sec, now.tv_usec );
	i += fprintf( trf, "%02d ", HERE );
	i += fprintf( trf, "%s ", thr_print_id( thr_ps_id(), p ) );
	i += fprintf( trf, "%s:%ld ", ( ( LOCAL_MEM ) ( BR_GET_THREAD_LOCAL() ) ) ->file, ( ( LOCAL_MEM ) ( BR_GET_THREAD_LOCAL() ) ) ->line );
	while ( i++ < 45 )
		fprintf( trf, " " );
	fprintf( trf, "- " );
	fprintf( trf, s, a, b, c, d, e, f );
	fprintf( trf, "\n" );
	fflush( trf );
#ifdef SPINLOCK_LOCK

	SPINLOCK_UNLOCK( lck );
#else

	BR_UNLOCK( lck );
#endif
}

