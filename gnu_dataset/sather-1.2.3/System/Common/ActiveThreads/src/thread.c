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

#include "qt.h"
#include "at-int.h"

volatile int at_thr_num = 0;

AT_SPINLOCK_DEC( thr_num_lck );

void at_thread_elt_print( void *d )
{
	at_thread_t * t;
	t = ( at_thread_t * ) d;
	fprintf( stderr, "[%d]  ", t->id );
}

void at_thread_pool_print( at_pool_t *p )
{
	AT_POOL_LOCK( p );
	fprintf( stderr, " size |%d|  ", at_pool_size( p ) );
	at_pool_apply( p, at_thread_elt_print );
	fprintf( stderr, "\n\n" );
	AT_POOL_UNLOCK( p );
}


void at_thread_elt_init( void *d )
{
	at_thread_t * t;

	t = ( at_thread_t * ) d;


	AT_SPINLOCK_LOCK( thr_num_lck );
	t->id = ( int ) at_thr_num;
	at_thr_num++;
	AT_SPINLOCK_UNLOCK( thr_num_lck );

	t->sp = NULL;
	t->state = AT_ZOMBIE;

	/*
	t->sto = MALLOC_THREAD_STACK(at_cpu()); 
	if(at_local_size) {
	  t->thread_local = (void *) at_malloc(at_local_size);
	} */
	t->sto = NULL;
	t->thread_local = NULL;
}




