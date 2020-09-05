/*------------------------->  ANSI C - headerfile  <-------------------------*/
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

#ifndef _LWP_H_
#define _LWP_H_

#include <stdio.h>
#include <sys/types.h>
#include "at-lwp.h"


extern lwp_private_t *_lwp_getprivate() ;
extern void _lwp_setprivate(lwp_private_t *private) ;

#define LWP_GET_PRIVATE    _lwp_getprivate()
#define LWP_SET_PRIVATE(x) _lwp_setprivate(x)


int LWP_INIT(int concurrency);  /* Initialize machine-dependent stuff */

void LWP_START(void *start_routine(void *), void *arg, caddr_t stack_base, 
	      size_t stack_size, lwp_private_t *lwp_private );

void LWP_STOP(void) ;

void *MALLOC_THREAD_STACK(int pnum);
extern int at_nprocs;
#define at_nprocs() at_nprocs

#endif 
