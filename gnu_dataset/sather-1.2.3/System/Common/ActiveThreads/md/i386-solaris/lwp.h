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
#include <sys/lwp.h>
#include <ucontext.h>
#include <signal.h>
#include "at-lwp.h"
#include "wrappers.h"


/* 
 * There seems to be a problems with _lwp_setprivate()/_lwp_getprivate()
 * for the current version of Solaris for PCs (2.5.1). 
 * _lwp_setprivate(x) followed by _lwp_getprivate() returns x only 
 * for the first iteration. For all following calls, lwp_getprivate()
 * returns the same number (different from x) and independent of 
 * what the private storage was set to previously.
 *
 * We still used _lwp_setprivate/_lwp_getprivate, but we are very
 * careful to call _lwp_setprivate only once for each lwp. This seems
 * to work. The alternative is to use _lwp_self() to get the currenct
 * lwp number, but this call is about 6 times more expensive than
 * _lwp_get_private(): 2.5 vs 0.4 us on 200Mhz Pentium Pro.
 *
 * Too bad, we cannot reserve a register for these things like on the
 * sparcs.
 */

#define LWP_GET_PRIVATE    ((lwp_private_t *)_lwp_getprivate())
#define LWP_SET_PRIVATE(x) _lwp_setprivate(x)


int LWP_INIT(int);  /* Initialize machine-dependent stuff */

void LWP_START(void start_routine(void *), void *arg, caddr_t stack_base, 
	      size_t stack_size, lwp_private_t *lwp_private );


void *MALLOC_THREAD_STACK();

extern int at_nprocs;
#define at_nprocs() at_nprocs
#endif  




