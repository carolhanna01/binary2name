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


/* 
   Oh, well, in an attempt to coexist with all Solaris stuff that is
   thread safe (i.e. serialized), we do the lookup by calling _lwp_self()
   with an associated trap and use the result as index into a table that
   maps from lwps to lwp private storage.

   This does take about 2us (compared to 1 cycle if the address is in g7),
   but it allowes us to coexist with Solaris threads and locks which are
   used by all IO/Unix/etc calls. Luckily, the private LWP memory is 
   used only when there are no "work" threads and a "main" spinning thread
   is awakened. Thus, the context switch to a "main" thread will take
   about 2us more that a regular context swap, but it should happen 
   very rarely. The "main" thread runs until the runable queue is non empty.
   The switch from the "main" thread to the work thread is no more
   expensive the regular work/work thread switch.

   This seems like a reasonable compromise to make things relying on
   Solaris happy

   Failed attempt, still the slow way is more portable.
   Fast context swap: 4us
   slow context swap: 7us on SS10, 50Mhz
   */


register at_current_sp asm ("sp");
#define AT_CURRENT_SP at_current_sp

   
#define LWP_SET_PRIVATE(new) asm volatile \
("mov %1, %0" : "=r" (lwp_private) : "r" (new))
#define LWP_GET_PRIVATE lwp_private


int LWP_INIT(int concurrency);  /* Initialize machine-dependent stuff */

void LWP_START(void start_routine(void *), void *arg, caddr_t stack_base, 
	      size_t stack_size, lwp_private_t *lwp_private );

register lwp_private_t *lwp_private asm ("g7");	

void *MALLOC_THREAD_STACK(int pnum);

extern int at_nprocs;
#define at_nprocs() at_nprocs

#endif 








