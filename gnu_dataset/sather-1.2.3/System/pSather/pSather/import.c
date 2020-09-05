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
 * Cache management functions
 *
 * Version 1.0 (released for 1.0.9) by Claudio Fleiner (fleiner@icsi.berkeley.edu)
 */
#include <stdio.h>
#include <stdarg.h>
#include <assert.h>
#include <string.h>
#include "pSather.h"
#include "locks.h"
#include "local.h"
#include "stat.h"
#include "debug.h"
#include "trace.h"

unsigned long import_ctr; /* local per process (should be local per cluster probably) */
#ifdef SPINLOCK_LOCK
static spinlock_t import_lck;
#define LOCK_IMPORT SPINLOCK_LOCK(import_lck)
#define UNLOCK_IMPORT SPINLOCK_UNLOCK(import_lck)
#else
static BR_lock_t import_lck;
#define LOCK_IMPORT BR_LOCK(import_lck)
#define UNLOCK_IMPORT BR_UNLOCK(import_lck)
#endif

void init_import_export()
{
#ifndef SPINLOCK_LOCK
	import_lck=BR_LOCK_CREATE();
#endif
	import_ctr=0;
#ifdef IMPORT_CACHE
	{
		extern void init_cache(void);
		init_cache();
	}
#endif
}

void p_import()
{
	unsigned long l;
	LOCK_IMPORT;l= ++import_ctr;UNLOCK_IMPORT;
	((LOCAL_MEM)(BR_GET_THREAD_LOCAL()))->import=l;
}

void p_export() /* we have only sync. exports anyway, so this function is a noop */
{
	LOCAL_MEM m=((LOCAL_MEM)(BR_GET_THREAD_LOCAL()));
	STAT_ADD(EXPORTS_WAITING,m->exports_waiting);
	am_wait_for(m->exports_waiting==0);
}

