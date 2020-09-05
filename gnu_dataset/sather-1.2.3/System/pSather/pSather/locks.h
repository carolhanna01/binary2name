/*------------------------->  ANSI C - headerfile  <-------------------------*/
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
 * Interface for the lock manager
 *
 * Version 1.0 (released for 1.0.9) by Claudio Fleiner (fleiner@icsi.berkeley.edu)
 */
#ifndef _LOCKS_LH_
#define _LOCKS_LH_

#include <sys/types.h>
#include <sys/timeb.h>
#include <sys/time.h>
#include <stdio.h>
#include "pSather.h"

#define LOWEST_BIT(a) ((a)&0xFFFF? \
                          ((a)&0xFF? \
			     ((a)&0xF? \
			         ((a)&0x03? ((a)&0x01?0:1):((a)&0x04?2:3)) \
				:((a)&0x30? ((a)&0x10?4:5):((a)&0x40?6:7)) \
			     ):((a)&0xF00? \
			         ((a)&0x0300? ((a)&0x0100? 8: 9):((a)&0x0400?10:11)) \
				:((a)&0x3000? ((a)&0x1000?12:13):((a)&0x4000?14:15)) \
			     ) \
			  ):((a)&0xFF0000? \
			     ((a)&0xF0000? \
			         ((a)&0x030000? ((a)&0x010000?16:17):((a)&0x040000?18:19)) \
				:((a)&0x300000? ((a)&0x100000?20:21):((a)&0x400000?22:23)) \
			     ):((a)&0xF000000? \
			         ((a)&0x03000000? ((a)&0x01000000?24:25):((a)&0x04000000?26:27)) \
				:((a)&0x30000000? ((a)&0x10000000?28:29):((a)&0x40000000?30:31)) \
			     ) \
			  ))

struct LOCK_T_L {
	OB_HEADER header;
				/* a list of functions called when locking,
				 * unlocking or trylocking the lock.
				 * Additionally some functions to create and
				 * delete the lock
				 */
	struct LOCK_F_struct *fnct;
	unsigned long seq;
	unsigned hidden_reserved:1;
};

typedef struct DLOCK_struct {
	LOCK *lck;
	short type;
	char count;
} DLOCK;

typedef struct LOCK_T_struct {
	struct LOCK_T_L l;
	DLOCK dlock[1];
} *LOCK_T;

typedef int (*LOCK_FUNC)(LOCK_T,BR_thread_t,short);
typedef void (*LOCK_PROC)(LOCK_T,BR_thread_t,short);
typedef LOCK_T (*LOCK_FUNC_V)();

void p_init_locks(void);
void p_ex_init_thread();

#endif
