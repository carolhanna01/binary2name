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
 * Some additional functions useful for active message programs
 *
 * Version 1.0 (released for Sather 1.0.9) by Claudio Fleiner (fleiner@icsi.berkeley.edu)
 */
#include <signal.h>
#include <stdio.h>
#include <stddef.h>
#include <unistd.h>
#include <malloc.h>
#include <assert.h>
#include <memory.h>
#include <sys/types.h>
#ifndef __CYGWIN32__
# include <sys/ipc.h>
# include <sys/shm.h>
#endif
#include <errno.h>

#include "am.h"
#include "am_int.h"


#ifdef SUNOS4
int fprintf(FILE*,const char *,...);
int printf(const char *,...);
int fflush(FILE *);
void perror(char *);
#endif

int am_clusters;
int am_my_cluster_id;

void am_dummy(vnn_t from,void *a,int b,void *c) { }

void cntr_incr(cntr_t cnt)
{
	CNTR_INCR(cnt);
}
void cntr_incr_by(cntr_t cnt,long v)
{
	CNTR_INCR_BY(cnt,v);
}
void r_cntr_incr(vnn_t from,long cnt)
{
	CNTR_INCR((cntr_t)cnt);
}
void r_cntr_incr_mem(vnn_t from,void *a,int size,void *cnt)
{
	CNTR_DECR((cntr_t)cnt);
}
void cntr_decr(cntr_t cnt)
{
	CNTR_DECR(cnt);
}
void cntr_decr_by(cntr_t cnt,long v)
{
	CNTR_DECR_BY(cnt,v);
}
void r_cntr_decr(vnn_t from,long cnt)
{
	CNTR_DECR((cntr_t)cnt);
}
void r_cntr_decr_mem(vnn_t from,void *a,int size,void *cnt)
{
	CNTR_DECR((cntr_t)cnt);
}
void cntr_wait_for_zero(cntr_t cnt)
{
	CNTR_WAIT_FOR_ZERO(cnt);
}
int cntr_is_zero(cntr_t cnt)
{
	return CNTR_IS_ZERO(cnt);
}
void ta_sema_signal(ta_sema_t cnt)
{
	TA_SEMA_SIGNAL(cnt);
}
void r_ta_sema_signal(vnn_t from,long cnt)
{
	TA_SEMA_SIGNAL((ta_sema_t)cnt);
}
void r_ta_sema_signal_mem(vnn_t from,void *a,int size,void *cnt)
{
	TA_SEMA_SIGNAL((ta_sema_t)cnt);
}
void ta_sema_wait(ta_sema_t cnt)
{
	TA_SEMA_WAIT(cnt);
}
void r_ta_sema_signal_reply_mem(vnn_t from,void *a,int size,void * cnt)
{
	am_reply_1(from,r_ta_sema_signal,(long)cnt);
}
void r_cntr_incr_reply_mem(vnn_t from,void *a,int size,void * cnt)
{
	am_reply_1(from,r_cntr_incr,(long)cnt);
}
void r_cntr_decr_reply_mem(vnn_t from,void *a,int size,void * cnt)
{
	am_reply_1(from,r_cntr_decr,(long)cnt);
}

#ifdef AM_THREADS
void yield(void) { YIELD; }
void r_lck_unlock(vnn_t from,long lck)
{
	LCK_UNLOCK((lock_t)lck);
}
void r_lck_unlock_mem(vnn_t from,void *a,int size,void *lck)
{
	LCK_UNLOCK((lock_t)lck);
}
void r_lck_unlock_reply_mem(vnn_t from,void *a,int size,void *lck)
{
	am_reply_1(from,r_lck_unlock,(long)lck);
}
void r_sema_signal(vnn_t from,long sem)
{
	SEMA_SIGNAL((sema_t)sem);
}
void r_sema_signal_mem(vnn_t from,void *s,int size,void *sem)
{
	SEMA_SIGNAL((sema_t)sem);
}
void r_sema_signal_reply_mem(vnn_t from,void *s,int size,void *sem)
{
	am_reply_1(from,r_sema_signal,(long)sem);
}
#endif
