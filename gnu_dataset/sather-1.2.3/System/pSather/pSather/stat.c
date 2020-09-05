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
 * Some functions to get statistics during execution of pSather programs.
 *
 * Version 1.0 (released for 1.0.9) by Claudio Fleiner (fleiner@icsi.berkeley.edu)
 */
#include <stdio.h>
#include <sys/stat.h>
#include <assert.h>
#include "pSather.h"
#include "stat.h"

#ifdef PSATHER_STAT
#define stat_ctr ((unsigned long *)CLMEM)
#ifdef SPINLOCK_LOCK
#define STAT_LOCK(a) 		SPINLOCK_LOCK(stat_lck[a])
#define STAT_UNLOCK(a) 		SPINLOCK_UNLOCK(stat_lck[a])
#define STAT_LOCK_CREATE	0
static spinlock_t stat_lck[LAST+1];
#else
static BR_lock_t stat_lck[LAST+1];
#define STAT_LOCK(a) 		BR_LOCK(stat_lck[a])
#define STAT_UNLOCK(a) 		BR_UNLOCK(stat_lck[a])
#define STAT_LOCK_CREATE	BR_LOCK_CREATE()
#endif


static int statistics=1;
void use_statistics(int i)
{ statistics=i; }
void init_stat()
{
	int i;
	for(i=0;i<LAST;i++) {
		stat_ctr[i]=0;
		stat_lck[i]=STAT_LOCK_CREATE;
	}
}


void stat_add(int i,int j)
{
	if(!statistics) return;
	assert(i>=0 && i<LAST);
	STAT_LOCK(i);
	stat_ctr[i]+=j;
	STAT_UNLOCK(i);
}
void stat_incr(int i)
{
	if(!statistics) return;
	assert(i>=0 && i<LAST);
	STAT_LOCK(i);
	stat_ctr[i]++;
	STAT_UNLOCK(i);
}

void stat_specul_no_wait()
{
	STAT(PREFETCH_SPECUL_NO_WAIT);
}
void stat_specul_wait(int a)
{
	STAT_ADD(PREFETCH_SPECUL_WAIT,a);
}
void stat_no_wait()
{
	STAT(PREFETCH_NO_WAIT);
}
void stat_pre_wait(int a)
{
	STAT_ADD(PREFETCH_WAIT,a);
}

void stat_print(char *s)
{
	FILE *f;
	char buf[256];
	if(strlen(s)>240) {
		fprintf(stderr,"cannot open statistic file %s.st/CL%03d.stat (filename too long)\n",s,HERE);
		return;
	}
	strcpy(buf,s);strcat(buf,".st");mkdir(buf,0755);
	sprintf(buf,"%s.st/CL%03d.stat",s,HERE);
	f=fopen(buf,"w");
	if(f==NULL) {
		fprintf(stderr,"cannot open file %s for writing\n",buf);
		return;
	}
#define P(x)	if(stat_ctr[x]) fprintf(f,"%03d %3d %-20s %ld\n",HERE,x,#x,stat_ctr[x])

	P(IMPORT_CACHE_HIT);
	P(IMPORT_CACHE_MISS);
	P(READ_R);
	P(READ_VA);
	P(READ_VS1);
	P(READ_VS2);
	P(READ_VS4);
	P(READ_VS8);
	P(READ_V);
	P(PREFETCH_R);
	P(PREFETCH_VA);
	P(PREFETCH_VS1);
	P(PREFETCH_VS2);
	P(PREFETCH_VS4);
	P(PREFETCH_VS8);
	P(PREFETCH_V);
	P(PREFETCH_CACHE_HIT);
	P(PREFETCH_NO_WAIT);
	P(PREFETCH_WAIT);
	P(PREFETCH_SPECUL_NO_WAIT);
	P(PREFETCH_SPECUL_WAIT);
	P(WRITE_R);
	P(WRITE_VA);
	P(WRITE_VS1);
	P(WRITE_VS2);
	P(WRITE_VS4);
	P(WRITE_VS8);
	P(WRITE_V);
	P(POST_WRITE_R);
	P(POST_WRITE_VA);
	P(POST_WRITE_VS1);
	P(POST_WRITE_VS2);
	P(POST_WRITE_VS4);
	P(POST_WRITE_VS8);
	P(POST_WRITE_V);
	P(REMOTE_TAG);
	P(REMOTE_WRITE);
	P(EXPORTS_WAITING);
	P(THREAD_STARTED);
	P(FAST_LOCK);
	P(LOCK_STMT);
	P(REMOTE_EXEC);
	fclose(f);
}
#else
void use_statistics(int i) {}
#endif


