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
 * Some useful functions for debugging
 *
 * Version 1.0 (released for 1.0.9) by Claudio Fleiner (fleiner@icsi.berkeley.edu)
 */
#include <stdio.h>
#include <sys/time.h>
/*#include <varargs.h>*/
#include <stdarg.h>
#include "pSather.h"
#include "local.h"

#ifdef DEBUG
int fprintf(FILE *,const char *,...);
int printf(const char *,...);
int gettimeofday(struct timeval *,void *);
int fflush(FILE *);
BR_thread_t thr_ps_id(void);

/* void debug(const char *file,long line,const char *cc,long a,long b,long c,long d,long e,long f)  */
void debug(va_alist)
va_dcl
{
	va_list ap;
	static BR_lock_t lck=NULL;
	struct timeval now;
	extern int start_sec;
	extern void init_thread();
	char p[30],p2[30];
	char x[200];
	char *file;
	long line;
	char *form;

	va_start(ap);
	file=va_arg(ap,char*);
	line=va_arg(ap,long);
	form=va_arg(ap,char *);
	if((BR_GET_THREAD_LOCAL())==NULL) init_thread();
	thr_ps_id(); /* make sure ID is already there (otherwise we deadlock) */
	if(lck==NULL) lck=BR_LOCK_CREATE();
	BR_LOCK(lck);
	gettimeofday(&now,NULL);

	vsprintf(x,form,ap);
	printf("%03ld.%06ld %02d %s %s %s:%ld - %s\n",now.tv_sec-start_sec,
		now.tv_usec,HERE,thr_print_id(thr_id(),p2),thr_print_id(thr_ps_id(),p),file,line,x);
	fflush(stdout);
	BR_UNLOCK(lck);
}

#endif

#ifdef REGISTER_THREADS
#define MAX_REGS 1000
static struct REG_STRUCT {
	BR_thread_t id;
	BR_thread_t ps_id;
	LOCAL_MEM m;
	char *file;
	int line;
	void (*func)();
	void *arg;
} regs[MAX_REGS];
static BR_lock_t reg_lock=NULL;
static int regsNotInitialized = 1;

void reg_thread(char *file,int line,void (*func)(),void *arg)
{
	int h,f;
	if(reg_lock==NULL) reg_lock=BR_LOCK_CREATE();
	BR_LOCK(reg_lock);
	if ( regsNotInitialized)
	{
		for( h = 0; h < MAX_REGS; h++)
			regs[h].id = BR_INVALID_ID( );
		regsNotInitialized = 0;
	}
	f=h=BR_THREAD_HASH(BR_THREAD_ID())%MAX_REGS;
	while(!BR_THREAD_SAME(regs[h].id,BR_INVALID_ID())) {
		h=(h+1)%MAX_REGS;
		if(h==f) {
			fprintf(stderr,"you are registering too many threads, ignoring this one\n");
			BR_UNLOCK(reg_lock);
			return;
		}
	}
	BR_UNLOCK(reg_lock);
	regs[h].id=BR_THREAD_ID();
	regs[h].ps_id=thr_ps_id();
	regs[h].m=(void*)(BR_GET_THREAD_LOCAL());
	regs[h].file=file;
	regs[h].line=line;
	regs[h].func=func;
	regs[h].arg=arg;
}

void unreg_thread(char *file,int line)
{
	int h,f;
	f=h=BR_THREAD_HASH(BR_THREAD_ID())%MAX_REGS;
	while(!BR_THREAD_SAME(regs[h].id,BR_THREAD_ID())) {
		h=(h+1)%MAX_REGS;
		if(h==f) return;
	}
	regs[h].id=BR_INVALID_ID();
}

void PRS()
{
	int h;
	char p[30];
	BR_LOCK(reg_lock);
	for(h=0;h<MAX_REGS;h++) {
		if(!BR_THREAD_SAME(regs[h].id,BR_INVALID_ID())) {
			printf("%.3d: %s  %s:%d\n",h,thr_print_id(regs[h].id,p),regs[h].file,regs[h].line);
		}
	}
	BR_UNLOCK(reg_lock);
	fflush(stdout);
}

void PR(int h)
{
	char p[30],p2[30];
	BR_LOCK(reg_lock);
	if(!BR_THREAD_SAME(regs[h].id,BR_INVALID_ID())) {
		printf("id: %s    ps_id: %s\n",thr_print_id(regs[h].id,p),thr_print_id(regs[h].ps_id,p2));
		printf("c-file: %s:%d\n",regs[h].file,regs[h].line);
		printf("sather: %s:%d\n",regs[h].m->file,(int)regs[h].m->line);
#ifdef PRINT_BACKTRACE
		printf("trace:  p PT(0x%p)\n",regs[h].m->pFF);
#endif
		printf("function: p 0x%p(0x%p)\n",regs[h].func,regs[h].arg);
		printf("local: p *(LOCAL_MEM)0x%p\n",regs[h].m);
	} else printf("Thread has continued its mission\n");
	BR_UNLOCK(reg_lock);
	fflush(stdout);
}

#endif
