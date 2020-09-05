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
 * Code to attach new threads to an object, and for remote execution
 *
 * Version 1.0 (released for 1.0.9) by Claudio Fleiner (fleiner@icsi.berkeley.edu)
 */
#include <stdio.h>
#include <setjmp.h>
#include <signal.h>

#include "pSather.h"
#include "locks.h"
#include "local.h"
#include "trace.h"
#include "memory.h"
#include "stat.h"
#undef DEBUG
#include "debug.h"
#include "attach.h"

#define PARLOOP_QUEUE_SIZE    1000
#define MAX_THREADS 		50 /* should be larger than the largest */
				   /* cluster size */
#define MAX_PARLOOPS		50 /* max number of parloops that can */
				   /* enqueue their jobs at the same time */

static struct func_arg_struct {
	int job;
	FORK_FUNC func;
	FOB self;
	FOB arg;
	FOB attach;
} func_args[PARLOOP_QUEUE_SIZE];

#define QUEUE_SIZE (head>=tail?head-tail:PARLOOP_QUEUE_SIZE+head-tail)

static int head=0,tail=0;
static spinlock_t queue_lock=0;

static BR_lock_t thread_locks[MAX_THREADS];
static char   thread_wait=1;
static char   thread_wait_0=1;

static int job_number=0;
static BR_lock_t     parloop_enter_lock;
static BR_lock_t 	  parloop_locks[MAX_PARLOOPS];
static char 	  parloop_used[MAX_PARLOOPS];
static char 	  parloop_more_work[MAX_PARLOOPS];
static int    	  job_counter[MAX_PARLOOPS];
static spinlock_t job_counter_lock[MAX_PARLOOPS];
static BR_lock_t parloop_wait;

void parloop_begin() {
	int i;
	BR_LOCK(parloop_enter_lock);
	while(parloop_used[job_number]) {
		job_number=(job_number+1)%MAX_PARLOOPS;
		if(job_number==0) BR_THREAD_YIELD();
	}
	if(thread_wait) {
		thread_wait=0;
		/* printf("setting thread_wait to 0\n"); */
		for(i=1;i<BR_PROCESSORS();i++)
			BR_UNLOCK(thread_locks[i]);
	}
	parloop_used[job_number]=1;
	job_counter[job_number]=1;
	/* printf("BEGINNING PARLOOP\n"); */
}

void parloop_end()
{
	int i,new_job,job=job_number;
	/* printf("PARLOOP DONE, WATIING\n"); */
	if(thread_wait_0) {
		thread_wait_0=0;
		BR_UNLOCK(thread_locks[0]);
	}
	new_job=job_number=(job_number+1)%MAX_PARLOOPS;
	SPINLOCK_LOCK(job_counter_lock[job]);
	BR_UNLOCK(parloop_enter_lock);
	job_counter[job]--;
	i=job_counter[job];
	SPINLOCK_UNLOCK(job_counter_lock[job]);
	if(i!=0) BR_LOCK(parloop_locks[job]);
	parloop_used[job]=0;
	if(BR_TRY_LOCK(parloop_enter_lock)) {
		/* stop all threads if no other parloop is running */
		if(job_number==new_job) {
			thread_wait=1;
			/* printf("setting thread_wait to 1\n"); */
		}
		BR_UNLOCK(parloop_enter_lock);
	}
	/* printf("PARLOOP DONE\n"); */
}

void parloop_enqueue(FORK_FUNC func,FOB self,FOB arg,FOB attach)
{
	SPINLOCK_LOCK(queue_lock);
	while(QUEUE_SIZE>PARLOOP_QUEUE_SIZE-3) {
		SPINLOCK_LOCK(job_counter_lock[job_number]);
		parloop_more_work[job_number]=1;
		if(thread_wait_0) {
			thread_wait_0=0;
			BR_UNLOCK(thread_locks[0]);
		}
		SPINLOCK_UNLOCK(queue_lock);
		BR_LOCK(parloop_locks[job_number]);
		SPINLOCK_LOCK(queue_lock);
	}
	func_args[head].job=job_number;
	func_args[head].func=func;
	func_args[head].self=self;
	func_args[head].arg=arg;
	func_args[head].attach=attach;
	/* printf("ENQUEUING WORK, QUEUE_SIZE=%d, pos=%d\n",QUEUE_SIZE,head); */
	head=(head+1)%PARLOOP_QUEUE_SIZE;
	SPINLOCK_LOCK(job_counter_lock[job_number]);
	job_counter[job_number]++;
	SPINLOCK_UNLOCK(job_counter_lock[job_number]);
	SPINLOCK_UNLOCK(queue_lock);
}

static void parloop_work_thread(vnn_t from,BR_word_t n)
{
	int job_entry,job,last_job=-1;
	struct func_arg_struct *f;
	int threshold=BR_PROCESSORS()*5;

	LOCAL_MEM m;
	init_remote_exec_thread();
	INIT_PROTECT_BEGIN
		PROTECT_BEGIN
			TRACE("new thread");
			m=(LOCAL_MEM)(BR_GET_THREAD_LOCAL());
			m->attach=NULL;
#ifdef AM_SOLARIS_THREADS
			thr_setprio(thr_self(),psather_prio);
#endif

	while(1) { /* this is a deamon thread */
restart:
		/* printf("parloop work thread loop\n"); */
		if(thread_wait || (n==0 && thread_wait_0)) {
			/* printf("THREAD %d WAITS, %d\n",n,thread_wait); */
			BR_LOCK(thread_locks[n]);
			/* printf("THREAD %d RESTARTS, %d\n",n,thread_wait); */
			/* BR_UNLOCK(thread_locks[n]); */
		}
		job_entry=-1;
		while(job_entry==-1) {
			am_wait_for(head!=tail || thread_wait || (thread_wait_0 && n==0));
			if(thread_wait || (thread_wait_0 && n==0)) goto restart;
			SPINLOCK_LOCK(queue_lock);
			if(head!=tail) {
				job_entry=tail;
				tail++;
				if(tail==PARLOOP_QUEUE_SIZE) tail=0;
			}
			SPINLOCK_UNLOCK(queue_lock);
		}
		f=func_args+job_entry;
		job=f->job;
		if(job!=last_job) SYS_IMPORT;
		last_job=job;

		/* printf("EXECUTING WORK by thread %d, pos %d\n",n,job_entry); */
		(*f->func)(f->self,f->arg,f->attach,HERE);
		/* printf("WORK DONE by thread %d\n",n); */
		f->self=NULL;
		f->arg=NULL;
		f->attach=NULL;

		SYS_EXPORT;
		SPINLOCK_LOCK(job_counter_lock[job]);
		job_counter[job]--;
		/* everything is done, parloop may terminate */
		if(!job_counter[job]) {
			BR_UNLOCK(parloop_locks[job]);
			/* printf("TERMINATING PARLOOP\n"); */

		/* there is enough space to create some more jobs */
		} else if(parloop_more_work[job] && QUEUE_SIZE<threshold) {
			parloop_more_work[job]=0;
			BR_UNLOCK(parloop_locks[job]);
			/* make sure that this thread waits until more jobs exist */
			thread_wait_0=1;
			/* BR_LOCK(thread_locks[n]); */
		}
		SPINLOCK_UNLOCK(job_counter_lock[job]);
	}

		PROTECT_WHEN
			RFATAL("unhandled exception, aborting program");
		PROTECT_END
	INIT_PROTECT_END
	end_remote_exec_thread();
}


/*
 * this is the initialization which is done BEFORE
 * calling BR_init()
 */
void init_parloop_locks()
{
	int i;
	for(i=0;i<MAX_THREADS;i++) {
		thread_locks[i]=BR_LOCK_CREATE();
	}
	for(i=0;i<MAX_PARLOOPS;i++) {
		parloop_locks[i]=BR_LOCK_CREATE();
		/* attach_objects[i]=NULL; */
	}
	parloop_wait=BR_LOCK_CREATE();
	parloop_enter_lock=BR_LOCK_CREATE();
}

/* 
 * the initialization done here has to occur after
 * the cluster started
 */
void init_parloops()
{
	int i;
	for(i=0;i<MAX_THREADS;i++) {
		BR_LOCK(thread_locks[i]);
	}
	for(i=0;i<BR_PROCESSORS();i++) {
		BR_FORK_1(HERE,parloop_work_thread,(BR_word_t)i);
	}
	for(i=0;i<MAX_PARLOOPS;i++) {
		BR_LOCK(parloop_locks[i]);
	}
}
