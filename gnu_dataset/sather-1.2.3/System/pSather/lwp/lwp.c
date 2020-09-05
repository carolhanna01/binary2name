/*------------------------->  ANSI C - sourcefile  <-------------------------*/
/* Copyright (C) 1991-93 by Stephen Crane                                    */
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
 * lwp.c -- lightweight process creation, destruction and manipulation.
 *
 * author: Stephen Crane, (jsc@doc.ic.ac.uk), Department of Computing,
 * Imperial College of Science, Technology and Medicine, 180 Queen's
 * Gate, London SW7 2BZ, England.
 *
 * This version of lwp has been significantly changed by Claudio Fleiner
 * (fleiner@icsi.berkeley.edu) in 1995. New features include locks, 
 * the possibility to interrupt threads, thread local memory, definitions of
 * functions to execute whenever a thread is rescheduled or killed, and more.
 * These features have been tested under sun 4.1.3 and linux 1.1.59.
 */

#include <stdio.h>
#include <stddef.h>
#include <unistd.h>
#include <signal.h>
#ifndef __CYGWIN32__
# include <malloc.h>
# include <syscall.h>
#endif
#include <assert.h>
#include "lwp.h"

/* extern int sigpause(int); - already in signal.h */

#ifdef WIN32
/* (JN) made posix comliant for portability 8/1/99 */
int sigsetmask(const int set) {
  /* int sigprocmask(int how, const sigset_t *set, sigset_t *oset); */
  return sigprocmask(SIG_SETMASK, (sigset_t *)set, NULL) ;
}
#else
extern int sigsetmask(int);
#endif


static short thread_id=1;

struct lpq schedq[MAXTPRI], deadq;
struct pcb *currp;
void (*lwp_sched_func)(void)=NULL;
int maxpri=0;		/* maximum priority so far */

static int oldmask;

/*
 * growsdown -- check stack direction
 */
static int growsdown (void *x) { int y; return x > (void *)&y; }

/*
 * reschedp -- schedule another process.  we also check for dead
 * processes here and free them.
 */
long thr_sw=0;
void reschedp (void)
{
	static struct pcb *to_del=NULL;
	extern struct lpq schedq[];
	struct pcb * volatile nextp=NULL;
	long i;
	static int lcount = LCOUNT;

#ifdef DO_TIMING
	short last_timing;
	last_timing=TM_timing_type(TM_THREADS);
	if(currp!=0) {
		currp->true_time+=TM_now()-currp->restarted;
		currp->restarted=TM_now();
	}
#endif

	if(to_del!=NULL && to_del!=currp) {
		free(to_del->sbtm);
		free(to_del);
		to_del=NULL;
	}
	if (!--lcount) {
		int p = prisetp (MAXTPRI-1);
		lcount = LCOUNT;
		sigsetmask (sigsetmask (oldmask));
		currp->pri = p;
	}
	if(!currp->dead && currp->on_desched) {
		int p= prisetp(MAXTPRI -1); /* no resched */
		if(currp->on_desched!=NULL) (*currp->on_desched)();
		currp->pri=p;
	}
	if(lwp_sched_func!=NULL) {
		int p= prisetp(MAXTPRI -1); /* no resched */
		if(lwp_sched_func!=NULL) (*lwp_sched_func)();
		currp->pri=p;
	}
	for (i=maxpri+1; (i--); )
		while ((nextp = hoq (&schedq[i])))
			if (nextp->dead) {
				if(*nextp->on_kill!=NULL)
					(*nextp->on_kill)(nextp->thread_memory);
				if (nextp!=currp) {
					free (nextp->sbtm);
					free (nextp);
				} else {
					to_del=currp;
					currp = 0;
				}
			} else
				goto change;
change:
	/* change context? ... save current context */
	if (currp != nextp) {
		thr_sw++;
		if(currp!=0) {
			if(savep (currp->context)==0) {
				/* restore previous context */
				currp = nextp;
				restorep (currp->context);
			}
		} else {
			currp = nextp;
			restorep (currp->context);
		}
	}
#ifdef DO_TIMING
	TM_timing_type(last_timing);
	currp->restarted=TM_now();
#endif
	if(currp->got_intr && !currp->block_intr) {
		currp->got_intr=0;
		if(currp->intr_handler!=NULL)
			(*(currp->intr_handler))();
	}

	if(currp->on_resched!=NULL) (*currp->on_resched)();
}

/*
 * wrapp -- process entry point.
 */
void wrapp (void)
{
	extern struct pcb *currp;

	sigsetmask (SIGNALS);
	(*currp->entry) (currp->argc, currp->argv, currp->envp);
	suicidep ();
}

/*
 * nullp -- a null process, always ready to run.
 * it (1) sets its priority to maximum to prevent a signal doing a
 * reschedule (2) enables signals, waits for one and handles it
 * and (3) resets its priority causing a reschedule.
 */
struct timeval idle_time;
static void nullp (void)
{
	struct timeval st,en;
	extern int gettimeofday(struct timeval *,struct timezone *);
#ifdef DO_TIMING
	short t;
	t=TM_timing_type(TM_IDLE);
#endif
	for (;;) {
		int p = prisetp (MAXTPRI-1);
		gettimeofday(&st,NULL);
		sigpause (oldmask);
		gettimeofday(&en,NULL);
		idle_time.tv_sec+=en.tv_sec-st.tv_sec;
		if(en.tv_usec>=st.tv_usec) {
			idle_time.tv_usec+=en.tv_usec-st.tv_usec;
		} else {
			idle_time.tv_usec+=1000000+en.tv_usec-st.tv_usec;
			idle_time.tv_sec--;
		}
		while(idle_time.tv_usec>=1000000) {
			idle_time.tv_sec++;
			idle_time.tv_usec-=1000000;
		}
		prisetp (p);
	}
#ifdef DO_TIMING
	TM_timing_type(t);
#endif
}
void print_idle(FILE *f)
{
	fprintf(f,"IDLE: %ld.%06ld\n",(long)idle_time.tv_sec,(long)idle_time.tv_usec);
	fprintf(f,"THREAD_SWITCHES: %ld\n",(long)thr_sw);
}

void InitThreadStruct(struct pcb *newp)
{
#ifdef DO_TIMING
	newp->start_time=TM_now();
	newp->true_time=0;
	newp->restarted=TM_now();
#endif
	newp->id=thread_id++;
	newp->on_resched=NULL;
	newp->on_desched=NULL;
	newp->on_kill=NULL;
	newp->thread_memory=NULL;
}
/*
 * creatp -- create a process.
 */
struct pcb *creatp (int priority, void (*entry)(), int size, int argc, char *argv[], char *envp)
{
	struct pcb *newp;
	int *s, x;
	void *sp;
	extern struct pcb *currp;

	if (!(newp = (struct pcb *)calloc (sizeof(struct pcb),1)))
		return (0);
	size += sizeof(stkalign_t);
	if (!(s = (int *)malloc (size)))
		return (0);
	InitThreadStruct(newp);
	newp->entry = entry;
	newp->argc = argc;
	newp->argv = argv;
	newp->envp = envp;

	sp = (void *)(growsdown (&x)? (size+(int)s)&-sizeof(stkalign_t): (int)s);
	if (MAXTPRI <= priority)
		priority = MAXTPRI-1;
	if (maxpri < (newp->pri = priority))
		maxpri = priority;
	newp->sbtm = s;
	newp->size = size;
	newp->dead = 0;
	readyp (newp);
	readyp (currp);
	initp (newp, sp);	/* architecture-dependent: from $(ARCH).c */
	reschedp ();
	return (newp);
}

/*
 * readyp -- put process on ready queue.  if null, assume current.
 */
void readyp (struct pcb *p)
{
	extern struct pcb *currp;
	extern struct lpq schedq[];

	if (!p)
		p = currp;
	toq (&schedq[p->pri], p);
}

/*
 * getenvp -- return back-pointer to user's data
 */
void *getenvp (struct pcb *p)
{
	if (!p) p = currp;
	return (p->envp);
}

/*
 * setenvp -- set back-pointer to user's data
 */
void setenvp (struct pcb *p, void *ud)
{
	if (!p) p = currp;
	p->envp = ud;
}

/*
 * yieldp -- yield the processor to another thread.
 */
void yieldp (void)
{
	readyp (currp);
	reschedp ();
}

/*
 * suicidep -- cause the current process to be scheduled for deletion.
 */
void suicidep (void)
{
	currp->dead = 1;
	yieldp ();
}

/*
 * destroyp -- mark a process as dead, so it will never be rescheduled.
 */
void destroyp (struct pcb *p) { p->dead = 1; }

/*
 * prisetp -- set the thread's priority, returning the old.
 * if the new priority is lower than the old, we reschedule.
 */
int prisetp (int new)
{
	int old = currp->pri;

	if (MAXTPRI <= new)
		new = MAXTPRI-1;
	if (maxpri < new)
		maxpri = new;
	currp->pri = new;
	if (new < old)
		yieldp ();
	return (old);
}

/*
 * prisetp -- set the thread's priority, returning the old.
 * Does not do any rescheduling. The thread will run with
 * this priority until the next yieldp()
 */
int prichangep (int new)
{
	int old = currp->pri;

	if (MAXTPRI <= new)
		new = MAXTPRI-1;
	if (maxpri < new)
		maxpri = new;
	currp->pri = new;
	return (old);
}

/*
 * initlp -- initialise the coroutine structures
 */
struct pcb *initlp (int pri)
{
	extern struct lpq schedq[];
	extern struct pcb *currp;
	extern void onalarm ();
	struct lpq *q;
	int i, *stack;
	struct sigaction s;

	if (!(currp = (struct pcb *)calloc (sizeof (struct pcb),1)))
		return (0);
	if (!(stack = (int *)malloc (64)))
		return (0);
	if (MAXTPRI <= pri)
		pri = MAXTPRI-1;
	if (maxpri < pri)
		maxpri = pri;
	InitThreadStruct(currp);
	currp->next = 0;
	currp->sbtm = stack;	/* dummy stack */
	currp->pri = pri;
	currp->dead = 0;
	currp->size = 0; /* dummy size */

	for (i=MAXTPRI, q=schedq; i--; q++)
		q->head = q->tail = 0;
	deadq.head = deadq.tail = 0;

	s.sa_handler = &onalarm;
	s.sa_flags = SA_INTERRUPT;
/*	s.sa_mask = 0; -- Nobbi: had to change this for Linux 2.2.11: */
	sigemptyset(&s.sa_mask);

	sigaction (SIGALRM, &s, (struct sigaction *)0);

	oldmask = sigsetmask (SIGNALS);

	creatp (0, nullp, 8192, 0, 0, 0);
	return (currp);
}

intr_handler_t set_intr_handler(intr_handler_t ih)
{
	intr_handler_t old=currp->intr_handler;
	currp->intr_handler=ih;
	return old;
}
#include "arch.lc"
