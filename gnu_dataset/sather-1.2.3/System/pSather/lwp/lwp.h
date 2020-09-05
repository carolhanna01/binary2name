/*------------------------->  ANSI C - headerfile  <-------------------------*/
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
 * lwp.h -- prototypes and structures for lightweight processes
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
#ifndef _LWP_H
#define _LWP_H

#include <signal.h>

/*
 * with timing set to !, it is possible to measure how much
 * real time each thread used
 */
#define nDO_TIMING

#ifdef DO_TIMING

#define MAX_TIME_TYPE 100

#define TM_IGNORE       (-1)
#define TM_IDLE         0
#define TM_THREADS	1

extern long time_elapsed[MAX_TIME_TYPE];

#ifdef __cplusplus
extern "C" {
#endif
short TM_timing_type(int new_type);
long TM_now();
void TM_print(void);
#ifdef __cplusplus
}
#endif

#endif

/* maximum thread priority: number of priority levels */
#define MAXTPRI	5 /* used to be 8, changed by CMF, 01.11.1994 */
/* `liveness' counter: check signals every `n' visits to the scheduler */
/* note: the lower this value, the more responsive the system but the */
/* more inefficient the context switch time */
#define LCOUNT	1

#ifdef __CYGWIN32__
# define sigmask(n) ((unsigned int)1 << (((n) - 1) & (32 - 1)))
# define SA_INTERRUPT 0  /* (JN) not supported yet (10/1/99) in C lib */
#endif

#define SIGNALS (sigmask(SIGIO)|sigmask(SIGALRM))

/* this is a link to the real arch header file */
/*
 * The original lwp had an extra architecture file per 
 * architecture. This made it rather hard to include this file
 * in another program, as it also needed these architecure
 * dependend file. Therefore, this file is textually included here,
 * and the correct version is selected by ifdef's. This works
 * only for Linux and Sun4 currently.
 */

#ifdef __linux__
#ifdef __cplusplus
extern "C" {
#endif
int getdtablesize (void);
#ifdef __cplusplus
}
#endif

#else /* Sun4 */
#undef OWN_CONTEXT_SWITCH
#endif

/*
 * End of architecutre dependend part
 */

#if defined(OWN_CONTEXT_SWITCH)
#ifdef __cplusplus
extern "C" {
#endif
int savep (jmp_buf);
void restorep (jmp_buf);
#ifdef __cplusplus
}
typedef int jmp_buf[16];
#endif
#else
#ifndef _SETJMP
#include <setjmp.h>
#define savep(x)	setjmp(x)
#define restorep(x)	longjmp(x, 1)
#endif
#endif

#ifndef _sys_time_h
#include <sys/time.h>
#endif

typedef void (*intr_handler_t)();
/* process control block.  do *not* change the position of context */
struct pcb {
	jmp_buf	context;	/* processor context area */
	void	*sbtm;		/* bottom of stack attached to it */
	int     size;           /* size of stack */
	void	(*entry)();	/* entry point */
	unsigned dead:1;	/* whether the process can be rescheduled */
	unsigned ignore_intr:1; /* whether the process ignores interrupts */
	unsigned block_intr:1;  /* whether the process blocks interrupts */
	unsigned got_intr:1;    /* whether the process got an unhandled interrupt */
	unsigned lock_intr:1;   /* whether the process was interrupted during a waits() or lockl() */
	unsigned intr_lock:1;   /* whether locks should be interrupted */ 
	int	pri;		/* which scheduling queue we're on */
	struct timeval	dt;
	int	argc;		/* initial arguments */
	char	**argv;
	void	*envp;		/* user structure pointer */
	intr_handler_t intr_handler; /* interrupt handler */
	struct lpq *lock_queue;
	struct pcb	*next;
	void *thread_memory;
	void (*on_resched)(void);
	void (*on_kill)(void*);
	void (*on_desched)(void);
		
#ifdef DO_TIMING
	long start_time;
	long true_time;
	long restarted;
#endif
	short id;

};
extern struct pcb *currp;

/* queue */
struct lpq {
	struct pcb *head, *tail;
};

/* lock */
struct lock {
#ifdef DEADLOCK_DETECTION
	struct pcb *cur;
#endif
	int locked;
	struct lpq q;
};

/* semaphore */
struct sem {
	int count;
	struct lpq q;
};

/* signal struct */
struct sig_info {
	struct sig_info *next;
	int des;
	void (*han) (void *, int);
	void *ctx;
};

typedef double stkalign_t;

#ifdef __cplusplus
extern "C" {
#endif

/* lock operations */
struct lock *creatl (void);
void unlockl (struct lock *);
int lockl (struct lock *); /* returns 0 on succes, -1 if interrupted */
int tryl(struct lock *);
#define deletel(x) free((x))

/* semaphore operations */
struct sem *creats (int);
void signals (struct sem *);
int waits (struct sem *); /* returns 0 on succes, -1 if interrupted */
int tests(struct sem *);
#define deletes(x) free((x))

typedef struct pcb *p_id_t;
p_id_t idp();
void intrp(p_id_t);
#define ignore_intr() 	(currp->ignore_intr=1)
#define intr_ignored()	(currp->ignore_intr)
#define block_intr() 	(currp->block_intr=1)
#define intr_blocked() 	(currp->block_intr)
#define enable_intr() 	do { (currp->ignore_intr=currp->block_intr=0); \
			   if(currp->got_intr && currp->intr_handler!=NULL) {\
				   currp->got_intr=0;\
				   (*currp->intr_handler)();\
			}\
			clear_intr(); } while(0)
#define intr_enabled()  (!(currp->block_intr || currp->ignore_intr))
#define got_intr() 	(currp->got_intr)
#define clear_intr() 	(currp->got_intr=0)

intr_handler_t set_intr_handler(intr_handler_t);
#define locks_interruptable()		(currp->intr_lock=1)
#define locks_non_interruptable()	(currp->intr_lock=0)


/* queue operations */
void toq (struct lpq *, struct pcb *);
struct pcb *hoq (struct lpq *);

/* process operations */
struct pcb *creatp (int prio, void (*entry)(), int size, int argc, char *argv[], char *envp);
void suicidep (void);
void destroyp (struct pcb *);
void readyp (struct pcb *);
void yieldp (void);
void *getenvp (struct pcb *);
void setenvp (struct pcb *, void *);
int prisetp (int);
int prichangep (int);
void reschedp (void);

/* timer operations */
void delayp (long);
void onalarm ();

/* initialisation */
struct pcb *initlp (int);
void initp (volatile struct pcb *volatile newp, void *sp);
void wrapp (void);

/* set the private memory of a thread */
#define set_thread_mem(p)	(currp->thread_memory=(p))
#define get_thread_mem()	(currp->thread_memory)
/* set the function to be run whenever the current thread is rescheduled */
#define set_resched_func(f)	(currp->on_resched=(void (*)(void))(f))
#define set_desched_func(f)	(currp->on_desched=(void (*)(void))(f))
#define set_kill_func(f)	(currp->on_kill=(void (*)(void *))(f))
extern void (*lwp_sched_func)(void);


/* signal stuff */
int sigioset (int, void (*)(void *, int), void *);
int sigioclr (int);
#ifdef __cplusplus
}
#endif	/* __cplusplus */

#endif	/* _LWP_H */
