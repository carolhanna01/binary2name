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
 * The GAM interface, including the thread module
 *
 * Version 1.0 (released for Sather 1.0.9) by Claudio Fleiner (fleiner@icsi.berkeley.edu)
 */
#ifndef _AM_H_
#define _AM_H_

#include <assert.h>
#include <stdio.h>

typedef int vnn_t;
typedef void (*handler_0_t)(vnn_t);
typedef void (*handler_1_t)(vnn_t,long);
typedef void (*handler_2_t)(vnn_t,long,long);
typedef void (*handler_3_t)(vnn_t,long,long,long);
typedef void (*handler_4_t)(vnn_t,long,long,long,long);
typedef void (*handler_df_t)(vnn_t,long,long,double);
typedef void (*handler_mem_t)(vnn_t,void *,int,void *);
typedef void (*signal_handler_t)(void);
typedef void (*handler_t)(vnn_t,...);

void am_enable(int,int argc,char *argv[]);
void am_disable(void);
void am_poll(void);
int  am_procs(void);
int  am_my_cluster_size(void);
int  am_set_my_cluster_size(void);
int  am_max_size(void);
vnn_t am_my_proc(void);

extern int am_clusters;
extern int am_my_cluster_id;
extern int am_cluster_size;
#define AM_PROCS	am_clusters
#define AM_MY_PROC	am_my_cluster_id
#define AM_MY_CLUSTER_SIZE am_cluster_size

int am_request_0(vnn_t dest,handler_0_t handler);
int am_request_1(vnn_t dest,handler_1_t handler,long arg0);
int am_request_2(vnn_t dest,handler_2_t handler,long arg0,long arg1);
int am_request_3(vnn_t dest,handler_3_t handler,long arg0,long arg1,long arg2);
int am_request_4(vnn_t dest,handler_4_t handler,long arg0,long arg1,long arg2,long arg3);
int am_request_df(vnn_t dest,handler_df_t handler,long arg0,long arg1,double arg2);

int am_reply_0(vnn_t dest,handler_0_t handler);
int am_reply_1(vnn_t dest,handler_1_t handler,long arg0);
int am_reply_2(vnn_t dest,handler_2_t handler,long arg0,long arg1);
int am_reply_3(vnn_t dest,handler_3_t handler,long arg0,long arg1,long arg2);
int am_reply_4(vnn_t dest,handler_4_t handler,long arg0,long arg1,long arg2,long arg3);
int am_reply_df(vnn_t dest,handler_df_t handler,long arg0,long arg1,double arg2);

int am_store(vnn_t dest,void *lva,void *rva,int nbytes,handler_mem_t handler,void *h_arg);
int am_get(vnn_t dest,void *rva,void *lva,int nbytes,handler_mem_t handler,void *h_arg);
int am_store_async(vnn_t dest,void *lva,void *rva,int nbytes,handler_mem_t handler,void *h_arg,handler_mem_t endfunc,void *e_arg);

void am_dummy(vnn_t,void *,int,void*);

#define sleep(a) am_sleep(a)
void am_sleep(unsigned long);

#define am_wait_for(cond) do { if(cond) break;am_thread_poll(); } while(1)
void am_thread_poll(void);

#ifdef AM_THREADS
#ifdef LWP
#include <lwp.h>

#define THR_ENABLE_SIGNAL	enable_intr()
#define THR_IGNORE_SIGNAL	ignore_intr()
#define THR_BLOCK_SIGNAL	block_intr()
#define THR_SIGNAL_ENABLED	intr_enabled()
#define THR_SIGNAL_IGNORED	intr_ignored()
#define THR_SIGNAL_BLOCKED	intr_blocked()
#define THR_RESET_SIGNAL	clear_intr()
#define THR_GOT_SIGNAL		got_intr()
#define THR_SET_SIGNAL_HANDLER(p) set_intr_handler(p)

#define LCK_CREATE		(lock_t)creatl()
#define LCK_LOCK(l)		lockl((struct lock *)l)
#define LCK_UNLOCK(l)		unlockl((struct lock *)l)
#define LCK_TRY(l)		tryl((struct lock *)l)
#define LCK_DELETE(l)		free((void *)l)

#define SEMA_CREATE(count) 	(sema_t *)creats(count)
#define SEMA_SIGNAL(s)	   	signals((struct sem *)s)
#define SEMA_WAIT(s)		waits((struct sem *)s)
#define SEMA_TRY(s)		tests((struct sem *)s)
#define SEMA_DELETE(s)		free((void *)s)

#define THR_LOCAL		(get_thread_mem)
#define THR_SET_LOCAL(p)	set_thread_mem(p)

#define YIELD			yieldp()

typedef struct {
	void *pcb;
	short node;
} thread_t;
#define THR_SAME_ID(id1,id2) ((id1).pcb==(id2).pcb && (id1).node==(id2).node)
#define THR_LT(id1,id2)      ((id1).node<(id2).node?1:((id1).node>(id2).node)?0:(id1).pcb<(id2).pcb?1:0)
#define THR_HASH(id)	     ((id).node+(long)((id).pcb))

#endif /* LWP */

#ifdef SOLARIS_THREADS
#include <thread.h>

#define SOLARIS_GOT_SIGNAL          1
#define SOLARIS_IGNORE_SIGNAL       2
#define SOLARIS_BLOCK_SIGNAL   	4

#define LCK_UNLOCK(l)		mutex_unlock((mutex_t *)l)
#define LCK_TRY(l)		(!mutex_trylock((mutex_t *)l))
#define LCK_DELETE(l)		free((void *)l)

/*
** Spinlocks can be used for small critical regions.  They are implemented
** with test-and-set and exponential backoff, with thread yielding taking
** place when the backoff limit is reached.  These settings are mildly
** tuned for SS10 4 cpu 66MHz hypersparcs, with the loop unrolled to
** optimize the case where there is no contention.  Presently only the
** Sparc v8 ISA compiled using gcc is supported efficiently;
** Spinlocks don't come with any kind of fairness
** or deadlock avoidance guarantees, so they should be used with care.
** For example, when acquiring multiple spinlocks always do so in address
** order.
** First version January 1996 by David Stoutamire
** adapted in February 1996 for pSather by Claudio Fleiner
*/

#if defined(_REENTRANT) && defined(__GNUC__) && defined(__sparc__)

/* x must be an address of a word, and y must be a register local */
typedef volatile short spinlock_t;
# define TEST_AND_SET(x,y) asm volatile \
	("ldstub [%1], %0" : "=r" (y) : "r" (x))
 
# define SPINLOCK_LOCK(x)		\
{  register int XyZ, XyZd; 		\
   TEST_AND_SET(&(x),XyZ);		\
   if (XyZ != 0) {			\
      XyZd=1;				\
      while (1) {			\
	 if (XyZd>1050) am_poll();	\
	 if (XyZd>100000) YIELD; 	\
	 else {				\
	    for (XyZ=XyZd; XyZ--;);	\
	    XyZd*=2;			\
	 }				\
	 TEST_AND_SET(&(x),XyZ);	\
	 if (XyZ == 0) break;		\
      }					\
   }					\
}
# define SPINLOCK_TRYLOCK(r,x)		\
{  register int XyZ; 		\
   TEST_AND_SET(&(x),XyZ);		\
   r=XyZ==0;				\
}
# define SPINLOCK_UNLOCK(x) (x)=0
#endif
#ifdef MEIKO
#define MEIKO_POLL_POSSIBLE (*am_queue_test!=-1)
extern int *am_queue_test;
#else
#define MEIKO_POLL_POSSIBLE 1
#endif

/*
 * Solaris uses sema_t too, but not as pointer. Therefore we redefine
 * sema_t here
 */
typedef sema_t solaris_sema_t; /* assumes that sema_t is defined later */
#define sema_t am_sema_t
#if 0
#define SEMA_SIGNAL(s)	   	do { printf("%s:%d %d sema_signal(%d)\n",__FILE__,__LINE__,(int)thr_self(),(int)s);my_sema_signal(s);printf("%s:%d %d sema_signal done(%d)\n",__FILE__,__LINE__,(int)thr_self(),(int)s); } while(0)
#define SEMA_TRY(s)		(my_sema_try((s)))
#define SEMA_DELETE(s)		free((void *)s)
#define SEMA_WAIT(s)	   	do { printf("%s:%d %d sema_wait(%d)\n",__FILE__,__LINE__,(int)thr_self(),(int)s);my_sema_wait(s);printf("%s:%d %d sema_wait done(%d)\n",__FILE__,__LINE__,(int)thr_self(),(int)s); } while(0)
#define sema_wait my_sema_wait
#define sema_signal my_sema_signal
#define sema_try my_sema_try
#define sema_create my_sema_create
#define sema_delete my_sema_delete
#endif
#define sema_t am_sema_t
#ifdef PRINT_SEMA
#define SEMA_SIGNAL(s)	   	do { printf("%s:%d %d sema_post(%d)\n",__FILE__,__LINE__,(int)thr_self(),(int)s);sema_post((solaris_sema_t *)s);printf("%s:%d %d sema_post done(%d)\n",__FILE__,__LINE__,(int)thr_self(),(int)s); } while(0)
#else
#define SEMA_SIGNAL(s)	   	sema_post((solaris_sema_t *)s)
#endif
#define SEMA_TRY(s)		(!sema_trywait((solaris_sema_t *)s))
#define SEMA_DELETE(s)		free((void *)s)

#define THR_SET_LOCAL(p)	assert(thr_setspecific(am_local_key,p)==0)

#ifdef POLLING
#define YIELD			do { am_poll();thr_yield(); } while(0)
#else
#define YIELD			thr_yield()
#endif

/* thread_t is already defined in solaris, so we have to change 
 * this
 */
typedef thread_t solaris_thread_t;
#define thread_t am_thread_t
typedef struct {
	solaris_thread_t tid;
	short node;
} thread_t;
#define THR_SAME_ID(id1,id2) ((id1).tid==(id2).tid && (id1).node==(id2).node)
#define THR_LT(id1,id2)      ((id1).node<(id2).node?1:((id1).node>(id2).node)?0:(id1).tid<(id2).tid?1:0)
#define THR_HASH(id)	     ((id).node+(long)((id).tid))

extern thread_key_t am_local_key,am_signal_key,am_signal_function_key;

/*
 * SOLARIS needs special wait counters to avoid race conditions.
 */
typedef struct solaris_wait_counter {
	mutex_t lck;
	long cntr;
} *cntr_t;
#define WAITCNTR(a)		struct solaris_wait_counter __cntr_var_ ## a; \
				cntr_t a=(((cntr_t)(mutex_init(&__cntr_var_ ## a.lck,USYNC_THREAD,NULL),__cntr_var_ ## a.cntr=0)),&__cntr_var_ ## a)
				/* a->cntr=,0 */

#define CNTR_INCR(a)		do { int b;if((b=thr_signal_enabled())) thr_block_signal();mutex_lock(&(a)->lck);((a)->cntr)++;mutex_unlock(&((a)->lck));if(b) thr_enable_signal(); } while(0)
#define CNTR_DECR(a)		do { int b;if((b=thr_signal_enabled())) thr_block_signal();mutex_lock(&(a)->lck);((a)->cntr)--;mutex_unlock(&((a)->lck));if(b) thr_enable_signal(); } while(0)
#define CNTR_INCR_BY(a,v)	do { int b;if((b=thr_signal_enabled())) thr_block_signal();mutex_lock(&(a)->lck);((a)->cntr)+=(v);mutex_unlock(&((a)->lck));if(b) thr_enable_signal(); } while(0)
#define CNTR_DECR_BY(a,v)	do { int b;if((b=thr_signal_enabled())) thr_block_signal();mutex_lock(&(a)->lck);((a)->cntr)-=(v);mutex_unlock(&((a)->lck));if(b) thr_enable_signal(); } while(0)
#define CNTR_WAIT_FOR_ZERO(a)	am_wait_for((a)->cntr==0)
#define CNTR_IS_ZERO(a)		((a)->cntr)
void cntr_incr(cntr_t cnt);
void cntr_decr(cntr_t cnt);
void cntr_incr_by(cntr_t cnt,long v);
void cntr_decr_by(cntr_t cnt,long v);
void cntr_wait_for_zero(cntr_t cnt);
int cntr_is_zero(cntr_t cnt);

/*
 * lock_t is already used on solaris machines, so we redefine our lock_t:
 * Ditto for sema_*
 */
#define lock_t am_lock_t

#define sema_wait am_sema_wait
	
#endif /* SOLARIS_THREADS */

int thr_create_0(vnn_t where,handler_0_t handler);
int thr_create_1(vnn_t where,handler_1_t handler,long arg0);
int thr_create_2(vnn_t where,handler_2_t handler,long arg0,long arg1);
int thr_create_3(vnn_t where,handler_3_t handler,long arg0,long arg1,long arg2);
int thr_create_4(vnn_t where,handler_4_t handler,long arg0,long arg1,long arg2,long arg3);
int thr_create_df(vnn_t where,handler_df_t handler,long arg0,long arg1,double arg2);

char *thr_print_id(thread_t,char *);

typedef struct {
	unsigned long sec;
	unsigned long nsec;
} delay_t;
typedef void (*handler_delay_t)(void *);
void thr_delay_signal(delay_t time);
void thr_delay_function(delay_t time,handler_delay_t func,void *arg);

void thr_set_local(void *);
void *thr_local(void);

void yield(void);

void thr_signal(thread_t thr);
signal_handler_t thr_set_signal_handler(signal_handler_t signal_handler);

void thr_ignore_signal();
void thr_enable_signal();
void thr_block_signal();
int thr_signal_enabled();
int thr_signal_blocked();
int thr_signal_ignored();
void thr_reset_signal();
int thr_got_signal();

typedef void *lock_t;
lock_t lck_create();
void lck_lock(lock_t lck);
void lck_unlock(lock_t lck);
int lck_try(lock_t lck);
void lck_delete(lock_t lck);
void r_lck_unlock(vnn_t from,long lck);
void r_lck_unlock_mem(vnn_t from,void *a,int size,void *lck);
void r_lck_unlock_reply_mem(vnn_t from,void *a,int size,void *lck);

typedef void *sema_t;
sema_t sema_create(unsigned int count);
void sema_signal(sema_t sem);
int sema_wait(sema_t sem);
int sema_try(sema_t sem);
void sema_delete(sema_t sem);
void r_sema_signal(vnn_t from,long sem);
void r_sema_signal_mem(vnn_t from,void *s,int size,void *sem);
void r_sema_signal_reply_mem(vnn_t from,void *s,int size,void *sem);

thread_t thr_id();
int thr_same_id(thread_t id1,thread_t id2);
int thr_lt(thread_t id1,thread_t id2);
int thr_hash(thread_t id1);
thread_t thr_no_thread();
#ifndef THR_ID
#define THR_ID			thr_id()
#endif
#ifndef THR_SAME_ID
#define THR_SAME_ID(id1,id2)	thr_same_id((id1),(id2))
#endif
#ifndef THR_LT
#define THR_LT(id1,id2)		thr_lt((id1),(id2))
#endif
#ifndef THR_HASH
#define THR_HASH(id)		thr_hash((id))
#endif
#ifndef THR_NO_THREAD
#define THR_NO_THREAD		thr_no_thread()
#endif

#ifndef LCK_CREATE
#define LCK_CREATE		lck_create()
#endif
#ifndef LCK_LOCK
#define LCK_LOCK(l)		lck_lock(l)
#endif
#ifndef LCK_UNLOCK
#define LCK_UNLOCK(l)		lck_unlock(l)
#endif
#ifndef LCK_TRY
#define LCK_TRY(l)		lck_try(l)
#endif
#ifndef LCK_DELETE
#define LCK_DELETE(l)		lck_delete(l)
#endif

#ifndef SEMA_CREATE
#define SEMA_CREATE(count) 	sema_create(count)
#endif
#ifndef SEMA_SIGNAL
#define SEMA_SIGNAL(s)	   	sema_signal(s)
#endif
#ifndef SEMA_WAIT
#define SEMA_WAIT(s)		sema_wait(s)
#endif
#ifndef SEMA_TRY
#define SEMA_TRY(s)		sema_try(s)
#endif
#ifndef SEMA_DELETE
#define SEMA_DELETE(s)		sema_delete(s)
#endif

#ifndef THR_SET_LOCAL
#define THR_SET_LOCAL(p)	thr_set_local(p)
#endif
#ifndef THR_LOCAL
#define THR_LOCAL		thr_local()
#endif

#ifndef THR_IGNORE_SIGNAL
#define THR_IGNORE_SIGNAL	thr_ignore_signal()
#endif
#ifndef THR_ENABLE_SIGNAL
#define THR_ENABLE_SIGNAL	thr_enable_signal()
#endif
#ifndef THR_BLOCK_SIGNAL
#define THR_BLOCK_SIGNAL	thr_block_signal()
#endif
#ifndef THR_SIGNAL_ENABLED
#define THR_SIGNAL_ENABLED	thr_signal_enabled()
#endif
#ifndef THR_SIGNAL_BLOCKED
#define THR_SIGNAL_BLOCKED	thr_signal_blocked()
#endif
#ifndef THR_SIGNAL_IGNORED
#define THR_SIGNAL_IGNORED	thr_signal_ignored()
#endif
#ifndef THR_RESET_SIGNAL
#define THR_RESET_SIGNAL	thr_reset_signal()
#endif
#ifndef THR_GOT_SIGNAL
#define THR_GOT_SIGNAL		thr_got_signal()
#endif
#ifndef THR_SIGNAL
#define THR_SIGNAL(thr) 	thr_signal(thr)
#endif
#ifndef THR_SET_SIGNAL_HANDLER
#define THR_SET_SIGNAL_HANDLER(t) thr_set_signal_handler((signal_handler_t)t)
#endif

#endif /* AM_THREADS */

/*
 * as it is often usefull to incerement or decrement a counter
 * on a remote nore, two function stubs for this are given here
 * Note that cntr_wait_for_zero may not
 * be called withing an am_request, as it could block.
 */
void r_cntr_incr(vnn_t from,long cnt);
void r_cntr_decr(vnn_t from,long cnt);
void r_cntr_incr_mem(vnn_t from,void *a,int size,void *cnt);
void r_cntr_decr_mem(vnn_t from,void *a,int size,void *cnt);
void r_cntr_incr_reply_mem(vnn_t from,void *a,int size,void * cnt);
void r_cntr_decr_reply_mem(vnn_t from,void *a,int size,void * cnt);


/*
 * A wait counter is declared as a local variable, and may only
 * used as long as the function does exit.
 * If they use a special implementation, WAITCNTR has already been defined
 * above.
 */
#ifndef WAITCNTR
typedef unsigned long *cntr_t;
#define WAITCNTR(a)		unsigned long __cntr_var_ ## a=0; \
				cntr_t a= &__cntr_var_ ## a
#define CNTR_INCR(a)		(*(a))++
#define CNTR_DECR(a)		(*(a))--
#define CNTR_INCR_BY(a,v)	((*(a))+=(v))
#define CNTR_DECR_BY(a,v)	((*(a))-=(v))
#define CNTR_WAIT_FOR_ZERO(a)	am_wait_for(*(a)==0)
#define CNTR_IS_ZERO(a)		(*(a))
void cntr_incr(cntr_t cnt);
void cntr_decr(cntr_t cnt);
void cntr_incr_by(cntr_t cnt,long v);
void cntr_decr_by(cntr_t cnt,long v);
void cntr_wait_for_zero(cntr_t cnt);
int cntr_is_zero(cntr_t cnt);
#endif



/*
 * a throw away semaphore is like a normal semaphore, but you can
 * just wait and signal it once, and then you have to throw it away
 */
typedef unsigned long *ta_sema_t;
#define TA_SEMAPHORE(a)		unsigned long __cntr_var_ ## a=1; \
				ta_sema_t a= &__cntr_var_ ## a
#define TA_SEMA_SIGNAL(a)	(*(a))=0
#define TA_SEMA_WAIT(a)		am_wait_for(*(a)==0)
void ta_sema_signal(ta_sema_t cnt);
void ta_sema_wait(ta_sema_t cnt);
void r_ta_sema_signal(vnn_t from,long cnt);
void r_ta_sema_signal_mem(vnn_t from,void *a,int size,void *cnt);
void r_ta_sema_signal_reply_mem(vnn_t from,void *a,int size,void *cnt);

#endif
