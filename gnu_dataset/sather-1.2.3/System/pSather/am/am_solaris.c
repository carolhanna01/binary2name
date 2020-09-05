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
 * Support for Solaris Threads
 *
 * Version 1.0 (released for Sather 1.0.9) by Claudio Fleiner (fleiner@icsi.berkeley.edu)
 */
#ifdef SOLARIS_THREADS
#include <stdio.h>
#include <malloc.h>
#include <thread.h>
#include <assert.h>
#include <signal.h>
#include <sys/time.h>
#include <sys/types.h>
#include <sys/uio.h>
#include <unistd.h>
#include <stdlib.h>

#include "am.h"
#include "am_int.h"

/* am_int.h redefines sema_wait to be am_sema_wait. As we need to access
 * the true sema_wait function in this file, we undefine this macro
 */
#undef sema_wait 

#ifdef MEIKO
#define POLLING
#endif
static void am_solaris_signal_handler();
static void init_timer_thread();
static void pool_init();
int am_cluster_size;

thread_key_t am_local_key,am_signal_key,am_signal_function_key;
void solaris_am_init_m()
{
	struct sigaction act;
#if !defined(TCP) && !defined(LANAI_NET) && !defined(MEIKO)
	int i;
	for(i=0;i<am_procs() && am_procs()>1;i++)
		sema_init(&am_pool[i].poll,0,USYNC_PROCESS,0);
#endif

	/* the default concurrency does not work really well */
	am_cluster_size = am_set_my_cluster_size();
#if !defined(TCP) && !defined(LANAI_NET) && !defined(MEIKO)
	/* correction for the smp model */
	am_cluster_size=(am_cluster_size+am_procs()-1)/am_procs();
	if(am_cluster_size==0) am_cluster_size=1;
#endif
	thr_setconcurrency(am_cluster_size);
	assert(thr_keycreate(&am_local_key,NULL)==0);
	assert(thr_keycreate(&am_signal_key,NULL)==0);
	assert(thr_keycreate(&am_signal_function_key,NULL)==0);

	assert(thr_setspecific(am_local_key,NULL)==0);
	assert(thr_setspecific(am_signal_key,NULL)==0);
	assert(thr_setspecific(am_signal_function_key,NULL)==0);
	pool_init(); /* initialize thread pool */

	act.sa_sigaction=(void (*)(int,siginfo_t *,void *))am_solaris_signal_handler;
	sigemptyset(&act.sa_mask);
	act.sa_flags=SA_SIGINFO;
	assert(sigaction(SIGUSR1,&act,NULL)==0);
}

#ifdef LANAI_NET
extern unsigned int LANAI_fd[64];
#endif

static void solaris_poll()
{
#if !defined(TCP) && !defined(LANAI_NET) && !defined(MEIKO)
	solaris_sema_t *s;
	s= &am_pool[am_my_proc()].poll;
#endif
	if(am_clusters==1) return;
	if(getenv("AM_POLL_PRIO")) {
		int p;
		assert(thr_setprio(thr_self(),atoi(getenv("AM_POLL_PRIO")))==0);
		thr_getprio(thr_self(),&p);
		printf("polling with priority %d\n",p);
	}
	while(1) {
#if !defined(TCP) && !defined(LANAI_NET) && !defined(MEIKO)
		sema_wait(s);
#endif
#ifdef LANAI_NET
		/*char buf[4];
 		read(LANAI_fd[0], buf, 4);*/
		YIELD;
#endif
#ifdef MEIKO
		/* YIELD; */
#endif
#ifdef POLLING
		thr_setprio(thr_self(),4);
#endif
		am_poll();
#ifdef POLLING
		thr_setprio(thr_self(),0);
#endif
	}
}

char * thr_print_id(thread_t id,char *p)
{
	sprintf(p,"%02d-%-4d",id.node,id.tid);
	return p;
}

solaris_thread_t poll_manager_id;
void solaris_am_init()
{
	thr_create(NULL,0,(void *(*)(void *))solaris_poll,NULL,THR_DETACHED|THR_NEW_LWP|THR_DAEMON,&poll_manager_id);
	init_timer_thread();
}

#if defined(TCP) || defined(LANAI_NET) || defined(MEIKO)
/*
 * only relevant for tam
 */
int thread_may_poll()
{
	return poll_manager_id==thr_self();
}
#endif

#if !defined(TCP) && !defined(LANAI_NET) && !defined(MEIKO)
void solaris_am_signal(vnn_t proc)
{
	sema_post(&am_pool[proc].poll);
}
#endif

void am_thread_poll(void)
{
#ifdef POLLING
	am_poll();
#else
	thr_yield();
#endif
}
static void am_solaris_signal_handler()
{
	long sigs;
	void (*func)(void);
	assert(thr_getspecific(am_signal_key,(void *)&sigs)==0);

	if(!(sigs&SOLARIS_IGNORE_SIGNAL)) {
		if(sigs&SOLARIS_BLOCK_SIGNAL) {
			sigs|=SOLARIS_GOT_SIGNAL;
			thr_setspecific(am_signal_key,(void *)sigs);
		} else {
			thr_getspecific(am_signal_function_key,(void *)&func);
			if(func!=NULL) {
				sigs&= ~SOLARIS_GOT_SIGNAL;
				sigs|=SOLARIS_BLOCK_SIGNAL;
				thr_setspecific(am_signal_key,(void *)sigs);
				(*func)();
				sigs&= ~SOLARIS_BLOCK_SIGNAL;
				thr_setspecific(am_signal_key,(void *)sigs);
			}
		}
	}
}

static void thr_signal_it(vnn_t from,solaris_thread_t tid)
{
	thr_kill(tid,SIGUSR1);
}

int thr_signal_ignored()
{
	long sigs;
	thr_getspecific(am_signal_key,(void *)&sigs);
	return sigs&SOLARIS_IGNORE_SIGNAL;
}
int thr_signal_blocked()
{
	long sigs;
	thr_getspecific(am_signal_key,(void *)&sigs);
	return sigs&SOLARIS_BLOCK_SIGNAL;
}
int thr_signal_enabled()
{
	long sigs;
	thr_getspecific(am_signal_key,(void *)&sigs);
	return !(sigs&(SOLARIS_IGNORE_SIGNAL|SOLARIS_BLOCK_SIGNAL));
}
void thr_ignore_signal()
{
	long sigs;
	thr_getspecific(am_signal_key,(void *)&sigs);
	sigs|=SOLARIS_IGNORE_SIGNAL;
	thr_setspecific(am_signal_key,(void *)sigs);
}
void thr_enable_signal()
{
	long sigs;
	thr_getspecific(am_signal_key,(void *)&sigs);
	sigs&=~(SOLARIS_IGNORE_SIGNAL|SOLARIS_BLOCK_SIGNAL);
	thr_setspecific(am_signal_key,(void *)sigs);
	if(sigs&SOLARIS_GOT_SIGNAL) am_solaris_signal_handler();
}
void thr_block_signal()
{
	long sigs;
	thr_getspecific(am_signal_key,(void *)&sigs);
	sigs|=SOLARIS_BLOCK_SIGNAL;
	thr_setspecific(am_signal_key,(void *)sigs);
}
void thr_reset_signal()
{
	long sigs;
	thr_getspecific(am_signal_key,(void *)&sigs);
	sigs&=~SOLARIS_GOT_SIGNAL;
	thr_setspecific(am_signal_key,(void *)sigs);
}
int thr_got_signal()
{
	long sigs;
	thr_getspecific(am_signal_key,(void *)&sigs);
	return sigs&SOLARIS_GOT_SIGNAL;
}

void thr_signal(thread_t id)
{
	if(id.node!=AM_MY_PROC) {
		am_request_1(id.node,(handler_1_t)thr_signal_it,(long)id.tid);
	} else {
		thr_kill(id.tid,SIGUSR1);
	}
}

signal_handler_t thr_set_signal_handler(signal_handler_t signal_handler)
{
	signal_handler_t t;
	thr_getspecific(am_signal_function_key,(void **)&t);
	thr_setspecific(am_signal_function_key,(void *)signal_handler);
	return t;
}


lock_t lck_create() 	
{ 
	mutex_t *n;
	n=(mutex_t *)malloc(sizeof(mutex_t));
	mutex_init(n,USYNC_THREAD,NULL);
	return (lock_t)n;
}
void lck_lock(lock_t lck) 		
{ 
	long sigs;
	thr_getspecific(am_signal_key,(void *)&sigs);
	if(!(sigs&(SOLARIS_BLOCK_SIGNAL|SOLARIS_IGNORE_SIGNAL))) {
		THR_BLOCK_SIGNAL;
		mutex_lock(lck);
		THR_ENABLE_SIGNAL;
	} else mutex_lock(lck);
}
void lck_unlock(lock_t lck)	 	{ LCK_UNLOCK(lck); }
int lck_try(lock_t lck) 		{ return LCK_TRY(lck); }
void lck_delete(lock_t lck) 		{ LCK_DELETE(lck); }

sema_t sema_create(unsigned int count)
{
	solaris_sema_t *n;
	n=(solaris_sema_t *)malloc(sizeof(solaris_sema_t));
	sema_init(n,count,USYNC_THREAD,NULL);
	return (sema_t)n;
}
void sema_signal(sema_t sem) 		{ SEMA_SIGNAL(sem); }
int am_sema_wait(sema_t sem) 		{ return sema_wait(sem); }
int sema_try(sema_t sem) 		{ return SEMA_TRY(sem); }
void sema_delete(sema_t sem) 		{ SEMA_DELETE(sem); }

void thr_set_local(void *p) 		{ THR_SET_LOCAL(p); }
void *thr_local()
{
	void *p;
	assert(thr_getspecific(am_local_key,&p)==0);
	return p;
}

thread_t thr_id()
{
	thread_t id;
	id.node=AM_MY_PROC;
	id.tid=thr_self();
	return id;
}
thread_t thr_no_thread()
{
	thread_t id;
	id.node=0;
	id.tid=0;
	return id;
}

#define addtmv(a,b) 	do { (a).tv_sec+=(b).tv_sec;(a).tv_usec+=(b).tv_usec;if((a).tv_usec>1000000) { (a).tv_usec-=1000000;(a).tv_sec++; } } while(0)
#define subtmv(a,b) 	do { (a).tv_sec-=(b).tv_sec;(a).tv_usec-=(b).tv_usec;if((a).tv_usec<0) { (a).tv_usec+=1000000;(a).tv_sec--; } } while(0)
#define cmptmvi(a,b) 	(((a)>(b))?1:((a)<(b)?-1:0))
#define cmptmv(a,b)  	((a).tv_sec==(b).tv_sec?cmptmvi((a).tv_usec,(b).tv_usec):cmptmvi((a).tv_sec,(b).tv_sec))
#define cptmv(a,b) 	a=b
#define usectmv(a) 	((a).tv_sec*1000000+(a).tv_usec)
#define settmvusec(a,s) do {a.tv_sec=s/100000;a.tv_usec=s%1000000; } while(0)

typedef struct TIMER_struct {
	struct TIMER_struct *next;
	struct timeval tv;
	thread_t tid;
	void (*func)(void *);
	void *arg;
} *TIMER;

TIMER timer_first=NULL;
TIMER timer_done=NULL;
solaris_sema_t timer_semaphore;
mutex_t timer_lock;
timer_t timer_id;
solaris_thread_t timer_thread_id;

extern int gettimeofday(struct timeval *,void *);
static void handle_timer_call()
{
        struct timeval now;
	struct itimerspec its;
	TIMER t;

	mutex_lock(&timer_lock);

	while(timer_first!=NULL) {
		gettimeofday(&now,NULL);
		if(cmptmv(now,timer_first->tv)<0) break;

		if(timer_first->func==NULL) {
			thr_signal(timer_first->tid);
		} else (*timer_first->func)(timer_first->arg);

		/*
		 * as free may not be used in a signal handler, we have
		 * to store obsolete timers and have them deleted elsewhere
		 */
		t=timer_first;
		timer_first=t->next;
		t->next=timer_done;
		timer_done=t;
	}
	
	if(timer_first!=NULL) {
		its.it_interval.tv_sec=0;
		its.it_interval.tv_nsec=0;
		its.it_value.tv_sec=timer_first->tv.tv_sec;
		its.it_value.tv_nsec=timer_first->tv.tv_usec*1000;
		assert(timer_settime(timer_id,TIMER_ABSTIME,&its,NULL)==0);
	}

	mutex_unlock(&timer_lock);
}

static void timer_thread()
{
	struct sigevent evp;
	mutex_t my_lock;	
	struct sigaction act;

	timer_done=NULL;
	timer_first=NULL;

	evp.sigev_notify=SIGEV_SIGNAL;
	evp.sigev_signo=SIGALRM;
	evp.sigev_value.sival_int=1;
	assert(timer_create(CLOCK_REALTIME,&evp,&timer_id)==0);

	act.sa_sigaction=(void (*)(int,siginfo_t *,void *))handle_timer_call;
	sigemptyset(&act.sa_mask);
	act.sa_flags=SA_SIGINFO;
	assert(sigaction(SIGALRM,&act,NULL)==0);
		        
	sema_post(&timer_semaphore);

	mutex_init(&my_lock,USYNC_THREAD,NULL);
	mutex_lock(&my_lock);
	mutex_lock(&my_lock);
}

static void init_timer_thread()
{
	thr_create(NULL,0,(void *(*)(void *))timer_thread,NULL,
	     THR_BOUND|THR_DAEMON|THR_NEW_LWP,&timer_thread_id);

	sema_wait(&timer_semaphore);
}

void thr_delay_signal(delay_t delay)
{
	thr_delay_function(delay,NULL,NULL);
}
void thr_delay_function(delay_t delay,handler_delay_t func,void *arg)
{
        struct timeval now,cur;
	TIMER ti;

	mutex_lock(&timer_lock);

	/*
	 * delete all obsolete timers
	 */
	while(timer_done!=NULL) {
		TIMER t;
		t=timer_done;
		timer_done=timer_done->next;
		free(t);
	}

	gettimeofday(&now,NULL);
	cur.tv_sec=delay.sec;
	cur.tv_usec=delay.nsec/1000;
	addtmv(cur,now);

	ti=(struct TIMER_struct *)malloc(sizeof(struct TIMER_struct));
	ti->tid=thr_id();
	ti->next=NULL;
	ti->tv=cur;
	ti->arg=arg;
	ti->func=func;
	if(timer_first==NULL) {
		timer_first=ti;
		thr_kill(timer_thread_id,SIGALRM);
	} else if(cmptmv(now,timer_first->tv)<0) {
		ti->next=timer_first;
		timer_first=ti;
		thr_kill(timer_thread_id,SIGALRM);
	} else {
		TIMER t1=timer_first;
		while(t1->next!=NULL) {
			if(cmptmv(now,t1->next->tv)<0) {
				ti->next=t1->next;
				t1->next=ti;
				break;
			}
			t1=t1->next;
		}
		if(t1->next==NULL) t1->next=ti;
	}
	
	mutex_unlock(&timer_lock);
	return;
}

int thr_same_id(thread_t id1,thread_t id2) 
{ return THR_SAME_ID(id1,id2); }
int thr_lt(thread_t id1,thread_t id2) 
{ return THR_LT(id1,id2); }
int thr_hash(thread_t id1)
{ return THR_HASH(id1); }


/* Things necessary to determine the number of procs for a cluster */

#include <kstat.h>
#include <fcntl.h>
#include <sys/sysinfo.h>
#include <stdlib.h>
#include <string.h>

static struct kstat_list {
	kstat_t *k;
	struct kstat_list *next;
} *cpu=NULL;

int am_set_my_cluster_size() {
  kstat_t *k;
  kstat_ctl_t *kctl;
  struct kstat_list *cpucur=NULL;
  int ncpu = 0;

 
  /* Open the kstat interface */
  if(NULL==(kctl=kstat_open())){
    perror("kstat");
    exit(1);
  }
  
  /* 
   * Follow the kstat chain, making note of each useful module we
   * come across. Usefulness = disk if diskflag, cpu if cpuflag.
   */
  for(k=kctl->kc_chain;k;k=k->ks_next){
      if(0==(strncmp(k->ks_name, "cpu_stat", 8))){
	struct kstat_list *new;
	int cpunum;
	
	(void)sscanf(k->ks_name, "cpu_stat%d", &cpunum);
	new = (struct kstat_list *)malloc(sizeof(struct kstat_list));
	new->k=k;
	new->next=NULL;
	if(NULL==cpu){
	  cpu=new;
	  cpucur=new;
	} else {
	  cpucur->next=new;
	  cpucur=new;
	}
	ncpu++;
      }
  }
  return ncpu;
}

/* sleep is redefined as am_sleep. As we need sleep in this code
 * (the original one), we undefine it here
 */
#ifdef sleep
#undef sleep
#endif
void am_sleep(unsigned long duration)
{
	/* we would like to use sleep() of course, but halas, it returns
	   to early (at least on the system I used to develop this package 
	   Therefor we call it until the time is up */

        struct timeval now;
        struct timeval end;
	extern int gettimeofday(struct timeval *,void *);
#define cmptmvi(a,b) 	(((a)>(b))?1:((a)<(b)?-1:0))
#define cmptmv(a,b)  	((a).tv_sec==(b).tv_sec?cmptmvi((a).tv_usec,(b).tv_usec):cmptmvi((a).tv_sec,(b).tv_sec))

        gettimeofday(&end,NULL);
        end.tv_sec+=duration;

        while(1) {
                gettimeofday(&now,NULL);
                if(cmptmv(now,end)>=0) break;
		sleep(end.tv_sec-now.tv_sec);
		thr_yield();
        }
}

/* implements a thread pool for solaris threads. Despite the 
 * claims in the manual this seems to be faster than to recreate
 * a thread each time. The thread pool is only used on solaris.
 * Implemented by Ben
 * FINAL VERSION July 11 *
 */

/*
#define POOLDEB(x)  fprintf(stderr,"%s\n",x);
#define WORKDEB(x) fprintf(stderr,"WORKER:%d %s\n",poolindex,x);
*/
#define POOLDEB(x)  ;
#define WORKDEB(x) ;

/* x must be an address of a word, and y must be a register local */

static void pool_init(void);		/*  Initialize pool variables */
static void *pool_worker_thread(void *); /* A thread in the pool of threads */
static void pool_add_thread(int);	/* Internal function - must be called 
				   serially or within the spinlock*/

/* Mutex on which the worker thread blocks until freed for a task */
mutex_t pool_thr_worker_blocking_mutex;
/* Spinlock that ensures mutual exclusion on entry to the pool_thr_create */
mutex_t pool_mutex; 

spinlock_t pool_spinlock=0;	/* For the data in the pool */

/* Number of threads in the current pool */
volatile int pool_num_threads_available;
int pool_num_threads_created;

void *(*pool_thr_func)(void *)=NULL;	/* Global for passing the function ptr */
void *pool_thr_arg=NULL;		/* Global for passing the argument */

static void pool_init(){
  /*  Initialize locks and variables for the pool of threads */
  int ret;

  if ((ret = mutex_init(&pool_thr_worker_blocking_mutex,USYNC_THREAD,NULL))) {
    fprintf(stderr,"Error in initializing pool_thr_worker mutex! %d\n",ret);
    exit(1);
  }
  if ((ret = mutex_init(&pool_mutex,USYNC_THREAD,NULL))) {
    fprintf(stderr,"Error in initializing pool_thr_worker mutex! %d\n",ret);
    exit(1);
  }

  if ((ret = mutex_lock(&pool_thr_worker_blocking_mutex))) {
    /* The  pool first claims the worker mutex, so all workers are initially
       blocked */
    fprintf(stderr,"Error in claiming the worker mutex! %d",ret) ;
    exit(1);
  }
  pool_spinlock = 0;
  pool_num_threads_available = 0;
  pool_num_threads_created = 0;
}

static void pool_add_thread(int thread_arg){
  /* Must be called within the pool_mutex or serially*/
  int ret;
  int arg;
  arg = thread_arg;
  pool_num_threads_created++;
  thread_arg = pool_num_threads_created;
  if ((ret=thr_create(NULL,0,pool_worker_thread,(void *)thread_arg,
		      THR_DETACHED,NULL))
      != 0) {
    printf("Bad pool thr_create. thread_arg=%d ret=%d\n",thread_arg,ret);
  }
  pool_num_threads_available++;
  /* fprintf(stderr,"Adding initial thread%d\n",arg); */
}

int pool_thr_create(void *stkaddr, 
		    size_t stksize, 
		    void *(*func)(void *),
		    void *arg, 
		    long flags,
		    void *tid) {
  int ret;			/* Error return values */

  if ((stkaddr != NULL) 
      || (stksize != 0) 
      || (flags != THR_DETACHED)
      || (tid != NULL)) {
    POOLDEB("POOL:Not using the pool -args did not match");
    return thr_create(stkaddr,stksize,func,arg,flags,tid);
  }

  POOLDEB("POOL:Args ok, trying to get into the pool");
  if ((ret = mutex_lock(&pool_mutex)) != 0) {
      fprintf(stderr,"Erorr in unblocking the pool_mutex! %d",ret) ;
      exit(1);
  }

  POOLDEB("POOL:Aquired the pool mutex, checking for free threads");
  SPINLOCK_LOCK(pool_spinlock);
  POOLDEB("POOL:Aquired the pool spinlock");
  if (pool_num_threads_available == 0) {
    int thread_arg;
    POOLDEB("POOL:Not enough threads in pool - creating a new one");
    pool_num_threads_created++;
    thread_arg = pool_num_threads_created;
    pool_add_thread(thread_arg);
  }
  pool_num_threads_available--;
  SPINLOCK_UNLOCK(pool_spinlock);

  POOLDEB("POOL:Setting the arguments for the pool thread");
  pool_thr_func = func;
  pool_thr_arg  = arg;

  POOLDEB("POOL:Going to unlock a thread");
  if ((ret = mutex_unlock(&pool_thr_worker_blocking_mutex)) != 0) {
      fprintf(stderr,"Erorr in unblocking the worker_mutex! %d",ret) ;
      exit(1);
  }
  POOLDEB("POOL:Unlocked a thread, done with create");
  return 0;
}


void *pool_worker_thread(void *poolindex_ptr) {
  /*  Worker thread - member of the pool */
  int poolindex;
  void *(*func)(void *);
  void *arg;
  void *funcres;
  int ret;

  poolindex = (int)poolindex_ptr;

#ifdef POLLING
  thr_setprio(thr_self(),2);
#endif
  WORKDEB("Creating worker thread");
  for(;;){
    WORKDEB("Waiting for work");
    /*  Block until there is a thread request */
    if ((ret = mutex_lock(&pool_thr_worker_blocking_mutex)) != 0) {
      printf("Workder erorr-unblocking worker mutex! %d %d",ret,poolindex) ;
    };

    WORKDEB("Unblocked worker, getting args");
    func = pool_thr_func;
    arg = pool_thr_arg;
    if ((ret = mutex_unlock(&pool_mutex)) != 0) {
      printf("Workder error-unblocking pool mutex! %d %d",ret,poolindex) ;
    }

    /* Call the actual function */
    /* fprintf(stderr,"using pool thread %d\n",poolindex); */
#ifdef POLLING
    thr_setprio(thr_self(),2);
#endif
    funcres = (*func)(arg);

    WORKDEB("Finished work. Incrementing available thread counter");
    SPINLOCK_LOCK(pool_spinlock);
    WORKDEB("Incrementing threads available");
    pool_num_threads_available++;
    SPINLOCK_UNLOCK(pool_spinlock);
  }

}
#endif /* SOLARIS_THREADS */





