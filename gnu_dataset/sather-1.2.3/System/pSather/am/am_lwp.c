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
 * Thread support for Sun and Linux (uses LWP)
 *
 * Version 1.0 (released for Sather 1.0.9) by Claudio Fleiner (fleiner@icsi.berkeley.edu)
 */
#ifdef LWP
#include <stdio.h>
#include <malloc.h>
#include <lwp.h>
#include "am.h"
#include "am_int.h"

static void wakeup_thread()
{
	/*
	 * This thread ensures that all 250usec am_poll() is exeucted.
	 * If we have only one cluster, we don't need that thread.
	 */
	if(am_clusters>1) 
		while(1) {
			am_poll();
			delayp(10000);
		}
}

void lwp_am_init()
{
	creatp(3,wakeup_thread,200000,0,NULL,NULL);
}

#define addtmv(a,b) 	do { (a).tv_sec+=(b).tv_sec;(a).tv_usec+=(b).tv_usec;if((a).tv_usec>1000000) { (a).tv_usec-=1000000;(a).tv_sec++; } } while(0)
#define subtmv(a,b) 	do { (a).tv_sec-=(b).tv_sec;(a).tv_usec-=(b).tv_usec;if((a).tv_usec<0) { (a).tv_usec+=1000000;(a).tv_sec--; } } while(0)
#define cmptmvi(a,b) 	(((a)>(b))?1:((a)<(b)?-1:0))
#define cmptmv(a,b)  	((a).tv_sec==(b).tv_sec?cmptmvi((a).tv_usec,(b).tv_usec):cmptmvi((a).tv_sec,(b).tv_sec))
#define cptmv(a,b) 	a=b
#define usectmv(a) 	((a).tv_sec*1000000+(a).tv_usec)
#define settmvusec(a,s) do {a.tv_sec=s/100000;a.tv_usec=s%1000000; } while(0)
extern int gettimeofday(struct timeval *,struct timezone *);
void am_sleep(unsigned long duration)
{
	delayp(duration*1000000);
}

void am_thread_poll(void)
{
	yieldp();
}

static void thr_signal_it(vnn_t from,struct pcb *pcb)
{
	intrp(pcb);
}


void thr_ignore_signal()	{ THR_IGNORE_SIGNAL; }
void thr_block_signal() 	{ THR_BLOCK_SIGNAL; }
void thr_enable_signal()	{ THR_ENABLE_SIGNAL; }
int thr_signal_ignored()	{ return THR_SIGNAL_IGNORED; }
int thr_signal_blocked()	{ return THR_SIGNAL_BLOCKED; }
int thr_signal_enabled()	{ return THR_SIGNAL_ENABLED; }
void thr_reset_signal() 	{ THR_RESET_SIGNAL; }
int thr_got_signal() 		{ return THR_GOT_SIGNAL; }

void thr_signal(thread_t id)
{
	if(id.node!=AM_MY_PROC) 
		am_request_1(id.node,(handler_1_t)thr_signal_it,(long)id.pcb);
	else
		intrp(id.pcb);
}

signal_handler_t thr_set_signal_handler(signal_handler_t signal_handler)
{
	return THR_SET_SIGNAL_HANDLER(signal_handler);
}

char * thr_print_id(thread_t id,char *p)
{
	sprintf(p,"%02d-%-8ld",id.node,(long)id.pcb);
	return p;
}

struct delay_struct {
	delay_t delay;
	handler_delay_t func;
	void *arg;
	thread_t tid;
};

static void delay_thread(int c,struct delay_struct *ds)
{
	delayp(ds->delay.sec*1000000+ds->delay.nsec/1000);
	if(ds->func!=NULL) {
		(*(ds->func))(ds->arg);
	} else thr_signal(ds->tid);
	free(ds);
}

void thr_delay_signal(delay_t delay)
{
	thr_delay_function(delay,NULL,NULL);
}

void thr_delay_function(delay_t delay,handler_delay_t func,void *arg)
{
	struct delay_struct *ds;
	ds=(struct delay_struct *)malloc(sizeof(struct delay_struct));
	ds->func=func;
	ds->delay=delay;
	ds->arg=arg;
	ds->tid=thr_id();
	creatp(3,(void (*)())delay_thread,20000,1,(char **)ds,NULL);
}



lock_t lck_create() 			{ return LCK_CREATE; }
void lck_lock(lock_t lck) 		{ LCK_LOCK(lck); }
void lck_unlock(lock_t lck)	 	{ LCK_UNLOCK(lck); }
int lck_try(lock_t lck) 		{ return LCK_TRY(lck); }
void lck_delete(lock_t lck) 		{ LCK_DELETE(lck); }

sema_t sema_create(unsigned int count) 	{ return SEMA_CREATE(count); }
void sema_signal(sema_t sem) 		{ SEMA_SIGNAL(sem); }
int sema_wait(sema_t sem) 		{ return SEMA_WAIT(sem); }
int sema_try(sema_t sem) 		{ return SEMA_TRY(sem); }
void sema_delete(sema_t sem) 		{ SEMA_DELETE(sem); }

void thr_set_local(void *p)		{ THR_SET_LOCAL(p); }
void *thr_local()			{ return THR_LOCAL; }

thread_t thr_id()
{
	thread_t id;
	id.node=AM_MY_PROC;
	id.pcb=currp;
	return id;
}
int thr_same_id(thread_t id1,thread_t id2) 
{ return THR_SAME_ID(id1,id2); }
int thr_lt(thread_t id1,thread_t id2) 
{ return THR_LT(id1,id2); }
int thr_hash(thread_t id1)
{ return THR_HASH(id1); }

thread_t thr_no_thread()
{
	thread_t id;
	id.node=0;
	id.pcb=NULL;
	return id;
}


/* things necessary to determine the number of processors on
   the calling cluster */
#ifdef __linux__

int am_set_my_cluster_size() {
  return 1;
}

#else  /* SunOS 4. */

#include <sys/types.h>
#include <sys/file.h>
#include <nlist.h>
#include <kvm.h>
 
struct nlist	nl[] = {
#define N_NCPU          0
        { "_ncpu" },
};
static kvm_t *kd;

void kernel_get(addr, dest, size, info)
u_long addr;
char *dest;
u_int size;
char *info;
{
	/* get something from the kernel */
	if(kvm_read(kd, addr, dest, size)!=size){
		perror(info);
		exit(1);
	}
}

void kernel_init(name)
char *name;
{
  char *kernelSymbols;
	/* initialize kernel */
	if((kvm_t *)NULL==(kd=kvm_open(kernelSymbols, NULL, NULL, O_RDONLY, 
			name))){
		perror("kvm_open");
		exit(1);
	}

	/* read namelist */
	if(-1==kvm_nlist(kd, nl)){
		perror("kvm_nlist");
	
	exit(1);
	}
}

int am_set_my_cluster_size()
{
  /* count cpus */
  
  int n=1; /* assume uniprocessor by default */
  
  if(N_UNDF!=nl[N_NCPU].n_type) {
    kernel_get(nl[N_NCPU].n_value, (char *)&n, sizeof(n), "ncpu");
    if(4<n || n<1){
      /* if n is unreasonable, assume uniprocessor */
      n=1;
    }
  } 
  return(n);
}

#endif /* __linux__ */
/* Nobbi: just a dummy var to get it compiled. */
int am_cluster_size;

#endif /* LWP */










