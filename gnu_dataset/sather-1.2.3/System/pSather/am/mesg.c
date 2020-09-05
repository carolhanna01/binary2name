/*------------------------->  ANSI C - sourcefile  <-------------------------*/
/* Copyright (C) 199x by International Computer Science Institute            */
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

#include <stdio.h>
#include <stdlib.h>
#include <unistd.h>
#include <sys/time.h>
#include <sys/times.h>
#include <limits.h>
#include <assert.h>
#include "am.h"

#define subtmv(a,b) 	do { (a).tv_sec-=(b).tv_sec;(a).tv_usec-=(b).tv_usec;if((a).tv_usec<0) { (a).tv_usec+=1000000;(a).tv_sec--; } } while(0)
/* extern int gettimeofday(struct timeval *,void *);  -- Nobbi: not needed as of Linux 2.2.11 */

int printf(const char *,...);
int fprintf(FILE *,const char *,...);
int _filbuf();

int fflush(FILE *);
long reply_buf=0;

#ifdef AM_THREADS
lock_t mtx;
#endif

#define AM_NEXT ((AM_MY_PROC+1)%AM_PROCS)

void bye(int from)
{
	am_disable();
	exit(0);
}

void benchmark(int);
void test_speed();
void test_request(int);
void test_store(void);
void test_waitcounter(int);
void test_tas_counter(int);
void test_thr(int);
void test_lock(int);
void test_signal();
void test_signal_sema();
void test_delay();
void barrier();


int main(int argc,char *argv[])
{
	long msg;
	struct timeval start,end;

	printf("starting %d nodes\n",atoi(argv[1]));

	if (atoi(argv[1]) < 2) {
	    printf("Error: Need at least 2 nodes!");
	    exit(1);
	}

#ifdef AM_THREADS
	mtx=LCK_CREATE;
#ifdef SOLARIS_THREADS
/* Nobbi */
	thr_setconcurrency(2);
#endif
#endif
	msg=atoi(argv[2]);

	fflush(stdout);
	am_enable(atoi(argv[1]),argc,argv);

	/*
	benchmark(msg);
	*/
#define PR(a,name) if(!AM_MY_PROC) {\
			gettimeofday(&start,NULL);\
			printf("test of `%s' starting now\n",name);\
			fflush(stdout);\
		   }\
		   a;barrier();\
		   if(!AM_MY_PROC) {\
			   gettimeofday(&end,NULL);\
			   subtmv(end,start);\
			   printf("test of `%s' done after %ld.%06ld seconds\n",name,end.tv_sec,end.tv_usec);\
			   fflush(stdout);\
		   }


	/* barrier(); */
	PR(test_speed(msg),"am_speed");
	PR(test_request(msg),"am_request_N and am_reply_N");
	PR(test_store(),"am_store, am_store_async, am_get");
	PR(test_waitcounter(msg),"waitcounters");
	PR(test_tas_counter(1),"throw away semaphores");
	
#ifdef AM_THREADS
	PR(test_thr(msg),"thr_create_N, Semaphores and local memory");
	PR(test_lock(msg),"locks");
	PR(test_delay(),"delays");
	PR(test_signal(),"signals");
	PR(test_signal_sema(),"signals and semaphores");
#endif
	am_disable();
	return 0;
}

int barr_var=0;
void barrier()
{
	int i=1;
	int n;
	n=AM_NEXT;
	if(AM_MY_PROC==0) {
		barr_var=0;
		am_store(n,&i,&barr_var,sizeof(i),(handler_mem_t)am_dummy,NULL);
		am_wait_for(barr_var);
		i=2;
		am_store(n,&i,&barr_var,sizeof(i),(handler_mem_t)am_dummy,NULL);
		am_wait_for(barr_var==2);
		barr_var=0;
	} else {
		am_wait_for(barr_var==1);
		am_store(n,&i,&barr_var,sizeof(i),(handler_mem_t)am_dummy,NULL);
		am_wait_for(barr_var==2);
		i=2;
		barr_var=0;
		am_store(n,&i,&barr_var,sizeof(i),(handler_mem_t)am_dummy,NULL);
	}
}

/****************************** BENCHMARK, TEST 1 *********************/
static int b_got=0;
void bench_reply(int from,long arg)
{
#ifdef AM_THREADS
	LCK_LOCK(mtx);
#endif
	reply_buf++;
#ifdef AM_THREADS
	LCK_UNLOCK(mtx);
#endif
}

void got_bench_msg(int from,long arg)
{
#ifdef AM_THREADS
	LCK_UNLOCK(mtx);
#endif
	b_got++;
#ifdef AM_THREADS
	LCK_UNLOCK(mtx);
#endif
	am_reply_1(from,bench_reply,arg+1);
}
void benchmark(int msg)
{
	struct timeval start,end;
	double f;
	int i,j;
	if(am_my_proc()==0) {
		printf("process 0 sends now %d requests to all other processes\n",msg);
		gettimeofday(&start,NULL);
		for(j=0;j<msg;j++) 
			for(i=1;i<am_procs();i++) 
				am_request_1(i,got_bench_msg,i+100);
		am_wait_for(reply_buf==msg*(am_procs()-1));
		gettimeofday(&end,NULL);
		subtmv(end,start);

		printf("The master got all %d reply's after %ld.%06ld sec\n",msg*(am_procs()-1),end.tv_sec,end.tv_usec);
		f=end.tv_sec+(double)end.tv_usec/1000000;
		printf("av. time per message: %.9f\n",f/(double)(msg*(am_procs()-1)));
	} else {
		am_wait_for(b_got==msg);
	}
}

/************* test request_n and reply_n ************************/
#ifdef P
#undef P
#endif
#define pP	printf("%ld: %ld %ld\n",(long)AM_MY_PROC,r_req,r_get);fflush(stdout)
#define P
static long r_get=0,r_req=0;
void r_reply_0(vnn_t from)
{
#ifdef AM_THREADS
	LCK_LOCK(mtx);
#endif
	r_get++;
	P;
#ifdef AM_THREADS
	LCK_UNLOCK(mtx);
#endif
}
void r_reply_1(vnn_t from,long arg1)
{
#ifdef AM_THREADS
	LCK_LOCK(mtx);
#endif
	if(arg1!=AM_MY_PROC+199) {
		fprintf(stderr,"r_reply_1: node %d got a garbeled reply from node %d\n",AM_MY_PROC,from);
		fflush(stdout);
		abort();exit (-1);
	}
	r_get++;
	P;
#ifdef AM_THREADS
	LCK_UNLOCK(mtx);
#endif
}
void r_reply_2(vnn_t from,long arg1,long arg2)
{
#ifdef AM_THREADS
	LCK_LOCK(mtx);
#endif
	if(arg1!=AM_MY_PROC+199 || AM_MY_PROC*2+from!=arg2) {
		fprintf(stderr,"r_reply_2: node %d got a garbeled reply from node %d\n",AM_MY_PROC,from);
		fflush(stdout);
		abort();exit (-1);
	}
	r_get++;
	P;
#ifdef AM_THREADS
	LCK_UNLOCK(mtx);
#endif
}
void r_reply_3(vnn_t from,long arg1,long arg2,long arg3)
{
#ifdef AM_THREADS
	LCK_LOCK(mtx);
#endif
	if(arg1!=AM_MY_PROC+199 || AM_MY_PROC*2+from!=arg2 || AM_MY_PROC*AM_MY_PROC!=arg3) {
		fprintf(stderr,"r_reply_3: node %d got a garbeled reply from node %d\n",AM_MY_PROC,from);
		fflush(stdout);
		abort();exit (-1);
	}
	r_get++;
	P;
#ifdef AM_THREADS
	LCK_UNLOCK(mtx);
#endif
}
void r_reply_4(vnn_t from,long arg1,long arg2,long arg3,long arg4)
{
#ifdef AM_THREADS
	LCK_LOCK(mtx);
#endif
	if(arg1!=AM_MY_PROC+199 || AM_MY_PROC*2+from!=arg2 || AM_MY_PROC*AM_MY_PROC!=arg3 || from*from!=arg4) {
		fprintf(stderr,"r_reply_4: node %d got a garbeled reply from node %d\n",AM_MY_PROC,from);
		fflush(stdout);
		abort();exit (-1);
	}
	r_get++;
	P;
#ifdef AM_THREADS
	LCK_UNLOCK(mtx);
#endif
}
void r_reply_df(vnn_t from,long arg1,long arg2,double arg3)
{
	double x=(double)(from+13)/(double)(AM_MY_PROC+17);
#ifdef AM_THREADS
	LCK_LOCK(mtx);
#endif
	if(arg1!=AM_MY_PROC+199 || AM_MY_PROC*2+from!=arg2 || arg3!=x) {
		fprintf(stderr,"r_reply_df: node %d got a garbeled reply from node %d\n",AM_MY_PROC,from);
		fprintf(stderr,"got:        %ld %ld %.20f\n",arg1,arg2,arg3);
		fprintf(stderr,"instead of: %ld %ld %.20f\n",(long)(AM_MY_PROC+199),(long)(AM_MY_PROC*2+from),x);
		fflush(stdout);
		abort();exit (-1);
	}
	r_get++;
	P;
#ifdef AM_THREADS
	LCK_UNLOCK(mtx);
#endif
}


void test_speed_reply_spin(vnn_t from,long a)
{
	*(long *)a=0;
}
void test_speed_request_spin(vnn_t from,long a)
{
	am_reply_1(from,test_speed_reply_spin,a);
}

#ifdef AM_THREADS
void test_speed_reply_mutex(vnn_t from,long a)
{
	LCK_UNLOCK((lock_t)a);
}
void test_speed_request_mutex(vnn_t from,long a)
{
	am_reply_1(from,test_speed_reply_mutex,a);
}
#endif

void test_speed()
{
	if(AM_MY_PROC==0) {
		/* local speed */
		long t;
		volatile long spin;
		int r=5000;
		int i;
#ifdef AM_THREADS
		lock_t lck=LCK_CREATE;
#endif
		struct tms b;

		printf("\nSpeed of am_request/am_reply");
#ifdef AM_THREADS
		LCK_LOCK(lck);
#endif
		t=times(&b);
		for(i=0;i<r;i++) {
			spin=1;
			am_request_1(0,test_speed_request_spin,(long)&spin);
			am_wait_for(spin==0);
		}
		t=times(&b)-t;
		printf("local, spinlocks: %d req/rep: %f sec\n",r,(float)t/(float)(CLK_TCK));


		t=times(&b);
		for(i=0;i<r;i++) {
			spin=1;
			am_request_1(1,test_speed_request_spin,(long)&spin);
/* Nobbi: this could not work!! */
/*#ifdef AM_THREADS
			while(spin);
#else*/
			am_wait_for(spin==0);
/*#endif*/
		}
		t=times(&b)-t;
		printf("remote, spinlocks: %d req/rep: %f sec\n",r,(float)t/(float)(CLK_TCK));

#ifdef AM_THREADS
		t=times(&b);
		for(i=0;i<r;i++) {
			spin=1;
			am_request_1(0,test_speed_request_mutex,(long)lck);
			LCK_LOCK(lck);
		}
		t=times(&b)-t;
		printf("local, mutex: %d req/rep: %f sec\n",r,(float)t/(float)(CLK_TCK));
		t=times(&b);
		for(i=0;i<r;i++) {
			spin=1;
			am_request_1(1,test_speed_request_mutex,(long)lck);
			LCK_LOCK(lck);
		}
		t=times(&b)-t;
		printf("remote, spinlocks: %d req/rep: %f sec\n",r,(float)t/(float)(CLK_TCK));
#endif
	}
}

void r_req_0(vnn_t from)
{
#ifdef AM_THREADS
	LCK_LOCK(mtx);
#endif
	r_req++;
	P;
#ifdef AM_THREADS
	LCK_UNLOCK(mtx);
#endif
	am_reply_0(from,r_reply_0);
}
void r_req_1(vnn_t from,long arg1)
{
#ifdef AM_THREADS
	LCK_LOCK(mtx);
#endif
	if(arg1!=from+199) {
		fprintf(stderr,"r_req_1: node %d got a garbeled req from node %d\n",AM_MY_PROC,from);
		fflush(stdout);
		abort();exit (-1);
	}
	r_req++;
	P;
#ifdef AM_THREADS
	LCK_UNLOCK(mtx);
#endif
	am_reply_1(from,r_reply_1,arg1);
}
void r_req_2(vnn_t from,long arg1,long arg2)
{
#ifdef AM_THREADS
	LCK_LOCK(mtx);
#endif
	if(arg1!=from+199 || from*2+AM_MY_PROC!=arg2) {
		fprintf(stderr,"r_req_2: node %d got a garbeled req from node %d\n",AM_MY_PROC,from);
		fflush(stdout);
		abort();exit (-1);
	}
	r_req++;
	P;
#ifdef AM_THREADS
	LCK_UNLOCK(mtx);
#endif
	am_reply_2(from,r_reply_2,arg1,arg2);
}
void r_req_3(vnn_t from,long arg1,long arg2,long arg3)
{
#ifdef AM_THREADS
	LCK_LOCK(mtx);
#endif
	if(arg1!=from+199 || from*2+AM_MY_PROC!=arg2 || from*from!=arg3) {
		fprintf(stderr,"r_req_3,node %d got a garbeled req from node %d\n",AM_MY_PROC,from);
		fflush(stdout);
		abort();exit (-1);
	}
	r_req++;
	P;
#ifdef AM_THREADS
	LCK_UNLOCK(mtx);
#endif
	am_reply_3(from,r_reply_3,arg1,arg2,arg3);
}
void r_req_4(vnn_t from,long arg1,long arg2,long arg3,long arg4)
{
#ifdef AM_THREADS
	LCK_LOCK(mtx);
#endif
	if(arg1!=from+199 || from*2+AM_MY_PROC!=arg2 || from*from!=arg3 || AM_MY_PROC*AM_MY_PROC!=arg4) {
		fprintf(stderr,"r_req_4: node %d got a garbeled req from node %d\n",AM_MY_PROC,from);
		fflush(stdout);
		abort();exit (-1);
	}
	r_req++;
	P;
#ifdef AM_THREADS
	LCK_UNLOCK(mtx);
#endif
	am_reply_4(from,r_reply_4,arg1,arg2,arg3,arg4);
}
void r_req_df(vnn_t from,long arg1,long arg2,double arg3)
{
	double x=(double)(AM_MY_PROC+13)/(double)(from+17);
#ifdef AM_THREADS
	LCK_LOCK(mtx);
#endif
	if(arg1!=from+199 || from*2+AM_MY_PROC!=arg2 || arg3!=x) {
		fprintf(stderr,"r_req_df: node %d got a garbeled reply from node %d\n",AM_MY_PROC,from);
		fprintf(stderr,"got:        %ld %ld %.20f  %lx  %lx\n",arg1,arg2,arg3,((long*)&arg3)[0],((long*)&arg3)[1]);
		fprintf(stderr,"instead of: %ld %ld %.20f  %lx  %lx\n",(long)(from+199),(long)(from*2+AM_MY_PROC),x,((long*)&x)[0],((long*)&x)[1]);
		fflush(stdout);
		abort();exit (-1);
	}
	r_req++;
	P;
#ifdef AM_THREADS
	LCK_UNLOCK(mtx);
#endif
	am_reply_df(from,r_reply_df,arg1,arg2,arg3);
}

long rrr=0;
void test_request(int msg)
{
	int i;
	int j,k;
	P;
	for(j=0;j<msg;j++)
		for(k=0;k<6;k++) 
			for(i=0;i<AM_PROCS;i++) {
				rrr++;
				switch(k) {
				case 0: am_request_0(i,r_req_0);break;
				case 1: am_request_1(i,r_req_1,199+AM_MY_PROC);break;
				case 2: am_request_2(i,r_req_2,199+AM_MY_PROC,AM_MY_PROC*2+i);break;
				case 3: am_request_3(i,r_req_3,199+AM_MY_PROC,AM_MY_PROC*2+i,AM_MY_PROC*AM_MY_PROC);break;
				case 4: am_request_4(i,r_req_4,199+AM_MY_PROC,AM_MY_PROC*2+i,AM_MY_PROC*AM_MY_PROC,i*i);break;
				case 5: am_request_df(i,r_req_df,199+AM_MY_PROC,AM_MY_PROC*2+i,(double)(i+13)/(double)(AM_MY_PROC+17));break;
				}
			}

	am_wait_for(r_get==msg*6*AM_PROCS && r_req==msg*6*AM_PROCS);
}


/********************** TEST STORE ***********************/

char buf[400];
int r_got_it=0;
#define MSG "Hey, this is my rather long message, are you interested?"

static void r_store(vnn_t from,void *lva,int size,void *arg)
{
	if(&r_got_it!=arg || strcmp(buf,MSG)!=0 || buf[255]!=from 
   	   || buf[155]!=from || buf[size-1]!=from || size!=400) {
		fprintf(stderr,"Node %d got garbeled store from %d\n",AM_MY_PROC,from);
		abort();
	}
	r_got_it=1;
}
static void r_get_m(vnn_t from,char *buf,int size,void *arg)
{
	if(&r_got_it!=arg || strcmp(buf,MSG)!=0 || buf[255]!=from 
   	   || buf[155]!=from || buf[size-1]!=from || size!=400) {
		fprintf(stderr,"Node %d got garbeled get from %d\n",AM_MY_PROC,from);
		abort();
	}
	r_got_it=3;
}

static void p_st_end(vnn_t from,void *arg,int size,int *g)
{
	if(g!=&r_got_it) {
		fprintf(stderr,"Node %d got garbeled endfunc from %d\n",AM_MY_PROC,from);
		abort();
	}
	*g=4;
}

void test_store()
{
	char b[400];
	int i;
	if(AM_MY_PROC==0) {
		strcpy(buf,MSG);
		buf[155]=buf[255]=buf[399]=AM_MY_PROC;
		am_store(AM_NEXT,buf,buf,400,r_store,&r_got_it);
		am_wait_for(r_got_it);
		buf[155]=buf[255]=buf[399]=AM_MY_PROC;
		am_get(AM_NEXT,buf,b,400,(handler_mem_t)r_get_m,&r_got_it);
		am_wait_for(r_got_it==3);
		i=2;
		am_store(AM_NEXT,&i,&r_got_it,sizeof(i),(handler_mem_t)am_dummy,NULL);
		am_wait_for(r_got_it==2);
	} else {
		am_wait_for(r_got_it);
		buf[155]=buf[255]=buf[399]=AM_MY_PROC;
		if(AM_MY_PROC==AM_PROCS-1)
			am_store(0,buf,buf,400,r_store,&r_got_it);
		else
			am_store(AM_MY_PROC+1,buf,buf,400,r_store,&r_got_it);
		am_wait_for(r_got_it==2);
		if(AM_MY_PROC==AM_PROCS-1)
			am_get(0,buf,b,400,(handler_mem_t)r_get_m,&r_got_it);
		else
			am_get(AM_MY_PROC+1,buf,b,400,(handler_mem_t)r_get_m,&r_got_it);
		am_wait_for(r_got_it==3);
		i=2;
		if(AM_MY_PROC==AM_PROCS-1) 
			am_store_async(0,&i,&r_got_it,sizeof(i),(handler_mem_t)am_dummy,NULL,(handler_mem_t)p_st_end,&r_got_it);
		else
			am_store_async(AM_MY_PROC+1,&i,&r_got_it,sizeof(i),(handler_mem_t)am_dummy,NULL,(handler_mem_t)p_st_end,&r_got_it);
		am_wait_for(r_got_it==4);
	}
}


/*
 * we check the counters by using functions. As the functions
 * use the macros, we know after the test that both will work
 */
static void t_wt_counter(vnn_t from,cntr_t ctr)
{
	am_reply_1(from,r_cntr_decr,(long)ctr);
}

void test_waitcounter(int msg)
{
	int i,j;
	WAITCNTR(wc);

	for(i=0;i<msg;i++) {
		for(j=0;j<AM_PROCS;j++) {
			CNTR_INCR(wc);
			am_request_1(j,(handler_1_t)t_wt_counter,(long)wc);
		}
	}
	cntr_wait_for_zero(wc);
}

static void t_b_counter(vnn_t from,cntr_t ctr)
{
	am_reply_1(from,r_ta_sema_signal,(long)ctr);
}

void test_tas_counter(int msg)
{
	int i,j;

	for(i=0;i<msg;i++) {
		for(j=0;j<AM_PROCS;j++) {
			TA_SEMAPHORE(wc);
			am_request_1(j,(handler_1_t)t_b_counter,(long)wc);
			ta_sema_wait(wc);
		}
	}
}


#ifdef AM_THREADS
#define PP printf("%02d thread started\n",AM_MY_PROC);fflush(stdout)
#define P2 printf("%02d! waiting\n",AM_MY_PROC);fflush(stdout);
#define P1(a) printf("%02d# sending signal now to %d\n",AM_MY_PROC,a);fflush(stdout)
#define THR 6
static volatile int ctr_thr= 0;
static void t_thr_0(vnn_t from)
{
#ifdef SOLARIS_THREADS
/* Nobbi */
	int prio;
	thr_getprio(thr_self(),&prio);
#endif
	LCK_LOCK(mtx);
	ctr_thr++;
	LCK_UNLOCK(mtx);
	THR_SET_LOCAL(&from);
	am_wait_for(ctr_thr>=THR);
	LCK_LOCK(mtx);
	ctr_thr++;
	LCK_UNLOCK(mtx);
	if(THR_LOCAL!=(void *)&from) {
		fprintf(stderr,"Local memory on Node %d garbeled\n",AM_MY_PROC);
		abort();
	}
}
void mr_sema_signal(vnn_t from,long sem)
{
        SEMA_SIGNAL((sema_t)sem);
}

static void t_thr_1(vnn_t from,long sem)
{
#ifdef SOLARIS_THREADS
/* Nobbi */
	int prio;
	thr_getprio(thr_self(),&prio);
#endif
	LCK_LOCK(mtx);
	ctr_thr++;
	LCK_UNLOCK(mtx);
	THR_SET_LOCAL(&from);
	am_wait_for(ctr_thr>=THR);
	am_request_1(from,mr_sema_signal,sem);
	LCK_LOCK(mtx);
	ctr_thr++;
	LCK_UNLOCK(mtx);
	if(THR_LOCAL!=(void *)&from) {
		fprintf(stderr,"Local memory on Node %d garbeled\n",AM_MY_PROC);
		abort();
	}
}
static void t_thr_2(vnn_t from,long sem,long arg1)
{
#ifdef SOLARIS_THREADS
/* Nobbi */
	int prio;
	thr_getprio(thr_self(),&prio);
#endif
	LCK_LOCK(mtx);
	ctr_thr++;
	LCK_UNLOCK(mtx);
	THR_SET_LOCAL(&from);
	if(arg1!=from*2) {
		fprintf(stderr,"Node %d got a garbled thr_create_2 from %d\n",AM_MY_PROC,from);
		abort();
	}
	am_wait_for(ctr_thr>=THR);
	am_request_1(from,mr_sema_signal,sem);
	LCK_LOCK(mtx);
	ctr_thr++;
	LCK_UNLOCK(mtx);
	if(THR_LOCAL!=(void *)&from) {
		fprintf(stderr,"Local memory on Node %d garbeled\n",AM_MY_PROC);
		abort();
	}
}
static void t_thr_3(vnn_t from,long sem,long arg1,long arg2)
{
#ifdef SOLARIS_THREADS
/* Nobbi */
	int prio;
	thr_getprio(thr_self(),&prio);
#endif
	LCK_LOCK(mtx);
	ctr_thr++;
	LCK_UNLOCK(mtx);
	THR_SET_LOCAL(&from);
	if(arg1!=from*2 || arg2!=from+AM_MY_PROC) {
		fprintf(stderr,"Node %d got a garbled thr_create_3 from %d\n",AM_MY_PROC,from);
		abort();
	}
	am_wait_for(ctr_thr>=THR);
	am_request_1(from,mr_sema_signal,sem);
	LCK_LOCK(mtx);
	ctr_thr++;
	LCK_UNLOCK(mtx);
	if(THR_LOCAL!=(void *)&from) {
		fprintf(stderr,"Local memory on Node %d garbeled\n",AM_MY_PROC);
		abort();
	}
}
static void t_thr_4(vnn_t from,long sem,long arg1,long arg2,long arg3)
{
#ifdef SOLARIS_THREADS
/* Nobbi */
	int prio;
	thr_getprio(thr_self(),&prio);
#endif
	LCK_LOCK(mtx);
	ctr_thr++;
	LCK_UNLOCK(mtx);
	THR_SET_LOCAL(&from);
	if(arg1!=from*2 || arg2!=from+AM_MY_PROC || arg3!=99*AM_MY_PROC) {
		fprintf(stderr,"Node %d got a garbled thr_create_4 from %d\n",AM_MY_PROC,from);
		abort();
	}
	YIELD;
	am_wait_for(ctr_thr>=THR);
	am_request_1(from,mr_sema_signal,sem);
	LCK_LOCK(mtx);
	ctr_thr++;
	LCK_UNLOCK(mtx);
	if(THR_LOCAL!=(void *)&from) {
		fprintf(stderr,"Local memory on Node %d garbeled\n",AM_MY_PROC);
		abort();
	}
}
static void t_thr_df(vnn_t from,long sem,long arg1,double arg2)
{
	volatile double d;
#ifdef SOLARIS_THREADS
/* Nobbi */
	int prio;
	thr_getprio(thr_self(),&prio);
#endif
	LCK_LOCK(mtx);
	ctr_thr++;
	LCK_UNLOCK(mtx);
	THR_SET_LOCAL(&from);
	d=(double)(from+29)/(double)(AM_MY_PROC+13);
	if(arg1!=from*2 || arg2!=d) {
		fprintf(stderr,"Node %d got a garbled thr_create_df from %d\n",AM_MY_PROC,from);
		fprintf(stderr,"got:        %ld %.20f  %lx  %lx\n",arg1,arg2,((long*)&arg2)[0],((long*)&arg2)[1]);
		fprintf(stderr,"instead of: %ld %.20f  %lx  %lx\n",(long)(from*2),d,((long*)&d)[0],((long*)&d)[1]);
		fprintf(stderr,"            arg1=%d || arg2=%d = %d\n",arg1!=from*2,arg2!=d,(arg1!=from*2 || arg2!=d));
		abort();
	}
	YIELD;
	am_wait_for(ctr_thr>=THR);
	am_request_1(from,mr_sema_signal,sem);
	LCK_LOCK(mtx);
	ctr_thr++;
	LCK_UNLOCK(mtx);
	if(THR_LOCAL!=(void *)&from) {
		fprintf(stderr,"Local memory on Node %d garbeled\n",AM_MY_PROC);
		abort();
	}
}

sema_t sem;
void test_thr(int msg)
{
	int i;
	double d;
	sem=sema_create(1);
	if(!sema_try(sem)) {
		fprintf(stderr,"node %d did not succeed in a sema_try for a free sempahore\n",AM_MY_PROC);
		abort();
	}
	if(sema_try(sem)) {
		fprintf(stderr,"node %d succeeded in sema_try for a blocked sempahore\n",AM_MY_PROC);
		abort();
	}
	thr_create_0((AM_MY_PROC+1)%AM_PROCS,t_thr_0);
	thr_create_1((AM_MY_PROC+2)%AM_PROCS,t_thr_1,(long)sem);
	thr_create_2((AM_MY_PROC+3)%AM_PROCS,t_thr_2,(long)sem,AM_MY_PROC*2);
	thr_create_3((AM_MY_PROC+4)%AM_PROCS,t_thr_3,(long)sem,AM_MY_PROC*2,AM_MY_PROC+(AM_MY_PROC+4)%AM_PROCS);
	thr_create_4((AM_MY_PROC+5)%AM_PROCS,t_thr_4,(long)sem,AM_MY_PROC*2,AM_MY_PROC+(AM_MY_PROC+5)%AM_PROCS,99*((AM_MY_PROC+5)%AM_PROCS));
	d=(double)(AM_MY_PROC+29)/(double)((AM_MY_PROC+6)%AM_PROCS+13);
	thr_create_df((AM_MY_PROC+6)%AM_PROCS,t_thr_df,(long)sem,AM_MY_PROC*2,d);
	for(i=0;i<THR-1;i++) {
		sema_wait(sem);
	}
	sema_delete(sem);
	YIELD;
	am_wait_for(ctr_thr==THR*2);
}

static void unlock_me(vnn_t from,long lck)
{
	am_reply_1(from,r_lck_unlock,lck);
}

void test_lock(int msg)
{
	lock_t lck;
	lck=lck_create();
	if(!lck_try(lck)) {
		fprintf(stderr,"node %d did not succeed in a lck_try for a free lock\n",AM_MY_PROC);
		abort();
	}
	if(lck_try(lck)) {
		fprintf(stderr,"node %d succeeded in a lck_try for a blocked lock\n",AM_MY_PROC);
		abort();
	}
	am_request_1(AM_NEXT,unlock_me,(long)lck);
	lck_lock(lck);
	lck_unlock(lck);
	if(!lck_try(lck)) {
		fprintf(stderr,"node %d did not succeed in a lck_try for a free lock\n",AM_MY_PROC);
		abort();
	}
	if(lck_try(lck)) {
		fprintf(stderr,"node %d succeeded in a lck_try for a blocked lock\n",AM_MY_PROC);
		abort();
	}
	lck_unlock(lck);
	lck_delete(lck);
}


thread_t tid;
int got_signal=0;
int no_signal=0;
static void my_handler(void)
{
	if(no_signal) {
		fprintf(stderr,"huch, a thread on node %d got a signal after thr_ignore_signal\n",AM_MY_PROC);
		abort();
	}
	got_signal++;
}
static void s_test(vnn_t from,long bc)
{
	THR_SET_SIGNAL_HANDLER(my_handler);
	if(thr_set_signal_handler(my_handler)!=my_handler) {
		fprintf(stderr,"thr_set_signal_handler should return the previous hander, but it doesn't\n");
		abort();
	}
	if(thr_signal_ignored()) {
		fprintf(stderr,"huch, thr_signal_ignored()==1 at the beginning of a thread\n");
		abort();
	}
	if(thr_signal_blocked()) {
		fprintf(stderr,"huch, thr_signal_blocked()==1 at the beginning of a thread\n");
		abort();
	}
	if(!thr_signal_enabled()) {
		fprintf(stderr,"huch, thr_signal_enabled()==0 at the beginning of a thread\n");
		abort();
	}
	thr_ignore_signal();
	if(!thr_signal_ignored()) {
		fprintf(stderr,"huch, I called 'thr_ignore_signal()' but thr_signal_ignored()!=1\n");
		abort();
	}
	if(thr_signal_blocked()) {
		fprintf(stderr,"huch, thr_signal_blocked() should be 0 here\n");
		abort();
	}
	if(thr_signal_enabled()) {
		fprintf(stderr,"huch, thr_signal_enabled() should be 0 here\n");
		abort();
	}

	tid=thr_id();
	thr_signal(tid);
	if(got_signal!=0 || thr_got_signal()) {
		fprintf(stderr,"huch, a thread on node %d got a signal after thr_ignore_signal\n",AM_MY_PROC);
		abort();
	}
	thr_enable_signal();
	if(thr_signal_ignored()) {
		fprintf(stderr,"huch, thr_signal_ignored()==0 after thr_enable_signal()\n");
		abort();
	}
	if(thr_signal_blocked()) {
		fprintf(stderr,"huch, thr_signal_blocked() should be 0 after thr_enable_signal()\n");
		abort();
	}
	if(!thr_signal_enabled()) {
		fprintf(stderr,"huch, I enabled signals but thr_signal_enabled()==0\n");
		abort();
	}
	thr_block_signal();
	if(thr_signal_ignored()) {
		fprintf(stderr,"huch, thr_signal_ignored()==0 after thr_enable_signal() & thr_block_signal()\n");
		abort();
	}
	if(!thr_signal_blocked()) {
		fprintf(stderr,"huch, I blocked signals, but thr_signal_blocked()==0\n");
		abort();
	}
	if(thr_signal_enabled()) {
		fprintf(stderr,"huch, I blocked signals, but thr_signal_enabled()==1\n");
		abort();
	}
	thr_signal(tid);
	if(got_signal!=0) {
		fprintf(stderr,"huch, there is a problem with thr_block_signal() on node %d\nA signal was not delivered after it was received even after blocking signals.\n",AM_MY_PROC);
		abort();
	}
	if(!thr_got_signal()) {
		fprintf(stderr,"huch, there is a problem with thr_block_signal() on node %d\nThe signal was indeed blocked, but thr_got_signal()==0!\n",AM_MY_PROC);
		abort();
	}
	thr_enable_signal();
	if(got_signal!=1) {
		fprintf(stderr,"huch, there is a problem with thr_enable_signal() when expecting a signal on node %d\nA waiting signal has not been delivered after thr_enable_signal()!\n",AM_MY_PROC);
		abort();
	}
	if(thr_got_signal()) {
		fprintf(stderr,"huch, there is a problem with thr_enable_signal() when expecting a signal on node %d\nAfter a signal has been delivered, thr_got_signal() still returns 1!\n",AM_MY_PROC);
		abort();
	}
	thr_signal(tid);
	if(got_signal!=2) {
		fprintf(stderr,"huch, there is a problem with thr_signal() when expecting a signal on node %d\nEven though signals are enabled, the signals handler has not been called!\n",AM_MY_PROC);
		abort();
	}
	if(thr_got_signal()) {
		fprintf(stderr,"huch, there is a problem with thr_signal() when expecting a signal on node %d\nEven though a signal has been delivered, thr_got_signal()==1!",AM_MY_PROC);
		abort();
	}
	TA_SEMA_SIGNAL((ta_sema_t)bc);
}

static thread_t mid,fid;

static int signal_arrived=0;
static void mysig()
{
	thr_signal(fid);
	signal_arrived=1;
}
static void cth(vnn_t from,thread_t *father,long cnt)
{
	thread_t myid;
	TA_SEMAPHORE(bb);
	WAITCNTR(a);
	thr_block_signal();
	thr_set_signal_handler(mysig);
	if(thr_set_signal_handler(mysig)!=mysig) {
		fprintf(stderr,"thr_set_signal_handler should return the previous hander, but it doesn't\n");
		abort();
	}
	myid=thr_id();
	cntr_incr_by(a,1);
	CNTR_INCR_BY(a,2);
	CNTR_DECR_BY(a,1);
	cntr_decr_by(a,1);
	am_get(from,father,&fid,sizeof(fid),r_cntr_decr_mem,a);
	CNTR_WAIT_FOR_ZERO(a);
	am_request_1(from,r_ta_sema_signal,cnt);
	if(AM_MY_PROC==0) {
		thr_signal(fid);
	} else {
		thr_create_2(AM_NEXT,(handler_2_t)cth,(long)&myid,(long)bb);
		TA_SEMA_WAIT(bb);
		thr_enable_signal();
		YIELD;
		am_wait_for(signal_arrived);
	}
}
static int sdone=0;
void endsig()
{
	sdone=1;
}
void test_signal()
{
	TA_SEMAPHORE(a);
	TA_SEMAPHORE(b);
	thr_block_signal();

	thr_create_1(AM_MY_PROC,s_test,(long)a);
	ta_sema_wait(a);

	if(AM_MY_PROC==0) { 
		mid=thr_id();
		thr_set_signal_handler(endsig);
		if(thr_set_signal_handler(endsig)!=endsig) {
			fprintf(stderr,"thr_set_signal_handler should return the previous hander, but it doesn't\n");
			abort();
		}
		thr_create_2(AM_NEXT,(handler_2_t)cth,(long)&mid,(long)b);
		TA_SEMA_WAIT(b);
		thr_enable_signal();
		am_wait_for(sdone);
	}
	thr_enable_signal();
}

int sigs=0;
static void delay_sig_handler()
{
	sigs=1;
}
static void delay_func_handler(int *i)
{
	(*i)=1;
}

void test_delay()
{
	struct timeval start,end;
	long i;
	delay_t delay={0,500000000};
	thr_set_signal_handler(delay_sig_handler);

	gettimeofday(&start,NULL);
	am_sleep(2);
	gettimeofday(&end,NULL);
	i=end.tv_usec-start.tv_usec;
	i/=1000;
	if(i<0) { i+=1000;end.tv_sec--; }
	i+=(end.tv_sec-start.tv_sec)*1000;
	if(i<2000) {
		fprintf(stderr,"Node %d: am_sleep() returned to early after %ldmsec instead of 500msec\n",AM_MY_PROC,i);
		abort();
	}
	if(i>3500) 
		printf("Node %d: very slow: am_sleep() returned only after %ldmsec (delay was 500msec)\n",AM_MY_PROC,i);fflush(stdout);
	

	gettimeofday(&start,NULL);
	thr_delay_signal(delay);
	am_wait_for(sigs);
	gettimeofday(&end,NULL);
	i=end.tv_usec-start.tv_usec;
	i/=1000;
	if(i<0) { i+=1000;end.tv_sec--; }
	i+=(end.tv_sec-start.tv_sec)*1000;
	if(i<500) {
		fprintf(stderr,"Node %d: delayed signal was delivered to early after %ldmsec instead of 500msec\n",AM_MY_PROC,i);
		abort();
	}
	if(i>2500) 
		printf("Node %d: very slow: delayed signal was received after %ldmsec (delay was 500msec)\n",AM_MY_PROC,i);fflush(stdout);
	
	sigs=0;
	gettimeofday(&start,NULL);
	thr_delay_function(delay,(handler_delay_t)delay_func_handler,&sigs);
	am_wait_for(sigs);
	gettimeofday(&end,NULL);
	i=(end.tv_usec-start.tv_usec)/1000;
	if(i<0) { i+=1000;end.tv_sec--; }
	i+=(end.tv_sec-start.tv_sec)*1000;
	if(i<499) {
		fprintf(stderr,"Node %d: delayed function was executed to early after %ldmsec instead of 500msec\n",AM_MY_PROC,i);
		abort();
	}
	if(i>2500) 
		printf("Node %d: very slow: delayed function was executed after %ldmsec (delay was 500msec)\n",AM_MY_PROC,i);fflush(stdout);
}	

static int t_state=0;
static void ss_delayed()
{
	if(t_state==2) {
		fprintf(stderr,"Node %d: either threads executing a sema_wait() don't get signals,\nor you have a real slow system\n",AM_MY_PROC);
		abort();
	}
}
static void blocking_thread(vnn_t from,lock_t lck,sema_t sem,thread_t *id,int *done)
{
	assert(from==AM_MY_PROC);
	*id=thr_id();
	*done=1;
	if(sema_wait(sem)==0) {
		fprintf(stderr,"Node %d: a sema_wait returned successfull even though it was interrupted\n",AM_MY_PROC);
		abort();
	}
	t_state=1;
}

void test_signal_sema()
{
	thread_t id;
	int done;
	lock_t lck;
	sema_t sem;
	delay_t delay={10,0};
	lck=LCK_CREATE;
	sem=SEMA_CREATE(0);
	lck_lock(lck);
	done=0;
	thr_create_4(AM_MY_PROC,(handler_4_t)blocking_thread,(long)lck,(long)sem,(long)&id,(long)&done);
	am_wait_for(done);
	am_sleep(2); /* give thread enough time to start lck_lock */
	YIELD;
	thr_delay_function(delay,ss_delayed,NULL);
	thr_signal(id);
	am_wait_for(t_state==1);
	LCK_DELETE(lck);
	SEMA_DELETE(sem);
}


#endif /* AM_THREADS */
