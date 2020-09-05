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
#include <pSather.h>
#include <sys/time.h>
#include <debug.h>

#define GATES_TO_ENQUEUE 4 
#define LOCKS_TO_LOCK	 20
#define THREADS		 10
#define ITERATIONS	 3 /* 3 */
#define NESTINGS	 6 /* 6 */
#define MULTI_LOCK	 3
#define MAX_WORK	 1000001
#define MIN_WORK         2000

GATET g[GATES_TO_ENQUEUE];
MUTEX m[LOCKS_TO_LOCK];

#ifndef SOLARIS_THREADS
#ifndef __linux__
long nrand48(short *);
#endif
#endif
int gettimeofday(struct timeval *,struct timezone *);
#ifndef SOLARIS
int printf(const char *,...);
#endif
int fflush(FILE *);
int sscanf(const char *,const char *,...);

void mk_dequeue(short *rn)
{
	int i;
	i=nrand48(rn)%GATES_TO_ENQUEUE;
	while(i-->0) {
		int j;
		FOB v;
		j=nrand48(rn)%GATES_TO_ENQUEUE;
		GATET_DEQUEUE(g[j],v);
		GATET_ENQUEUE(g[j],v);
	}
}

void some_work(short *rn,struct timeval *tot)
{
	long duration;
	struct timeval now,stp,start;

	duration=nrand48(rn)%MAX_WORK;
	if(duration<MIN_WORK) duration=MIN_WORK;

	gettimeofday(&start,NULL);
	gettimeofday(&stp,NULL);

	stp.tv_sec+=duration/1000000;
	stp.tv_usec+=duration%1000000;
	while(stp.tv_usec>1000000) {
		stp.tv_usec-=1000000;
		stp.tv_sec++;
	}

	DEBUG0("working");
	while(1) {
		gettimeofday(&now,NULL);
		if(now.tv_sec>stp.tv_sec || (now.tv_sec==stp.tv_sec && now.tv_usec>stp.tv_usec))
			break;
	}
	DEBUG0("working done");

	gettimeofday(&stp,NULL);
	stp.tv_sec-=start.tv_sec;
	if(stp.tv_usec>start.tv_usec) {
		stp.tv_usec-=start.tv_usec;
	} else {
		stp.tv_usec+=1000000-start.tv_usec;
		stp.tv_sec--;
	}
	tot->tv_sec+=stp.tv_sec;
	tot->tv_usec+=stp.tv_usec;
	while(tot->tv_usec>1000000) {
		tot->tv_usec-=1000000;
		tot->tv_sec++;
	}
	SYS_DEFER;
}

void lockem(int bot,int it,short *rn,struct timeval *tot)
{
	int lcks;
	MUTEX lk[MULTI_LOCK];
	int i,n;
	volatile int nbot=bot;

	some_work(rn,tot);
	if(it==NESTINGS || bot==LOCKS_TO_LOCK-1) return;

	mk_dequeue(rn);
	lcks=(nrand48(rn)%MULTI_LOCK)+1;
	if(lcks+bot>=LOCKS_TO_LOCK) lcks=LOCKS_TO_LOCK-bot;
	if(lcks<=0) return;

	for(i=0;i<lcks;i++) {
		n=-1;
		do {
			n=nrand48(rn)%LOCKS_TO_LOCK;
		} while(n<=bot);
		lk[i]=m[n];
		if(nbot<n) nbot=n;
	}

	switch(lcks) {
	case 1:
		LOCK1(lk[0])
			lockem(nbot,it+1,rn,tot);
		ENDLOCK
		break;
	case 2:
		LOCK2(lk[0],lk[1])
			lockem(nbot,it+1,rn,tot);
		ENDLOCK
		break;
	case 3:
		LOCK3(lk[0],lk[1],lk[2])
			lockem(nbot,it+1,rn,tot);
		ENDLOCK
		break;
	case 4:
		LOCK4(lk[0],lk[1],lk[2],lk[3])
			lockem(nbot,it+1,rn,tot);
		ENDLOCK
		break;
	case 5:
		LOCK5(lk[0],lk[1],lk[2],lk[3],lk[4])
			lockem(nbot,it+1,rn,tot);
		ENDLOCK
		break;
	}
}

void forkfunc(FOB self,FOB arg,GATE gate,int pos)
{
	int j;
	short rn[3];
	struct timeval tot;
		
	rn[0]= arg.cl;
	rn[1]= arg.cl;
	rn[2]= arg.cl;
	tot.tv_sec=tot.tv_usec=0;

	for(j=0;j<NESTINGS;j++) {
	      lockem(0,0,rn,&tot);
	}
	printf("WORK: %ld.%06ld\n",tot.tv_sec,tot.tv_usec);
}


int main(int argc,char *argv[])
{
	int i;
	GATE pargate;
	struct timeval now,end;
	extern double l_k;
	extern int debug_psather;
	sscanf(argv[1],"%lf",&l_k);
	fprintf(stderr,"K=%f\n",l_k);
	fflush(stderr);
	if(argc>=2 && strcmp(argv[1],"-debug")==0) debug_psather=1;
	PSATHER_START(argc,argv);
	gettimeofday(&now,NULL);
	pargate=GATE_CREATE;

	for(i=0;i<GATES_TO_ENQUEUE;i++) g[i]=GATET_CREATE;
	for(i=0;i<LOCKS_TO_LOCK;i++) m[i]=MUTEX_CREATE;

	for(i=0;i<THREADS;i++) {
		volatile FOB a;
		a.cl=i;
		ATTACH(forkfunc,FNULL,a,pargate,HERE);
	}
	for(i=0;i<GATES_TO_ENQUEUE;i++)
		GATET_ENQUEUE(g[i],MAKEFAR((void *)1));

	LOCK1(GATE_NO_THREADS(pargate))
	ENDLOCK
	gettimeofday(&end,NULL);
	end.tv_usec-=now.tv_usec;
	if(end.tv_usec<0) {
		end.tv_usec+=1000000;
		end.tv_sec--;
	}
	end.tv_sec-=now.tv_sec;
	printf("done after %ld.%06ld sec\n",end.tv_sec,end.tv_usec);
	fflush(stdout);
	PSATHER_STOP;

	return(0);
}
