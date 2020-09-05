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
#include "am.h"

#define addtmv(a,b) 	do { (a).tv_sec+=(b).tv_sec;(a).tv_usec+=(b).tv_usec;if((a).tv_usec>1000000) { (a).tv_usec-=1000000;(a).tv_sec++; } } while(0)
#define subtmv(a,b) 	do { (a).tv_sec-=(b).tv_sec;(a).tv_usec-=(b).tv_usec;if((a).tv_usec<0) { (a).tv_usec+=1000000;(a).tv_sec--; } } while(0)
#define cmptmvi(a,b) 	(((a)>(b))?1:((a)<(b)?-1:0))
#define cmptmv(a,b)  	((a).tv_sec==(b).tv_sec?cmptmvi((a).tv_usec,(b).tv_usec):cmptmvi((a).tv_sec,(b).tv_sec))
#define cptmv(a,b) 	a=b
#define usectmv(a) 	((a).tv_sec*1000000+(a).tv_usec)
#define settmvusec(a,s) do {a.tv_sec=s/100000;a.tv_usec=s%1000000; } while(0)
extern int gettimeofday(struct timeval *,struct timezone *);

int printf(const char *,...);
int _filbuf(...);

int fflush(FILE *);
long reply_buf=0;

#ifdef SOLARIS_THREADS
#include <thread.h>
mutex_t mtx;
#endif

void reply(int from,long arg)
{
#ifdef SOLARIS_THREADS
	mutex_lock(&mtx);
#endif
	reply_buf++;
#ifdef SOLARIS_THREADS
	mutex_unlock(&mtx);
#endif
/*	printf("cluster %d got reply %ld from %d:  %ld\n",am_my_proc(),reply_buf,from,arg);
	fflush(stdout); */
}
void got_msg(int from,long arg)
{
	/*
	printf("cluster %d got a request from %d:  %ld\n",am_my_proc(),from,arg);
	fflush(stdout);
	*/
	am_reply_1(from,reply,arg+1);
}

void got_mem(int from,char *rva,int size,void *arg)
{
	/*
	rva[size-1]=0;
	printf("cluster %d got memory from %d:  size: %d\n",am_my_proc(),from,size);
	printf("contents: %s\n",rva);
	fflush(stdout);
	*/
	reply_buf++;
}

int main(int argc,char *argv[])
{
	int i,j;
	long msg;
	struct timeval start,end;
	char buf[100];
	printf("starting %d clusters\n",atoi(argv[1]));
#ifdef SOLARIS_THREADS
	mutex_init(&mtx,USYNC_THREAD,0);
#endif
#ifdef AM_DEBUG
	am_enable(atoi(argv[1]),atoi(argv[3]));
#else
	am_enable(atoi(argv[1]));
#endif
	msg=atoi(argv[2]);
	switch(am_my_proc()) {
	case 0:
		printf("cluster %d\n",am_my_proc());fflush(stdout);
		sleep(1);
		printf("cluster 0 sends now %ld mem to all other clusters\n",msg);
		gettimeofday(&start,NULL);
		for(j=0;j<msg;j++) 
			for(i=1;i<am_procs();i++) {
				sprintf(buf,"message %d to process %d, courtesy of AM",j,i);
				am_get(i,buf,buf,43,(handler_mem_t)got_mem,NULL);
			}
		wait_for(reply_buf==msg*(am_procs()-1));
		gettimeofday(&end,NULL);
		subtmv(end,start);
		{
			double f;
			printf("The master got all %ld reply's after %ld.%06ld sec\n",msg*(am_procs()-1),end.tv_sec,end.tv_usec);
			f=end.tv_sec+(double)end.tv_usec/1000000;
			printf("av. time per mesg: %.9f\n",f/(double)(msg*(am_procs()-1)));
		}
		break;

	default:
		/* all other clusters just sleep and wait for messages */
		printf("cluster %d\n",am_my_proc());fflush(stdout);
		sprintf(buf,"This is the private buffer of process %2d     ",am_my_proc());
		while(1) sleep(10000);
	}
	am_disable();
	return (0);
}

