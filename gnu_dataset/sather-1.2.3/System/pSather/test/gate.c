/*------------------------->  ANSI C - sourcefile  <-------------------------*/
/* Copyright (C) 1994 by International Computer Science Institute            */
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
#include <stddef.h>
#include <unistd.h>
#include <pSather.h>
#include <debug.h>

int printf(const char *,...);
int fflush(FILE *);
extern int debug_psather;

struct MT {
	MUTEX mt1,mt2,mt3;
	GATET values;
};

void consumer(FOB self,FOB arg,GATE gate,int pos)
{
	int i;
	FOB j;
	for(i=0;i<15;i++) {
		GATET_DEQUEUE(gate,j);
		printf("<%c>",(int)j.p);
		fflush(stdout);
	}
}

void m1(FOB self,FOB arg,GATE gate,int pos)
{
	volatile int i;
	char a;
	MUTEX mt1,mt2,mt3;
	GATET values;
	F_RATTR_NA(mt1,self,struct MT *,mt1);
	F_RATTR_NA(mt2,self,struct MT *,mt2);
	F_RATTR_NA(mt3,self,struct MT *,mt3);
	F_RATTR_NA(values,self,struct MT *,values);
	a=(char)(long)arg.p;
	EXCEPTION_PROLOGUE
		for(i=0;i<5;i++) {
			LOCK2(mt1,mt2)
				putchar(a);
				fflush(stdout);
				GATET_ENQUEUE(values,MAKEFAR((void *)(a-32)));
				sleep(1);
				putchar(a);
				fflush(stdout);
			ENDLOCK
		}
		GATE_SYNC(gate);
		PROTECT_BEGIN
			i=0;
			while(1) {
				if(i++==9) RAISE(FNULL);
				putchar(a-32);
				fflush(stdout);
				sleep(1);
			}
		PROTECT_WHEN
			printf("+%c+",a);
			fflush(stdout);
		PROTECT_END
	EXCEPTION_EPILOGUE;
}

void m2(FOB self,FOB arg,GATE gate,int pos)
{
	volatile int i;
	char a=(char)(long)arg.p;
	MUTEX mt1,mt2,mt3;
	GATET values;
	F_RATTR_NA(mt1,self,struct MT *,mt1);
	F_RATTR_NA(mt2,self,struct MT *,mt2);
	F_RATTR_NA(mt3,self,struct MT *,mt3);
	F_RATTR_NA(values,self,struct MT *,values);
	EXCEPTION_PROLOGUE
		for(i=0;i<5;i++) {
			LOCK2(mt2,mt3)
				putchar(a);
				fflush(stdout);
				GATET_ENQUEUE(values,MAKEFAR((void *)(a-32)));
				sleep(1);
				putchar(a);
				fflush(stdout);
			ENDLOCK
		}
		GATE_SYNC(gate);
		PROTECT_BEGIN
			i=0;
			while(1) {
				if(i++==7) RAISE(FNULL);
				putchar(a-32);
				fflush(stdout);
				sleep(1);
			}
		PROTECT_WHEN
			printf("+%c+",a);
			fflush(stdout);
		PROTECT_END
	EXCEPTION_EPILOGUE;
}
void m3(FOB self,FOB arg,GATE gate,int pos)
{
	volatile int i;
	char a=(char)(long)arg.p;
	MUTEX mt1,mt2,mt3;
	GATET values;
	F_RATTR_NA(mt1,self,struct MT *,mt1);
	F_RATTR_NA(mt2,self,struct MT *,mt2);
	F_RATTR_NA(mt3,self,struct MT *,mt3);
	F_RATTR_NA(values,self,struct MT *,values);
	EXCEPTION_PROLOGUE
		for(i=0;i<5;i++) {
			LOCK2(mt3,mt1);
				putchar(a);
				fflush(stdout);
				GATET_ENQUEUE(values,MAKEFAR((void *)(a-32)));
				sleep(1);
				putchar(a);
				fflush(stdout);
			ENDLOCK
		}
		GATE_SYNC(gate);
		PROTECT_BEGIN
			i=5;
			while(i-->0) {
				putchar(a-32);
				fflush(stdout);
				sleep(1);
			}
			RAISE(FNULL);
		PROTECT_WHEN
			printf("+%c+",a);
			fflush(stdout);
		PROTECT_END
	EXCEPTION_EPILOGUE;
}

void m4(FOB self,FOB arg,GATE gate,int pos)
{
	volatile int i;
	char a=(char)(long)arg.p;
	MUTEX mt1,mt2,mt3;
	GATET values;
	F_RATTR_NA(mt1,self,struct MT *,mt1);
	F_RATTR_NA(mt2,self,struct MT *,mt2);
	F_RATTR_NA(mt3,self,struct MT *,mt3);
	F_RATTR_NA(values,self,struct MT *,values);
	for(i=1;i<10;i++) {
		TRY1(mt2)
			putchar(a);
			fflush(stdout);
		TRYELSE
			putchar('*');
			fflush(stdout);
		TRYEND
		sleep(1);
	}
}
void par_func(FOB self,FOB arg,GATE gate,int pos)
{
	MUTEX mt1,mt2,mt3;
	struct MT *x;

	x=malloc(sizeof(*x));
	mt1=MUTEX_CREATE;
	mt2=MUTEX_CREATE;
	mt3=MUTEX_CREATE;
	x->mt1=mt1;
	x->mt2=mt2;
	x->mt3=mt3;
	x->values=arg;

	ATTACH(m1,MAKEFAR(x),MAKEFAR((void *)'a'),gate,(HERE+1)%CLUSTERS);
	ATTACH(m2,MAKEFAR(x),MAKEFAR((void *)'b'),gate,(HERE+2)%CLUSTERS);
	ATTACH(m3,MAKEFAR(x),MAKEFAR((void *)'c'),gate,(HERE+3)%CLUSTERS);
	ATTACH(m4,MAKEFAR(x),MAKEFAR((void *)'T'),gate,(HERE+4)%CLUSTERS);
}

int main(int argc,char *argv[])
{
	GATE par_gate;
	GATE values;
	
	if(argc>=2 && strcmp(argv[1],"-debug")==0) debug_psather=1;
	PSATHER_START(argc,argv);

	par_gate=GATE_CREATE;
	values=GATET_CREATE;
	
	ATTACH(consumer,FNULL,FNULL,values,HERE);
	ATTACH(par_func,FNULL,values,par_gate,HERE);

	LOCK1(GATE_NO_THREADS(par_gate))
	ENDLOCK

	PSATHER_STOP;
	putchar('\n');
	return 0;
}
