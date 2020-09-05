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
#include <sys/types.h>
#include <sys/time.h>
#include <sys/timeb.h>
#include "lwp.h"

#ifdef DO_TIMING
extern int printf(const char *,...);
extern int gettimeofday(struct timeval *,struct timezone *);

long time_elapsed[MAX_TIME_TYPE];

static long last_check=0;
static short current_type=TM_IGNORE;

long TM_now()
{
	static unsigned long start_of_prg=0;
	static unsigned long n;
	struct timeval tv;
	struct timezone tz;
	gettimeofday(&tv,&tz);
	n= (tv.tv_sec&0x3fff)*100000+tv.tv_usec/10;

	/***************************
	struct timeb now;
	ftime(&now);
	n= (now.time&0xFFFF)*1000+now.millitm; 
	****************************/

	if(start_of_prg==0) start_of_prg=n;
	return n-start_of_prg;
}

short TM_timing_type(int new_type)
{
	long act_time;
	short old;

	act_time=TM_now();
	if(current_type!=-1) time_elapsed[current_type]+=act_time-last_check;
	last_check=act_time;
	old=current_type;
	current_type=new_type;
	return old;
}

		
void TM_print()
{
	short lt;
	long total;
	long i;

	lt=TM_timing_type(TM_IGNORE);
	total=0;
	for(i=0;i<MAX_TIME_TYPE;i++) {
		if(time_elapsed[i]) printf("%3ld. %8ld ms\n",i,time_elapsed[i]);
		total+=time_elapsed[i];
	}
	printf("Tot: %8ld\n",total);
	TM_timing_type(lt);
}

#endif
