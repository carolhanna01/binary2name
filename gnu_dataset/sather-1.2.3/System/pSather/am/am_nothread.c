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
 * Some functions that are needed if thread support is not used. 
 *
 * Version 1.0 (released for Sather 1.0.9) by Claudio Fleiner (fleiner@icsi.berkeley.edu)
 */
#ifndef AM_THREADS
#include <stdio.h>
#include <stdlib.h>
#include <sys/time.h>

#include "am.h"
#include "am_int.h"

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
	struct timeval now;
	struct timeval end;

	gettimeofday(&end,NULL);
	end.tv_sec+=duration;

	while(1) {
		am_poll();
		gettimeofday(&now,NULL);
		if(cmptmv(now,end)>=0) break;
	}
}
void am_thread_poll(void)
{
	am_poll();
}

#endif /* THREADS */
