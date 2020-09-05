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
 * Interface to the statistic functions.
 *
 * Version 1.0 (released for 1.0.9) by Claudio Fleiner (fleiner@icsi.berkeley.edu)
 */
#ifndef _STAT_H_
#define _STAT_H_
#ifdef PSATHER_STAT
enum {
	IMPORT_CACHE_HIT,
	IMPORT_CACHE_MISS,
	READ_R,
	READ_VA,
	READ_VS1,
	READ_VS2,
	READ_VS4,
	READ_VS8,
	READ_V,
	PREFETCH_R,
	PREFETCH_VA,
	PREFETCH_VS1,
	PREFETCH_VS2,
	PREFETCH_VS4,
	PREFETCH_VS8,
	PREFETCH_V,
	PREFETCH_CACHE_HIT,
	PREFETCH_NO_WAIT,
	PREFETCH_WAIT,
	PREFETCH_SPECUL_NO_WAIT,
	PREFETCH_SPECUL_WAIT,
	WRITE_R,
	WRITE_VA,
	WRITE_VS1,
	WRITE_VS2,
	WRITE_VS4,
	WRITE_VS8,
	WRITE_V,
	POST_WRITE_R,
	POST_WRITE_VA,
	POST_WRITE_VS1,
	POST_WRITE_VS2,
	POST_WRITE_VS4,
	POST_WRITE_VS8,
	POST_WRITE_V,
	REMOTE_WRITE,
	REMOTE_TAG,
	EXPORTS_WAITING,
	THREAD_STARTED,
	FAST_LOCK,
	LOCK_STMT,
	REMOTE_EXEC,
	LAST 
};

void use_statistics(int); /* turn statistics on/off (on by default if -psather_stats is used) */
void disable_statistics();
void init_stat(void);
void stat_incr(int i);
void stat_add(int i,int j);
void stat_print(char *);
#define STAT(c) stat_incr(c)
#define STAT_ADD(c,i) stat_add(c,i)

#else

#define STAT(c)
#define STAT_ADD(c,i)
#define stat_print(s)

#endif

#endif
