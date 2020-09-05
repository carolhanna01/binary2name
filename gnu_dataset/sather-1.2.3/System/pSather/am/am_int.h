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
 * Internal Shared Memory AM stuff
 *
 * Version 1.0 (released for Sather 1.0.9) by Claudio Fleiner (fleiner@icsi.berkeley.edu)
 */
#ifndef _AM_INT_H_
#define _AM_INT_H_

#if defined(sun) && defined(sparc)
#     include <errno.h>
#     ifdef ECHRNG
#       define SUNOS5
#     else
#       define SUNOS4
#     endif
#endif

#include "am.h"


#ifndef AM_THREADS
#include <malloc.h>

#define THREAD_PRIVATE_STUFF
#define T_INIT_CLUSTER
#define T_INIT_MACHINE
#define AM_INIT_THREAD
#define SIGNAL_PROC(proc)

#ifdef SUNOS5
	/* SOLARIS already has a lock_t type */
#define lock_t am_lock_t
#endif

typedef short *lock_t;
#define LCK_LOCK(a) 	do { assert(*a==0);*a=1; } while(0)
#define LCK_CREATE 	(lock_t)calloc(sizeof(short),1)
#define LCK_UNLOCK(a)  	*a=0

#else  /* THREADS */

#ifdef __linux__
#ifndef LWP
#define LWP
#endif
#endif

#ifdef SUNOS4
#ifndef LWP
#define LWP
#endif
#endif

#ifdef SUNOS5
#ifndef SOLARIS_THREADS
#define SOLARIS_THREADS
#endif
#ifndef _REENTRANT
#define _REENTRANT
#endif
#endif

#ifdef LWP
#include <lwp.h>

void lwp_check_for_signal(void);
#define USE_AM_TO_START_THREAD
#define AM_START_THREAD(msg)		assert(creatp(1,(void (*)())am_start_thread,200000,1,(char **)msg,NULL))
#define AM_START_THREAD_DCL(msg)	void am_start_thread(int argc,msg)
#define AM_INIT_THREAD			

#define THREAD_PRIVATE_STUFF
#define T_INIT_MACHINE initlp(1);
#define T_INIT_CLUSTER lwp_am_init();
#define SIGNAL_PROC(proc)
void lwp_am_init(void);

#endif

#ifdef SOLARIS_THREADS

#include <thread.h>
int pool_thr_create(void *,size_t stksize,void *(*)(void *),void *,long,void *);
#define USE_AM_TO_START_THREAD
#define AM_START_THREAD(msg)		assert(pool_thr_create(NULL,0,(void *(*)(void *))am_start_thread,(void *)msg,THR_DETACHED,NULL)==0)
#define AM_START_THREAD_DCL(msg)	void am_start_thread(msg)
#define AM_INIT_THREAD			do {\
						assert(thr_setspecific(am_signal_function_key,NULL)==0); \
						assert(thr_setspecific(am_signal_key,NULL)==0); \
					} while(0)
					
#define THREAD_PRIVATE_STUFF solaris_sema_t poll;
#define T_INIT_CLUSTER solaris_am_init();
#define T_INIT_MACHINE solaris_am_init_m();
#define SIGNAL_PROC(proc) solaris_am_signal(proc)
void solaris_am_init(void);
void solaris_am_init_m(void);
void solaris_am_signal(vnn_t proc);
#if defined(TCP) || defined(LANAI_NET) || defined(MEIKO)
int thread_may_poll();
#endif
#endif

#endif /* else THREADS */


#define LOWEST_BIT(a) ((a)&0xFFFF? \
                          ((a)&0xFF? \
			     ((a)&0xF? \
			         ((a)&0x03? ((a)&0x01?0:1):((a)&0x04?2:3)) \
				:((a)&0x30? ((a)&0x10?4:5):((a)&0x40?6:7)) \
			     ):((a)&0xF00? \
			         ((a)&0x0300? ((a)&0x0100? 8: 9):((a)&0x0400?10:11)) \
				:((a)&0x3000? ((a)&0x1000?12:13):((a)&0x4000?14:15)) \
			     ) \
			  ):((a)&0xFF0000? \
			     ((a)&0xF0000? \
			         ((a)&0x030000? ((a)&0x010000?16:17):((a)&0x040000?18:19)) \
				:((a)&0x300000? ((a)&0x100000?20:21):((a)&0x400000?22:23)) \
			     ):((a)&0xF000000? \
			         ((a)&0x03000000? ((a)&0x01000000?24:25):((a)&0x04000000?26:27)) \
				:((a)&0x30000000? ((a)&0x10000000?28:29):((a)&0x40000000?30:31)) \
			     ) \
			  ))

#define AM_MSG_PER_NODE 	30
#define AM_MAX_NODES		32
#define AM_ATOM_MSG_SIZE	 7

/*
 * ARG:  0: GET or PUT function
 *       1-5: 0-4 long args
 *       6:  2 long args, 1 double arg
 */
#define AM_ARG_MASK		0x07
#define AM_GET			0x08
#define AM_FIRST_PIECE		0x20
#define AM_LAST_PIECE		0x40
#define AM_LOCAL_MSG		0x80
#define AM_COMPLETE		AM_FIRST_PIECE|AM_LAST_PIECE
#define AM_IS_COMPLETE(a)	(((a)&(AM_LAST_PIECE|AM_FRIST_PIECE))==(AM_LAST_PIECE|AM_FIRST_PIECE))
#define AM_IS_PUT(a)		(!((a)&(AM_ARG_MASK|AM_GET)))
#define AM_IS_GET(a)		(((a)&(AM_ARG_MASK|AM_GET))==AM_GET)
#define AM_IS_ARGS(a)		((a)&AM_ARG_MASK)
#define AM_ARGS(a)		(((a)&AM_ARG_MASK)-1)
#define AM_HAS_DBL_ARG(a)	(((a)&AM_ARG_MASK))==AM_ARG_DBL)
#define AM_ARG0			0x01
#define AM_ARG1			0x02
#define AM_ARG2			0x03
#define AM_ARG3			0x04
#define AM_ARG4			0x05
#define AM_ARG_DBL		0x06

#ifdef USE_AM_TO_START_THREAD
#define AM_NEW_THREAD		0x10
#ifndef AM_INIT_THREAD
#define AM_INIT_THREAD
#endif
#ifndef AM_END_THREAD
#define AM_END_THREAD
#endif
#endif

#define AM_MAX_MSG_SIZE ((AM_MSG_PER_NODE/2-1)*(AM_ATOM_MSG_SIZE*sizeof(long)+2)+16)

typedef union AM_MSG_struct {
	struct {
		unsigned char flags;
		char next;
	} f;
	struct {
		unsigned char flags;
		char next;
		short bytes;
		void *rva;
		void *lva;
		void (*handler)();
		void *arg;
	} g;
	struct {
		unsigned char flags;
		char next;
		short bytes;
		void *rva;
		void (*handler)();
		void *arg;
		char c[16];
	} m;
	struct {
		unsigned char flags;
		char next;
		char c[AM_ATOM_MSG_SIZE*sizeof(long)+2];
	} c;
	struct {
		unsigned char flags;
		char next;
		void (*handler)();
		long a0,a1,a2,a3;
	} i;
	struct {
		unsigned char flags;
		char next;
		void (*handler)();
		long a0,a1;
		double a2;
	} d;
} AM_MSG;

typedef struct AM_WRITE_BUF {
	unsigned long r;
	unsigned long w;
	AM_MSG msg[AM_MSG_PER_NODE];
} AM_WRITE_BUF;

typedef struct AM_NODE_MSG_POOL_struct {
	THREAD_PRIVATE_STUFF /* defined in the thread library interface */
	char dead;
	AM_WRITE_BUF node[AM_MAX_NODES];
} AM_NODE_MSG_POOL;

typedef AM_NODE_MSG_POOL *AM_MSG_POOL;

/* note: sizeof(AM_NODE_MSG_POOL) <= 1MB on sun and solaris */

extern int am_clusters;
extern int am_my_cluster_id;
extern AM_MSG_POOL am_pool;

#define AM_REPLY_MASK 0x7FFF
#define AM_REQUEST_MASK 0x3FFF8000

void am_work_on_messages(unsigned long);
void am_stop_cluster(void);
int am_start_cluster(int no_of_clusters);

#ifdef AM_STAT
#ifdef TCP
#define AM_MAX_S 512
#endif
#ifdef LANAI
#define AM_MAX_S 2048
#endif
#ifndef AM_MAX_S
#define AM_MAX_S 512
#endif
extern long req_stat[AM_MAX_NODES][6];
extern long rep_stat[AM_MAX_NODES][6];
extern long thr_stat[AM_MAX_NODES][6];
extern long sto_stat[AM_MAX_NODES][AM_MAX_S+1];
extern long sto_async_stat[AM_MAX_NODES][AM_MAX_S+1];
extern long get_stat[AM_MAX_NODES][AM_MAX_S+1];
extern char *stat_dir;
void print_am_stat(void);
#define STAT(x)	((x)++)
#else
#define STAT(x)
#endif

#endif
