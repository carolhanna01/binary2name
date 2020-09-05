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
 * Code to read/write memory on other clusters.
 *
 * Version 1.0 (released for 1.0.9) by Claudio Fleiner (fleiner@icsi.berkeley.edu)
 */
#include <stdio.h>
#include <stdarg.h>
#include <stdlib.h>
#include <assert.h>
#include <memory.h>
#include <string.h>
#include "pSather.h"
#include "atomic.h"
#include "locks.h"
#include "local.h"
#include "trace.h"
#include "stat.h"
#include "memory.h"
#include "debug.h"

/*
 * WARNING:the code in this file assumes that sizeof(long)==sizeof(FOB)
 */

#if defined(PSATHER_TRACE) && defined(ATTR_TRACE)
#define TAI ,void *rr,char *type,char *attr,int index
#define TAIarg ,rr,type,attr,index
#define TAInull ,NULL,"","",0
static char *atch="NA";
static char *ppch="NP";
#else
#define TAI
#define TAIarg
#define TAInull
#endif

#define POLL_CACHE
#ifdef POLL_CACHE
#define CACHE_LOCK SPINLOCK_LOCK
#define CACHE_UNLOCK SPINLOCK_UNLOCK
#else
#error use spinlocks!
#define CACHE_LOCK BR_LOCK
#define CACHE_UNLOCK BR_UNLOCK
#endif

#ifdef IMPORT_CACHE
struct cache_struct {
#ifdef POLL_CACHE
	long lck;
#else
	BR_lock_t lck;
#endif
	void *adr;
	unsigned long import;
	unsigned long import2; /* dummy to align value correclty */
	char value[IMPORT_CACHE_TRESHOLD];
}

         ;
#define cache ((struct cache_struct *)(CLMEM[101]))
extern unsigned long import_ctr;
#endif

#define CACHE_WARNING IMPORT_CACHE_HASH(0xfc194961)

#ifdef IMPORT_CACHE
void init_cache() {
	int i;
	MALLOC(cache,sizeof(*cache)*IMPORT_CACHE_SIZE);
	for(i=0;i<IMPORT_CACHE_SIZE;i++) {
#ifndef POLL_CACHE
		cache[i].lck=BR_LOCK_CREATE();
#else
		cache[i].lck=0;
#endif
		cache[i].import=0;
		cache[i].adr=NULL;
	}
}
#endif /* IMPORT_CACHE */

typedef struct MULIT_PREFETCH_struct {
	volatile PREFETCH *a,*b;
} * MPRE;

#define VS8_ACCESS1(a) (((long *)(&(a)))[0])
#define VS8_ACCESS2(a) (((long *)(&(a)))[1])
#define VS8_JOIN(r,a,b) (VS8_ACCESS1(r)=(a)),(VS8_ACCESS2(r)=(b))


#define INCR_PREFETCH(p,p1) do { (*(p))++;if((p1)!=NULL) { LOCKV(p1);(*(p1))++;UNLOCKV(p1); } } while(0)
#define DECR_PREFETCH(p,p1) do { (*(p))--;if((p1)!=NULL) { LOCKV(p1);(*(p1))--;UNLOCKV(p1); } } while(0)


#if !defined(USE_ATTR_SPINLOCK) && !defined(POLLING)
static BR_lock_t lck_create_locked_lock() { BR_lock_t l=BR_LOCK_CREATE();BR_LOCK(l);return l; }
#undef TA_SEMAPHORE
#undef TA_SEMA_SIGNAL
#undef TA_SEMA_WAIT
#define TA_SEMAPHORE(a)		BR_lock_t a=lck_create_locked_lock()
#define TA_SEMA_SIGNAL(a) 	BR_UNLOCK(a)
#define TA_SEMA_WAIT(a)		do { BR_LOCK(a);BR_LOCK_DELETE(a); } while(0)
#define ta_sema_t		BR_lock_t
#define r_ta_sema_signal	BR_unlock_handler
#define r_ta_sema_signal_reply_mem	r_lck_unlock_reply_mem
#endif


/*
 * get some heap memory and copy a portion of the memory
 * on this cluster here. This is used to copy memory
 * to a safe position to guarantee atomicity.
 * copy_to_local is used for values that can be copied
 *               atomically, while
 * copyv_to_local is used for values that cannot be copied
 *                atomically. 
 * Both function are meant to be used through am_request
 * The pointer to the memory is returned, the memory has to 
 * be freed, it is not garbage collected
 */
/* These seem not to be needed:
static void copy_to_local_r(vnn_t r,void **p,void *rp)
{
	*p=rp;
}
static void copy_to_local(vnn_t r,void *value,int size,long rp)
{
	char *p;
	MALLOC(p,size);
	ATOMIC_MEMCPY(p,value,size);
	BR_REPLY_2(r,(BR_handler_2_t)copy_to_local_r,rp,(long)p);
}
static void copyv_to_local(vnn_t r,void *value,int size,long rp,int lck)
{
	char *p;
	MALLOC(p,size);
	if(lck) {
		LOCKV(value);
		memcpy(p,value,size);
		UNLOCKV(value);
	} else memcpy(p,value,size);
	BR_REPLY_2(r,(BR_handler_2_t)copy_to_local_r,rp,(long)p);
}
*/
static void free_local(vnn_t r, BR_word_t p) 
{
	FREE((void *)p);
}

/* Some functions to copy memory blocks from/to
 * some other clusters
 */
void get_memory(void *local,FOB remote,int size)
{
	BR_sema_t sem;
	int where;
	int j,max;

	where=WHERE(remote);
	max=BR_MAX_XFER();
	sem=BR_SEMA_CREATE(0);
	for(j=0;j<size-max;j+=max)
		BR_GET(where,((char *)SENDFOBHOME(remote))+j,((char *)local)+j,max,BR_signal_mem_handler,(BR_word_t)sem);
	BR_GET(where,((char *)SENDFOBHOME(remote))+j,((char *)local)+j,size%max,BR_signal_mem_handler,(BR_word_t)sem);
	for(j=0;j<size;j+=max) BR_WAIT(sem);
	BR_SEMA_DELETE(sem);
}	

void r_sema_signal_reply_mem(vnn_t from,caddr_t s,size_t size,BR_word_t sem)
{
	BR_REPLY_1(from,BR_signal_handler,sem);
}
void r_lck_unlock_reply_mem(vnn_t from,caddr_t s,size_t size,BR_word_t sem)
{
	BR_REPLY_1(from,BR_unlock_handler,sem);
}

/*
 * like get_memory, but 'where' is passed as argument
 */
void get_memory_from(void *local,int where,void *remote,int size)
{
	BR_sema_t sem;
	int j,max;

	max=BR_MAX_XFER();
	sem=BR_SEMA_CREATE(0);
	for(j=0;j<size-max;j+=max)
		BR_GET(where,((char *)remote)+j,((char *)local)+j,max,BR_signal_mem_handler,(BR_word_t)sem);
	BR_GET(where,((char *)remote)+j,((char *)local)+j,size%max,BR_signal_mem_handler,(BR_word_t)sem);
	for(j=0;j<size;j+=max) BR_WAIT(sem);
	BR_SEMA_DELETE(sem);
}	

void put_memory(FOB remote,void *local,int size)
{
	BR_sema_t sem;
	int where;
	int j,max;

	where=WHERE(remote);
	max=BR_MAX_XFER();
	sem=BR_SEMA_CREATE(0);
	for(j=0;j<size-max;j+=max)
		BR_STORE(where,((char *)local)+j,((char *)SENDFOBHOME(remote))+j,max,r_sema_signal_reply_mem,(BR_word_t)sem);
	BR_STORE(where,((char *)local)+j,((char *)SENDFOBHOME(remote))+j,size%max,r_sema_signal_reply_mem,(BR_word_t)sem);
	for(j=0;j<size;j+=max) BR_WAIT(sem);
	BR_SEMA_DELETE(sem);
}	
/*
 * like put_memory, but 'to' is passed as argument
 */
void put_memory_to(int to,void *remote,void *local,int size)
{
	BR_sema_t sem;
	int j,max;

	max=BR_MAX_XFER();
	sem=BR_SEMA_CREATE(0);
	for(j=0;j<size-max;j+=max)
		BR_STORE(to,((char *)local)+j,((char *)(remote))+j,max,r_sema_signal_reply_mem,(BR_word_t)sem);
	BR_STORE(to,((char *)local)+j,((char *)(remote))+j,size%max,r_sema_signal_reply_mem,(BR_word_t)sem);
	for(j=0;j<size;j+=max) BR_WAIT(sem);
	BR_SEMA_DELETE(sem);
}	


/****************************************************************************
 remote_read functions are used to read and writesome memory from a remote cluster
 to the local cluster. Write functions end with _write, and read functions end
 with _read.
 There are functions to read/write four different data types:
 reference types:          the name includes a _r_. Useful to read pointers. Those pointers
                           will be adjusted for the local cluster
 atomic value types:       the name includes a _va_. Used to read value types that are 
		           atomic in the sense that the hardware supports atomic assignment 
		           for those types. Such types may not have a reference object inside.
 short atomic value types: Should not be called directly, the runtime will automatically use 
			   this routine when calling a _va_ routine for short types. 
			   Has a _vs_ in its name.
 value types:		   Used for all other value types. Includes a _v_ in its name.
			   Does change reference values contained in the object.
 There are several versions of each read function:
 caching on:		   uses cache_ as prefix
 prefetching:		   uses pre_ as prefex
 There are several version of the write function:
 post writing on:	   uses post_ as prefix
****************************************************************************/



/***
 *** Read Reference Value from remote cluster
 ***/


/*
 * Standard reading of reference value
 */
	static void reply_remote_r_read(vnn_t from,FOB *local,FOB remote,ta_sema_t a)
	{
		*local=remote;
		TA_SEMA_SIGNAL(a);
	}
	static void request_remote_r_read(vnn_t from,void *local,FOB *remote,long ta)
	{
		BR_REPLY_3(from,(BR_handler_3_t)reply_remote_r_read,(long)local,(long)SENDFOB(*remote,from),(long)ta);
	}
void remote_r_read(int cluster,FOB *local,FOB* remote TAI)
{
	TA_SEMAPHORE(a);
#if defined(ATTR_TRACE)
	if(attr!=NULL) ATTR_TR4("F_R_RATTR_AA %02d-(%s)%x->%s",cluster,type,rr,attr);
	else ATTR_TR4("F_R_RATTR_AA %02d-(%s)%x->[%d]",cluster,type,rr,index);
#endif
	STAT(READ_R);
	BR_REQUEST_3(cluster,(BR_handler_3_t)request_remote_r_read,(long)local,(long)remote,(long)a);
	TA_SEMA_WAIT(a);
}




/*
 * reading of reference value  with prefetching
 */
	static void reply_pre_remote_r_read(vnn_t from,FOB *local,FOB remote,volatile PREFETCH *a,volatile PREFETCH *b)
	{
		*local=remote;
		DECR_PREFETCH(a,b);
	}
	static void request_pre_remote_r_read(vnn_t from,void *local,FOB *remote,long p,long p1)
	{
		BR_REPLY_4(from,(BR_handler_4_t)reply_pre_remote_r_read,(long)local,(long)SENDFOB(*remote,from),(long)p,(long)p1);
	}
void pre_remote_r_read(volatile PREFETCH *p,volatile PREFETCH *p1,int cluster,FOB *local,FOB* remote TAI)
{ 
#if defined(ATTR_TRACE)
	if(attr!=NULL) ATTR_TR4("PRE_F_R_RATTR_AA %02d-(%s)%x->%s",cluster,type,rr,attr);
	else ATTR_TR4("PRE_F_R_RATTR_AA %02d-(%s)%x->[%d]",cluster,type,rr,index);
#endif
	INCR_PREFETCH(p,p1);
	STAT(PREFETCH_R);
	STAT(READ_R);
	BR_REQUEST_4(cluster,(BR_handler_4_t)request_pre_remote_r_read,(long)local,(long)remote,(long)p,(long)p1);
}




/*
 * Reading reference value with cache on
 */
#ifdef IMPORT_CACHE
void cache_r_read(int cluster,FOB *local,FOB* remote TAI)
{
	/* TA_SEMAPHORE(a); */
#if defined(LARGEST_ATOMIC)  /* the cache acn only operate on reference values if each slot is large enough */
# if LARGEST_ATOMIC>IMPORT_CACHE_TRESHOLD
	if(sizeof(FOB)<=IMPORT_CACHE_TRESHOLD) {
# endif
#endif
		int im=((LOCAL_MEM)(BR_GET_THREAD_LOCAL()))->import;
		int ce=IMPORT_CACHE_HASH(cluster,remote);
		CACHE_LOCK(cache[ce].lck);
		if(cache[ce].adr!=remote || cache[ce].import<im) {
			cache[ce].import=import_ctr;
			cache[ce].adr=remote;
			remote_r_read(cluster,(FOB *)cache[ce].value,remote TAIarg);
			STAT(IMPORT_CACHE_MISS);
		} else {
			STAT(IMPORT_CACHE_HIT);
#if defined(ATTR_TRACE)
			if(attr!=NULL) ATTR_TR4("CACHED F_R_RATTR_AA %02d-(%s)%x->%s",cluster,type,rr,attr);
			else ATTR_TR4("CACHED F_R_RATTR_AA %02d-(%s)%x->[%d]",cluster,type,rr,index);
#endif
		}
		*local= *(FOB *)cache[ce].value;
		CACHE_UNLOCK(cache[ce].lck);
#if defined(LARGEST_ATOMIC) 
# if LARGEST_ATOMIC>IMPORT_CACHE_TRESHOLD
	} else { /* we cannot use the cache */
		remote_r_read(cluster,local,remote TAIarg);
	}
# endif
#endif
}
#endif /* IMPORT_CACHE */



/*
 * reference reading with prefetching and cache on
 */
#ifdef IMPORT_CACHE
	struct PREFETCH_IMPORT_struct {
		volatile PREFETCH *p,*p1;
		FOB *local,*remote;
		int cluster;
	};
	static thr_pre_cache_r_read(int rc,struct PREFETCH_IMPORT_struct *p) 
	{
		int ce=IMPORT_CACHE_HASH(p->cluster,p->remote);
		cache[ce].import=import_ctr;
		cache[ce].adr=p->remote;
		remote_r_read(p->cluster,(FOB *)cache[ce].value,p->remote TAInull);
		STAT(IMPORT_CACHE_MISS);
		*p->local= *(FOB *)cache[ce].value;
		CACHE_UNLOCK(cache[ce].lck);
		DECR_PREFETCH(p->p,p->p1);
		FREE(p);
	}
void pre_cache_r_read(volatile PREFETCH *p,volatile PREFETCH *p1,int cluster,FOB *local,FOB* remote TAI)
{
#if defined(LARGEST_ATOMIC)  /* the cache acn only operate on reference values if each slot is large enough */
# if LARGEST_ATOMIC>IMPORT_CACHE_TRESHOLD
	if(sizeof(FOB)<=IMPORT_CACHE_TRESHOLD) {
# endif
#endif
		int im=((LOCAL_MEM)(BR_GET_THREAD_LOCAL()))->import;
		int ce=IMPORT_CACHE_HASH(cluster,remote);
		STAT(PREFETCH_R);
		CACHE_LOCK(cache[ce].lck);
		if(cache[ce].adr!=remote || cache[ce].import<im) {
			struct PREFETCH_IMPORT_struct *pi;
			MALLOC(pi,sizeof(*pi));
			pi->cluster=cluster;
			pi->remote=remote;
			pi->local=local;
			pi->p1=p1;
			pi->p=p;
			INCR_PREFETCH(p,p1);
			BR_FORK_1(HERE,(BR_handler_1_t)thr_pre_cache_r_read,(BR_word_t)pi);
		} else { /* no prefetching done */
			STAT(IMPORT_CACHE_HIT);
			STAT(PREFETCH_CACHE_HIT);
#if defined(ATTR_TRACE)
			if(attr!=NULL) ATTR_TR4("CACHED F_R_RATTR_AA %02d-(%s)%x->%s",cluster,type,rr,attr);
			else ATTR_TR4("CACHED F_R_RATTR_AA %02d-(%s)%x->[%d]",cluster,type,rr,index);
#endif
			*local= *(FOB *)cache[ce].value;
			CACHE_UNLOCK(cache[ce].lck);
		}
#if defined(LARGEST_ATOMIC) 
# if LARGEST_ATOMIC>IMPORT_CACHE_TRESHOLD
	} else { /* we cannot use the cache */
		pre_remote_r_read(p,p1,cluster,local,remote TAIarg);
	}
# endif
#endif
}
#endif /* IMPORT_CACHE */






/***
 *** Read Short Atomic Value from remote cluster, comes in four versions, 
 *** for 1, 2, 4 and 8 Bytes
 ***/


/*
 * Standard reading of short atomic value
 */
	static void reply_remote_vs1_read(vnn_t from,char *local,long remote,ta_sema_t a)
	{
		*local=remote;
		TA_SEMA_SIGNAL(a);
	}
	static void reply_remote_vs2_read(vnn_t from,short *local,long remote,ta_sema_t a)
	{
		*local=remote;
		TA_SEMA_SIGNAL(a);
	}
	static void reply_remote_vs4_read(vnn_t from,long *local,long remote,ta_sema_t a)
	{
		*local=remote;
		TA_SEMA_SIGNAL(a);
	}
	static void reply_remote_vs8_read(vnn_t from,double *local,long remote,long remote2,ta_sema_t a)
	{
		double d;
		VS8_JOIN(d,remote,remote2);
		*local=d;
		TA_SEMA_SIGNAL(a);
	}
	static void request_remote_vs1_read(vnn_t from,void *local,char *remote,long ta)
	{
		BR_REPLY_3(from,(BR_handler_3_t)reply_remote_vs1_read,(long)local,*remote,(long)ta);
	}
	static void request_remote_vs2_read(vnn_t from,void *local,short *remote,long ta)
	{
		BR_REPLY_3(from,(BR_handler_3_t)reply_remote_vs2_read,(long)local,*remote,(long)ta);
	}
	static void request_remote_vs4_read(vnn_t from,void *local,long *remote,long ta)
	{
		BR_REPLY_3(from,(BR_handler_3_t)reply_remote_vs4_read,(long)local,*remote,(long)ta);
	}
	static void request_remote_vs8_read(vnn_t from,long local,double *remote,long ta)
	{
		BR_REPLY_4(from,(BR_handler_4_t)reply_remote_vs8_read,(long)local,VS8_ACCESS1(*remote),VS8_ACCESS2(*remote),(long)ta);
	}
static void remote_vs1_read(int cluster,FOB *local,FOB* remote TAI)
{
	TA_SEMAPHORE(a);
	STAT(READ_VS1);
	BR_REQUEST_3(cluster,(BR_handler_3_t)request_remote_vs1_read,(long)local,(long)remote,(long)a);
	TA_SEMA_WAIT(a);
}
static void remote_vs2_read(int cluster,FOB *local,FOB* remote TAI)
{
	TA_SEMAPHORE(a);
	STAT(READ_VS2);
	BR_REQUEST_3(cluster,(BR_handler_3_t)request_remote_vs2_read,(long)local,(long)remote,(long)a);
	TA_SEMA_WAIT(a);
}
static void remote_vs4_read(int cluster,FOB *local,FOB* remote TAI)
{
	TA_SEMAPHORE(a);
	STAT(READ_VS4);
	assert((long)remote>100);
	BR_REQUEST_3(cluster,(BR_handler_3_t)request_remote_vs4_read,(long)local,(long)remote,(long)a);
	TA_SEMA_WAIT(a);
}
static void remote_vs8_read(int cluster,FOB *local,FOB* remote TAI)
{
	TA_SEMAPHORE(a);
	STAT(READ_VS8);
	*(double*)local=1.234; /* assert alignment */
	BR_REQUEST_3(cluster,(BR_handler_3_t)request_remote_vs8_read,(long)local,(long)remote,(long)a);
	TA_SEMA_WAIT(a);
}


/*
 * reading of short value with prefetching
 */
	static void reply_pre_remote_vs1_read(vnn_t from,char *local,long remote,volatile PREFETCH *a,volatile PREFETCH *b)
	{
		*local=remote;
		DECR_PREFETCH(a,b);
	}
	static void reply_pre_remote_vs2_read(vnn_t from,short *local,long remote,volatile PREFETCH *a,volatile PREFETCH *b)
	{
		*local=remote;
		DECR_PREFETCH(a,b);
	}
	static void reply_pre_remote_vs4_read(vnn_t from,long *local,long remote,volatile PREFETCH *a,volatile PREFETCH *b)
	{
		*local=remote;
		DECR_PREFETCH(a,b);
	}
	static void reply_pre_remote_vs8_read(vnn_t from,double *local,long remote,long remote2,MPRE p)
	{
		double a;
		VS8_JOIN(a,remote,remote2);
		*local=a;
		DECR_PREFETCH(p->a,p->b);
		FREE(p);
	}
	static void request_pre_remote_vs1_read(vnn_t from,void *local,char *remote,long p,long p1)
	{
		BR_REPLY_4(from,(BR_handler_4_t)reply_pre_remote_vs1_read,(long)local,(long)(*remote),(long)p,(long)p1);
	}
	static void request_pre_remote_vs2_read(vnn_t from,void *local,short *remote,long p,long p1)
	{
		BR_REPLY_4(from,(BR_handler_4_t)reply_pre_remote_vs2_read,(long)local,(long)(*remote),(long)p,(long)p1);
	}
	static void request_pre_remote_vs4_read(vnn_t from,void *local,long *remote,long p,long p1)
	{
		BR_REPLY_4(from,(BR_handler_4_t)reply_pre_remote_vs4_read,(long)local,(long)(*remote),(long)p,(long)p1);
	}
	static void request_pre_remote_vs8_read(vnn_t from,void *local,double *remote,long p)
	{
		BR_REPLY_4(from,(BR_handler_4_t)reply_pre_remote_vs8_read,(long)local,VS8_ACCESS1(*remote),VS8_ACCESS2(*remote),p);
	}
static void pre_remote_vs1_read(volatile PREFETCH *p,volatile PREFETCH *p1,int cluster,char *local,char *remote TAI)
{ 
	INCR_PREFETCH(p,p1);
	STAT(PREFETCH_VS1);
	STAT(READ_VS1);
	BR_REQUEST_4(cluster,(BR_handler_4_t)request_pre_remote_vs1_read,(long)local,(long)remote,(long)p,(long)p1);
}
static void pre_remote_vs2_read(volatile PREFETCH *p,volatile PREFETCH *p1,int cluster,char *local,char *remote TAI)
{ 
	INCR_PREFETCH(p,p1);
	STAT(PREFETCH_VS2);
	STAT(READ_VS2);
	BR_REQUEST_4(cluster,(BR_handler_4_t)request_pre_remote_vs2_read,(long)local,(long)remote,(long)p,(long)p1);
}
static void pre_remote_vs4_read(volatile PREFETCH *p,volatile PREFETCH *p1,int cluster,char *local,char *remote TAI)
{ 
	INCR_PREFETCH(p,p1);
	STAT(PREFETCH_VS4);
	STAT(READ_VS4);
	BR_REQUEST_4(cluster,(BR_handler_4_t)request_pre_remote_vs4_read,(long)local,(long)remote,(long)p,(long)p1);
}
static void pre_remote_vs8_read(volatile PREFETCH *p,volatile PREFETCH *p1,int cluster,char *local,char *remote TAI)
{ 
	MPRE pp;
	INCR_PREFETCH(p,p1);
	STAT(PREFETCH_VS8);
	STAT(READ_VS8);
	*(double*)local=0.0; /* assert alignment */
	MALLOC(p,sizeof(*p));
	pp->a=p;
	pp->b=p1;
	BR_REQUEST_3(cluster,(BR_handler_3_t)request_pre_remote_vs8_read,(long)local,(long)remote,(long)pp);
}



/*
 * Reading short value with cache on
 */
#ifdef IMPORT_CACHE
static void cache_vs_read(int cluster,FOB *local,FOB* remote,int size TAI)
{
	/* TA_SEMAPHORE(a); */
#if defined(LARGEST_ATOMIC)  /* the cache acn only operate on values if each slot is large enough */
# if LARGEST_ATOMIC>IMPORT_CACHE_TRESHOLD
	if(size<=IMPORT_CACHE_TRESHOLD) {
# endif
#endif
		int im=((LOCAL_MEM)(BR_GET_THREAD_LOCAL()))->import;
		int ce=IMPORT_CACHE_HASH(cluster,remote);
		CACHE_LOCK(cache[ce].lck);
		if(cache[ce].adr!=remote || cache[ce].import<im) {
			cache[ce].import=import_ctr;
			cache[ce].adr=remote;
			switch(size) {
			case 1: remote_vs1_read(cluster,(FOB *)cache[ce].value,remote TAIarg);break;
			case 2: remote_vs2_read(cluster,(FOB *)cache[ce].value,remote TAIarg);break;
			case 4: remote_vs4_read(cluster,(FOB *)cache[ce].value,remote TAIarg);break;
			case 8: remote_vs8_read(cluster,(FOB *)cache[ce].value,remote TAIarg);break;
			}
			/*
			if(ce==CACHE_WARNING && *(long *)cache[ce].value==0) {
				printf("got it 0x%p\n",remote);
			}
			*/
			STAT(IMPORT_CACHE_MISS);
		} else {
			STAT(IMPORT_CACHE_HIT);
#if defined(ATTR_TRACE)
			if(attr!=NULL) ATTR_TR4("CACHED F_VA_RATTR_AA %02d-(%s)%x->%s",cluster,type,rr,attr);
			else ATTR_TR4("CACHED F_VA_RATTR_AA %02d-(%s)%x->[%d]",cluster,type,rr,index);
#endif
		}
		ATOMIC_MEMCPY(local,cache[ce].value,size);
		CACHE_UNLOCK(cache[ce].lck);
#if defined(LARGEST_ATOMIC) 
# if LARGEST_ATOMIC>IMPORT_CACHE_TRESHOLD
	} else { /* we cannot use the cache */
		switch(size) {
		case 1: remote_vs1_read(cluster,local,remote TAIarg);break;
		case 2: remote_vs2_read(cluster,local,remote TAIarg);break;
		case 4: remote_vs4_read(cluster,local,remote TAIarg);break;
		case 8: remote_vs8_read(cluster,local,remote TAIarg);break;
		}
	}
# endif
#endif
}
#endif /* IMPORT_CACHE */



/*
 * short value reading with prefetching and cache on
 */
#ifdef IMPORT_CACHE
	struct PREFETCH_IMPORT_VS_struct {
		volatile PREFETCH *p,*p1;
		void *local,*remote;
		int size;
		int cluster;
	};
	static thr_pre_cache_vs_read(int rc,struct PREFETCH_IMPORT_VS_struct *p) 
	{
		int ce=IMPORT_CACHE_HASH(p->cluster,p->remote);
		cache[ce].import=import_ctr;
		cache[ce].adr=p->remote;
		switch(p->size) {
		case 1: remote_vs1_read(p->cluster,(FOB *)cache[ce].value,p->remote TAInull);break;
		case 2: remote_vs2_read(p->cluster,(FOB *)cache[ce].value,p->remote TAInull);break;
		case 4: remote_vs4_read(p->cluster,(FOB *)cache[ce].value,p->remote TAInull);break;
		case 8: remote_vs8_read(p->cluster,(FOB *)cache[ce].value,p->remote TAInull);break;
		}
		STAT(IMPORT_CACHE_MISS);
		ATOMIC_MEMCPY(p->local,cache[ce].value,p->size);
		CACHE_UNLOCK(cache[ce].lck);
		DECR_PREFETCH(p->p,p->p1);
		FREE(p);
	}
void pre_cache_vs_read(volatile PREFETCH *p,volatile PREFETCH *p1,int cluster,FOB *local,FOB* remote,int size TAI)
{
#if defined(LARGEST_ATOMIC)  /* the cache acn only operate on reference values if each slot is large enough */
# if LARGEST_ATOMIC>IMPORT_CACHE_TRESHOLD
	if(size<=IMPORT_CACHE_TRESHOLD) {
# endif
#endif
		int im=((LOCAL_MEM)(BR_GET_THREAD_LOCAL()))->import;
		int ce=IMPORT_CACHE_HASH(cluster,remote);
		STAT(PREFETCH_R);
		CACHE_LOCK(cache[ce].lck);
		if(cache[ce].adr!=remote || cache[ce].import<im) {
			struct PREFETCH_IMPORT_VS_struct *pi;
			MALLOC(pi,sizeof(*pi));
			pi->cluster=cluster;
			pi->size=size;
			pi->remote=remote;
			pi->local=local;
			pi->p1=p1;
			pi->p=p;
			INCR_PREFETCH(p,p1);
			BR_FORK_1(HERE,(BR_handler_1_t)thr_pre_cache_vs_read,(BR_word_t)pi);
		} else { /* no prefetching done */
			STAT(IMPORT_CACHE_HIT);
			STAT(PREFETCH_CACHE_HIT);
#if defined(ATTR_TRACE)
			if(attr!=NULL) ATTR_TR4("CACHED F_R_RATTR_AA %02d-(%s)%x->%s",cluster,type,rr,attr);
			else ATTR_TR4("CACHED F_R_RATTR_AA %02d-(%s)%x->[%d]",cluster,type,rr,index);
#endif
			ATOMIC_MEMCPY(local,cache[ce].value,size);
			CACHE_UNLOCK(cache[ce].lck);
		}
#if defined(LARGEST_ATOMIC) 
# if LARGEST_ATOMIC>IMPORT_CACHE_TRESHOLD
	} else { /* we cannot use the cache */
		switch(size) {
		case 1: pre_remote_vs1_read(p,p1,cluster,local,remote TAIarg); break;
		case 2: pre_remote_vs2_read(p,p1,cluster,local,remote TAIarg); break;
		case 4: pre_remote_vs4_read(p,p1,cluster,local,remote TAIarg); break;
		case 8: pre_remote_vs8_read(p,p1,cluster,local,remote TAIarg); break;
		}
	}
# endif
#endif
}
#endif /* IMPORT_CACHE */






/***
 *** Read atomic Value from remote cluster
 ***/


/*
 * the standard read procedure
 */
	static void request_thr_copy_va(int where,void *l,int size,ta_sema_t ta)
	{ TA_SEMA_SIGNAL(ta); }
	static void thr_copy_va(int where,void *l,int size,void *remote,void *ta)
	{
		void *r;
#ifdef USE_ALLOCA
		r=alloca(size);
#else
		char mem[ATOMIC_READ_MEM];
		r=mem;
		if(size>=ATOMIC_READ_MEM) { MALLOC(r,size); }
#endif
		ATOMIC_MEMCPY(r,remote,size);
		if(size<=BR_MAX_XFER()) 
			BR_STORE(where,r,l,size,(BR_handler_mem_t)request_thr_copy_va,(BR_word_t)ta);
		else {
			put_memory_to(where,l,r,size);
			BR_REQUEST_1(where,r_ta_sema_signal,(BR_word_t)ta);
		}
#ifndef USE_ALLOCA
		if(size>=ATOMIC_READ_MEM) { FREE(r); }
#endif
	}

void remote_va_read(int cluster,void *local,int atm_l,void* remote,int atm_r,int size TAI)
{
#if defined(ATTR_TRACE)
	if(attr!=NULL) ATTR_TR6("F_VA_RATTR_%c%c %02d-(%s)%x->%s",atch[atm_l],atch[atm_r],cluster,type,rr,attr);
	else ATTR_TR6("F_VA_RATTR_%c%c %02d-(%s)%x->[%d]",atch[atm_l],atch[atm_r],cluster,type,rr,index);
#endif
	switch(size) {
	case 1: remote_vs1_read(cluster,local,remote TAIarg);break;
	case 2: remote_vs2_read(cluster,local,remote TAIarg);break;
	case 4: remote_vs4_read(cluster,local,remote TAIarg);break;
	case 8: remote_vs8_read(cluster,local,remote TAIarg);break;
	default: {
		void *l;

#ifdef USE_ALLOCA
		if(atm_l) l=alloca(size);
#else
		char mem[ATOMIC_READ_MEM];
		l=mem;
		if(size>=ATOMIC_READ_MEM && atm_l) { MALLOC(l,size); }
#endif
		if(!atm_l) l=local;

		/* the easiest way would be to make an BR_GET, but
		   this does not work as BR_GET is not atomic. So we
		   need to copy the value to a safe position before
		   copying it over. */
		/* we cannot just use an am_request wich executes
		   an BR_STORE on the remote cluster (AM does not
		   allow this */
		STAT(READ_VA);
		if(atm_r) {
			TA_SEMAPHORE(ta);
			BR_FORK_4(cluster,(BR_handler_4_t)thr_copy_va,(long)l,(long)size,(long)remote,(long)ta);
			TA_SEMA_WAIT(ta);
		} else 
			get_memory_from(local,cluster,remote,size);

		if(atm_l) ATOMIC_MEMCPY(local,l,size);
#ifndef USE_ALLOCA
		if(size>=ATOMIC_READ_MEM && atm_l) { FREE(l); }
#endif /* USE_ALLOCA */
	} }
}

/*
 * remote read of va with prefetching
 */
	struct PRE_REMOTE_VA_READ_struct {
		volatile PREFETCH *p,*p1;
		int cluster;
		void *local;
		int atm_l;
		void *remote;
		int atm_r;
		int size;
	};
	void thr_pre_remote_va_read(int cluster,struct PRE_REMOTE_VA_READ_struct *p) {
		STAT(PREFETCH_VA);
		remote_va_read(p->cluster,p->local,p->atm_l,p->remote,p->atm_r,p->size TAInull);
		DECR_PREFETCH(p->p,p->p1);
		FREE(p);
	}
void pre_remote_va_read(volatile PREFETCH *p,volatile PREFETCH *p1,int cluster,void *local,int atm_l,void* remote,int atm_r,int size TAI)
{
#if defined(ATTR_TRACE)
	/* other sizes are handled in the thread created here */
	if(size==1||size==2||size==4||size==8) {
		if(attr!=NULL) ATTR_TR6("F_VA_RATTR_%c%c %02d-(%s)%x->%s",atch[atm_l],atch[atm_r],cluster,type,rr,attr);
		else ATTR_TR6("F_VA_RATTR_%c%c %02d-(%s)%x->[%d]",atch[atm_l],atch[atm_r],cluster,type,rr,index);
	}
#endif
	switch(size) {
	case 1: pre_remote_vs1_read(p,p1,cluster,local,remote TAIarg);break;
	case 2: pre_remote_vs2_read(p,p1,cluster,local,remote TAIarg);break;
	case 4: pre_remote_vs4_read(p,p1,cluster,local,remote TAIarg);break;
	case 8: pre_remote_vs8_read(p,p1,cluster,local,remote TAIarg);break;
	default: {
	/* we use the easy way: create a thread to handle the read */
		struct PRE_REMOTE_VA_READ_struct *pi;
		INCR_PREFETCH(p,p1);
		MALLOC(pi,sizeof(*pi));
		pi->p=p;pi->p1=p1;
		pi->cluster=cluster;
		pi->local=local;pi->atm_l=atm_l;
		pi->remote=remote;pi->atm_r=atm_r;
		pi->size=size;
		BR_FORK_1(HERE,(BR_handler_1_t)thr_pre_remote_va_read,(BR_word_t)pi);
	} }
}

#ifdef IMPORT_CACHE
void cache_va_read(int cluster,void *local,int atm_l,void* remote,int atm_r,int size TAI)
{
	if(size==1||size==2||size==4||size==8) 
		cache_vs_read(cluster,local,remote,size TAIarg);
	else {
#if defined(LARGEST_ATOMIC) 
# if LARGEST_ATOMIC>IMPORT_CACHE_TRESHOLD
		if(size<=IMPORT_CACHE_TRESHOLD) {
# endif
#endif
			int im=((LOCAL_MEM)(BR_GET_THREAD_LOCAL()))->import;
			int ce=IMPORT_CACHE_HASH(cluster,remote);
			CACHE_LOCK(cache[ce].lck);
			if(cache[ce].adr!=remote || cache[ce].import<im) {
				/* DEBUG0("CACHE miss"); */
				remote_va_read(cluster,cache[ce].value,0,remote,atm_r,size TAIarg);
				cache[ce].import=im;
				cache[ce].adr=remote;
				STAT(IMPORT_CACHE_MISS);
			} else {
				STAT(IMPORT_CACHE_HIT);
#if defined(ATTR_TRACE)
				if(attr!=NULL) ATTR_TR6("CACHED F_RATTR_%c%c %02d-(%s)%x->%s",atch[atm_l],atch[atm_r],cluster,type,rr,attr);
				else ATTR_TR6("CACHED F_RATTR_%c%c %02d-(%s)%x->[%d]",atch[atm_l],atch[atm_r],cluster,type,rr,index);
#endif
			}
			if(atm_l) ATOMIC_MEMCPY(local,cache[ce].value,size);
			else memcpy(local,cache[ce].value,size);
			CACHE_UNLOCK(cache[ce].lck);
#if defined(LARGEST_ATOMIC) 
# if LARGEST_ATOMIC>IMPORT_CACHE_TRESHOLD
		} else { /* cannot use the cache */
			remote_va_read(cluster,local,atm_l,remote,atm_r,size TAIarg);
		}
# endif
#endif
	}
}

/*
 * prefetch atomic value type and use the cache
 */
	struct PREFETCH_IMPORT_VA_struct {
		volatile PREFETCH *p,*p1;
		void *remote,*local;
		int atm_l,atm_r;
		int cluster,size;
	};
	thr_pre_cache_va_read(int where,struct PREFETCH_IMPORT_VA_struct *p)
	{
		int ce=IMPORT_CACHE_HASH(p->cluster,p->remote);
		cache[ce].import=import_ctr;
		remote_va_read(p->cluster,cache[ce].value,0,p->remote,p->atm_r,p->size TAInull);
		cache[ce].adr=p->remote;
		STAT(IMPORT_CACHE_MISS);
		if(p->atm_l) ATOMIC_MEMCPY(p->local,cache[ce].value,p->size);
		else memcpy(p->local,cache[ce].value,p->size);
		CACHE_UNLOCK(cache[ce].lck);
		DECR_PREFETCH(p->p,p->p1);
		FREE(p);
	}
void pre_cache_va_read(volatile PREFETCH *p,volatile PREFETCH *p1,int cluster,void *local,int atm_l,void* remote,int atm_r,int size TAI)
{ 
	if(size==1||size==2||size==4||size==8) {
		pre_cache_vs_read(p,p1,cluster,local,remote,size TAIarg);
	} else {
#if defined(LARGEST_ATOMIC) 
# if LARGEST_ATOMIC>IMPORT_CACHE_TRESHOLD
		if(size<=IMPORT_CACHE_TRESHOLD) {
# endif
#endif
			int im=((LOCAL_MEM)(BR_GET_THREAD_LOCAL()))->import;
			int ce=IMPORT_CACHE_HASH(cluster,remote);
			STAT(PREFETCH_VA);
			CACHE_LOCK(cache[ce].lck);
			if(cache[ce].adr!=remote || cache[ce].import<im) {
				struct PREFETCH_IMPORT_VA_struct *pi;
				INCR_PREFETCH(p,p1);
				MALLOC(pi,sizeof(*pi));
				pi->p=p;pi->p1=p1;
				pi->remote=remote;pi->local=local;
				pi->atm_l=atm_l;pi->atm_r=atm_r;
				pi->cluster=cluster;pi->size=size;
				BR_FORK_1(HERE,(BR_handler_1_t)thr_pre_cache_va_read,(BR_word_t)pi);
			} else {
				STAT(PREFETCH_CACHE_HIT);
				STAT(IMPORT_CACHE_HIT);
#if defined(ATTR_TRACE)
				if(attr!=NULL) ATTR_TR6("CACHED F_RATTR_%c%c %02d-(%s)%x->%s",atch[atm_l],atch[atm_r],cluster,type,rr,attr);
				else ATTR_TR6("CACHED F_RATTR_%c%c %02d-(%s)%x->[%d]",atch[atm_l],atch[atm_r],cluster,type,rr,index);
#endif
				if(atm_l) ATOMIC_MEMCPY(local,cache[ce].value,size);
				else memcpy(local,cache[ce].value,size);
				CACHE_UNLOCK(cache[ce].lck);
			}
#if defined(LARGEST_ATOMIC) 
# if LARGEST_ATOMIC>IMPORT_CACHE_TRESHOLD
		} else { /* cannot use the cache */
			pre_remote_va_read(p,p1,cluster,local,atm_l,remote,atm_r,size TAIarg);
		}
# endif
#endif
	}
}
#endif /* IMPORT_CACHE */




/***
 *** Read Value from remote cluster
 ***/


/*
 * the standard read procedure
 */
	static void thr_copy_v(int where,void *l,int size,void *remote,void *ta)
	{
		void *r;
#ifdef USE_ALLOCA
		r=alloca(size);
#else
		char mem[ATOMIC_READ_MEM];
		r=mem;
		if(size>=ATOMIC_READ_MEM) { MALLOC(r,size); }
#endif
		LOCKV(remote);
		memcpy(r,remote,size);
		UNLOCKV(remote);
		if(size<=BR_MAX_XFER()) 
			BR_STORE(where,r,l,size,(BR_handler_mem_t)request_thr_copy_va,(BR_word_t)ta);
		else {
			put_memory_to(where,l,r,size);
			BR_REQUEST_1(where,r_ta_sema_signal,(long)ta);
		}
#ifndef USE_ALLOCA
		if(size>=ATOMIC_READ_MEM) { FREE(r); }
#endif
	}
void remote_v_read(int cluster,void *local,int lck_l,int tag,void* remote,int lck_r,int size TAI)
{
	void *l;

#ifdef USE_ALLOCA
	if(lck_l) l=alloca(size);
#else
	char mem[ATOMIC_READ_MEM];
	l=mem;
	if(size>=ATOMIC_READ_MEM && lck_l) { MALLOC(l,size); }
#endif
	if(!lck_l) l=local;
	STAT(READ_V);

	if(lck_r) {
		TA_SEMAPHORE(ta);
		BR_FORK_4(cluster,(BR_handler_4_t)thr_copy_v,(long)l,(long)size,(long)remote,(long)ta);
		TA_SEMA_WAIT(ta);
		RECVOB(tag,l,cluster);
	} else {
		get_memory_from(l,cluster,remote,size);
		RECVOB(tag,l,cluster);
	}

#if defined(ATTR_TRACE)
	if(attr!=NULL) ATTR_TR6("F_V_RATTR_%c%c %02d-(%s)%x->%s",ppch[lck_l],ppch[lck_r],cluster,type,rr,attr);
	else ATTR_TR6("F_V_RATTR_%c%c %02d-(%s)%x->[%d]",ppch[lck_l],ppch[lck_r],cluster,type,rr,index);
#endif
	if(lck_l) { 
		LOCKV(local);
		memcpy(local,l,size);
		UNLOCKV(local);
#ifndef USE_ALLOCA
		if(size>=ATOMIC_READ_MEM) { FREE(l); }
#endif /* USE_ALLOCA */
	}
}

/*
 * remote read of v with prefetching
 */
	struct PRE_REMOTE_V_READ_struct {
		volatile PREFETCH *p,*p1;
		int cluster;
		void *local;
		int lck_l;
		void *remote;
		int lck_r;
		int size,tag;
	};
	void thr_pre_remote_v_read(int cluster,struct PRE_REMOTE_V_READ_struct *p) {
		remote_v_read(p->cluster,p->local,p->lck_l,p->tag,p->remote,p->lck_r,p->size TAInull);
		STAT(PREFETCH_V);
		DECR_PREFETCH(p->p,p->p1);
		FREE(p);
	}
void pre_remote_v_read(volatile PREFETCH *p,volatile PREFETCH *p1,int cluster,void *local,int lck_l,int tag,void* remote,int lck_r,int size TAI)
{
	/* we use the easy way: create a thread to handle the read */
	struct PRE_REMOTE_V_READ_struct *pi;
	INCR_PREFETCH(p,p1);
	MALLOC(pi,sizeof(*pi));
	pi->p=p;pi->p1=p1;
	pi->cluster=cluster;
	pi->local=local;pi->lck_l=lck_l;
	pi->remote=remote;pi->lck_r=lck_r;
	pi->size=size;pi->tag=tag;
	BR_FORK_1(HERE,(BR_handler_1_t)thr_pre_remote_v_read,(BR_word_t)pi);
}
/*
 * remote v read with cache
 */
#ifdef IMPORT_CACHE
void cache_v_read(int cluster,void *local,int lck_l,int tag,void* remote,int lck_r,int size TAI)
{
	if(size<=IMPORT_CACHE_TRESHOLD) {
		int im=((LOCAL_MEM)(BR_GET_THREAD_LOCAL()))->import;
		int ce=IMPORT_CACHE_HASH(cluster,remote);
		CACHE_LOCK(cache[ce].lck);
		if(cache[ce].adr!=remote || cache[ce].import<im) {
			/* DEBUG0("CACHE miss"); */
			remote_v_read(cluster,cache[ce].value,0,tag,remote,lck_r,size TAIarg);
			cache[ce].import=im;
			cache[ce].adr=remote;
			STAT(IMPORT_CACHE_MISS);
		} else {
			STAT(IMPORT_CACHE_HIT);
#if defined(ATTR_TRACE)
			if(attr!=NULL) ATTR_TR6("CACHED F_V_RATTR_%c%c %02d-(%s)%x->%s",ppch[lck_l],ppch[lck_r],cluster,type,rr,attr);
			else ATTR_TR6("CACHED F_V_RATTR_%c%c %02d-(%s)%x->[%d]",ppch[lck_l],ppch[lck_r],cluster,type,rr,index);
#endif
		}
		if(lck_l) {
			LOCKV(local);
			memcpy(local,cache[ce].value,size);
			UNLOCKV(local);
		} else memcpy(local,cache[ce].value,size);
		CACHE_UNLOCK(cache[ce].lck);
	} else { /* cannot use the cache */
		remote_v_read(cluster,local,lck_l,tag,remote,lck_r,size TAIarg);
	}
}

/*
 * prefetch value type and use the cache
 */
	struct PREFETCH_IMPORT_V_struct {
		volatile PREFETCH *p,*p1;
		void *remote,*local;
		int lck_l,lck_r,tag;
		int cluster,size;
	};
	thr_pre_cache_v_read(int where,struct PREFETCH_IMPORT_V_struct *p)
	{
		int ce=IMPORT_CACHE_HASH(p->cluster,p->remote);
		cache[ce].import=import_ctr;
		remote_v_read(p->cluster,cache[ce].value,0,p->tag,p->remote,p->lck_r,p->size TAInull);
		cache[ce].adr=p->remote;
		STAT(IMPORT_CACHE_MISS);
		if(p->lck_l) {
			LOCKV(p->local);
			memcpy(p->local,cache[ce].value,p->size);
			UNLOCKV(p->local);
		} else memcpy(p->local,cache[ce].value,p->size);
		CACHE_UNLOCK(cache[ce].lck);
		DECR_PREFETCH(p->p,p->p1);
		FREE(p);
	}
void pre_cache_v_read(volatile PREFETCH *p,volatile PREFETCH *p1,int cluster,void *local,int lck_l,int tag,void* remote,int lck_r,int size TAI)
{ 
	if(size<=IMPORT_CACHE_TRESHOLD) {
		int im=((LOCAL_MEM)(BR_GET_THREAD_LOCAL()))->import;
		int ce=IMPORT_CACHE_HASH(cluster,remote);
		STAT(PREFETCH_V);
		CACHE_LOCK(cache[ce].lck);
		if(cache[ce].adr!=remote || cache[ce].import<im) {
			struct PREFETCH_IMPORT_V_struct *pi;
			INCR_PREFETCH(p,p1);
			MALLOC(pi,sizeof(*pi));
			pi->p=p;pi->p1=p1;
			pi->remote=remote;pi->local=local;
			pi->lck_l=lck_l;pi->lck_r=lck_r;
			pi->cluster=cluster;pi->size=size;
			pi->tag=tag;
			BR_FORK_1(HERE,(BR_handler_1_t)thr_pre_cache_v_read,(BR_word_t)pi);
		} else {
			STAT(IMPORT_CACHE_HIT);
			STAT(PREFETCH_CACHE_HIT);
#if defined(ATTR_TRACE)
			if(attr!=NULL) ATTR_TR6("CACHED F_V_RATTR_%c%c %02d-(%s)%x->%s",ppch[lck_l],ppch[lck_r],cluster,type,rr,attr);
			else ATTR_TR6("CACHED F_V_RATTR_%c%c %02d-(%s)%x->[%d]",ppch[lck_l],ppch[lck_r],cluster,type,rr,index);
#endif
			if(lck_l) {
				LOCKV(local);
				memcpy(local,cache[ce].value,size);
				UNLOCKV(local);
			} else memcpy(local,cache[ce].value,size);
			CACHE_UNLOCK(cache[ce].lck);
		}
	} else { /* cannot use the cache */
		pre_remote_v_read(p,p1,cluster,local,lck_l,tag,remote,lck_r,size TAIarg);
	}
}
#endif /* IMPORT_CACHE */




/************************************************************************
 * Writes some local memory to a remote cluster. Comes in three
 * versions, for reference, atomic value types and other value types
 ************************************************************************/

/***********************
 * WARNING             
 * The writes should arrive in order, this is not 
 * yet guaranteed when using post_writes!
 **********************/

/*
 * As we cannot directly write the memory to the desired place
 * (this would break the atomicity requirement) we allocate some
 * local memory first. At the beginning of this memory block we
 * store the pointer to where this memory should ultimatly be stored.
 * This is necessary, as we cannot pass this information in the
 * BR_STORE due to lack of arguments.
 * The structure write_mem is used for this transaction.
 */
struct write_mem {
	void *remote;
	char mem[1];
};
/*
 * get some local memory AND store a pointer in this memory block.
 * It returns the pointer to memory portion that can be overwritten,
 * NOT to memory block actually allocated (the first 4 bytes are used
 * by the pointer passed as argument)
 */
static void get_local_mem_r(BR_cluster_t from,BR_word_t rem,BR_word_t mem,BR_word_t ta)
{
	*((void **)rem)=(void *)mem;
	TA_SEMA_SIGNAL((ta_sema_t)ta);
}
static void get_local_mem(BR_cluster_t from,BR_word_t rem,BR_word_t size,BR_word_t ta,BR_word_t remote)
{
	struct write_mem *r;
	MALLOC(r,sizeof(struct write_mem)+size);
	r->remote=(void *)remote;
	BR_REPLY_3(from,get_local_mem_r,rem,(BR_word_t)r->mem,ta);
}

/*
 * Copies the memory stored on a remote cluster (via BR_STORE) to
 * the correct position (this position has been defined when allocating
 * the memory with get_local_mem()). One function is used for value types
 * that are assigned atomically, the other one for all others.
 */
static void write_to_local(BR_cluster_t from,caddr_t rva,size_t size,BR_word_t arg)
{
	struct write_mem *r=(struct write_mem *)((long)rva-sizeof(void*));
	ATOMIC_MEMCPY(r->remote,r->mem,size);
	BR_REPLY_1(from,r_ta_sema_signal,arg);
}
static void writev_to_local(vnn_t from,caddr_t rva,size_t size,BR_word_t arg)
{
	struct write_mem *r=(struct write_mem *)((long)rva-sizeof(void*));
	LOCKV(r->remote);
	memcpy(r->remote,r->mem,size);
	UNLOCKV(r->remote);
	BR_REPLY_1(from,r_ta_sema_signal,arg);
}


/**
*** writing of reference values
***/

/*
 * Standard writing of reference value, with or without cache
 */
	static void request_remote_r_write(vnn_t from,FOB *remote,FOB local,ta_sema_t a)
	{
		*remote=local;
		BR_REPLY_1(from,r_ta_sema_signal,(long)a);
	}
void remote_r_write(int cluster,FOB* remote,FOB *local TAI)
{
	TA_SEMAPHORE(a);
#if defined(ATTR_TRACE)
	if(attr!=NULL) ATTR_TR4("F_R_WATTR_AA %02d-(%s)%x->%s",cluster,type,rr,attr);
	else ATTR_TR4("F_R_WATTR_AA %02d-(%s)%x->[%d]",cluster,type,rr,index);
#endif
	STAT(WRITE_R);
#ifdef IMPORT_CACHE
# if defined(LARGEST_ATOMIC) 
#  if LARGEST_ATOMIC>IMPORT_CACHE_TRESHOLD
	if(sizeof(FOB)<=IMPORT_CACHE_TRESHOLD) 
#  endif
# endif
		{
			int im=((LOCAL_MEM)(BR_GET_THREAD_LOCAL()))->import;
			int ce=IMPORT_CACHE_HASH(cluster,remote);
			CACHE_LOCK(cache[ce].lck);
			cache[ce].adr=remote;
			cache[ce].import=im;
			*(FOB*)cache[ce].value=*local;
			CACHE_UNLOCK(cache[ce].lck);
		}
#endif
	BR_REQUEST_3(cluster,(BR_handler_3_t)request_remote_r_write,(long)remote,(long)SENDFOB(*local,cluster),(long)a);
	TA_SEMA_WAIT(a);
}

/*
 * post writing of reference value, with or without cache
 */
	static void r_decr(BR_cluster_t from,BR_word_t a)
	{
		(*(unsigned long *)a)--;
	}
	static void request_post_r_write(vnn_t from,FOB *remote,FOB local,long a)
	{
		*remote=local;
		BR_REPLY_1(from,r_decr,(BR_word_t)a);
	}
void post_remote_r_write(int cluster,FOB* remote,FOB *local TAI)
{
#if defined(ATTR_TRACE)
	if(attr!=NULL) ATTR_TR4("POST_R_WATTR_AA %02d-(%s)%x->%s",cluster,type,rr,attr);
	else ATTR_TR4("POST_R_WATTR_AA %02d-(%s)%x->[%d]",cluster,type,rr,index);
#endif
	STAT(POST_WRITE_R);
#ifdef IMPORT_CACHE
# if defined(LARGEST_ATOMIC) 
#  if LARGEST_ATOMIC>IMPORT_CACHE_TRESHOLD
	if(sizeof(FOB)<=IMPORT_CACHE_TRESHOLD) 
#  endif
# endif
		{
			int im=((LOCAL_MEM)(BR_GET_THREAD_LOCAL()))->import;
			int ce=IMPORT_CACHE_HASH(cluster,remote);
			CACHE_LOCK(cache[ce].lck);
			cache[ce].adr=remote;
			cache[ce].import=im;
			*(FOB*)cache[ce].value=*local;
			CACHE_UNLOCK(cache[ce].lck);
		}
#endif
	((LOCAL_MEM)(BR_GET_THREAD_LOCAL()))->exports_waiting++;
	BR_REQUEST_3(cluster,(BR_handler_3_t)request_post_r_write,(long)remote,(long)SENDFOB(*local,cluster),(long)&((LOCAL_MEM)(BR_GET_THREAD_LOCAL()))->exports_waiting);
}


/**
*** writing of short values
***/

/*
 * Standard writing of short value, with or without cache
 */
	static void request_remote_vs1_write(vnn_t from,char *remote,long local,ta_sema_t a)
	{
		*remote=local;
		BR_REPLY_1(from,r_ta_sema_signal,(long)a);
	}
	static void request_remote_vs2_write(vnn_t from,short *remote,long local,ta_sema_t a)
	{
		*remote=local;
		BR_REPLY_1(from,r_ta_sema_signal,(long)a);
	}
	static void request_remote_vs4_write(vnn_t from,long *remote,long local,ta_sema_t a)
	{
		*remote=local;
		BR_REPLY_1(from,r_ta_sema_signal,(long)a);
	}
	static void request_remote_vs8_write(vnn_t from,double *remote,long local,long local2,ta_sema_t a)
	{
		double d;
		VS8_JOIN(d,local,local2);
		*remote=d;
		BR_REPLY_1(from,r_ta_sema_signal,(long)a);
	}
static void remote_vs_write(int cluster,void* remote,void *local,int size TAI)
{
	TA_SEMAPHORE(a);
#if defined(ATTR_TRACE)
	if(attr!=NULL) ATTR_TR4("F_R_WATTR_AA %02d-(%s)%x->%s",cluster,type,rr,attr);
	else ATTR_TR4("F_R_WATTR_AA %02d-(%s)%x->[%d]",cluster,type,rr,index);
#endif
#ifdef IMPORT_CACHE
# if defined(LARGEST_ATOMIC) 
#  if LARGEST_ATOMIC>IMPORT_CACHE_TRESHOLD
	if(size<=IMPORT_CACHE_TRESHOLD) 
#  endif
# endif
		{
			int im=((LOCAL_MEM)(BR_GET_THREAD_LOCAL()))->import;
			int ce=IMPORT_CACHE_HASH(cluster,remote);
			CACHE_LOCK(cache[ce].lck);
			cache[ce].adr=remote;
			cache[ce].import=im;
			ATOMIC_MEMCPY(cache[ce].value,local,size);
			CACHE_UNLOCK(cache[ce].lck);
		}
#endif
	switch(size) {
	case 1: STAT(WRITE_VS1);BR_REQUEST_3(cluster,(BR_handler_3_t)request_remote_vs1_write,(long)remote,*(char *)local,(long)a);break;
	case 2: STAT(WRITE_VS2);BR_REQUEST_3(cluster,(BR_handler_3_t)request_remote_vs2_write,(long)remote,*(short *)local,(long)a);break;
	case 4: STAT(WRITE_VS4);BR_REQUEST_3(cluster,(BR_handler_3_t)request_remote_vs4_write,(long)remote,*(long *)local,(long)a);break;
	case 8: STAT(WRITE_VS8);BR_REQUEST_4(cluster,(BR_handler_4_t)request_remote_vs8_write,(long)remote,VS8_ACCESS1(*(double *)local),VS8_ACCESS2(*(double *)local),(long)a);break;
	}
	TA_SEMA_WAIT(a);
}

/*
 * post writing of short value, with or without cache
 */
	static void request_post_vs1_write(vnn_t from,char *remote,long local,long a)
	{
		*remote=local;
		BR_REPLY_1(from,r_decr,(BR_word_t)a);
	}
	static void request_post_vs2_write(vnn_t from,short *remote,long local,long a)
	{
		*remote=local;
		BR_REPLY_1(from,r_decr,(BR_word_t)a);
	}
	static void request_post_vs4_write(vnn_t from,long *remote,long local,long a)
	{
		*remote=local;
		BR_REPLY_1(from,r_decr,(BR_word_t)a);
	}
	static void request_post_vs8_write(vnn_t from,double *remote,long local,long local2,long a)
	{
		double d;
		VS8_JOIN(d,local,local2);
		*remote=d;
		BR_REPLY_1(from,r_decr,(BR_word_t)a);
	}
void post_remote_vs_write(int cluster,FOB* remote,FOB *local,int size TAI)
{
#if defined(ATTR_TRACE)
	if(attr!=NULL) ATTR_TR4("POST_R_WATTR_AA %02d-(%s)%x->%s",cluster,type,rr,attr);
	else ATTR_TR4("POST_R_WATTR_AA %02d-(%s)%x->[%d]",cluster,type,rr,index);
#endif
#ifdef IMPORT_CACHE
# if defined(LARGEST_ATOMIC) 
#  if LARGEST_ATOMIC>IMPORT_CACHE_TRESHOLD
	if(size<=IMPORT_CACHE_TRESHOLD) 
#  endif
# endif
		{
			int im=((LOCAL_MEM)(BR_GET_THREAD_LOCAL()))->import;
			int ce=IMPORT_CACHE_HASH(cluster,remote);
			CACHE_LOCK(cache[ce].lck);
			cache[ce].adr=remote;
			cache[ce].import=im;
			ATOMIC_MEMCPY(cache[ce].value,local,size);
			CACHE_UNLOCK(cache[ce].lck);
		}
#endif
	((LOCAL_MEM)(BR_GET_THREAD_LOCAL()))->exports_waiting++;
	switch(size) {
	case 1: STAT(POST_WRITE_VS1);BR_REQUEST_3(cluster,(BR_handler_3_t)request_post_vs1_write,(long)remote,(long)(*(char *)local),(long)&((LOCAL_MEM)(BR_GET_THREAD_LOCAL()))->exports_waiting);break;
	case 2: STAT(POST_WRITE_VS2);BR_REQUEST_3(cluster,(BR_handler_3_t)request_post_vs2_write,(long)remote,(long)(*(short *)local),(long)&((LOCAL_MEM)(BR_GET_THREAD_LOCAL()))->exports_waiting);break;
	case 4: STAT(POST_WRITE_VS4);BR_REQUEST_3(cluster,(BR_handler_3_t)request_post_vs4_write,(long)remote,(long)(*(long *)local),(long)&((LOCAL_MEM)(BR_GET_THREAD_LOCAL()))->exports_waiting);break;
	case 8: STAT(POST_WRITE_VS8);BR_REQUEST_4(cluster,(BR_handler_4_t)request_post_vs8_write,(long)remote,VS8_ACCESS1(*(double *)local),VS8_ACCESS2(*(double *)local),(long)&((LOCAL_MEM)(BR_GET_THREAD_LOCAL()))->exports_waiting);break;
	}
}



/**
*** writing a VA value
**/
/*
 * standard VA write
 */
void remote_va_write(int cluster,void* remote,int atm_r,int size,void *local,int atm_l TAI)
{
	if(size==1||size==2||size==4||size==8) {
		remote_vs_write(cluster,remote,local,size TAIarg);
	} else {
		void *l;
		TA_SEMAPHORE(ta);
		TA_SEMAPHORE(tb);

#ifdef USE_ALLOCA
		if(atm_l) l=alloca(size);
#else
		char mem[ATOMIC_READ_MEM];
		l=mem;
		if(size>=ATOMIC_READ_MEM && atm_l) { MALLOC(l,size); }
#endif
		STAT(WRITE_VA);
		if(!atm_l) l=local;
		else ATOMIC_MEMCPY(l,local,size);

#if defined(ATTR_TRACE)
		if(attr!=NULL) ATTR_TR6("F_VA_WATTR_%c%c %02d-(%s)%x->%s",atch[atm_l],atch[atm_r],cluster,type,rr,attr);
		else ATTR_TR6("F_VA_WATTR_%c%c %02d-(%s)%x->[%d]",atch[atm_l],atch[atm_r],cluster,type,rr,index);
#endif
#ifdef IMPORT_CACHE
# if defined(LARGEST_ATOMIC) 
#  if LARGEST_ATOMIC>IMPORT_CACHE_TRESHOLD
		if(size<=IMPORT_CACHE_TRESHOLD) 
#  endif
# endif
			{
				int im=((LOCAL_MEM)(BR_GET_THREAD_LOCAL()))->import;
				int ce=IMPORT_CACHE_HASH(cluster,remote);
				CACHE_LOCK(cache[ce].lck);
				cache[ce].adr=remote;
				cache[ce].import=im;
				if(atm_l) ATOMIC_MEMCPY(cache[ce].value,local,size);
				else memcpy(cache[ce].value,local,size);
				CACHE_UNLOCK(cache[ce].lck);
			}
#endif
		if(atm_r) { 
			void *rem;
			BR_REQUEST_4(cluster,get_local_mem,(BR_word_t)&rem,size,(BR_word_t)ta,(BR_word_t)remote);
			TA_SEMA_WAIT(ta);
			BR_STORE(cluster, l, rem, size, write_to_local, (BR_word_t)tb);
			TA_SEMA_WAIT(tb);
			BR_REQUEST_1(cluster,(BR_handler_1_t)free_local,(long)rem-sizeof(void*)); /* no reply */
		} else {
			BR_STORE(cluster, l, remote, size, r_ta_sema_signal_reply_mem, (BR_word_t)ta);
			TA_SEMA_WAIT(ta);
		}

#ifndef USE_ALLOCA
		if(size>=ATOMIC_READ_MEM && atm_l) { FREE(l); }
#endif
	}
}
/*
 * post VA write
 */
	static void r_reply_decr(vnn_t from,caddr_t x,size_t size,BR_word_t a)
	{
		BR_REPLY_1(from,r_decr,a);
	}
	struct VA_WRITE_struct {
		int cluster;
		void *remote;
		int size;
		volatile unsigned long *counter;
		char local[1];
	};
	static void thr_post_va_write(vnn_t from,struct VA_WRITE_struct *p)
	{
		TA_SEMAPHORE(ta);
		TA_SEMAPHORE(tb);
		void *rem;
		BR_REQUEST_4(p->cluster,get_local_mem,(long)&rem,p->size,(long)ta,(long)p->remote);
		TA_SEMA_WAIT(ta);
		BR_STORE(p->cluster, p->local, rem, p->size, write_to_local, (BR_word_t)tb);
		TA_SEMA_WAIT(tb);
		LOCKV(p->counter);
		(*p->counter)--;
		UNLOCKV(p->counter);
		BR_REQUEST_1(p->cluster,(BR_handler_1_t)free_local,(long)rem-sizeof(void*)); /* no reply */
		FREE(p);
	}
void post_remote_va_write(int cluster,void* remote,int atm_r,int size,void *local,int atm_l TAI)
{
	if(size==1||size==2||size==4||size==8) {
		post_remote_vs_write(cluster,remote,local,size TAIarg);
	} else {
		void *l;

#ifdef USE_ALLOCA
		if(atm_l) l=alloca(size);
#else
		char mem[ATOMIC_READ_MEM];
		l=mem;
		if(size>=ATOMIC_READ_MEM && atm_l) { MALLOC(l,size); }
#endif
		STAT(POST_WRITE_VA);
		if(!atm_l) l=local;
		else ATOMIC_MEMCPY(l,local,size);

#if defined(ATTR_TRACE)
		if(attr!=NULL) ATTR_TR6("F_VA_WATTR_%c%c %02d-(%s)%x->%s",atch[atm_l],atch[atm_r],cluster,type,rr,attr);
		else ATTR_TR6("F_VA_WATTR_%c%c %02d-(%s)%x->[%d]",atch[atm_l],atch[atm_r],cluster,type,rr,index);
#endif
#ifdef IMPORT_CACHE
# if defined(LARGEST_ATOMIC) 
#  if LARGEST_ATOMIC>IMPORT_CACHE_TRESHOLD
		if(size<=IMPORT_CACHE_TRESHOLD) 
#  endif
# endif
			{
				int im=((LOCAL_MEM)(BR_GET_THREAD_LOCAL()))->import;
				int ce=IMPORT_CACHE_HASH(cluster,remote);
				CACHE_LOCK(cache[ce].lck);
				cache[ce].adr=remote;
				cache[ce].import=im;
				if(atm_l) ATOMIC_MEMCPY(cache[ce].value,local,size);
				else memcpy(cache[ce].value,local,size);
				CACHE_UNLOCK(cache[ce].lck);
			}
#endif
		if(atm_r) { 
			struct VA_WRITE_struct *pp;
			MALLOC(pp,sizeof(*pp)+size);
			memcpy(pp->local,l,size);
			pp->cluster=cluster;
			pp->size=size;
			pp->remote=remote;
			pp->counter= &((LOCAL_MEM)(BR_GET_THREAD_LOCAL()))->exports_waiting;
			((LOCAL_MEM)(BR_GET_THREAD_LOCAL()))->exports_waiting++;
			BR_FORK_1(HERE,(BR_handler_1_t)thr_post_va_write,(BR_word_t)&pp);
		} else {
			((LOCAL_MEM)(BR_GET_THREAD_LOCAL()))->exports_waiting++;
			BR_STORE(cluster, l, remote, size, r_reply_decr, (BR_word_t)(&((LOCAL_MEM)(BR_GET_THREAD_LOCAL()))->exports_waiting));
		}

#ifndef USE_ALLOCA
		if(size>=ATOMIC_READ_MEM && atm_l) { FREE(l); }
#endif
	}
}

/**
*** remote write of standard value types
**/
void remote_v_write(int cluster,void* remote,int atm_r,int size,void *local,int atm_l,int tag TAI)
{
	void *l;
	TA_SEMAPHORE(ta);
	TA_SEMAPHORE(tb);

	/* we have to make a copy and change the pointers it contains */
#ifdef USE_ALLOCA
	l=alloca(size);
#else
	char mem[ATOMIC_READ_MEM];
	l=mem;
	if(size>=ATOMIC_READ_MEM) { MALLOC(l,size); }
#endif
	if(atm_l) LOCKV(local);
	memcpy(l,local,size);
	if(atm_l) UNLOCKV(local);

#if defined(ATTR_TRACE)
	if(attr!=NULL) ATTR_TR6("F_V_WATTR_%c%c %02d-(%s)%x->%s",ppch[atm_l],ppch[atm_r],cluster,type,rr,attr);
	else ATTR_TR6("F_V_WATTR_%c%c %02d-(%s)%x->[%d]",ppch[atm_l],ppch[atm_r],cluster,type,rr,index);
#endif
	STAT(WRITE_V);
#ifdef IMPORT_CACHE
	if(size<=IMPORT_CACHE_TRESHOLD) {
		int im=((LOCAL_MEM)(BR_GET_THREAD_LOCAL()))->import;
		int ce=IMPORT_CACHE_HASH(cluster,remote);
		CACHE_LOCK(cache[ce].lck);
		cache[ce].adr=remote;
		cache[ce].import=im;
		memcpy(cache[ce].value,l,size);
		CACHE_UNLOCK(cache[ce].lck);
	}
#endif
	SENDOB(tag,l,cluster);
	if(atm_r) { 
		void *rem;
		BR_REQUEST_4(cluster,get_local_mem,(BR_word_t)&rem,size,(BR_word_t)ta,(BR_word_t)remote);
		TA_SEMA_WAIT(ta);
		BR_STORE(cluster, l, rem, size, writev_to_local, (BR_word_t)tb);
		TA_SEMA_WAIT(tb);
		BR_REQUEST_1(cluster, free_local, (BR_word_t)rem-sizeof(void*)); /* no reply */
	} else {
		BR_STORE(cluster, l, remote, size, r_ta_sema_signal_reply_mem, (BR_word_t)ta);
		TA_SEMA_WAIT(ta);
	}

#ifndef USE_ALLOCA
	if(size>=ATOMIC_READ_MEM) { FREE(l); }
#endif
}


	static void thr_post_v_write(vnn_t from,struct VA_WRITE_struct *p)
	{
		TA_SEMAPHORE(ta);
		TA_SEMAPHORE(tb);
		void *rem;
		BR_REQUEST_4(p->cluster,get_local_mem,(BR_word_t)&rem,p->size,(BR_word_t)ta,(BR_word_t)p->remote);
		TA_SEMA_WAIT(ta);
		BR_STORE(p->cluster, p->local, rem, p->size, writev_to_local, (BR_word_t)tb);
		TA_SEMA_WAIT(tb);
		LOCKV(p->counter);
		(*p->counter)--;
		UNLOCKV(p->counter);
		BR_REQUEST_1(p->cluster,(BR_handler_1_t)free_local,(long)rem-sizeof(void*)); /* no reply */
		FREE(p);
	}
void post_remote_v_write(int cluster,void* remote,int atm_r,int size,void *local,int atm_l,int tag TAI)
{
	void *l;

	/* we have to make a copy and change the pointers it contains */
#ifdef USE_ALLOCA
	l=alloca(size);
#else
	char mem[ATOMIC_READ_MEM];
	l=mem;
	if(size>=ATOMIC_READ_MEM) { MALLOC(l,size); }
#endif
	if(atm_l) LOCKV(local);
	memcpy(l,local,size);
	if(atm_l) UNLOCKV(local);

#if defined(ATTR_TRACE)
	if(attr!=NULL) ATTR_TR6("F_V_WATTR_%c%c %02d-(%s)%x->%s",ppch[atm_l],ppch[atm_r],cluster,type,rr,attr);
	else ATTR_TR6("F_V_WATTR_%c%c %02d-(%s)%x->[%d]",ppch[atm_l],ppch[atm_r],cluster,type,rr,index);
#endif
	STAT(POST_WRITE_V);
#ifdef IMPORT_CACHE
	if(size<=IMPORT_CACHE_TRESHOLD) {
		int im=((LOCAL_MEM)(BR_GET_THREAD_LOCAL()))->import;
		int ce=IMPORT_CACHE_HASH(cluster,remote);
		CACHE_LOCK(cache[ce].lck);
		cache[ce].adr=remote;
		cache[ce].import=im;
		memcpy(cache[ce].value,l,size);
		CACHE_UNLOCK(cache[ce].lck);
	}
#endif
	SENDOB(tag,l,cluster);
	if(atm_r) { 
		/* we can use the same structure as for VA_WRITE */
		struct VA_WRITE_struct *pp;
		MALLOC(pp,sizeof(*pp)+size);
		memcpy(pp->local,l,size);
		pp->cluster=cluster;
		pp->size=size;
		pp->remote=remote;
		pp->counter= &((LOCAL_MEM)(BR_GET_THREAD_LOCAL()))->exports_waiting;
		((LOCAL_MEM)(BR_GET_THREAD_LOCAL()))->exports_waiting++;
		BR_FORK_1(HERE,(BR_handler_1_t)thr_post_v_write,(BR_word_t)&pp);
	} else {
		((LOCAL_MEM)(BR_GET_THREAD_LOCAL()))->exports_waiting++;
		BR_STORE(cluster, l, remote, size, r_reply_decr,(BR_word_t)(&((LOCAL_MEM)(BR_GET_THREAD_LOCAL()))->exports_waiting));
	}

#ifndef USE_ALLOCA
	if(size>=ATOMIC_READ_MEM) { FREE(l); }
#endif
}



/*
 * use the largest atomic assignment possible to copy memory
 * from one position to another. The regions may not overlap.
 */
static void amemcpy(ATOMIC_1_TYPE *dest,ATOMIC_1_TYPE *src,int size)
{
#ifdef ATOMIC_ALL
	ATOMIC_MEMCPY(dest,src,size);
#else
	int i;
# ifdef ATOMIC_16
	if(IS_ALIGNED(ALIGNMENT_16,dest) && IS_ALIGNED(ALIGNMENT_16,src)) {
		ATOMIC_16_TYPE *s16=(ATOMIC_16_TYPE*)src,*d16=(ATOMIC_16_TYPE*)dest;
		for(i=0;size>=16;size-=16,i++)
			d16[i]=s16[i];
		if(!size) return;
		dest+=i*16;src+=i*16;
	}
# endif
# ifdef ATOMIC_8
	if(IS_ALIGNED(ALIGNMENT_8,dest) && IS_ALIGNED(ALIGNMENT_8,src)) {
		ATOMIC_8_TYPE *s8=(ATOMIC_8_TYPE*)src,*d8=(ATOMIC_8_TYPE*)dest;
		for(i=0;size>=8;size-=8,i++)
			d8[i]=s8[i];
		if(!size) return;
		dest+=i*8;src+=i*8;
	}
# endif
# ifdef ATOMIC_4
	if(IS_ALIGNED(ALIGNMENT_4,dest) && IS_ALIGNED(ALIGNMENT_4,src)) {
		ATOMIC_4_TYPE *s4=(ATOMIC_4_TYPE*)src,*d4=(ATOMIC_4_TYPE*)dest;
		for(i=0;size>=4;size-=4,i++)
			d4[i]=s4[i];
		if(!size) return;
		dest+=i*4;src+=i*4;
	}
# endif
# ifdef ATOMIC_2
	if(IS_ALIGNED(ALIGNMENT_2,dest) && IS_ALIGNED(ALIGNMENT_2,src)) {
		ATOMIC_2_TYPE *s2=(ATOMIC_2_TYPE*)src,*d2=(ATOMIC_2_TYPE*)dest;
		for(i=0;size>=2;size-=2,i++)
			d2[i]=s2[i];
		if(!size) return;
		dest+=i*2;src+=i*2;
	}
# endif
# ifdef ATOMIC_1
	for(i=0;i<size;i++) dest[i]=src[i];
# endif
#endif
}
static void remote_amemcpy(vnn_t from,void *dest,void *src,int size,long tas)
{
	amemcpy(dest,src,size);
	BR_REPLY_1(from,r_ta_sema_signal,tas);
}

/*
 * use the largest atomic assignment possible to set a memory region to 0
 */
static void amemclear(ATOMIC_1_TYPE *dest,int size)
{
#ifdef ATOMIC_ALL
	memset(dest,0,size);
#else
	int i;
# ifdef ATOMIC_16
	char clr[16];
	memset(clr,0,16);
	if(IS_ALIGNED(ALIGNMENT_16,dest)) {
		ATOMIC_16_TYPE *d=(ATOMIC_16_TYPE*)dest;
		for(i=0;size>=16;size-=16,i++)
			d[i]= *(LARGEST_ATOMIC_TYPE*)clr;
		if(!size) return;
		dest+=i*16;
	}
# else
#  ifdef ATOMIC_8
	char clr[8];
	memset(clr,0,8);
#  endif
# endif
# ifdef ATOMIC_8
	if(IS_ALIGNED(ALIGNMENT_8,dest)) {
		ATOMIC_8_TYPE *d=(ATOMIC_8_TYPE*)dest;
		for(i=0;size>=8;size-=8,i++)
			d[i]= *(LARGEST_ATOMIC_TYPE*)clr;
		if(!size) return;
		dest+=i*8;
	}
# endif
# ifdef ATOMIC_4
	if(IS_ALIGNED(ALIGNMENT_4,dest)) {
		ATOMIC_4_TYPE *d=(ATOMIC_4_TYPE*)dest;
		for(i=0;size>=4;size-=4,i++) d[i]=0;
		if(!size) return;
		dest+=i*4;
	}
# endif
# ifdef ATOMIC_2
	if(IS_ALIGNED(ALIGNMENT_2,dest)) {
		ATOMIC_2_TYPE *d=(ATOMIC_2_TYPE*)dest;
		for(i=0;size>=2;size-=2,i++) d[i]=0;
		if(!size) return;
		dest+=i*2;
	}
# endif
	for(i=0;i<size;i++) dest[i]=0;
#endif
}
static void remote_amemclear(vnn_t from,void *dest,int size,long tas)
{
	amemclear(dest,size);
	BR_REPLY_1(from,r_ta_sema_signal,tas);
}

/*
 * copy's an object from a remote cluster to the local cluster.
 * If tag!=0 it will also change references stored in those objects.
 * Note that this works only if the type tables supplied by the sather
 * compiler exist, otherwise it wont.
 * If elems==0 it assumes that there is just this one object to fix,
 * otherwise it assumes that it has an array of objects. In the latter
 * case an array of reference objects is assumed to be an array
 * of pointers! So elems==0 is NOT equivalent with elems==1!
 * If elems==-1, it will NOT adjust an ev. array part of an object
 */
void p_r_object(void *local,int latm,FOB remote,int ratm,int size,int tag,int elems)
{
	void *res;
	FOB rem=NULL;
	int where;
	TA_SEMAPHORE(a);
	DEBUG5("p_r_object(local=%p, latm=%d, remote=%p, int ratm=%d, size=%d)",local,latm,remote,ratm,size);
	if(NEAR(remote)) {
		amemcpy(local,remote,size);
		return;
	}
	where=WHERE(remote);
	if(ratm) { /* copy the memory to a safe place */
		rem=r_alloc(where,size);
		BR_REQUEST_4(where,(BR_handler_4_t)remote_amemcpy,(long)SENDFOBHOME(rem),(long)SENDFOBHOME(remote),size,(long)a);
	}
	if(latm) {
#ifdef USE_ALLOCA
		res=alloca(size);
#else
		MALLOC(res,size);
#endif
	} else res=local;
	if(ratm) TA_SEMA_WAIT(a); /* wait for copy started above */
	else rem=remote;
	get_memory(res,rem,size);
	if(tag) {
		if(elems==-1) p_sr_only_ob(p_recv_fob,tag,res,where);
		else if(elems) p_sr_array(p_recv_fob,tag,res,where,elems);
		else p_sr_ob(p_recv_fob,tag,res,where);
	}
	if(ratm) { r_free(rem); }
	if(latm) {
		amemcpy(local,res,size);
#ifndef USE_ALLOCA
		FREE(res);
#endif
	}
	return;
}

void p_w_object(FOB remote,int ratm,void *local,int latm,int size,int tag,int elems)
{
	void *res;
	FOB rem;
	int where;
	TA_SEMAPHORE(a);
	DEBUG5("p_w_object(remote=%p, ratm=%d, local=%p, int latm=%d, size=%d)",remote,ratm,local,latm,size);
	if(NEAR(remote)) {
		amemcpy(remote,local,size);
		return;
	}
	where=WHERE(remote);
	if(ratm) rem=r_alloc(where,size);
	else rem=remote;

	if(latm || tag) {
#ifdef USE_ALLOCA
		res=alloca(size);
#else
		MALLOC(res,size);
#endif
		amemcpy(res,local,size);
		if(elems==-1) p_sr_only_ob(p_send_fob,tag,res,where);
		else if(elems) p_sr_array(p_send_fob,tag,res,where,elems);
		else p_sr_ob(p_send_fob,tag,res,where);
	} else res=local;
	put_memory(rem,res,size);
	if(ratm) { 
		BR_REQUEST_4(where,(BR_handler_4_t)remote_amemcpy,(long)SENDFOBHOME(remote),(long)SENDFOBHOME(rem),size,(long)a);
		TA_SEMA_WAIT(a);
		r_free(rem);
	}
#ifndef USE_ALLOCA
	if(latm) {
		FREE(res);
	}
#endif
	return;
}

struct p_c_object_struct {
	FOB remote1;
	FOB remote2;
	int size,tag,elems;
	char ratm1,ratm2;
};

static void request_c_object(vnn_t from,struct p_c_object_struct *pcos,int size)
{
	p_r_object(pcos->remote1,pcos->ratm1,pcos->remote1,pcos->ratm2,
	     pcos->size,pcos->tag,pcos->elems);
}
void p_c_object(FOB remote1,int ratm1,FOB remote2,int ratm2,int size,int tag,int elems)
{
	if(NEAR(remote1)) { 
	   p_r_object(MAKENEAR(remote1),
		      ratm1,
		      remote2,
		      ratm2,
		      size,
		      tag,
		      elems);
	}
	else if(NEAR(remote2)) p_w_object(MAKENEAR(remote2),ratm2,remote1,ratm2,size,tag,elems);
	else {
		struct p_c_object_struct 
		         pcos = { NULL,NULL,size,tag,elems,ratm1,ratm2 };
		pcos.remote1=SENDFOBHOME(remote1);
		pcos.remote2=SENDFOB(remote2,WHERE(remote1));
		execp_mem(WHERE(remote1),(void *)request_c_object,&pcos,sizeof(pcos));
	}
}
/*
 * Warning: the next two functions overwrite asize, although this
 * should not happen. The corrections happen in the call macros 
 * defined in pSather.h
 */
void p_r_array_object(void *local,int latm,int lsize,int arr_offset,FOB remote,int ratm,int rsize,int elemsize,int tag,int eltag)
{
	int csize=lsize<rsize?lsize:rsize;
	DEBUG6("p_r_array_object(local=%p, lsize=%d, offset=%d, remote=%p, rsize=%d, elemsize=%d)",local,lsize,arr_offset,remote,rsize,elemsize);
	if(latm==0 && ratm==0) {
		p_r_object(local,latm,remote,ratm,arr_offset+csize*elemsize,tag,0);
	} else {
		/* to preserve atomicity we copy the object in 2 phases */
		p_r_object(local,latm,remote,ratm,arr_offset,tag,-1);
		p_r_object(((char *)local)+arr_offset,latm,((char *)remote)+arr_offset,ratm,csize*elemsize,eltag,csize);
	}
}
void p_w_array_object(FOB remote,int ratm,int rsize,int arr_offset,void *local,int latm,int lsize,int elemsize,int tag,int eltag)
{
	int csize=lsize<rsize?lsize:rsize;
	DEBUG6("p_r_array_object(remote=%p, rsize=%d, offset=%d, local=%p, lsize=%d, elemsize=%d)",remote,rsize,arr_offset,local,lsize,elemsize);
	if(latm==0 && ratm==0) {
		p_w_object(remote,ratm,local,latm,arr_offset+csize*elemsize,tag,0);
	} else {
		/* to preserve atomicity we copy the object in 2 phases */
		p_w_object(remote,ratm,local,latm,arr_offset,tag,-1);
		p_w_object(((char *)remote)+arr_offset,ratm,((char *)local)+arr_offset,latm,csize*elemsize,eltag,csize);
	}
}
void p_c_array_object(FOB remote1,int ratm,int rsize,int arr_offset,void *remote2,int latm,int lsize,int elemsize,int tag,int eltag)
{
	int csize=lsize<rsize?lsize:rsize;
	DEBUG6("p_r_array_object(remote1=%p, rsize=%d, offset=%d, remote2=%p, lsize=%d, elemsize=%d)",remote1,rsize,arr_offset,remote2,lsize,elemsize);
	/*
	 * if elemsize==1 (char array), we don't have to worry about atomicity when
	 * copying the array portion (we assume that chars are always atomic assignments)
	 */
	if((latm==0 && ratm==0) || elemsize==1) {
		p_c_object(remote1,ratm,remote2,latm,arr_offset+csize*elemsize,tag,0);
	} else {
		/* to preserve atomicity we copy the object in 2 phases */
		assert((arr_offset&1)==0); /* when arr_ofset&1 we assume that elemsize==1 */
		p_c_object(remote1,ratm,remote2,latm,arr_offset,tag,-1);
		p_c_object(((char *)remote1)+arr_offset,ratm,((char *)remote2)+arr_offset,latm,csize*elemsize,eltag,csize);
	}
}

void p_r_array(void *local,int latm,int lsize,int arr_offset,FOB remote,int ratm,int rsize,int elemsize,int eltag)
{
	/* we cannot have an odd arrayoffset (this would generate a pointer with the lowest
	 * bit 1, which is used to mark far pointers. If the arrayoffset is odd, we 
	 * have to deal with a char array (and hence elemsize==1). We copy one element
	 * more than actually necessary and restore it again after the copy has been done
	 */
	int csize=lsize<rsize?lsize:rsize;
	if(arr_offset&1) {
		assert(elemsize==1);
		get_memory_from(((char *)local)+arr_offset,WHERE(remote),((char *)SENDFOBHOME(remote))+arr_offset,elemsize*csize);
	} else p_r_object(((char *)local)+arr_offset,latm,((char *)remote)+arr_offset,ratm,csize*elemsize,eltag,csize);
}
void p_w_array(FOB remote,int ratm,int rsize,int arr_offset,void *local,int latm,int lsize,int elemsize,int eltag)
{
	/* we cannot have an odd arrayoffset (this would generate a pointer with the lowest
	 * bit 1, which is used to mark far pointers. If the arrayoffset is odd, we 
	 * have to deal with a char array (and hence elemsize==1). We copy one element
	 * less than actually necessary and copy the missing one at the end 
	 */
	int csize=lsize<rsize?lsize:rsize;
	if(arr_offset&1) {
		assert(elemsize==1);
		put_memory_to(WHERE(remote),((char *)SENDFOBHOME(remote))+arr_offset,((char *)local)+arr_offset,elemsize*csize);
	} else p_w_object(((char *)remote)+arr_offset,ratm,((char *)local)+arr_offset,latm,csize*elemsize,eltag,csize);
}
void p_c_array(FOB remote,int ratm,int rsize,int arr_offset,FOB local,int latm,int lsize,int elemsize,int eltag)
{
	int csize=lsize<rsize?lsize:rsize;
	if(NEAR(remote)) p_r_array(remote,ratm,rsize,arr_offset,local,latm,lsize,elemsize,eltag);
	else if(NEAR(local)) p_w_array(remote,ratm,rsize,arr_offset,local,latm,lsize,elemsize,eltag);
	else if(arr_offset&1) {
		/* see above for problems with an odd arr_offset */
		char *p;
		int s=csize*elemsize;
		MALLOC(p,s);
		get_memory_from(p,WHERE(local),((char *)SENDFOBHOME(local))+arr_offset,s);
		put_memory_to(WHERE(remote),((char *)SENDFOBHOME(remote))+arr_offset,p,s);
		FREE(p);
	} else
		p_c_object(((char *)remote)+arr_offset,ratm,((char *)local)+arr_offset,latm,csize*elemsize,eltag,csize);
}
void p_r_array_part(void *local,int latm,int lsize,int lbegin,int arr_offset,FOB remote,int ratm,int rsize,int rbegin,int elemsize,int elems,int eltag)
{
	int s;
	s=elems;
	if(lbegin+s>lsize) s=lsize-lbegin;
	if(rbegin+s>rsize) s=rsize-rbegin;
	if(s<=0) return;
	/* we assume that there won't be any odd pointers, unless elemsize==1 */
	if(elemsize==1) { 
		get_memory_from(((char *)local)+arr_offset+lbegin,WHERE(remote),((char *)SENDFOBHOME(remote))+arr_offset+rbegin,s);
	} else
		p_r_object(((char *)local)+arr_offset+lbegin*elemsize,latm,((char *)remote)+arr_offset+rbegin*elemsize,ratm,s*elemsize,eltag,s);
}
void p_w_array_part(FOB remote,int ratm,int rsize,int rbegin,int arr_offset,void *local,int latm,int lsize,int lbegin,int elemsize,int elems,int eltag)
{
	int s;
	s=elems;
	if(lbegin+s>lsize) s=lsize-lbegin;
	if(rbegin+s>rsize) s=rsize-rbegin;
	if(s<=0) return;
	if(elemsize==1) {
		put_memory_to(WHERE(remote),((char *)SENDFOBHOME(remote))+arr_offset+rbegin,((char *)local)+arr_offset+lbegin,s);
	} else
		p_w_object(((char *)remote)+arr_offset+rbegin*elemsize,ratm,((char *)local)+arr_offset+lbegin*elemsize,latm,s*elemsize,eltag,s);
}
void p_c_array_part(FOB remote,int ratm,int rsize,int rbegin,int arr_offset,FOB local,int latm,int lsize,int lbegin,int elemsize,int elems,int eltag)
{
	int s;
	s=elems;
	if(lbegin+s>lsize) s=lsize-lbegin;
	if(rbegin+s>rsize) s=rsize-rbegin;
	if(s<=0) return;
	if(NEAR(remote)) p_r_array_part(remote,ratm,rsize,rbegin,arr_offset,local,latm,lsize,lbegin,elemsize,elems,eltag);
	else if(NEAR(local)) p_w_array_part(remote,ratm,rsize,rbegin,arr_offset,local,latm,lsize,lbegin,elemsize,elems,eltag);
	else if(elemsize==1) {
		/* see above for problems with an odd arr_offset */
		char *p;
		MALLOC(p,s);
		get_memory_from(p,WHERE(local),((char *)SENDFOBHOME(local))+arr_offset+lbegin,s);
		put_memory_to(WHERE(remote),((char *)SENDFOBHOME(remote))+arr_offset+rbegin,p,s);
		FREE(p);
	} else
		p_c_object(((char *)remote)+arr_offset+rbegin*elemsize,ratm,((char *)local)+arr_offset+lbegin*elemsize,latm,s*elemsize,eltag,s);
}

static void reply_read_asize(vnn_t from,int asize,int *res)
{
	*res=asize;
}
static void request_read_asize(vnn_t from,int *asize,int *res)
{
	BR_REPLY_2(from,(BR_handler_2_t)reply_read_asize,*asize,(long)res);
}
int read_asize(FOB where)
{
	int i=-1;
	if(NEAR(where)) return *(int *)where;
	BR_REQUEST_2(WHERE(where),(BR_handler_2_t)request_read_asize,(long)SENDFOBHOME(where),(long)&i);
	am_wait_for(i>=0);
	return i;
}

void p_aref_aclear(FOB arr,int atm,int asize,int elemsize)
{
	/*
	 * we assume that atm is 1 (there seems to be no significantly faster
	 * way to do it if atm is 0)
	 */
	TA_SEMAPHORE(a);
	BR_REQUEST_3(WHERE(arr),(BR_handler_3_t)remote_amemclear,(long)arr,asize*elemsize,(long)a);
	TA_SEMA_WAIT(a);
}


INLINED_P_SEND_FOB
INLINED_P_RECV_FOB
INLINED_P_WHERE
INLINED_PS_WHERE
INLINED_PSR_WHERE
INLINED_PS_NEAR
INLINED_PSR_NEAR
INLINED_PS_FAR
INLINED_PSR_FAR
INLINED_PS_NEAR_OR_VOID
/*
FOB p_recv_fob(FOB pt,int cl)
{
	int a=CLUSTER(pt);
	if(cl==HERE || (a && ((unsigned long)pt&1)==0)) return pt;
	pt=(FOB)(((unsigned long)(pt)+CLUSTER_BIT((cl)-HERE)));
	if(CLUSTER(pt)==0) pt=(FOB)((unsigned long)pt&~1);
	else pt=(FOB)((unsigned long)pt|1);
	return pt;
}
*/

int do_memcmp(FOB a,FOB b,int s)
{
	void *pa,*pb;
	int i;
	int wa=WHERE(a);
	int wb=WHERE(b);
	if(wa==HERE && wb==HERE) return memcmp(a,b,s)==0;

	if(wa!=HERE) {
		MALLOC(pa,s);
		get_memory(pa,a,s);
	} else pa=a;
	if(wb!=HERE) {
		MALLOC(pb,s);
		get_memory(pb,b,s);
	} else pb=b;
	i=memcmp(pa,pb,s)==0;
	if(pa!=a) FREE(pa);
	if(pb!=b) FREE(pb);
	return i;
}

void p_sr_array(FOB (*sr)(FOB,int),int tag,void **pt,int cl,int size)
{
#ifdef PSATHER
	int i;
	if(sather_types[tag]->is_immutable) {
		/* if there are no references in the values, return
		   immediatly */
		char *p=(char *)pt;
		if(sather_types[tag]->refs[0]==-1) return;
		for(i=0;i<size;i++)
			p_sr_ob(sr,tag,(p+i*sather_types[tag]->size),cl);
	} else {
		for(i=0;i<size;i++) pt[i]=(*sr)(pt[i],cl);
	}
#endif
}

void p_sr_only_ob(FOB (*sr)(FOB,int),int tag,void *pt,int cl)
{
#ifdef PSATHER
	char *p=(char *)pt;
	struct sather_type_description *st=sather_types[tag];
	int *refs=st->refs;
	while(*refs>=0) {
		void *v;
		v= *((void **)(p+(*refs)));
		v= (*sr)(v,cl);
		*((void **)(p+(*refs)))=v;
		refs++;
	}
#endif
}

void p_sr_ob(FOB (*sr)(FOB,int),int tag,void *pt,int cl)
{
#ifdef PSATHER
	struct sather_attribute *ar,*as;
	int size;
	char *p=(char *)pt;
	struct sather_type_description *st;
	if(tag==0) {
		tag=((OB)pt)->header.tag;
		if(tag<0) p+=sather_types[tag]->boxed;
	}
	st=sather_types[tag];
	p_sr_only_ob(sr,tag,p,cl);
	if(st->is_aref) {
		ar= &st->attr[st->attrs+1];
		as= &st->attr[st->attrs];
		if(st->is_immutable) size=as->offset;
		else size= *(long *)(p+as->offset);
		p_sr_array(sr,ar->type,(void **)(p+ar->offset),cl,size);
	}
#endif
}

/*
 * Some functions used to broadcast changes to shareds to all clusters
 */
static void request_broadcast_global_r(vnn_t from,BR_word_t r, BR_word_t g, BR_word_t cnt)
{
	*(FOB*)g=(FOB)r;
	BR_REPLY_1(from,BR_signal_handler,cnt);
}
void broadcast_global_r(FOB *global)
{
	int i;
	BR_sema_t sm;
	sm=BR_SEMA_CREATE(0);
	for(i=0;i<CLUSTERS;i++) {
		if(i!=HERE) {
			FOB r= SENDFOB(*global,i);
			BR_REQUEST_3(i,(BR_handler_3_t)request_broadcast_global_r,(long)r,(long)global,(long)sm);
		}
	}
	for(i=0;i<CLUSTERS-1;i++) BR_WAIT(sm);
	BR_SEMA_DELETE(sm);
}

static void reply_broadcast_global_alloc(vnn_t from,BR_word_t p,BR_word_t l)
{
	*(void **)p=(void *)l;
}
static void request_broadcast_global_alloc(vnn_t from,BR_word_t size,BR_word_t p,BR_word_t loc)
{
	long *l;
	MALLOC(l,size+sizeof(long));
	*l++=loc;
	BR_REPLY_2(from,reply_broadcast_global_alloc,p,(BR_word_t)l);
}
static void store_broadcast_global(vnn_t from,caddr_t mem,size_t size,BR_word_t arg)
{
	void *g;
	g=*(((void **)mem)-1);
	amemcpy(g,mem,size);
	FREE(((void **)mem)-1);
	BR_REPLY_1(from,BR_signal_handler,arg);
}

void broadcast_global_va(void *local,void *remote,int size)
{
	int i;
	BR_sema_t sm;
	sm=BR_SEMA_CREATE(0);
	if(size==sizeof(FOB)) {
		long r=*(long *)local;
		for(i=0;i<CLUSTERS;i++) if(i!=HERE) {
			if(size==sizeof(FOB)) 
				BR_REQUEST_3(i,request_broadcast_global_r,(BR_word_t)r,(BR_word_t)remote,(BR_word_t)sm);
		}
	} else {
		void **p;CALLOC(p,sizeof(int)*CLUSTERS,1);
		for(i=0;i<CLUSTERS;i++) if(i!=HERE) 
			BR_REQUEST_3(i,request_broadcast_global_alloc,size,(BR_word_t)(p+i),(BR_word_t)remote);
		for(i=0;i<CLUSTERS;i++) if(i!=HERE) {
			am_wait_for(p[i]!=NULL);
			BR_STORE(i,local,p[i],size,store_broadcast_global,(BR_word_t)sm);
		}
		FREE(p);
	}
	for(i=0;i<CLUSTERS-1;i++) BR_WAIT(sm);
	BR_SEMA_DELETE(sm);
}

static void store_broadcast_global_v(vnn_t from,caddr_t mem,size_t size,BR_word_t arg)
{
	void *g;
	g=*(((void **)mem)-1);
	LOCKV(g);
	memcpy(g,mem,size);
	UNLOCKV(g);
	FREE(((void **)mem)-1);
	BR_REPLY_1(from,BR_signal_handler,arg);
}

void broadcast_global_v(void *local,void *remote,int size,int tg)
{
	int i;
	BR_sema_t sm;
	void **p;

	sm=BR_SEMA_CREATE(0);
	
	CALLOC(p,sizeof(int)*CLUSTERS,1);
	/* Initialize stuff - Claudio, Claudio ... */
	for(i=0; i<CLUSTERS; i++) p[i] = NULL;
	for(i=0;i<CLUSTERS;i++) if(i!=HERE) 
		BR_REQUEST_3(i,request_broadcast_global_alloc,size,(long)(p+i),(long)remote);
	for(i=0;i<CLUSTERS;i++) if(i!=HERE) {
		am_wait_for(p[i]!=NULL);
		SENDOB(tg,local,i);
		BR_STORE(i,local,p[i],size,store_broadcast_global_v,(BR_word_t)sm);
		RECVOB(tg,local,i);
	}
	FREE(p);

	for(i=0;i<CLUSTERS-1;i++) BR_WAIT(sm);
	BR_SEMA_DELETE(sm);
}
