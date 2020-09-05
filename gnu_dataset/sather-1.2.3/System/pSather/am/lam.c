#ifdef LANAI_NET
/*									8
 *
 * lam.c - Lanai Active Messages 
 *
 * 
 * "Copyright (c) 1995 The Regents of the University of California.
 * All rights reserved.
 * 
 * Permission to use, copy, modify, and distribute this software and its
 * documentation for any purpose, without fee, and without written agreement is
 * hereby granted, provided that the above copyright notice and the following
 * two paragraphs appear in all copies of this software.
 * 
 * IN NO EVENT SHALL THE UNIVERSITY OF CALIFORNIA BE LIABLE TO ANY PARTY FOR
 * DIRECT, INDIRECT, SPECIAL, INCIDENTAL, OR CONSEQUENTIAL DAMAGES ARISING OUT
 * OF THE USE OF THIS SOFTWARE AND ITS DOCUMENTATION, EVEN IF THE UNIVERSITY OF
 * CALIFORNIA HAS BEEN ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
 * 
 * THE UNIVERSITY OF CALIFORNIA SPECIFICALLY DISCLAIMS ANY WARRANTIES,
 * INCLUDING, BUT NOT LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY
 * AND FITNESS FOR A PARTICULAR PURPOSE.  THE SOFTWARE PROVIDED HEREUNDER IS
 * ON AN "AS IS" BASIS, AND THE UNIVERSITY OF CALIFORNIA HAS NO OBLIGATION TO
 * PROVIDE MAINTENANCE, SUPPORT, UPDATES, ENHANCEMENTS, OR MODIFICATIONS."
 *
 * Author: 		Richard P. Martin and Lok T. Liu
 * Version:		7
 * Creation Date:	Tue Feb 28 18:15:42 1995
 * Filename:		lam.c
 * History:
 *	RPM 7 Tue Aug 22 10:03:49 1995
 *		added long routes, sleep.
 *	RPM 2 Sat Jun  3 19:56:10 1995
 *		added relative addressing code. Fixed Q size problem.
 */

#include <stdio.h>
#include <stdlib.h>
#include <unistd.h>
#include <string.h>
#include <sys/types.h>
#include <sys/stat.h>
#include <fcntl.h>
#include <config.h>    /* LANAI definitions */
#include <lanai_dev.h> /* LANAI definitions */
#include <signal.h>
#include <sys/types.h>
#include <sys/time.h>
#include <sys/systeminfo.h> 
#include <errno.h>
#include "../config/lanai/lcp/ep.h"
#include "../config/lanai/lcp/ifc.h"
#include "am_int.h"

#ifdef DEBUG
#include <assert.h>
#endif

#define gethostname(n,l)  sysinfo(SI_HOSTNAME,n,l)

#ifdef __GNUC__
#define INLINE inline 
#else 
#define INLINE 
#endif

extern int gettimeofday(struct timeval *,void *);

static volatile int debug_am=1;
static unsigned short sts;
static int copy_block_len, units;
static int unit;

static volatile ep_t *endpt;
static volatile route_t *routes;
static volatile desc_t *in_d, *out_d;
static int in_num, out_num;

static volatile int *dma_base;
static volatile int *user_base;

static int *lam_dma_base;

int am_my_cluster_id;
int am_clusters = 2;

static int lam_reply = 0;  /* which node to reply to */

static volatile int flow_cntl[MAX_REMOTE];
static volatile int req_flow_in[MAX_REMOTE];
static volatile int req_flow_out [MAX_REMOTE];

#ifdef DEBUG
static int am_poll_count = 0;
#endif

static int rep_flow_in[MAX_REMOTE];
static int rep_flow_out[MAX_REMOTE];

static int lam_sync_flag[MAX_REMOTE];
static int lam_sync_count, lam_old_count;

typedef void (*handler_5_t)(vnn_t,long,long,long,long,long);
typedef void (*xfer_handler_t)(int src, int arg0, int arg1, int arg2);
extern unsigned int LANAI_fd[64];


#ifdef AM_THREADS
# ifdef SOLARIS_THREADS
static mutex_t am_llock;
#define LAM_LOCK mutex_lock(&am_llock)
#define LAM_UNLOCK mutex_unlock(&am_llock)
static mutex_t am_flock[MAX_REMOTE];
#define LAM_FLOCK(r) mutex_lock(&am_flock[(r)])
#define LAM_FUNLOCK(r) mutex_unlock(&am_flock[(r)])
# else
#  error "Don't know how to handle mutual exclusion"
# endif
#else
#define LAM_LOCK
#define LAM_UNLOCK
#define LAM_FLOCK(r)
#define LAM_FUNLOCK(r)
#endif


int am_procs() { return am_clusters; }
int am_my_proc() { return am_my_cluster_id; }
int am_max_size() { return LAM_PKT_WORDS * 4; }
static void lam_null_handler(int src) { }

#ifdef AM_THREADS
static AM_START_THREAD_DCL(desc_t *in_d)
{
	AM_INIT_THREAD;
	((handler_4_t)in_d->hdr.s_data[0])(in_d->hdr.s_data[5], in_d->hdr.s_data[1], in_d->hdr.s_data[2], in_d->hdr.s_data[3],
		      in_d->hdr.s_data[4]);
	free(in_d);
}

#endif /* AM_THREADS */ad
void am_poll()
{
	register int op_dest, src;

#ifdef AM_THREADS
	desc_t *in_dc;
	if (!thread_may_poll()) { 
		YIELD;
		return; 
	}
#endif

#ifdef DEBUG
	am_poll_count++;
#endif 

	while((in_d->op_dest & OP_MASK)  != 0 ) { /* if buffer has anything */
		op_dest = in_d->op_dest;
		src = (unsigned) (op_dest & DEST_MASK) >> DEST_SHIFT;

		if((op_dest & REQ_MASK) == REQUEST) {
			lam_reply = src;
#ifdef DEBUG
			req_flow_in[src]++;
#endif 
		} else {
			lam_reply = -1;
			LAM_FLOCK(src);
			flow_cntl[src]++;
#ifdef DEBUG
			rep_flow_in[src]++;
#endif 
			LAM_FUNLOCK(src);

		}

		switch(op_dest&OP_MASK) {
#ifdef AM_THREADS
		case THREAD:
			/* we need the copy as it has to survive until
			   the thread starts */
			in_dc=malloc(sizeof(desc_t));
			memcpy((void *)in_dc,(void *)in_d,sizeof(desc_t));
			in_dc->hdr.s_data[5]=src; /* need to store src */
			AM_START_THREAD(in_dc);
			in_d->op_dest = in_d->op_dest & (~OP_MASK) ;
			in_num = (in_num + 1) & IN_SIZE_MASK;
			in_d = &(endpt->in[in_num]);
			break;
#else
		case THREAD:
			fprintf(stderr,"Got the request to start a thread from node %d, cannot be done!\n",src);
			abort();
			break;
#endif
		case SHORT:
			((handler_5_t)in_d->hdr.s_data[0])(src, in_d->hdr.s_data[1], in_d->hdr.s_data[2], in_d->hdr.s_data[3],
			     					      in_d->hdr.s_data[4], in_d->hdr.s_data[5]);

			in_d->op_dest = in_d->op_dest & (~OP_MASK) ;
			in_num = (in_num + 1) & IN_SIZE_MASK;
			in_d = &(endpt->in[in_num]);
			break;
		case BULK:
			{
			register uint word1, word2, word3, word4, word5;
			word5 = in_d->hdr.b_hdr.data[3]; /* number of unaligned bytes */
			word1 = in_d->hdr.b_hdr.len;      /* length the Lanai gives us */
			word2 = in_d->hdr.b_hdr.data[0]; /* fun pointer */
			word3 = in_d->hdr.b_hdr.data[1]; /* dest address */
			word4 = in_d->hdr.b_hdr.data[2]; /* arg */

			if (word5 == 0) {

				memcpy((char *) word3, (char * ) ((uint * ) user_base + (LAM_PKT_WORDS * in_num)),word1) ;
			} else {
				if (word1 > 4) {
					word1 = ( (word1 - 4) & (~0x3)) + word5;
				} else {
					word1 = word5;
				}
				memcpy((char *) word3,(char *) ((uint * ) user_base + (LAM_PKT_WORDS * in_num)),word1);
			}
			((xfer_handler_t) word2)(src, word3, word1, word4);
			in_d->op_dest = in_d->op_dest & (~OP_MASK) ;
			in_num = (in_num + 1) & IN_SIZE_MASK;
			in_d = &(endpt->in[in_num]);
			}
			break;
		case BAD_CRC:
		default:
			printf("Wank! got crc error.\n");
			printf("in_num = 0x%0x in_d = 0x%0x\n", in_num, (unsigned int)in_d);
			printf("op_dest= 0x%0x s_data[0]= 0x%0x s_data[1]= 0x%0x\n", in_d->op_dest, in_d->hdr.s_data[0], in_d->hdr.s_data[1]);
			printf("s_data[2]= 0x%0x s_data[3]= 0x%0x s_data[4]= 0x%0x\n", in_d->hdr.s_data[2], in_d->hdr.s_data[3], in_d->hdr.s_data[4]);
			printf("s_data[5]= 0x%0x crc= 0x%0x\n", in_d->hdr.s_data[5], in_d->crc);

			in_d->op_dest = in_d->op_dest & (~OP_MASK) ;
			in_num = (in_num + 1) & IN_SIZE_MASK;
			in_d = &(endpt->in[in_num]);
			break;
		}

		if ( (op_dest & REQ_MASK) == REQUEST ) { /* was a request or reply? */
			if (lam_reply == src) {
				am_reply_0(src, (handler_0_t) lam_null_handler);
			}
		}
	}
}


static INLINE int am_request(int dest, handler_5_t fun, int arg0, int arg1, int arg2, int arg3, int arg4, int type) 
{
	int i;
/*#ifdef DEBUG*/
	int print_times = 0;
/*#endif */
#ifdef AM_THREADS
	int sigs;
	if((sigs=THR_SIGNAL_ENABLED)) THR_BLOCK_SIGNAL;
#endif
	if (dest != am_my_cluster_id) {

		LAM_LOCK;
		while (flow_cntl[dest] == 0) {
#ifdef AM_THREADS
			LAM_UNLOCK;
			YIELD;
			LAM_LOCK;
#else
			am_poll();
#endif

/*#ifdef DEBUG      */
			print_times++;
			if((print_times&63)==63)printf("req_4 node %d out of credits to %d times %d\r",
	     am_my_cluster_id,dest,print_times);
/*#endif */
		}

#ifdef DEBUG
		assert( (dest >= 0) && (dest < am_clusters));

		while ( ((out_d->op_dest) & OP_MASK) != 0) {
			am_poll();
			printf("\nWarning, am_request_5 found output queue full! \n");
		}
#endif 

		LAM_FLOCK(dest);
		flow_cntl[dest]--;
#ifdef DEBUG
		req_flow_out[dest]++;
#endif 
		LAM_FUNLOCK(dest);

		out_d->hdr.s_data[0] = (int) fun;
		out_d->hdr.s_data[1] = (int) arg0;
		out_d->hdr.s_data[2] = (int) arg1;
		out_d->hdr.s_data[3] = (int) arg2;
		out_d->hdr.s_data[4] = (int) arg3;
		out_d->hdr.s_data[5] = (int) arg4;

		(volatile) out_d->op_dest = i =  (int) (AM_TYPE) | (dest << DEST_SHIFT) | (REQUEST) | (type);

		out_num = (out_num + 1) & OUT_SIZE_MASK;
		out_d = &(endpt->out[out_num]);

#ifndef AM_THREADS
		am_poll();
#endif
		LAM_UNLOCK;
	} else {
		(*fun)(am_my_cluster_id, arg0, arg1, arg2, arg3, arg4);
#ifndef AM_THREADS
		am_poll();
#endif
	}
#ifdef AM_THREADS
	if(sigs) THR_ENABLE_SIGNAL;
#endif
	return 0;
}


INLINE int am_request_0(vnn_t dest, handler_0_t h)     			   
{ return am_request(dest, (handler_5_t)h, 1, 2, 3, 4, 5, SHORT); }

INLINE int am_request_1(vnn_t dest, handler_1_t h, long a1)     		   
{ return am_request(dest, (handler_5_t)h, a1, 2, 3, 4, 5, SHORT); }

INLINE int am_request_2(vnn_t dest, handler_2_t h, long a1, long a2)     	   
{ return am_request(dest, (handler_5_t)h, a1, a2, 3, 4, 5, SHORT); }

INLINE int am_request_3(vnn_t dest, handler_3_t h, long a1, long a2, long a3) 
{ return am_request(dest, (handler_5_t)h, a1, a2, a3, 4, 5, SHORT); }

INLINE int am_request_4(vnn_t dest, handler_4_t h, long a1, long a2, long a3, long a4) 
{ return am_request(dest, (handler_5_t)h, a1, a2, a3, a4, 5, SHORT); }

static INLINE int am_request_5(vnn_t dest, handler_5_t h, long a1, long a2, long a3, long a4, long a5) 
{ return am_request(dest, (handler_5_t)h, a1, a2, a3, a4, a5, SHORT); }

int am_request_df(vnn_t dest, handler_df_t fun, long arg2, long arg3, double d1) 
{
	int *i;
	/* code assume a double is 2x int. should work on Sparc v8 */
	i = (int *) & d1;
	return am_request_4(dest, (handler_4_t)fun, arg2, arg3, *i, *(i + 1));
}

#ifdef AM_THREADS
/*
 * Starts a new thread on dest with upto 4 interger arguments. Returns -1 on
 * encountering any error and sets AM_errno, return 0 otherwise.
 */
INLINE int thr_create_4(vnn_t dest,
handler_4_t fun,
long arg1, long arg2,
long arg3, long arg4)
{
	desc_t *in_d;
	int sigs;
	if((sigs=THR_SIGNAL_ENABLED)) THR_BLOCK_SIGNAL;

	if(dest != am_my_cluster_id) {
		am_request(dest,(handler_5_t)fun,arg1,arg2,arg3,arg4,0,THREAD);
	} else {
		in_d=malloc(sizeof(desc_t));
		in_d->hdr.s_data[0]=(int)fun;
		in_d->hdr.s_data[5]=dest;
		in_d->hdr.s_data[1]=arg1;
		in_d->hdr.s_data[2]=arg2;
		in_d->hdr.s_data[3]=arg3;
		in_d->hdr.s_data[4]=arg4;
		AM_START_THREAD(in_d);
	}
	if(sigs) THR_ENABLE_SIGNAL;
	return(0);
}
int thr_create_0(vnn_t dest,handler_0_t h)     			   { return thr_create_4(dest,(handler_4_t)h,1,2,3,4); }
int thr_create_1(vnn_t dest,handler_1_t h,long a1)     		   { return thr_create_4(dest,(handler_4_t)h,a1,2,3,4); }
int thr_create_2(vnn_t dest,handler_2_t h,long a1,long a2)     	   { return thr_create_4(dest,(handler_4_t)h,a1,a2,3,4); }
int thr_create_3(vnn_t dest,handler_3_t h,long a1,long a2,long a3) { return thr_create_4(dest,(handler_4_t)h,a1,a2,a3,4); }
int thr_create_df(vnn_t dest, handler_df_t fun, long arg2, long arg3, double d1) 
{
	int *i;
	/* code assume a double is 2x int. should work on Sparc v8 */
	i = (int *) & d1;
	return thr_create_4(dest, (handler_4_t)fun, arg2, arg3, *i, *(i + 1));
}
#endif /* AM_THREADS */

INLINE int am_store(int dest,void *lva,void *rva,int nbytes,handler_mem_t fun,void *arg0) 
{
	int i;

	if (nbytes <= (LAM_PKT_WORDS * sizeof(int))) {
		LAM_LOCK;
		if ( dest != am_my_cluster_id ) {
			while (flow_cntl[dest] == 0) {
#ifdef AM_THREADS
				LAM_UNLOCK;
				YIELD;
				LAM_LOCK;
#else
				am_poll();
#endif
			}

#ifdef DEBUG      
			assert(nbytes > 0 );   /* transfer should be >0 < max size */
			assert(nbytes <= LAM_PKT_WORDS * 4 );

			assert( (dest >= 0) && (dest < am_clusters));  /* dest oK? */
			while ( ((out_d->op_dest) & OP_MASK) != 0) {
				am_poll();
				printf("\nWarning, am_store found output queue full! \n");
			}
#endif 

			LAM_FLOCK(dest);
			flow_cntl[dest]--;
#ifdef DEBUG
			req_flow_out[dest]++;
#endif 
			LAM_FUNLOCK(dest);

			/* copy into io space buffer */
			/* need to check max len, assume that all buffers are word aligned */
			/* and in multiple of words */

			memcpy((int *) (user_base + ((LAM_PKT_WORDS * IN_Q_SIZE) +  (out_num * LAM_PKT_WORDS))), (char *) lva,
			     nbytes);

			out_d->hdr.b_hdr.la = (int *) lam_dma_base +  (LAM_PKT_WORDS * IN_Q_SIZE) + (out_num * LAM_PKT_WORDS);

			out_d->hdr.b_hdr.len = nbytes;
			out_d->hdr.b_hdr.data[0] = (int) fun;
			out_d->hdr.b_hdr.data[1] = (int) rva;
			out_d->hdr.b_hdr.data[2] = (int) arg0;
			out_d->hdr.b_hdr.data[3] = (nbytes & 0x3); /* store 'extra' bytes */

			if ( (nbytes & 0x3) != 0) {  /* round up to upper byte */
				out_d->hdr.b_hdr.len = (nbytes + 4) & (~0x3);
			}

			out_d->op_dest = i =  (int) (AM_TYPE) | (dest << DEST_SHIFT) | (REQUEST) | (BULK); /*go*/

			out_num = (out_num + 1) & OUT_SIZE_MASK;
			out_d = &(endpt->out[out_num]);
#ifndef AM_THREADS
			am_poll();
#endif
		} else {
			memmove(rva, lva, nbytes);
			(*fun)(dest, rva, nbytes, arg0);
#ifndef AM_THREADS
			am_poll();
#endif
		}
		LAM_UNLOCK;
	} else {
		printf("am_store called with size > %d\n", LAM_PKT_WORDS * sizeof(int));
	}
	return 0;
}

int am_reply_4(vnn_t dest, handler_4_t fun, long arg0, long arg1, long arg2, long arg3) 
{
	if ( dest != am_my_cluster_id ) {
		LAM_LOCK;
#ifdef DEBUG      
		assert( ((out_d->op_dest) & OP_MASK) == 0 );
		assert( (dest >= 0) && (dest < am_clusters));  /* dest oK? */
		rep_flow_out[dest]++;
#endif 
		lam_reply = -1;

		out_d->hdr.s_data[0] = (int) fun;
		out_d->hdr.s_data[1] = (int) arg0;
		out_d->hdr.s_data[2] = (int) arg1;
		out_d->hdr.s_data[3] = (int) arg2;
		out_d->hdr.s_data[4] = (int) arg3;

		out_d->op_dest = (int)  (AM_TYPE) | (dest << DEST_SHIFT) | (REPLY) | (SHORT);

		out_num = (out_num + 1) & OUT_SIZE_MASK;
		out_d = &(endpt->out[out_num]);
		LAM_UNLOCK;
	} else {
		(*fun)(am_my_cluster_id, arg0, arg1, arg2, arg3);
	}
	return 0;
}

int am_reply_0(vnn_t dest,handler_0_t h)     			   
{ return am_reply_4(dest,(handler_4_t)h,1,2,3,4); }

int am_reply_1(vnn_t dest,handler_1_t h,long a1)     		   
{ return am_reply_4(dest,(handler_4_t)h,a1,2,3,4); }

int am_reply_2(vnn_t dest,handler_2_t h,long a1,long a2)     	   
{ return am_reply_4(dest,(handler_4_t)h,a1,a2,3,4); }

int am_reply_3(vnn_t dest,handler_3_t h,long a1,long a2,long a3)   
{ return am_reply_4(dest,(handler_4_t)h,a1,a2,a3,4); }

int am_reply_df(vnn_t dest, handler_df_t fun, long arg2, long arg3, double d1) 
{
	int *i;
	i = (int *) & d1;
	return am_reply_4(dest, (handler_4_t)fun, arg2, arg3, *i, *(i + 1));
}

void am_reply_xfer(int dest, void *lva, void *rva, int nbytes, void (*fun)(), void *arg0) 
{

	if ( dest != am_my_cluster_id ) {
		LAM_LOCK;
#ifdef DEBUG
		assert( ((out_d->op_dest) & OP_MASK) == 0 );
		assert( (dest >= 0) && (dest < am_clusters));  /* dest oK? */
		rep_flow_out[dest]++;
#endif 
		lam_reply = -1;

		/* copy into io space buffer */
		/* need to check max len, assume that all buffers are word aligned */
		/* and in multiple of words */
		memcpy((char *)(user_base+((LAM_PKT_WORDS * IN_Q_SIZE)+(out_num * LAM_PKT_WORDS))),(char *) lva,nbytes);
		out_d->hdr.b_hdr.la = (int *) lam_dma_base +  (LAM_PKT_WORDS * IN_Q_SIZE) + (out_num * LAM_PKT_WORDS);

		out_d->hdr.b_hdr.len = nbytes;
		out_d->hdr.b_hdr.data[0] = (int) fun;
		out_d->hdr.b_hdr.data[1] = (int) rva;
		out_d->hdr.b_hdr.data[2] = (int) arg0;
		out_d->hdr.b_hdr.data[3] = (nbytes & 0x3); /* store 'extra' bytes */

		if ( (nbytes & 0x3) != 0) {
			out_d->hdr.b_hdr.len = (nbytes + 4) & (~0x3);
		}

		out_d->op_dest = (int) (AM_TYPE) | (dest << DEST_SHIFT) | (REPLY) | (BULK);
		out_num = (out_num + 1) & OUT_SIZE_MASK;
		out_d = &(endpt->out[out_num]);
		LAM_UNLOCK;
	} else {
		memmove(rva, lva, nbytes);
		(*fun)(dest, rva, nbytes, arg0);
	}
}


int am_get(vnn_t dest, void *rva, void *lva, int len, handler_mem_t handler, void *arg) 
{

#ifdef DEBUG
	assert(len > 0 ) ;
	assert(len <= LAM_PKT_WORDS * 4 );
	assert( ((out_d->op_dest) & OP_MASK) == 0 );
#endif 
	return am_request_5(dest, (handler_5_t)am_reply_xfer, (int) rva, (int) lva, len, (int) handler, (int) arg);
}


int am_store_async(vnn_t dest_node, void *src_buf, void *dest_buf,
int len, handler_mem_t request_handler, void *request_handler_arg1,
handler_mem_t endfunc, void *endfunc_arg1) 
{
	am_store(dest_node,src_buf,dest_buf,len,request_handler,request_handler_arg1);
	(*endfunc)(am_my_cluster_id, src_buf, len, endfunc_arg1);
	return 0;
}

static void lam_sync_rep_h(vnn_t src,long arg0)
{ lam_sync_count = arg0; }

static void lam_sync_req_h(vnn_t src)
{
	if (lam_sync_flag[src] == 0) {
		lam_sync_flag[src] = 1;
		lam_sync_count++;
	}
	am_reply_1(src, lam_sync_rep_h,lam_sync_count);
}

/*
 * may be used ONLY during lam_sync_call
 */
static void lam_restore_flow(int des) 
{
	if (flow_cntl[des] < MAX_DEPTH)
		flow_cntl[des]++;
}


/* really dumb way to sync all nodes */
/* all send to zero, when they get back that all have sent, they */
/* continue */

static int lam_sync_all() 
{
	int i;
	int done;
	int tries;
	int total_time;

	struct timeval tv_begin, tv_end, lapsed;

	done = 0;
	tries = 0;
	lam_sync_count = lam_old_count = 0;

	if (am_my_cluster_id != 0) {
		while (!done) {

			/* issue request */
			tries++;
			am_request_0(0,lam_sync_req_h);

			/* wait for reply or time out */
			gettimeofday(&tv_begin,NULL);
			total_time = 0;

			/* wait 1000 msec for zero to respond thread to change tick value */
			while ( (total_time < 200000) && (lam_sync_count == lam_old_count)) {
				gettimeofday(&tv_end,NULL);
				if (tv_begin.tv_usec > tv_end.tv_usec) {
					tv_end.tv_usec += 1000000;
					tv_end.tv_sec--;
				}
				lapsed.tv_usec = tv_end.tv_usec - tv_begin.tv_usec;
				lapsed.tv_sec =  tv_end.tv_sec - tv_begin.tv_sec;
				total_time = lapsed.tv_sec * 1000000 + lapsed.tv_usec ;

				am_poll();
			}
			if (lam_sync_count == am_clusters)
				done = 1;

			if (tries > 1000)
				done = 1;


			if (lam_old_count == lam_sync_count)
				lam_restore_flow(0);
			else
				lam_old_count = lam_sync_count;

		}

		if (lam_old_count == 0) {
			printf(" node zero not responding \n");
		} else {
			if (lam_sync_count != am_clusters) {
				printf(" some nodes not responding \n");
			}
		}
	} /* if I am not zero */ else { /* am zero, wait for up to 15 seconds before giving up */
		lam_sync_count++;

		gettimeofday(&tv_begin,NULL);
		total_time = 0;

		while ( !done ) {
			am_poll();

			gettimeofday(&tv_end,NULL);
			if (tv_begin.tv_usec > tv_end.tv_usec) {
				tv_end.tv_usec += 1000000;
				tv_end.tv_sec--;
			}
			lapsed.tv_usec = tv_end.tv_usec - tv_begin.tv_usec;
			lapsed.tv_sec =  tv_end.tv_sec - tv_begin.tv_sec;
			total_time = lapsed.tv_sec * 1000000 + lapsed.tv_usec ;

			if (total_time > 25000000 )
				done = 1;

			if ( lam_sync_count == am_clusters)
				done = 1;

		}

		if (lam_sync_count != am_clusters) {
			printf("proc %d: nodes which did not respond: ", am_my_cluster_id);
			for (i = 1; i < am_clusters; i++) {
				if (lam_sync_flag[i] == 0) printf("%d ", i);
			}
			printf("%%\n");
		}
	}

	if (lam_sync_count != am_clusters)
		return - 1;

	return 1;
}

extern solaris_thread_t poll_manager_id;
void lam_route_display() {
  int i,u;

  for(i=0;i<am_clusters;i++) {
    printf("Route %d ",i);
    for(u=0;u<4;u++) {
      printf(" %x", routes[i].path[u]);
    }
    printf("\n");
  }
}

static void check_lock()
{
	FILE *f;
	int i,j;
	char buf[10];
	f=fopen("/tmp/LANAI_AM","r");
	if(f!=NULL) {
		fread(buf,1,10,f);
		i=atoi(buf);
		j=kill(i,SIGKILL);
		if(j<0 && errno==EPERM) {
			fprintf(stderr,"There seems to be another process that uses\n");
			fprintf(stderr,"the LAM package (pid %d). I cannot kill this\n",i);
			fprintf(stderr,"process. If this is wrong, remove\n");
			fprintf(stderr,"the file /tmp/LANAI_AM and try again.\n");
			exit(-1);
		}
		fclose(f);
	}
	close(creat("/tmp/LANAI_AM",0666));
	chmod("/tmp/LANAI_AM",0666);
	f=fopen("/tmp/LANAI_AM","w");
	fprintf(f,"%d",(int)getpid());
	fclose(f);
}

static void remove_lock()
{
	unlink("/tmp/LANAI_AM");
}

void am_enable(int nodes,int argc,char *argv[]) 
{
	int i, j, hop;
	char *first_dot;
	char *config_path;
	static FILE *config_file;
	static char config_filename[256];
	static char config_dir[256];
	static char exec[512];
	char lcp_file[256];
	char node_name[256];
	char my_hostname[256];
	char temp_name[256];
	char paths[MAX_REMOTE][MAX_HOPS]; /* eight hops for now */
	char *dest_route, temp_path[8];

	check_lock();

	
	if ( ((config_path = getenv("LAM_CONFIG")) == (char *)NULL) ) {
		/* use current dir as default */
		if((config_path=getenv("SATHER_HOME" ))!=NULL) { 
			strcpy(config_dir,config_path);
			strcat(config_dir,"/System/Platforms/old/myrinet");
		} else strcpy(config_dir, ".");
	} else
		strcpy(config_dir, config_path);

	strcpy(lcp_file,config_dir);
	strcat(lcp_file,"/lcp.dat");

	unit = 0;
	units = open_lanai_copy_block(&copy_block_len, &sts);
	if (copy_block_len <= 0) {
		perror("ops: open_copy_block()");
		printf("error! Length = %d  <= 0\n", copy_block_len);
		abort();
	} else {
#ifdef LAM_DEBUG
		printf("ops: User opened copy_block \n");
		printf("ops: user_ptr = %x    dma_ptr = %x    copy_block_len = 0x%x\n", (unsigned)UBLOCK[unit], (unsigned)DBLOCK[unit],
		     copy_block_len);
#endif
	}

	if ( copy_block_len <  ((IN_Q_SIZE + OUT_Q_SIZE) * LAM_PKT_WORDS * sizeof(int))) {

		fprintf(stderr, "warning! copy_block_len to short, len %d bytes wanted %d\n", copy_block_len, (IN_Q_SIZE
		    + OUT_Q_SIZE) * LAM_PKT_WORDS * sizeof(int));
		abort();
	}

	lanai_interrupt(0);
	lanai_reset(1);
	clear_load(0, lcp_file);
	memset((void *)LANAI[0], 0, 128 * 1024) ;
	clear_load(0, lcp_file);

	lanai_reset(0);
	lanai_interrupt(1);
	while (!L_INIT_DONE);   /* wait for lanai init done */

	/* set up routes */
	endpt = (ep_t * ) ( (char *) LANAI[0] +  ((uint)BULK_PTR * 2) );
	routes = (route_t * ) ( (char *) LANAI[0] + ((uint) ROUTE_TAB * 2) );

#ifdef LAM_DEBUG
	printf("LANAI BASE %0x, ROUTE_TAB %0x, IN_BASE %0x, OUT_BASE %0x, BULK_PTR %0x\n", (int)LANAI[0], ROUTE_TAB, IN_BASE,
	    OUT_BASE , BULK_PTR);
#endif

	am_clusters=nodes;

	if (am_clusters > MAX_REMOTE) {
		printf("too many nodes, max is %d\n", MAX_REMOTE);
		exit(-1);
	}

	am_my_cluster_id = -1;
	gethostname(my_hostname, 80);
	strcpy(temp_name, my_hostname);
	if((first_dot = (char *) strchr(temp_name, '.')) != (char *) NULL) {
		first_dot[0] = '\0';
	}
	strcpy(config_filename,config_dir);
	strcat(config_filename,"/");
	strcat(config_filename,temp_name);
	config_file = fopen(config_filename, "r");
	if(config_file==NULL) {
		fprintf(stderr,"am_enable: cannot open file `%s'\n",config_filename);
		abort();
	}

	for (i = 0; i < am_clusters; i++) {
		if (fscanf(config_file, "%s %s", (char *)&paths[i], node_name) == EOF) {
			fprintf(stderr, "Not enough nodes in configuration file\n");
			exit(-1);
		}
#ifdef LAM_DEBUG
		printf("node in net: %s\n", node_name);
#endif
		if (strcmp(temp_name, node_name) == 0) {
			am_my_cluster_id = i;
		}
	}

	fclose(config_file);

	if (am_my_cluster_id == -1) {
		fprintf(stderr, "could not find my node (%s) in the configuration file `%s'\n", my_hostname,config_filename);
		exit(1);
	}
	if(am_my_cluster_id==0) {
		sprintf(exec,"%s/start_nodes %d ",config_dir,nodes);
		for(i=0;i<argc;i++) {
			strcat(exec," \"");
			strcat(exec,argv[i]);
			strcat(exec,"\"");
		}
		system(exec);
	}
	for (i = 0; i < am_clusters; i++) {
		routes[i].len = strlen(paths[i]);
		for (j = 0; j <  strlen(paths[i]); j++) {

			/* really dumb encoding */
			/* 01234567 is +0 ..  +7 */
			/* ABCDEFGH is -O ..  -7 */

			hop = (int) (paths[i][j] - '0') ;

			/* try a negative route */
			if ( (hop < 0) || (hop > 7) ) {
				hop = paths[i][j] - 'A' ;
				hop = -hop;

				if ((hop > 0) || (hop < -7)) {
					printf("invalid hop entry %c to node %d hop %d\n", (char) hop, i, j);
					exit(1);
				}
			}
			temp_path[j] = ((hop & 0x3f) | 0x80);
		}

		dest_route = (char *) routes[i].path;

		switch (routes[i].len) {
		case 0: break;
		case 1: memcpy(&dest_route[1], temp_path, 1);
			break;
		case 2: memcpy(dest_route + 0, temp_path, 2);
			break;
		case 3: memcpy(dest_route + 1, temp_path, 3);
			break;
		case 4: memcpy(dest_route + 0, temp_path, 4);
			break;
		case 5: memcpy(dest_route + 3, temp_path, 5);
			break;
		case 6: memcpy(dest_route + 2, temp_path, 6);
			break;
		case 7: memcpy(dest_route + 1, temp_path, 7);
			break;
		case 8: memcpy(dest_route + 0, temp_path, 8);
			break;
		default:
			fprintf(stderr,"route too long \n");
			abort();
			break;
		} /* end switch */
	} /* end for am_clusters */

	/* set return path */
	LANAI[0][P_END_PT] = am_my_cluster_id;

	in_num = out_num = 0 ;
	in_d = &(endpt->in[in_num]);
	out_d = &(endpt->out[out_num]);

	/* set up flow control info */
	for (i = 0; i < am_clusters; i++) {
		flow_cntl[i] = (DEPTH);
		req_flow_out[i] = rep_flow_in[i] = 0;
		req_flow_in[i] = rep_flow_out[i] = 0;
		lam_sync_flag[i] = 0;
	}

	dma_base = lam_dma_base = (unsigned *)DBLOCK[0];
	user_base = (volatile int * ) UBLOCK[0];

	/* set up receive bulk packets  */
	for (i = 0; i < IN_Q_SIZE ; i++) {
		endpt->bulk_d_p[i].hi = (uint * ) dma_base + (LAM_PKT_WORDS * i);
	}

	/* set up send */
	for (i = 0; i < OUT_Q_SIZE ; i ++) {
		endpt->out[i].hdr.b_hdr.la = (int *) dma_base +  (LAM_PKT_WORDS * 2 * DEPTH * MAX_REMOTE) + (i * LAM_PKT_WORDS);

	};

	H_DEBUG_GO = 0;

	H_INIT_DONE = 1;
/*	lam_route_display(); */
	if(getenv("DEBUG_AM") || getenv("DEBUG_PSATHER")) {
		char com[200];
		if(getenv("DEBUG_AM_COMMAND"))
			sprintf(com,getenv("DEBUG_AM_COMMAND"),argv[0],getpid());
		else if(getenv("DEBUG_PSATHER_COMMAND"))  
			sprintf(com,getenv("DEBUG_PSATHER_COMMAND"),argv[0],getpid());
		else
			sprintf(com,"xterm -fn 7x13 -T \"%d ($hostname) %s\" -e gdb %s %d&\n",am_my_cluster_id,argv[0],argv[0],(int)getpid());
		system(com);
		while(debug_am);
	}
			
#ifdef AM_THREADS
	poll_manager_id=thr_self();
#endif
	if(lam_sync_all()<0) {
		fprintf(stderr,"am_enable(): some node are not running, aborting\n");
		abort();
	}
#ifdef AM_THREADS
	poll_manager_id= -1;
	T_INIT_MACHINE;
	T_INIT_CLUSTER;
	AM_INIT_THREAD;
#endif

}

void am_disable() { remove_lock(); }

#endif
