#ifdef MEIKO
/*                                                                      tab:4
 *
 * meiam.c - Active Message calls for the CS-2
 *
 * "Copyright (c) 1993 by Chad Yoshikawa and The Regents of the University
 * of California.  All rights reserved."
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
 * PROVIDE MAINTENANCE, SUPPORT, UPDATES, ENHANCEMENTS, OR MODIFICATIONS.
 *
 * Made threadsafe by Claudio Fleiner (fleiner@icsi.berkeley.edu), 12/15/95

 * Author:                      Chad Yoshikawa
 * Version:                     1
 * Filename:                    meiam.c
 * History:
 */

/* The MEIKO am library requires that you link it with queueThread.o.
 * However, you need a special compiler to compile queueuThread.o, so
 * it is included as a binary. The source is in config/meiko/src
 */

#include <stdio.h>
#include <malloc.h>
#include <sys/time.h>
#include <assert.h>
#include <sys/types.h>
#include <elan/elan.h>
#include <ew/ew.h>
#include <elan/event.h>
#include <stdlib.h>
#include <string.h>
#include "am_int.h"

/* if all threads are allowed to poll, define POLLING */
/* works only if spinlocks exist 		      */
#define POLLING

#ifdef POLLING
spinlock_t poll_lock=0;
#endif

/* Return maximum buffer size in bytes for put and get */
int am_max_size();
#define INT_PKT 1
#define THREAD_PKT 2 /* Paket is a thread creation */
#define GET_PKT 4
#define REQUEST_PKT 8 /*Packet is a request*/
#define REPLY_PKT 16  /*Packet is a reply*/
#define DMA_PKT 1024 /*Elan sends a DMA*/

#define MAX_NUM_OUT 256 /*Maximum number of requests outstanding*/

 
#define QUEUE_ENTRY_SIZE 32 /*to fit in one writeblock*/
#define QUEUE_ENTRY_WORDS 8 /*one for the flag*/
#define STACK_SIZE 8192
#define DEFAULT_QUEUE_SIZE 512

/*Order of values in each queue element:*/
#define QFLAG 7
#define QREMOTE_NODE 7
#define QARG1 1
#define QARG2 2
#define QARG3 3
#define QARG4 4
#define QARG5 5
#define QARG6 6
#define QTYPE 0
#define QFLAG2 0

#ifdef __GNUC__
#define INLINE inline 
#else 
#define INLINE 
#endif


typedef struct {
    /*Queue specific values on the remote node*/
    int *queue_base_ptr;   /*global ptr to the queue base address*/
    int queue_size;        /*size of the remote queue in N byte packets*/

    /*Interface between sparc and elan on the sending side*/
    volatile int *message_ptr;
    volatile int list_head;
} queue_t;

void Queue_Thread(queue_t *queue_ptr);
queue_t *CreateQueue();
int Push(queue_t *queue, int node, ELAN_DMA *dma, int arg0,int arg1,
	 int arg2, int arg3, int arg4, int arg5, int arg6, int blocking);
int Pop(queue_t *queue,int *arg0, int *arg1, int *arg2, int *arg3,
	int *arg4 ,int *arg5, int *arg6, int blocking);


/*
 * Globals for the Split-C library
 */

int ___PROCS;
#define PROCS ___PROCS
int ___MYPROC;
#define MYPROC ___MYPROC

/*
 * Update the number of flow-control credits
 */

#define UPDATE_K_OUT() {volatile unsigned int K_tmp;\
			    K_tmp = K_S;\
			    K_Out+=(K_tmp - K_S_Old);\
			    K_S_Old = K_tmp;}


unsigned volatile int K_S = 0; 
unsigned volatile int K_S_Old= 0; 

#define K_STORES_Out  (K_S - K_S_Old)
#define bcopy(a,b,c) memmove(b,a,c)

static volatile int debug_am=1;
volatile int did_reply=0;        /*Request handler did reply?*/
static volatile int K_Out = MAX_NUM_OUT;  /*max oustanding requests per processor*/

queue_t *am_queue; /*Queue for all incoming/outgonig active messages*/
int *am_queue_test;

/* Structures needed for stores*/
ELAN_DMA *am_store_dmaDesc;
ELAN_EVENT *am_store_dmaEvent;

static void private_am_barrier(void);

#ifdef AM_THREADS
# ifdef SOLARIS_THREADS
#  ifdef SPINLOCK_LOCK
    static spinlock_t send_lock;
#   define MEIAM_LOCK 		SPINLOCK_LOCK(send_lock)
#   define MEIAM_UNLOCK 	SPINLOCK_UNLOCK(send_lock)
    static spinlock_t request_lock;
#   define MEIAM_RLOCK 		SPINLOCK_LOCK(request_lock)
#   define MEIAM_RUNLOCK 	SPINLOCK_UNLOCK(request_lock)
    static spinlock_t k_out_lock;
#   define MEIAM_KLOCK 		SPINLOCK_LOCK(k_out_lock)
#   define MEIAM_KUNLOCK 	SPINLOCK_UNLOCK(k_out_lock)
#  else
#error should use spinlocks
    static mutex_t send_lock;
#   define MEIAM_LOCK 		mutex_lock(&send_lock)
#   define MEIAM_UNLOCK 	mutex_unlock(&send_lock)
    static mutex_t request_lock;
#   define MEIAM_RLOCK 		mutex_lock(&request_lock)
#   define MEIAM_RUNLOCK 	mutex_unlock(&request_lock)
    static mutex_t k_out_lock;
#   define MEIAM_KLOCK 		mutex_lock(&k_out_lock)
#   define MEIAM_KUNLOCK 	mutex_unlock(&k_out_lock)
#  endif
# else
#  error "Don't know how to handle mutual exclusion"
# endif
#else
#define MEIAM_LOCK
#define MEIAM_UNLOCK
#define MEIAM_KLOCK
#define MEIAM_KUNLOCK
#define MEIAM_RLOCK
#define MEIAM_RUNLOCK
#endif


/*Just for flow-control reasons...*/
static void bogus_handler(vnn_t from) { }


/* void timer_init() { } */

/* 'Hardware barrier' -- no polling */
void am_hw_barrier()
{
	ew_fgsync(ew_base.segGroup);
}

INLINE int am_procs(void) { return (ew_base.segGroup->g_size); }
INLINE int am_my_proc(void) { return (ew_base.segGroup->g_self); }

/* Disable active messages - clean up*/
void am_disable()
{
	/*Wait for requests to come back and elan to finish w/ 'stores'*/
	while ((K_Out+K_STORES_Out)<MAX_NUM_OUT)
		am_poll();

	private_am_barrier();
	am_hw_barrier();

}

int am_max_size()
{
    return 2147483647;/*MAXINT*/
}


void am_store_init()
{
    /*One dma descriptor for store DMAs (store is syncrhonous)*/
    am_store_dmaDesc = (ELAN_DMA *)memalign(8,sizeof(ELAN_DMA));
    am_store_dmaDesc->dma_type = DMA_TYPE(TR_TYPE_BYTE,DMA_NORMAL,8);
    am_store_dmaDesc->dma_destEvent = NULL;
    
    /*One event for store DMAs (store is syncrhonous)*/
    am_store_dmaEvent = (ELAN_EVENT *)memalign(8,sizeof(ELAN_EVENT));
    am_store_dmaDesc->dma_sourceEvent = am_store_dmaEvent;
    ELAN_CLEAREVENT(am_store_dmaEvent);
}


/*
 * Enable active message functions
 */
void am_enable(int cl,int argc,char *argv[])
{
    int size; /*queue_size*/
    
#ifdef POLLING
    thr_setprio(thr_self(),2);
#endif
    if(getenv("RMS_NPROCS")==NULL) {
	    char com[500];
	    int i;
	    sprintf(com,"prun -t -n %d ",cl);
	    putenv("AM_ALREADY_RUNNING=1");
	    for(i=0;i<argc;i++)
		    sprintf(com+strlen(com)," \"%s\"",argv[i]);
	    /* printf("Executing %s\n",com); */
	    system(com);
	    exit(0);
    } else {
	    int nn;
	    nn=atoi(getenv("RMS_NPROCS"));
	    if(nn!=cl) {
		    fprintf(stderr,"The program has been started on %d nodes,\nbut you requested %d, aborting ...\n",nn,cl);
		    exit(1);
	    }
   }

    ew_baseInit();
    
    am_store_init();
    
    ___MYPROC = am_my_proc();
    ___PROCS =am_procs();
    
    
    size = ew_rup2(K_Out*___PROCS);  /*KP storage per node*/

    /* Create a queue of size KP for outgoing requests and DMAs
     * and outgoing replies
     */
    am_queue = CreateQueue(size*2,___PROCS); 
    am_queue_test= &(am_queue->queue_base_ptr[QFLAG]);
    am_hw_barrier();
    am_clusters=am_procs();
    am_my_cluster_id=am_my_proc();

	if(getenv("DEBUG_AM") || getenv("DEBUG_PSATHER")) {
		char com[200];
		char *p;
		extern int getpid();
		p=getenv("DEBUG_NODE");
		if(p==NULL || atoi(p)==am_my_cluster_id) { 
			if(getenv("DEBUG_AM_COMMAND"))
				sprintf(com,getenv("DEBUG_AM_COMMAND"),argv[0],getpid());
			else if(getenv("DEBUG_PSATHER_COMMAND"))  
				sprintf(com,getenv("DEBUG_PSATHER_COMMAND"),argv[0],getpid());
			else
				sprintf(com,"xterm -fn 7x13 -T \"%d ($hostname) %s\" -e gdb %s %d&\n",am_my_cluster_id,argv[0],argv[0],(int)getpid());
			system(com);
			if(getenv("DEBUG_NOSTOP")==NULL) 
				while(debug_am);
		}
	}
#ifdef AM_THREADS
	T_INIT_MACHINE;
	T_INIT_CLUSTER;
	AM_INIT_THREAD;
#endif

}

#ifdef AM_THREADS
struct start_t_struct {
	handler_4_t func;
	vnn_t n;
	int a[4];
};
static AM_START_THREAD_DCL(struct start_t_struct *in_d)
{
	AM_INIT_THREAD;
	(*in_d->func)(in_d->n, in_d->a[0],in_d->a[1],in_d->a[2],in_d->a[3]);
	free(in_d);
}
#endif /* AM_THREADS */


/*
 * Send a request message to 'dest' processor
 */
INLINE int am_request(vnn_t dest, handler_4_t handler, long arg0,
		 long arg1, long arg2, long arg3, int flag)
{
    int head;
    volatile int *message_ptr;
    
    /*loopback*/
    if (dest==___MYPROC){
	(*handler)(dest,arg0,arg1,arg2,arg3);
    } else {
	/*Wait for responses if necessary*/
	MEIAM_RLOCK;
	while (!K_Out && (K_S==K_S_Old))
#if defined(AM_THREADS) && !defined(POLLING)
	    YIELD;
#else
	    am_poll();
#endif

	MEIAM_LOCK;
	head = am_queue->list_head;
	message_ptr = am_queue->message_ptr+(head*QUEUE_ENTRY_WORDS);
	
	assert(*(message_ptr+QREMOTE_NODE)==-1);
	
	message_ptr[QTYPE]=INT_PKT | REQUEST_PKT | flag;
	message_ptr[QARG1]=(long)handler;
	message_ptr[QARG2]=___MYPROC;
	message_ptr[QARG3]=arg0;
	message_ptr[QARG4]=arg1;
	message_ptr[QARG5]=arg2;
	message_ptr[QARG6]=arg3;
	message_ptr[QREMOTE_NODE]=(dest);
	am_queue->list_head=(head+1)&(am_queue->queue_size-1);
	
	MEIAM_KLOCK;
	UPDATE_K_OUT();
	K_Out--; /*One less outstanding message*/
	MEIAM_KUNLOCK;
	MEIAM_UNLOCK;
	MEIAM_RUNLOCK;
	
    }
#if defined(AM_THREADS) && !defined(POLLING)
    YIELD;
#else
    am_poll();/*Drain the network*/
#endif
    return 0; 
}

INLINE int am_request_4(vnn_t dest,handler_4_t h,long a1,long a2,long a3,long a4)
{ return am_request(dest,(handler_4_t)h,a1,a2,a3,a4,0); }
INLINE int am_request_3(vnn_t dest,handler_3_t h,long a1,long a2,long a3)
{ return am_request(dest,(handler_4_t)h,a1,a2,a3,0,0); }
INLINE int am_request_2(vnn_t dest,handler_2_t h,long a1,long a2)
{ return am_request(dest,(handler_4_t)h,a1,a2,0,0,0); }
INLINE int am_request_1(vnn_t dest,handler_1_t h,long a1)
{ return am_request(dest,(handler_4_t)h,a1,0,0,0,0); }
INLINE int am_request_0(vnn_t dest,handler_0_t h)
{ return am_request(dest,(handler_4_t)h,0,0,0,0,0); }
INLINE int am_request_df(vnn_t dest, handler_df_t fun, long arg2, long arg3, double d1) 
{
	int *i;
	/* code assume a double is 2x int. should work on Sparc v8 */
	i = (int *) & d1;
	return am_request_4(dest, (handler_4_t)fun, arg2, arg3, *i, *(i + 1));
}

#ifdef AM_THREADS
INLINE int thr_create_4(vnn_t dest,handler_4_t h,long a1,long a2,long a3,long a4)
{ 
	struct start_t_struct *in_d;
	int sigs;
	if((sigs=THR_SIGNAL_ENABLED)) THR_BLOCK_SIGNAL;

	if(dest != am_my_proc()) {
		am_request(dest,h,a1,a2,a3,a4,THREAD_PKT);
	} else {
		in_d=(struct start_t_struct *)malloc(sizeof(*in_d));
		in_d->func=h;
		in_d->n=dest;
		in_d->a[0]=a1;
		in_d->a[1]=a2;
		in_d->a[2]=a3;
		in_d->a[3]=a4;
		AM_START_THREAD(in_d);
	}
	if(sigs) THR_ENABLE_SIGNAL;
	return(0);
}
INLINE int thr_create_3(vnn_t dest,handler_3_t h,long a1,long a2,long a3)
{ return thr_create_4(dest,(handler_4_t)h,a1,a2,a3,0); }
INLINE int thr_create_2(vnn_t dest,handler_2_t h,long a1,long a2)
{ return thr_create_4(dest,(handler_4_t)h,a1,a2,0,0); }
INLINE int thr_create_1(vnn_t dest,handler_1_t h,long a1)
{ return thr_create_4(dest,(handler_4_t)h,a1,0,0,0); }
INLINE int thr_create_0(vnn_t dest,handler_0_t h)
{ return thr_create_4(dest,(handler_4_t)h,0,0,0,0); }
INLINE int thr_create_df(vnn_t dest, handler_df_t fun, long arg2, long arg3, double d1) 
{
	int *i;
	/* code assume a double is 2x int. should work on Sparc v8 */
	i = (int *) & d1;
	return thr_create_4(dest, (handler_4_t)fun, arg2, arg3, *i, *(i + 1));
}
#endif

INLINE int am_reply_4(vnn_t requesting_vnn, handler_4_t handler,
	       long arg0, long arg1, long arg2, long arg3)
{
    int head;
    volatile int *message_ptr ;
    
    if (requesting_vnn==___MYPROC)
	(*handler)(requesting_vnn,arg0,arg1,arg2,arg3);
    else{
#if defined(AM_THREADS) && !defined(POLLING)
	if(!thread_may_poll()) {
		fprintf(stderr,"trying to send a reply, which should be a request instead!\n");
		return am_request_4(requesting_vnn,handler,arg0,arg1,arg2,arg3);
	}
#endif
	MEIAM_LOCK;
	head = am_queue->list_head;
	message_ptr = am_queue->message_ptr+(head*QUEUE_ENTRY_WORDS);
	
	assert(*(message_ptr+QREMOTE_NODE)==-1);
	
	message_ptr[QTYPE]=INT_PKT | REPLY_PKT;
	message_ptr[QARG1]=(long)handler;
	message_ptr[QARG2]=___MYPROC;
	message_ptr[QARG3]=arg0;
	message_ptr[QARG4]=arg1;
	message_ptr[QARG5]=arg2;
	message_ptr[QARG6]=arg3;
	message_ptr[QREMOTE_NODE]=(requesting_vnn);
	am_queue->list_head=(head+1)&(am_queue->queue_size-1);
	
	/*Don't poll*/
	
#ifndef POLLING
	assert(!did_reply && thread_may_poll());
#endif
	did_reply = 1; /*The request handler did the reply*/
	MEIAM_UNLOCK;
    }
    return 0;
}

int am_reply_df(vnn_t dest, handler_df_t fun, long arg2, long arg3, double d1) 
{
	int *i;
	i = (int *) & d1;
	return am_reply_4(dest, (handler_4_t)fun, arg2, arg3, *i, *(i + 1));
}

int am_reply_3(vnn_t from,handler_3_t h,long a1,long a2,long a3)
{ return am_reply_4(from,(handler_4_t)h,a1,a2,a3,0); }
int am_reply_2(vnn_t from,handler_2_t h,long a1,long a2)
{ return am_reply_4(from,(handler_4_t)h,a1,a2,0,0); }
int am_reply_1(vnn_t from,handler_1_t h,long a1)
{ return am_reply_4(from,(handler_4_t)h,a1,0,0,0); }
int am_reply_0(vnn_t from,handler_0_t h)
{ return am_reply_4(from,(handler_4_t)h,0,0,0,0); }

extern ELAN_EVENT done; /*Event declared in queueThread.s*/
/*Asynchronous reply_xfer*/
void am_reply_xfer(vnn_t dest_vnn,
		   void *rva,
		   void *lva,
		   int nbytes,
		   handler_mem_t request_handler,
		   void *handler_arg)
{
    int head;
    volatile int *message_ptr;
    
    MEIAM_LOCK;
    /*Store back the data, but don't do it synchonously*/
    /*Guaranteed storage--don't need to wait for anything*/    
    head = am_queue->list_head;
    message_ptr = am_queue->message_ptr+(head*QUEUE_ENTRY_WORDS);
    
    /*Also know that this function will never loopback*/
    /*Should never happen - should be guaranteed space for
      requests AND replies*/
    
    assert(*(message_ptr+QREMOTE_NODE)==-1);
    
    ((ELAN_DMA*)message_ptr)->dma_type=DMA_TYPE(TR_TYPE_BYTE,DMA_NORMAL,8);
    ((ELAN_DMA*)message_ptr)->dma_destEvent = NULL;
    ((ELAN_DMA*)message_ptr)->dma_sourceEvent = &done;
    
    ((ELAN_DMA*)message_ptr)->dma_size = nbytes;
    ((ELAN_DMA*)message_ptr)->dma_source = lva;
    ((ELAN_DMA*)message_ptr)->dma_dest = rva;
    ((ELAN_DMA*)message_ptr)->dma_destProc = dest_vnn;
    ((ELAN_DMA*)message_ptr)->dma_pad = DMA_PKT; /*signal ELAN*/
    
    am_queue->list_head=(head+1)&(am_queue->queue_size-1);
    MEIAM_UNLOCK;
    
    /*Send completion event*/
    am_reply_4(dest_vnn,(handler_4_t)request_handler,(long)rva,nbytes,(long)handler_arg,0);
    
}

/* After completion of the function, the buffer pointed
 * to by lva *can* be modified.  Thus, this is a syncrhonous store
 * After completion, invoke the handler on the remote processor
 */
int am_store(vnn_t dest_vnn, void *lva, void *rva,
	     int nbytes, handler_mem_t request_handler, void *handler_arg)
{
    if (dest_vnn == MYPROC){
	bcopy(lva, rva, nbytes);
	(*request_handler)(MYPROC,rva,nbytes,handler_arg);
    } else {
	MEIAM_LOCK;
	am_store_dmaDesc->dma_size = nbytes;
	am_store_dmaDesc->dma_source = lva;
	am_store_dmaDesc->dma_dest = rva;
	am_store_dmaDesc->dma_destProc = dest_vnn;

	/*Initiate DMA, and wait*/
	elan_dma(ew_ctx,am_store_dmaDesc);
	elan_waitevent(ew_ctx,am_store_dmaEvent,ELAN_POLL_EVENT);

	/*Send completion event*/
	MEIAM_UNLOCK;
	am_request_3(dest_vnn,(handler_3_t)request_handler,(long)rva,nbytes,(long)handler_arg);
    }	
    return 0;
}

int am_store_async(vnn_t dest_vnn, void *lva, void *rva, 
		   int nbytes, handler_mem_t request_handler, void *handler_arg,
			handler_mem_t endfunc,void *e_arg)
{
	am_store(dest_vnn,lva,rva,nbytes,request_handler,handler_arg);
	(*endfunc)(am_my_cluster_id, lva, nbytes, e_arg);
	return 0;
}

int am_get(vnn_t source_vnn, void *rva, void *lva, int nbytes,
	   handler_mem_t reply_handler, void *handler_arg)
{
    int head;
    volatile int *message_ptr;
    
    /*loopback*/
    if (source_vnn == MYPROC){
	bcopy(rva, lva, nbytes);
	(*reply_handler)(MYPROC,lva,nbytes,handler_arg);
    } else {
	
	/*Wait for responses if necessary*/
	MEIAM_RLOCK;
	while (!K_Out && K_S == K_S_Old)
#if defined(AM_THREADS) && !defined(POLLING)
	    YIELD;
#else
	    am_poll();/*Drain the network*/
#endif
	
	MEIAM_LOCK;
	head = am_queue->list_head;
	message_ptr = am_queue->message_ptr+(head*QUEUE_ENTRY_WORDS);
	
	assert(*(message_ptr+QREMOTE_NODE)==-1);

	/*This means that the handler is am_reply_xfer*/
	message_ptr[QTYPE]=GET_PKT|REQUEST_PKT;
	message_ptr[QARG1]=___MYPROC;
	message_ptr[QARG2]=(long)lva;
	message_ptr[QARG3]=(long)rva;
	message_ptr[QARG4]=nbytes;
	message_ptr[QARG5]=(long)reply_handler;
	message_ptr[QARG6]=(long)handler_arg;
	message_ptr[QREMOTE_NODE]=(source_vnn);
	am_queue->list_head=(head+1)&(am_queue->queue_size-1);

	MEIAM_KLOCK;
        UPDATE_K_OUT();
	K_Out--;
	MEIAM_KUNLOCK;
	MEIAM_UNLOCK;
	MEIAM_RUNLOCK;
    }
    return 0;    
}


/* Drain the network.  If the incoming message is a response,
 * then increase K_Out.  If the incoming message is a request, then
 * make sure it sends a response.  Important:  Drain the network
 * by removing the args into registers or something, then 
 * clear the incoming message bit
 * HANDLERS CANNOT POLL!  did_reply should be 0
 */
void am_poll()
{
  register int arg0,arg1,arg2,arg3,arg4,arg5,arg6;
  volatile int *message_address;

#ifdef AM_THREADS
# ifdef POLLING /* only one thread can poll at a time */
	{ int r;
	  SPINLOCK_TRYLOCK(r,poll_lock);
	  if(!r) {return;}
	}
# else
	if (!thread_may_poll()) { 
		YIELD;
		return; 
	}
# endif
#endif

  assert(!did_reply);

  message_address = am_queue->queue_base_ptr;
  
 Start_Poll:
  if ((message_address[QFLAG]==-1)) {
#ifdef POLLING
      SPINLOCK_UNLOCK(poll_lock);
#endif
      return;
  }
  
  /*MESSAGE there*/
  /*Quickly pull it out....*/
  arg0=message_address[QTYPE];
  arg1=message_address[QARG1];
  arg2=message_address[QARG2];
  arg3=message_address[QARG3];
  arg4=message_address[QARG4];
  arg5=message_address[QARG5];
  arg6=message_address[QARG6];
  /*Clear incoming buffer*/
  message_address[QFLAG]=-1;
  message_address[QFLAG2]=-1;  /*Order here is very important*/
  

#ifdef AM_THREADS
  if(arg0&THREAD_PKT) {
	  struct start_t_struct *d;
	  d=(struct start_t_struct *)malloc(sizeof(*d));
	  d->func=(handler_4_t)arg1;
	  d->n=arg2;
	  d->a[0]=arg3;
	  d->a[1]=arg4;
	  d->a[2]=arg5;
	  d->a[3]=arg6;
	  AM_START_THREAD(d);
  } else
#else
  if(arg0&THREAD_PKT) {
	fprintf(stderr,"Got the request to start a thread from node %d, cannot be done!\n",arg2);
	abort();
  } else
#endif
  if (arg0&INT_PKT){
      (*(handler_4_t)arg1)(arg2,arg3,arg4,arg5,arg6);
      
  } else if (arg0&GET_PKT){
      am_reply_xfer(arg1,(void *)arg2,(void *)arg3,arg4,(handler_mem_t)arg5,(void *)arg6);
  
  } else {
      ew_exception(EW_DEBUG,"Invalid packet type in poll()");
  }
  
  /*Have to be careful about the global did_reply value*/
  if (arg0&REQUEST_PKT)	{
      if (!did_reply){
	  am_reply_0(arg2,bogus_handler);
      }
      did_reply=0;/*reset*/
  }
  /*Doesn't matter if we move this up, since
    nothing in the context can am_request anyway...*/
  else if (arg0&REPLY_PKT) {
      MEIAM_KLOCK;
      K_Out++;
      MEIAM_KUNLOCK;
  }
  
  
  goto Start_Poll;  /* Drain network completely*/
}

static void private_am_dummy() {};
static int private_barr_var=0;
static void private_am_barrier()
{
	int i=1;
	int n;
	n=(am_my_proc()+1)%am_procs();;
	if(am_my_proc()==0) {
		private_barr_var=0;
		am_store(n,&i,&private_barr_var,sizeof(i),(handler_mem_t)private_am_dummy,NULL);
		while(private_barr_var!=1) am_poll();
		i=2;
		am_store(n,&i,&private_barr_var,sizeof(i),(handler_mem_t)private_am_dummy,NULL);
		while(private_barr_var!=2) am_poll();
		private_barr_var=0;
	} else {
		while(private_barr_var!=1) am_poll();
		am_store(n,&i,&private_barr_var,sizeof(i),(handler_mem_t)private_am_dummy,NULL);
		while(private_barr_var!=2) am_poll();
		i=2;
		private_barr_var=0;
		am_store(n,&i,&private_barr_var,sizeof(i),(handler_mem_t)private_am_dummy,NULL);
	}
}

/* Stack for the elan thread*/
static char stack[STACK_SIZE];

/*
 * Create a queue object and return a pointer to it
 * size *must* be a power of 2
 */
queue_t *CreateQueue(int size,int procs)
{
    queue_t *queue_ptr; 
    int i;
    
    /*Check for power of 2 size...*/
    if ((size & (-size))!=size)
	ew_exception(EW_DEBUG, "Queue size not a power of 2!");
    
    /*Create queue space...*/
    queue_ptr = (queue_t *)ew_allocate(ew_base.alloc,EW_ALIGN,sizeof(queue_t)); 
    queue_ptr->message_ptr=(int*)EW_ALIGNUP(calloc((size)+1,QUEUE_ENTRY_SIZE),EW_ALIGN);
    /*Size *2 since each request/reply can include DMA descriptor*/
    
    /*clear elems - the remote node doubles as full/empty outgoing element*/
    for (i=0;i<size;i++)
	*(queue_ptr->message_ptr+QREMOTE_NODE+QUEUE_ENTRY_WORDS*i) = -1;
    
    queue_ptr->queue_size = size;
    queue_ptr->queue_base_ptr = ew_allocate(ew_base.alloc,EW_ALIGN,QUEUE_ENTRY_SIZE);
    /*First flag for incoming conditional checks, second flag for the 
      sparc to read in*/
    queue_ptr->queue_base_ptr[QFLAG]=-1; /*Clear the incoming queue element*/
    queue_ptr->queue_base_ptr[QFLAG2]=-1; /*Clear the incoming queue element*/
    queue_ptr->list_head = 0;
    
    if (procs>1) /*No communication if only 1 processor...*/
	elan_runthread (ew_ctx, Queue_Thread, stack, sizeof(stack),1,(int)queue_ptr);
    
    return (queue_ptr);
}


#endif /* MEIKO */
