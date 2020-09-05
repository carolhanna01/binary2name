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
 * Version:		10
 * Creation Date:	Tue Feb 28 18:15:42 1995
 * Filename:		lam.c
 * History:
 *	RPM	8	Sun Sep 24 19:54:02 1995
 *		Added glunix defines 
 *	RPM	7	Tue Aug 22 10:03:49 1995
 *		added long routes, sleep.
 *	RPM	2	Sat Jun  3 19:56:10 1995
 *		added relative addressing code. Fixed Q size problem.
 */

#include  <stdio.h>
#include  <stdlib.h>
#include  <unistd.h>
#include <string.h>
#include  "lanai_device.h"
#include <signal.h>
/* #include "config.h" not needed for myrinet release 3.04e+ */
#include <sys/types.h>

#include <sys/ipc.h>
#include <sys/shm.h>

#include <sys/time.h>
#include <sys/stat.h>

#include "ep.h"

#include "brahma.h"
#include "lam_sync.h"

#ifdef SOLARIS
#include <sys/systeminfo.h> 
#define gethostname(n,l)  sysinfo(SI_HOSTNAME,n,l)
#else
#define memmove(d,s,l) bcopy(s,d,l)
#endif

#ifdef USE_GLUNIX
/* GLUnix interface */
#include <cam.h>
#include <glib.h>
#include <netdb.h>
#include <netinet/in.h>
#include <netinet/tcp.h>
#include <arpa/inet.h>
#include <sys/socket.h>

#endif 

#ifdef DEBUG
#include <assert.h>
#endif

#define LAM_CONFIG_DEFAULT "/usr/now/etc/myrinet_routes/lam_conf" 
#define LAM_MAP_DEFAULT    "/usr/now/etc/myrinet_routes/master.map" 

#define LAM_SHMEM_KEY     0x700328
#define LAM_SHMEM_SIZE    0x1000
#define PERMS             0666

#define LEN_SHIFT 8 


extern unsigned short sts;
extern int copy_block_len,units;
extern int unit;

extern volatile int *lanai_done, *host_done; 
extern volatile private_ep_t *p_endpt; 
extern volatile ep_t *endpt;
extern volatile route_t *routes;
extern volatile desc_t *in_d,*out_d;
extern int in_num,out_num;

extern lanai_symbol_table *lanai_symtab;

extern volatile int *dma_base;
extern volatile int *user_base;

extern int *lam_dma_base;

extern int lam_myproc;
extern int lam_procs ;
extern int lam_reply ;  /* which node to reply to */

extern int flow_cntl[MAX_REMOTE];
extern int req_flow_in[MAX_REMOTE];
extern int req_flow_out [MAX_REMOTE];

/* sequence numbers for more robust debugging */ 

extern unsigned int seq_req_out[MAX_REMOTE]; 
extern unsigned int seq_rep_in[MAX_REMOTE]; 

extern unsigned int seq_req_in[MAX_REMOTE]; 
extern unsigned int seq_glob;

extern int am_poll_count;
extern int good_polls;
extern int null_polls;

struct timespec poll_begin,poll_end,poll_lapsed;

extern int rep_flow_in[MAX_REMOTE];
extern int rep_flow_out[MAX_REMOTE];

/*-------------------------------------------------------------------*/
/* Synchronization stuff for thread safety                           */
/* Macros used here are defined in lam_sync.h                        */
BR_POLL_DEC;                 /* The polling lock                     */
BR_OUT_DEC;                  /* outgoing buffer lock                 */
BR_CREDITS_DEC(MAX_REMOTE);  /* Locks protecting destination specific
                                stuff (such as credits)              */ 
BR_SYNC_ALL_DEC;             /* guards lam_sync_all                  */
BR_THR_QUEUE_DEC;            /* guards the thread creation queue     */
/*-------------------------------------------------------------------*/

int lam_sync_flag[MAX_REMOTE];
int lam_sync_count, lam_old_count;


typedef void (*handler_t)(int src,int arg0, int arg1,int arg2, int arg3);
typedef void (*xfer_handler_t)(int src,int arg0, int arg1,int arg2);
extern unsigned int LANAI_fd[64];

int self_buf[LAM_PKT_WORDS];

#ifdef USE_GLUNIX
extern char *lam_my_hostname;
#endif 

extern int lam_frag_size; 

char * 
lam_set_host_addr();


void 
BR_POLL();

handler_t 
lam_null_handler(int src,int arg0, int arg1,int arg2, int arg3) {

}
extern int BR_BARRIER_init();


void init_locks() {
  BR_POLL_INIT;                  /* Initialize the lock guarding BR_POLL()*/
  BR_OUT_INIT;                   /* Initialize the lock protecting out buffer*/
  BR_CREDITS_INIT(MAX_REMOTE);   /* Locks that guard request destinations */
  BR_SYNC_ALL_INIT;              /* Guards lam_sync_all */ 
  BR_THR_QUEUE_INIT;             /* Guards the thread creation queue */
}

handler_t 
lam_sync_rep_h(int src,int arg0, int arg1,int arg2, int arg3) {
  lam_sync_count = arg0;
}

handler_t 
lam_sync_req_h(int src,int arg0, int arg1,int arg2, int arg3) {
  if (lam_sync_flag[src] == 0) {
    lam_sync_flag[src] =1;
    lam_sync_count++;
  }
#if 0
  printf("node 0 got sync request from node %d\n",src);
  fflush(stdout);
#endif
  BR_REPLY_4(src, (BR_handler_4_t) lam_sync_rep_h,lam_sync_count,0,0,0);
}


void 
gam_wait(volatile int *flag,int value)
{
  do {
    BR_POLL();
  } while (*flag < value);
 
  *flag -= value;
}

void 
gam_poll_wait(volatile int *flag,int value)
{

 while (*flag < value) {
   BR_POLL();
 }
 
  *flag -= value;
}


static inline void 
gam_reply_0(int dest,handler_t fun) {

  BR_OUT_LOCK;
  /*---------------------------------------------------------------------*/

#ifdef DEBUG
  rep_flow_out[dest]++;
  assert( (dest >=0) && (dest < lam_procs));
  assert(((out_d->op_dest)&OP_MASK) == 0); /* queue is empty */
#endif 

  lam_reply = -1;

#ifndef USE_DOUBLE
  out_d->hdr.s_data[0] = (int) fun;
#else 
  out_d->hdr.s_data[5] = (int) fun;
#endif 

#ifdef DEBUG
  out_d->op_dest=(int)(AM_TYPE)|(dest<<DEST_SHIFT)|seq_glob|(REPLY)|(SHORT);
#else 
  out_d->op_dest = (int) (AM_TYPE)|(dest<<DEST_SHIFT)|(REPLY)|(SHORT);
#endif 

  out_num = (out_num +1) & OUT_SIZE_MASK;
  out_d = &(endpt->out[out_num]);

  /*---------------------------------------------------------------------*/
  BR_OUT_UNLOCK;
}


#ifdef DEBUG
static volatile int polls_in_progress=0;
#endif

extern volatile int BR_started;

/* This polls only after BR_init has been called. This could be called
   from within a thread package if it is not sure if the network has been
   initialized. In general, there is a cyclic dependency: the network stuff
   needs threads for synchronization, threads need to poll the network */
void BR_safe_poll(){
  if(BR_started==1){
    BR_POLL();
  }
}

/* Set to 1 by fork handlers */
extern BR_fork_handler;
void 
BR_POLL(){
  register int op_dest,src;
  register unsigned fun;  
  register unsigned int seq_no;

  if (!BR_POLL_TRY) {
    /* another poll is already in progress. Simply return */
    return;
  }
  
  TNF_PROBE_0(BR_POLL_start, "BRAHMA BR_POLL BR_POLL_start", ""); 

#ifdef DEBUG
  am_poll_count++;
  polls_in_progress++;
  assert(polls_in_progress==1);
#endif 
  

  while (((op_dest = in_d->op_dest)&OP_MASK) != 0){ /*if buffer has anything*/

    src = (unsigned) (op_dest & DEST_MASK) >> DEST_SHIFT;

    
    if ( (op_dest&REQ_MASK) == REQUEST) {
#ifdef DEBUG
      req_flow_in[src]++;

      seq_glob = op_dest&SEQ_MASK;
      seq_no = (op_dest&SEQ_MASK) >> SEQ_SHIFT;

      assert(lam_reply == -1);
      assert(seq_no == seq_req_in[src]);
      seq_req_in[src] = (seq_req_in[src]+1) & SEQ_SIZE_MASK ;
#endif 
      lam_reply =src;
    } else {

      /* Reply message */
      /* protect the global data structures */
      BR_CREDITS_LOCK(src);
      
      flow_cntl[src]++;
      
#ifdef DEBUG
      rep_flow_in[src]++;
      seq_no = (op_dest&SEQ_MASK) >> SEQ_SHIFT;
      assert(seq_no == seq_rep_in[src]);
      seq_rep_in[src] = (seq_rep_in[src]+1) & SEQ_SIZE_MASK ;
#endif 
      
      BR_CREDITS_UNLOCK(src);

      TNF_PROBE_0(BR_POLL_reply_start, "BRAHMA BR_POLL BR_POLL_reply_start", "");
    }


    if ( (op_dest &OP_MASK) == SHORT)  {

#ifndef USE_DOUBLE
      /* this is needed if the data is mis-aligned, or am I just lazy? */
          ( *( (void (*)()) in_d->hdr.s_data[0]))(src,in_d->hdr.s_data[1],
						  in_d->hdr.s_data[2],
						  in_d->hdr.s_data[3],
						  in_d->hdr.s_data[4],
						  in_d->hdr.s_data[5]);
       
#else 
    /* this way used a double word-load and saved about 1 usec off the RTT*/
    /* but the packet format put the function last, args first, because */
    /* of alignment problems */
      register long long temp0, temp1;
      temp0 = *( ((long long *) &(in_d->hdr.s_data[1]))); 
      temp1 = *( ((long long *) &(in_d->hdr.s_data[3]))); 
      ( *( (void (*)()) in_d->hdr.s_data[5]))(src,in_d->hdr.s_data[0],
					      temp0,temp1);
#endif 
    }

    else { /* maybe a bulk or medium or error */
      if (  (op_dest &OP_MASK) == BULK) {
	{
	  register uint word1,word2,word3,word4,word5,word6;
	
	  word5 = in_d->hdr.b_hdr.data[3]; /* number of unaligned bytes */
	  word1 = in_d->hdr.b_hdr.len;      /* length the Lanai gives us */
	  word2 = in_d->hdr.b_hdr.data[0]; /* fun pointer */
	  word3 = in_d->hdr.b_hdr.data[1]; /* dest address */
	  word4 = in_d->hdr.b_hdr.data[2]; /* arg 0*/

	  word6 = word5 >> LEN_SHIFT; 
	  word5 = word5 & 0xf;

	  if (word5 == 0) {
	    memset((char *) word3,0,word1);
	    memcpy((char *) word3,
		   (char*) ((uint *) user_base + (LAM_PKT_WORDS*in_num)),
		   word1) ;

	  }
	  else {
	    if (word1 > 4) {
	      word1 = ( (word1-4)&(~0x3))+word5;
	    }
	    else {
	      word1 = word5;	    
	    }
	    memcpy((char *) word3, 
		   (char *) ((uint *) user_base + (LAM_PKT_WORDS*in_num)),
		   word1); 

	  }

	  if ( (xfer_handler_t) word2 != (xfer_handler_t) lam_null_handler ) {
	    /* I had to fix a bug here to compute a proper starting
	     * address if this is the last chunk of a large fragmented transfer
	     * Boris Weissman 9/7/97 */
	    if(word6<lam_frag_size){
	      ( (xfer_handler_t) word2)(src,word3,word6,word4);
	    }
	    else {
	      /* Have to infer the original starting address */
	      ((xfer_handler_t) word2)(src,(word3-(word6-word1)),word6,word4);
	    }
	  }

	}
      }
      else { /* maybe a medium or error */
	if ( (op_dest &OP_MASK) == MEDIUM) {
	  {
	    register uint word1,word2,word3,word4,word5;

	    word5 = in_d->hdr.b_hdr.data[3]; 
	    word1 = in_d->hdr.b_hdr.len;     /* length the Lanai gives us */
	    word2 = in_d->hdr.b_hdr.data[0]; /* handler */
	    word3 = in_d->hdr.b_hdr.data[1]; /* arg0 */
	    word4 = in_d->hdr.b_hdr.data[2]; /* arg1 */

	    word5 = word5 & 0x3;	/* number of unaligned bytes */

	    if (word5 != 0) {

	      if (word1 > 4) {
		word1 = ( (word1-4)&(~0x3))+word5;
	      }
	      else {
		word1 = word5;	    
	      }
	    }
	    ( (handler_t) word2)
	      (src,
	       (int ) ((uint *) user_base+(LAM_PKT_WORDS*in_num)),
	       word1, word3, word4);	
	  }
	}
	else {
	  /* CRC ERROR */
	  fprintf(stderr,"Wank! got crc error.\n");
	  fprintf(stderr,"in_num = 0x%0x in_d = 0x%0x\n",in_num,in_d);
	  fprintf(stderr,"op_dest= 0x%0x s_data[0]= 0x%0x s_data[1]= 0x%0x\n",
		  in_d->op_dest, in_d->hdr.s_data[0], in_d->hdr.s_data[1]);
	  fprintf(stderr,"s_data[2]= 0x%0x s_data[3]= 0x%0x s_data[4]= 0x%0x\n",
		  in_d->hdr.s_data[2], in_d->hdr.s_data[3], in_d->hdr.s_data[4]);
	  fprintf(stderr,"s_data[5]= 0x%0x crc= 0x%0x\n",
		  in_d->hdr.s_data[5], in_d->crc);
	}
      }
    }
    in_d->op_dest = op_dest & (~OP_MASK) ;
    in_num = (in_num+1)&IN_SIZE_MASK;
    in_d = &(endpt->in[in_num]);

    if ( (op_dest & REQ_MASK) == REQUEST ) { /* was a request or reply? */
      if (lam_reply == src) 
	gam_reply_0(src, (handler_t) lam_null_handler);
    }
  } /* end while messages */

#ifdef DEBUG
  polls_in_progress--;
#endif

  BR_POLL_UNLOCK;   /* unlock the polling lock */

  /* If we had a fork handler, yield to let it execute */
  /* Threads originating remotely run immediately */
  #ifdef AT_THREADS
  if(BR_fork_handler==1){
    BR_fork_handler=0;
    /* Here need to yield only if not called frm the main AT loop */
    /* Otherwise will seg fault!!!! 
    BR_THREAD_YIELD(); */
  }
  #endif
  TNF_PROBE_0(BR_POLL_end, "BRAHMA BR_POLL BR_POLL_end", "");
}


int 
BR_REQUEST_4(BR_cluster_t dest, BR_handler_4_t fun, BR_word_t arg0, 
	     BR_word_t arg1, BR_word_t arg2, BR_word_t arg3) {


  register long long *temp0; 
  int poll_count,seq_no ;

#ifdef DEBUG
  int print_times =0;
#endif 

  TNF_PROBE_0(BR_REQUEST_4_start, "BRAHMA BR_REQUEST_4 BR_REQUEST_4_start", "");
  poll_count = 0; 
  if (dest != lam_myproc) {

#ifdef DEBUG
    assert( (flow_cntl[dest] >=0) && (flow_cntl[dest] <= DEPTH));
#endif 

    /*
     * The following section needs syncnronized access to flow_cntl[dest]
     * and a few other global dest dependant data structures 
     * (it changes the global state)
     */
    
  get_req4_credits:
    
    BR_CREDITS_LOCK(dest);
    /*---------------------------------------------------------------------*/
    
    if (flow_cntl[dest] != 0) {
      
#ifdef DEBUG
      assert( (dest >=0) && (dest < lam_procs));

#endif 

      flow_cntl[dest]--; 

      /*---------------------------------------------------------------------*/
      BR_CREDITS_UNLOCK(dest);

      /*
       * What follows needs an exclusive access to out_d and 
       * out_num and is protected by the OUT lock
       */
      BR_OUT_LOCK;
      /*---------------------------------------------------------------------*/


#ifdef DEBUG
      /* This stuff needs to be serialized to make any sense at all
	 in the presence of threads. It is necessary just for debugging
	 so there is no performance implications */
      seq_no = ((seq_req_out[dest])<<SEQ_SHIFT) & SEQ_MASK; 
      seq_req_out[dest] = (seq_req_out[dest]+1) & SEQ_SIZE_MASK ;
      req_flow_out[dest]++;

      while ( ((out_d->op_dest)&OP_MASK) != 0) {
	BR_POLL();
	printf("\nWarning, am_request_4 found output queue full! \n"); 
      }
#endif


#ifndef USE_DOUBLE
      out_d->hdr.s_data[0] = (int) fun;
      out_d->hdr.s_data[1] = (int) arg0;
      out_d->hdr.s_data[2] = (int) arg1;
      out_d->hdr.s_data[3] = (int) arg2;
      out_d->hdr.s_data[4] = (int) arg3;
      
#else
      temp0 = (long long *) &(out_d->hdr.s_data[1]); 
      out_d->hdr.s_data[0] = (int) arg0;
      *temp0 = ((long long) arg1<<32) + arg2; 
      out_d->hdr.s_data[3] = (int) arg3;
      out_d->hdr.s_data[5] = (int) fun;
#endif 

#ifdef DEBUG
      (volatile) out_d->op_dest =
	(int) (AM_TYPE)|(dest<<DEST_SHIFT)|seq_no|(REQUEST)|(SHORT);
#else
      (volatile) out_d->op_dest =
	(int) (AM_TYPE)|(dest<<DEST_SHIFT)|(REQUEST)|(SHORT);
#endif 
      
      out_num = (out_num +1) & OUT_SIZE_MASK;
      out_d = &(endpt->out[out_num]);
      
      /*---------------------------------------------------------------------*/
      BR_OUT_UNLOCK;

      BR_POLL();
    } /* end if have credits */
    else {
      /* Oups, we are out of credits ... */
      /* Need to unlock to allow replies update the credits count */
      BR_CREDITS_UNLOCK(dest);
      do {
	BR_POLL();
	poll_count++;
	if (poll_count > MAX_POLL_COUNT) {
	  printf("am_request_4: failed to inject message to %d \n", dest); 
	  return -1;
	}
      } while(flow_cntl[dest] == 0);
      /* Try again */
      goto get_req4_credits;
    }
  } /* dest != me */
  else {
    (*fun)(lam_myproc,arg0, arg1, arg2, arg3); 
    BR_POLL();
  }

  TNF_PROBE_0(BR_REQUEST_4_end, "BRAHMA BR_REQUEST_4 BR_REQUEST_4_end", "");
  return 0;
}


int 
BR_REQUEST_5(BR_cluster_t dest, BR_handler_5_t fun, BR_word_t arg0, 
	     BR_word_t arg1, BR_word_t arg2, BR_word_t arg3, BR_word_t arg4) {
  int poll_count; 
#ifdef DEBUG
  int seq_no;
  int print_times =0;
#endif 

  TNF_PROBE_0(BR_REQUEST_5_start, "BRAHMA BR_REQUEST_5 BR_REQUEST_5_start", "");

  poll_count =0; 

  if (dest != lam_myproc ) {

    /*
     * The following section needs a syncnronized access to flow_cntl[dest]
     * and a few other global dest dependent data structures 
     * (it changes the global state)
     */

  get_req5_credits:

    BR_CREDITS_LOCK(dest);
    /*---------------------------------------------------------------------*/

#ifdef DEBUG
    assert( (flow_cntl[dest] >=0) && (flow_cntl[dest] <= DEPTH));
#endif 

    if (flow_cntl[dest] == 0){
      /* Oups, we are out of credits ... */
      /* Need to unlock to allow replies update the credits count */
      BR_CREDITS_UNLOCK(dest);
      do {
	BR_POLL();
	poll_count++;
	if (poll_count > MAX_POLL_COUNT) {
	  printf("BR_REQUEST_5: failed to inject message to %d \n", dest); 
	  return -1;
	}
      } while(flow_cntl[dest] == 0);
      /* Now, try over */
      goto get_req5_credits;
    }
    
    /*At this point, we have at least one credit and it is in a locked state*/

    flow_cntl[dest]--;
    
    /*---------------------------------------------------------------------*/
    BR_CREDITS_UNLOCK(dest);


    /*
     * What follows needs an exclusive access to out_d and 
     * out_num and is protected by the OUT lock
     */
    BR_OUT_LOCK;
    /*---------------------------------------------------------------------*/
#ifdef DEBUG
    assert( (dest >=0) && (dest < lam_procs));

    seq_no = ((seq_req_out[dest])<<SEQ_SHIFT) & SEQ_MASK; 
    seq_req_out[dest] = (seq_req_out[dest]+1) & SEQ_SIZE_MASK ;
    req_flow_out[dest]++;

    while ( ((out_d->op_dest)&OP_MASK) != 0) {
      BR_POLL();
      printf("\nWarning, BR_REQUEST_5 found output queue full! \n");
    }
#endif 

#ifndef USE_DOUBLE
    out_d->hdr.s_data[0] = (int) fun;
    out_d->hdr.s_data[1] = (int) arg0;
    out_d->hdr.s_data[2] = (int) arg1;
    out_d->hdr.s_data[3] = (int) arg2;
    out_d->hdr.s_data[4] = (int) arg3;
    out_d->hdr.s_data[5] = (int) arg4;
#else 
    out_d->hdr.s_data[0] = (int) arg0;
    out_d->hdr.s_data[1] = (int) arg1;  
    out_d->hdr.s_data[2] = (int) arg2;
    out_d->hdr.s_data[3] = (int) arg3;
    out_d->hdr.s_data[4] = (int) arg4;
    out_d->hdr.s_data[5] = (int) fun;
#endif 

#ifdef DEBUG
    out_d->op_dest =(int)(AM_TYPE)|(dest<<DEST_SHIFT)|seq_no|(REQUEST)|(SHORT);
#else
    out_d->op_dest = (int) (AM_TYPE)|(dest<<DEST_SHIFT)|(REQUEST)|(SHORT);
#endif 

    out_num = (out_num +1) & OUT_SIZE_MASK;
    out_d = &(endpt->out[out_num]);
    
    /*---------------------------------------------------------------------*/
    BR_OUT_UNLOCK;

    BR_POLL();
  }
  else {
     (*fun)(lam_myproc,arg0, arg1, arg2, arg3,arg4);
     BR_POLL();
  }
  
  TNF_PROBE_0(BR_REQUEST_5_end, "BRAHMA BR_REQUEST_5 BR_REQUEST_5_end", "");

  return 0;
}


int 
lam_store(int dest, void *lva, void *rva, int nbytes,
		  xfer_handler_t fun, void *arg0,int real_len) {
  int i;
  int poll_count ;
  int seq_no;

  poll_count =0;

  if (nbytes <= (LAM_PKT_WORDS*sizeof(int))) {

    if ( dest != lam_myproc ) {

    get_store_credits:

      BR_CREDITS_LOCK(dest);
      /*---------------------------------------------------------------------*/
      if (flow_cntl[dest] == 0){
	/* Oups, we are out of credits ... */
	/* Need to unlock to allow replies update the credits count */
	BR_CREDITS_UNLOCK(dest);
	do { 
	  BR_POLL();
	  poll_count++;
	  if (poll_count > MAX_POLL_COUNT) {
	    printf("am_store: failed to inject message to %d \n", dest); 
	    return -1;
	  }
	} while(flow_cntl[dest] == 0);
	/* Now, try again */
	goto get_store_credits;
      }
      
     /*At this point, we have at least one credit and it is in a locked state*/

#ifdef DEBUG      
      assert(nbytes > 0 );   /* transfer should be >0 < max size */
      assert(nbytes <= LAM_PKT_WORDS*4 );

      assert( (flow_cntl[dest] >=0) && (flow_cntl[dest] <= DEPTH));

      assert( (dest >=0) && (dest < lam_procs));  /* dest oK? */
#endif 
      
      flow_cntl[dest]--;  


      /*---------------------------------------------------------------------*/
      BR_CREDITS_UNLOCK(dest);


      BR_OUT_LOCK;
      /*---------------------------------------------------------------------*/

#ifdef DEBUG      
      seq_no = ((seq_req_out[dest])<<SEQ_SHIFT) & SEQ_MASK; 
      seq_req_out[dest] = (seq_req_out[dest]+1) & SEQ_SIZE_MASK ;
      req_flow_out[dest]++;

      while ( ((out_d->op_dest)&OP_MASK) != 0) {
	BR_POLL();
	printf("\nWarning, am_store found output queue full! \n");
      }
#endif

   /* copy into io space buffer */
   /* need to check max len, assume that all buffers are word aligned */
   /* and in multiple of words */

      memcpy((int *) (user_base+((LAM_PKT_WORDS*IN_Q_SIZE)+
				 (out_num*LAM_PKT_WORDS))),
	     (char *) lva, (nbytes));

      out_d->hdr.b_hdr.la = (int *) lam_dma_base +
	(LAM_PKT_WORDS*IN_Q_SIZE)+(out_num*LAM_PKT_WORDS);
      
      out_d->hdr.b_hdr.len = nbytes;
      out_d->hdr.b_hdr.data[0] = (int) fun;
      out_d->hdr.b_hdr.data[1] = (int) rva;
      out_d->hdr.b_hdr.data[2] = (int) arg0;
      /* store 'extra' bytes */
      out_d->hdr.b_hdr.data[3] = (real_len<<LEN_SHIFT) | (nbytes & 0x3); 

      if ( (nbytes & 0x3) != 0) {  /* round up to upper byte */
	out_d->hdr.b_hdr.len = (nbytes+4) & (~0x3);
      }

#ifdef DEBUG
      out_d->op_dest = i = 
	(int) (AM_TYPE)|(dest<<DEST_SHIFT)|seq_no|(REQUEST)|(BULK); /*go*/
#else 
      out_d->op_dest = i = 
	(int) (AM_TYPE)|(dest<<DEST_SHIFT)|(REQUEST)|(BULK); /*go*/
#endif 

      out_num = (out_num +1) & OUT_SIZE_MASK;
      out_d = &(endpt->out[out_num]);

      /*---------------------------------------------------------------------*/
      BR_OUT_UNLOCK;

      BR_POLL();
      return;
    }
    else {
      memmove(rva,lva,nbytes);
      (*fun)(dest,(int ) rva, nbytes,(int )arg0);    
      BR_POLL();
      return;
    }
  }
  else {
    printf("lam_store called with size > %d\n",LAM_PKT_WORDS*sizeof(int));
  }

  return 0;
}

/* almost the same implementation as am_request_xfer() */
int 
gam_request(int dest,handler_t fun,void *lva, int nbytes,
	   int arg0, int arg1) {

  int i;
  int seq_no;
  int poll_count =0;

  if (nbytes <= (LAM_PKT_WORDS*sizeof(int))) {

    if ( dest != lam_myproc ) {

    get_req_credits:

      BR_CREDITS_LOCK(dest);
      /*---------------------------------------------------------------------*/
      
#ifdef DEBUG
      assert( (flow_cntl[dest] >=0) && (flow_cntl[dest] <= DEPTH));
#endif 

      if (flow_cntl[dest] == 0){
	/* Oups, we are out of credits ... */
	/* Need to unlock to allow replies update the credits count */     
	BR_CREDITS_UNLOCK(dest);
	do {
	  poll_count++;
	  BR_POLL();
	  if (poll_count > MAX_POLL_COUNT) {
	    printf("am_request: failed to inject message to %d \n", dest); 
	    return -1;
	  }
	} while(flow_cntl[dest] == 0);
	/* Now, try again */
	goto get_req_credits;
      }

     /*At this point, we have at least one credit and it is in a locked state*/

#ifdef DEBUG      
      assert(nbytes > 0 );	/* transfer should be >0 < max size */
      assert(nbytes <= LAM_PKT_WORDS*4 );

      assert( (dest >=0) && (dest < lam_procs)); /* dest oK? */
#endif 
      
      flow_cntl[dest]--;  

      /*---------------------------------------------------------------------*/
      BR_CREDITS_UNLOCK(dest);    

      BR_OUT_LOCK;
      /*---------------------------------------------------------------------*/

#ifdef DEBUG
      seq_no = ((seq_req_out[dest])<<SEQ_SHIFT) & SEQ_MASK; 
      seq_req_out[dest] = (seq_req_out[dest]+1) & SEQ_SIZE_MASK ;
      req_flow_out[dest]++;

      while ( ((out_d->op_dest)&OP_MASK) != 0) {
        BR_POLL();
       	printf("\nWarning, am_request found output queue full! \n");
      }
#endif

      /* copy into io space buffer */
      /* need to check max len, assume that all buffers are word aligned */
      /* and in multiple of words */

      memcpy((int *) (user_base+((LAM_PKT_WORDS*IN_Q_SIZE)+
				 (out_num*LAM_PKT_WORDS))),
	     (char *) lva, nbytes);
      
      out_d->hdr.b_hdr.la = (int *) lam_dma_base +
       	(LAM_PKT_WORDS*IN_Q_SIZE)+(out_num*LAM_PKT_WORDS);
      
      out_d->hdr.b_hdr.len = nbytes;
      out_d->hdr.b_hdr.data[0] = (int) fun;
      out_d->hdr.b_hdr.data[1] = (int) arg0;
      out_d->hdr.b_hdr.data[2] = (int) arg1;
      /* store 'extra' bytes */			
      out_d->hdr.b_hdr.data[3] = (nbytes & 0x3);
      
      if ( (nbytes & 0x3) != 0) { /* round up to upper byte */
	out_d->hdr.b_hdr.len = (nbytes+4) & (~0x3);
      }
#ifdef DEBUG
      out_d->op_dest = i = 
	(int) (AM_TYPE)|(dest<<DEST_SHIFT)|seq_no|(REQUEST)|(MEDIUM); /*go*/
#else      
      out_d->op_dest = i = 
	(int) (AM_TYPE)|(dest<<DEST_SHIFT)|(REQUEST)|(MEDIUM); /*go*/
#endif 

      out_num = (out_num +1) & OUT_SIZE_MASK;
      out_d = &(endpt->out[out_num]);

      /*---------------------------------------------------------------------*/
      BR_OUT_UNLOCK;

      BR_POLL();
      return 0;
    }
    else {
      memmove(self_buf, lva, nbytes);
      (*fun)(lam_myproc,(int) self_buf, nbytes, arg0, arg1);
      BR_POLL();
      return 0;
    }
  }
  else {
    printf("am_request called with size > %d\n",LAM_PKT_WORDS*sizeof(int));
  }	

  return 0;
}




/* this function does fragmentation of outbound data */
/* assume the network is as in-order delivery semantics */

inline void
BR_STORE(BR_cluster_t dest,caddr_t l_buf,caddr_t r_buf,size_t nbytes,
	 BR_handler_mem_t fun, caddr_t arg0) {
  int i;

  TNF_PROBE_0(BR_STORE_start, "BRAHMA BR_STORE BR_STORE_start", "");

  if( nbytes <= lam_frag_size) {
   lam_store(dest,l_buf,r_buf,nbytes,(xfer_handler_t)fun,(void *)arg0,nbytes);  
  }
  else {
    /* chunk the data into maximum packet sizes */
    for (i=lam_frag_size; i<nbytes; i+=lam_frag_size) {
     lam_store(dest,l_buf,r_buf,lam_frag_size, (xfer_handler_t)
	       lam_null_handler, (void*)i, nbytes);
      (char *) l_buf+= lam_frag_size; (char *)r_buf+= lam_frag_size;  
    }
    i-=lam_frag_size;
    /*put the last packet */
    lam_store(dest,l_buf,r_buf,nbytes-i,(xfer_handler_t)fun,(void *)arg0,nbytes); 
  }

  TNF_PROBE_0(BR_STORE_end, "BRAHMA BR_STORE BR_STORE_end", "");

}



void 
BR_REPLY_4(BR_cluster_t dest, BR_handler_4_t fun, BR_word_t arg0, 
	   BR_word_t arg1, BR_word_t arg2, BR_word_t arg3) {

  register long long *temp0; 

  TNF_PROBE_0(BR_REPLY_4_start, "BRAHMA BR_REPLY_4 BR_REPLY_4_start", "");

  if ( dest != lam_myproc ) {

    lam_reply = -1;
    
    BR_OUT_LOCK;
    /*---------------------------------------------------------------------*/
#ifdef DEBUG      
      assert( ((out_d->op_dest)&OP_MASK) == 0 );   /* queue is empty */
      assert( (dest >=0) && (dest < lam_procs));  /* dest oK? */
      rep_flow_out[dest]++;
#endif 

#ifndef USE_DOUBLE
      out_d->hdr.s_data[0] = (int) fun;
      out_d->hdr.s_data[1] = (int) arg0;
      out_d->hdr.s_data[2] = (int) arg1;
      out_d->hdr.s_data[3] = (int) arg2;
      out_d->hdr.s_data[4] = (int) arg3;
#else 
    temp0 = (long long *) &(out_d->hdr.s_data[1]); 
    out_d->hdr.s_data[0] = (int) arg0;
    *temp0 = ((long long) arg1<<32) + arg2; 
    out_d->hdr.s_data[3] = (int) arg3;

    out_d->hdr.s_data[5] = (int) fun;
#endif 

#ifdef DEBUG
    out_d->op_dest=(int)(AM_TYPE)|(dest<<DEST_SHIFT)|seq_glob|(REPLY)|(SHORT);
#else
    out_d->op_dest = (int)  (AM_TYPE)|(dest<<DEST_SHIFT)|(REPLY)|(SHORT);
#endif

    out_num = (out_num +1) & OUT_SIZE_MASK;
    out_d = &(endpt->out[out_num]);

    /*---------------------------------------------------------------------*/
    BR_OUT_UNLOCK;
  }
  else {
    (*fun)(lam_myproc,arg0, arg1, arg2, arg3);
  }

  TNF_PROBE_0(BR_REPLY_4_end, "BRAHMA BR_REPLY_4 BR_REPLY_4_end", "");

}

void 
BR_REPLY_5(BR_cluster_t dest, BR_handler_5_t fun, BR_word_t arg0, 
	   BR_word_t arg1, BR_word_t arg2, BR_word_t arg3, BR_word_t arg4) {

  register long long *temp0; 

  TNF_PROBE_0(BR_REPLY_5_start, "BRAHMA BR_REPLY_5 BR_REPLY_5_start", "");

  if ( dest != lam_myproc ) {

    lam_reply = -1;
    
    BR_OUT_LOCK;
    /*---------------------------------------------------------------------*/
#ifdef DEBUG      
      assert( ((out_d->op_dest)&OP_MASK) == 0 );   /* queue is empty */
      assert( (dest >=0) && (dest < lam_procs));  /* dest oK? */
      rep_flow_out[dest]++;
#endif 

#ifndef USE_DOUBLE
      out_d->hdr.s_data[0] = (int) fun;
      out_d->hdr.s_data[1] = (int) arg0;
      out_d->hdr.s_data[2] = (int) arg1;
      out_d->hdr.s_data[3] = (int) arg2;
      out_d->hdr.s_data[4] = (int) arg3;
      out_d->hdr.s_data[5] = (int) arg4;
#else 
    out_d->hdr.s_data[0] = (int) arg0;
    out_d->hdr.s_data[1] = (int) arg1;  
    out_d->hdr.s_data[2] = (int) arg2;
    out_d->hdr.s_data[3] = (int) arg3;
    out_d->hdr.s_data[4] = (int) arg4;
    out_d->hdr.s_data[5] = (int) fun;
#endif 

#ifdef DEBUG
    out_d->op_dest=(int)(AM_TYPE)|(dest<<DEST_SHIFT)|seq_glob|(REPLY)|(SHORT);
#else
    out_d->op_dest = (int)  (AM_TYPE)|(dest<<DEST_SHIFT)|(REPLY)|(SHORT);
#endif

    out_num = (out_num +1) & OUT_SIZE_MASK;
    out_d = &(endpt->out[out_num]);

    /*---------------------------------------------------------------------*/
    BR_OUT_UNLOCK;
  }
  else {
    (*fun)(lam_myproc,arg0, arg1, arg2, arg3, arg4);
  }

  TNF_PROBE_0(BR_REPLY_5_end, "BRAHMA BR_REPLY_5 BR_REPLY_5_end", "");

}



void 
gam_reply_xfer(int dest, void *lva, void *rva, int nbytes,
		 void (*fun)(), void *arg0) {

  int real_len;

  real_len = nbytes; 
  if ( dest != lam_myproc ) {

    lam_reply = -1;

    BR_OUT_LOCK;
    /*---------------------------------------------------------------------*/
   
#ifdef DEBUG
    assert( ((out_d->op_dest)&OP_MASK) == 0 );
    assert( (dest >=0) && (dest < lam_procs));  /* dest oK? */
    rep_flow_out[dest]++;
#endif 


  /* copy into io space buffer */
  /* need to check max len, assume that all buffers are word aligned */
  /* and in multiple of words */
    memcpy( (char *)  (user_base+ ((LAM_PKT_WORDS*IN_Q_SIZE)+
				   (out_num*LAM_PKT_WORDS))),
	   (char *) lva, nbytes);
    out_d->hdr.b_hdr.la = (int *) lam_dma_base +
      (LAM_PKT_WORDS*IN_Q_SIZE)+(out_num*LAM_PKT_WORDS);

    out_d->hdr.b_hdr.len = nbytes;
    out_d->hdr.b_hdr.data[0] = (int) fun;
    out_d->hdr.b_hdr.data[1] = (int) rva;
    out_d->hdr.b_hdr.data[2] = (int) arg0;
    out_d->hdr.b_hdr.data[3] = (real_len<<LEN_SHIFT) | (nbytes & 0x3); 

    if ( (nbytes & 0x3) != 0) {
      out_d->hdr.b_hdr.len = (nbytes+4) & (~0x3);
    }

#ifdef DEBUG
    out_d->op_dest=(int) (AM_TYPE)|(dest<<DEST_SHIFT)|seq_glob|(REPLY)|(BULK);
#else 
    out_d->op_dest = (int) (AM_TYPE)|(dest<<DEST_SHIFT)|(REPLY)|(BULK);
#endif 

    out_num = (out_num +1) & OUT_SIZE_MASK;
    out_d = &(endpt->out[out_num]);

    /*---------------------------------------------------------------------*/
    BR_OUT_UNLOCK;
  }  
  else {
    memmove(rva,lva,nbytes);
    (*fun)(dest,rva,nbytes,arg0);    
  }
}


void 
BR_GET(BR_cluster_t dest, caddr_t rva, caddr_t lva, size_t len, 
	   BR_handler_mem_t handler, caddr_t arg) {

  TNF_PROBE_0(BR_GET_start, "BRAHMA BR_GET BR_GET_start", "");

#ifdef DEBUG
  assert(len >=0 ) ;
  assert(len <= LAM_PKT_WORDS*4 );
  /*  assert( ((out_d->op_dest)&OP_MASK) == 0 ); don't think we need this, Boris */
#endif 

  BR_REQUEST_5(dest, (BR_handler_5_t) gam_reply_xfer, (int)rva, (int)lva, (int)len, (int)
		handler, (int)arg);

  TNF_PROBE_0(BR_GET_end, "BRAHMA BR_GET BR_GET_end", "");
}

int 
gam_procs() {
  return lam_procs;
}

int 
gam_my_proc() {
  return lam_myproc;
}

int 
BR_MAX_XFER() {
  return LAM_PKT_WORDS*4; 
}

void 
BR_ASYNC_STORE(BR_cluster_t dest_node, caddr_t src_buf, caddr_t dest_buf,
	       size_t len, BR_handler_mem_t request_handler, 
	       BR_word_t request_handler_arg1, BR_handler_mem_t endfunc, 
	       BR_word_t endfunc_arg1) {

  TNF_PROBE_0(BR_ASYNC_STORE_start, "BRAHMA BR_ASYNC_STORE BR_ASYNC_STORE_start", "");

  BR_STORE(dest_node, src_buf, dest_buf, len, request_handler, 
	   (caddr_t)request_handler_arg1);

  (*endfunc)(dest_node, src_buf,len,(caddr_t)endfunc_arg1);    

  TNF_PROBE_0(BR_ASYNC_STORE_end, "BRAHMA BR_ASYNC_STORE BR_ASYNC_STORE_end", "");

}


int 
gam_reply(int dest, handler_t fun , void *lva, int nbytes,
	     int arg0, int arg1) {

  if ( dest != lam_myproc ) {


    lam_reply = -1;

    BR_OUT_LOCK;
    /*---------------------------------------------------------------------*/
    
#ifdef DEBUG
    assert( ((out_d->op_dest)&OP_MASK) == 0 );  /* the queue is empty */
    assert( (dest >=0) && (dest < lam_procs)); /* dest oK? */
    rep_flow_out[dest]++;
#endif 

    /* copy into io space buffer */
    /* need to check max len, assume that all buffers are word aligned */
    /* and in multiple of words */
    memcpy( (char *)  (user_base+ ((LAM_PKT_WORDS*IN_Q_SIZE)+
				   (out_num*LAM_PKT_WORDS))),
	   (char *) lva, nbytes);
    out_d->hdr.b_hdr.la = (int *) lam_dma_base +
      (LAM_PKT_WORDS*IN_Q_SIZE)+(out_num*LAM_PKT_WORDS);

    out_d->hdr.b_hdr.len = nbytes;
    out_d->hdr.b_hdr.data[0] = (int) fun;
    out_d->hdr.b_hdr.data[1] = (int) arg0;
    out_d->hdr.b_hdr.data[2] = (int) arg1;
    /* store 'extra' bytes */		
    out_d->hdr.b_hdr.data[3] = (nbytes & 0x3);

    if ( (nbytes & 0x3) != 0) {
      out_d->hdr.b_hdr.len = (nbytes+4) & (~0x3);
    }

#ifdef DEBUG
    out_d->op_dest=(int)(AM_TYPE)|(dest<<DEST_SHIFT)|seq_glob|(REPLY)|(MEDIUM);
#else
    out_d->op_dest = (int) (AM_TYPE)|(dest<<DEST_SHIFT)|(REPLY)|(MEDIUM);
#endif 
    out_num = (out_num +1) & OUT_SIZE_MASK;
    out_d = &(endpt->out[out_num]);

    /*---------------------------------------------------------------------*/
    BR_OUT_UNLOCK;

  }  
  else {
    memmove(self_buf, lva, nbytes);
    (*fun)(lam_myproc,(int) lva, nbytes, arg0, arg1);
  }

  return 0; 

}

void 
gam_init_tables() {
  int i; 
  i=1;
}

int 
gam_request_fd(int dest, void *fun, int arg2, int arg3, double d1) {

  int *i;

  /* code assume a double is 2x int. should work on Sparc v8 */
  i = (int *) &d1;
  BR_REQUEST_4(dest,fun,arg2,arg3,*i,*(i+1)); 

}

int 
gam_reply_fd(int dest, void *fun, int arg2, int arg3, double d1) {

  int *i;

  i = (int *) &d1;

  BR_REPLY_4(dest,fun,arg2,arg3,*i,*(i+1)); 

}

int 
gam_sleep() {
  return;
}

int
lam_restore_flow(int des) {
  if (flow_cntl[des] < MAX_DEPTH) 
    flow_cntl[des]++;
}

int 
lam_change_route(int v, int len, int *p) {
  char temp_path[8];
  char *dest_route;
  int j, hop;

#ifdef DEBUG
  if ( (v >=  MAX_REMOTE) ||  (len >= 9)) 
    fprintf(stderr,"node %d eek v =%d len=%d\n",lam_myproc,v,len);

  assert((v < MAX_REMOTE) && (len < 9));
#endif
  
  routes[v].len = len;
  for (j=0; j<len; j++) {
    hop = p[j];
    if ((hop > 7) || (hop < -7)) {
      printf("invalid hop entry %d to node %d hop %d\n",hop,v,j);
      return 0;
    }
    temp_path[j] = ((hop&0x3f) |0x80);
  }
  
  dest_route = (char *) routes[v].path;
  
  switch (routes[v].len) {
  case 0:
    break;
  case 1:
    memcpy(dest_route+1,temp_path,1);
    break;
  case 2:
    memcpy(dest_route+0,temp_path,2);
    break; 
  case 3:
    memcpy(dest_route+1,temp_path,3);	
    break;
  case 4:
    memcpy(dest_route+0,temp_path,4);	
    break;
  case 5:
    memcpy(dest_route+3,temp_path,5);	
    break;
  case 6:
    memcpy(dest_route+2,temp_path,6);	
    break;
  case 7:
    memcpy(dest_route+1,temp_path,7);	
    break;
  case 8:
    memcpy(dest_route+0,temp_path,8);	
    break;
  default:
    printf("route too long \n");
    exit(1);
    break; 
  } /* end switch */
  return 1;
}


void 
lam_route_display() {
  int i,u;

  for(i=0;i<lam_procs;i++) {
    printf("Route %d ",i);
    for(u=0;u<4;u++) {
      printf(" %x", routes[i].path[u]);
    }
    printf("\n");
  }
}


/* really dumb way to sync all nodes */
/* all send to zero, when they get back that all have sent, they */
/* continue */

/* try Glib_Signal(npid, vnn, sig)? instead */
int 
lam_sync_all() {
  int i;
  int done; 
  int tries; 
  int total_time;
  int num_nodes; 

  struct timeval tv_begin, tv_end,lapsed; 
  
  /* Make sure there is one only a single lam_sync_all in progress */
  BR_SYNC_ALL_LOCK;
  
  done = 0;
  tries = 0;
  lam_sync_count= lam_old_count = 0;
  num_nodes = BR_CLUSTERS();

  if (lam_myproc != 0) {
    while (!done) {
      
      /* issue request */
      tries++;
      BR_REQUEST_4(0,(void (*)())lam_sync_req_h,lam_myproc,0,0,0);
      /* wait for reply or time out */
      gettimeofday(&tv_begin,NULL); 
      total_time =0;

      /* wait 1000 msec for zero to respond thread to change tick value */
      while ( (total_time < 20000) && (lam_sync_count == lam_old_count)) {
	gettimeofday(&tv_end,NULL); 
	if (tv_begin.tv_usec > tv_end.tv_usec) {
	  tv_end.tv_usec += 1000000;
	  tv_end.tv_sec--;
	}
	lapsed.tv_usec = tv_end.tv_usec - tv_begin.tv_usec;
	lapsed.tv_sec =  tv_end.tv_sec - tv_begin.tv_sec;  
	total_time = lapsed.tv_sec*1000000+ lapsed.tv_usec ;

	BR_POLL();
      }
      if (lam_sync_count == lam_procs) 
	done =1;

      if (tries > num_nodes*1000) 
	done =1;

      
      if (lam_old_count == lam_sync_count)
	lam_restore_flow(0);
      else 
	lam_old_count = lam_sync_count;

    }

    if (lam_old_count == 0) {
      printf(" node zero not responding \n");
    } 
    else { 
      if (lam_sync_count != lam_procs) {
	printf(" some nodes not responding \n");
      }
    }
  } /* if I am not zero */
  else { /* am zero, wait for up to 15 seconds before giving up */
    lam_sync_count++;

    gettimeofday(&tv_begin,NULL); 
    total_time =0;
    
    while ( !done ) {
      BR_POLL();
      
      gettimeofday(&tv_end,NULL); 
      if (tv_begin.tv_usec > tv_end.tv_usec) {
	tv_end.tv_usec += 1000000;
	tv_end.tv_sec--;
      }
      lapsed.tv_usec = tv_end.tv_usec - tv_begin.tv_usec;
      lapsed.tv_sec =  tv_end.tv_sec - tv_begin.tv_sec;  
      total_time = lapsed.tv_sec*1000000+ lapsed.tv_usec ;

      if (total_time > 15000000*num_nodes )
	done =1;

      if ( lam_sync_count == lam_procs) 
	done =1;
      
    }

    if (lam_sync_count != lam_procs) {
      printf("proc %d: nodes which did not respond: ",lam_myproc);
      for (i=1; i< lam_procs; i++ ) {
	if (lam_sync_flag[i] == 0) 
	  printf("%d ",i);
      }
      printf("%\n");
    }
  }

  if (lam_sync_count != lam_procs) {
    BR_SYNC_ALL_UNLOCK;
    return -1;
  }

  BR_SYNC_ALL_UNLOCK;
  return 1;
}





