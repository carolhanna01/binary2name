/*									tab:8
 *
 * lcp.c - Lanai Control Program for the Lanai AM layer.
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
 * Version:		50
 * Creation Date:	Sun Feb  5 21:46:21 1995
 * Filename:		lcp.c
 * History:             
 *	RPM	50	Thu Apr 11 11:47:01 1996
 *		Modified to work with Lanai 4 
 */

#include <lanai4_def.h>
#include "ep.h"

#define E2L		1
#define L2E  		0

/*********/

#define START_USER_STACK (1024*254) 

#define HOST_DMA_FREE 0x0
#define HOST_DMA_RECV 0x1
#define HOST_DMA_SEND 0x2

/* one endpoint for now */
int pad0; /* must double word align these two. cheap hack forever */
int pad1; 
int dma_sts; 

ep_t endpt;
private_ep_t p_endpt;

route_t routes[MAX_REMOTE]; /* max 32 hosts for now */

/* staging areas  -- 4K each + descriptor + tail flit */
int snd_buf0[LAM_PKT_WORDS+sizeof(desc_t)+2];
int snd_buf1[LAM_PKT_WORDS+sizeof(desc_t)+2];

int rcv_buf0[LAM_PKT_WORDS+sizeof(desc_t)+2];

int time_start,time_stop;

volatile int debug_event;
int *l,*h;
int dropped, bad_type;

volatile int host_DMA_mutex;
volatile int prefetch; 
int *prefetch_buf, *send_buf;

int trace_num;
int trace_buf[32];
#define TRACE_POINT(x) {trace_buf[trace_num]=x ; trace_num=(trace_num+1)%32;}

int pc_trace_num;
int pc_trace_buf[4];
#define PC_TRACE_POINT(x) {pc_trace_buf[pc_trace_num]=x ; \
                           pc_trace_num=(pc_trace_num+1)%4;}
volatile int send_trace_num;
volatile int send_trace_buf[8];
#ifdef DEBUG
#define SEND_TRACE_POINT(x) {send_trace_buf[send_trace_num]=x ; \
                             send_trace_num=(send_trace_num+1)%8;}
#else 
#define  SEND_TRACE_POINT(x) 
#endif 

volatile int send_count;
volatile int recv_count;
volatile int crc_errors; 
volatile int len_errors; 
volatile int too_long; 
volatile int broken_myricom_stall_time;

volatile int recv_ticks, send_ticks, recv_count, send_count;

volatile int BOGUS_SH;

int network_fres_cnt;
int host_fres_cnt;

volatile int depth; 
volatile int max_remote; 
volatile int max_hops; 

volatile int lanai_done; 
volatile int host_done; 
volatile int wait,wait2; 
volatile int good_len; 

#ifdef BIG_CONTEXTS 
#define GAM_RESUME_SYSTEM   big_context_resume_system()
#define GAM_RESUME_USER     big_context_resume_user()
#else 
#define GAM_RESUME_SYSTEM   RESUME_SYSTEM
#define GAM_RESUME_USER     RESUME_USER
#endif

/***************************************************************/

void big_context_resume_user();

break_pt(int pc) {
 debug_event=pc;
 TRACE_POINT(0x55);
 TRACE_POINT(APC);
}

volatile unsigned int stalls,stall_spin;

void 
stall(int usec) {
  volatile unsigned time_now; 
  volatile unsigned time_return; 

  if (usec == 0 )
    return;

  stalls++;
  time_now = RTC; 
  time_return = time_now+usec; 

  if (time_return >  time_now) {
    while (RTC < time_return) {
      stall_spin++;
      GAM_RESUME_SYSTEM;
    }
  }
  else {
    while (time_return < RTC ) {
      stall_spin++;
      GAM_RESUME_SYSTEM;
    }
  }
  
}

void
recv_thread() {
  register  desc_t *in_d; /* current descriptor to receive */
  register int in_num;  /* input descriptor number */
  register unsigned int recv_all;
  register unsigned int good_crc;
  int i; volatile int j; 

  in_num = 0;
  in_d = &(endpt.in[0]);

  while (1) {
    /* try to receive anything, then send */

#ifdef DEBUG
    recv_ticks++;
#endif 

    /* interrupts are totally different in Lanai 4 */    
    /*    if (AM_INTR == 1) {
     *      {
     *	register int temp;
     *	temp = (in_num == 0) ? IN_SIZE_MASK : in_num-1;
     *	if ( (endpt.in[temp].op_dest&OP_MASK) != 0)
     *	  INT =1;   need to set lanai 4 interrupt here 
     *      }
     *    }
     */
    
    if (ISR & WORD_RDY_BIT ) {

#ifdef DEBUG
    recv_count++;
#endif

    recv_all = RW; 

    if ((recv_all & AM_TYPE_MASK) != AM_TYPE) {
      bad_type++;
      goto drop;
    }

    /* check if current descriptor is free */
    if ( (in_d->op_dest & OP_MASK) == 0) {

      /*MAKE SURE CRC BYTE DOESN'T COUNT!*/
      RMP = (int * ) &(in_d->hdr.s_data[0]) ;
      RML = (int * ) &(in_d->crc) ; 
      /* switch to send thread while waiting for header */
      /* if we get a small packet, both will be set, but on a */
      /* bulk packet, then only buff int gets set */

      while (!(ISR & (RECV_INT_BIT|BUFF_INT_BIT)))
	GAM_RESUME_USER;

	/* not needed ? */
	/* in_d->op_dest = in_d->op_destrecv_src ; */ 

	/* see if It's a small packet */
      if ( (recv_all &OP_MASK) == SHORT) {

	/* in_d->op_dest =  recv_all;	 cheap hack ;*/
	  
	/* check ORUN to make sure packet is  multiple of 4 bytes only */ 

	good_len = (ISR & (ORUN1_INT_BIT | ORUN2_INT_BIT));  
	  	  
	if (((in_d->crc&0xff000000 ) == 0) && /* good tail flit */
	    (good_len == (ORUN1_INT_BIT|ORUN2_INT_BIT))){ 
	  in_d->op_dest =  recv_all;	 /* end nextbuf free ??? */
	}
	else {
	  in_d->op_dest = recv_all|BAD_CRC;
	  crc_errors++;
	  
	  if (good_len != (ORUN1_INT_BIT|ORUN2_INT_BIT)) 
	    len_errors++;
	}

      }
	else {	/* a bulk packet */
	  /* try to suck in whole body */
          /* because the tail flit is appended to the packet, and counts 
           * as part of the length, the first 4 bytes got left in the CRC
           * Part of the header....
           * hack is to copy them here and set the pointer two down ..
           * Grrr.
           */

	  *rcv_buf0 = in_d->crc;
	  /* *(rcv_buf0+1) = in_d->junk; NOt needed -RPM */

	  RMP = (int * ) &(rcv_buf0[1]);
	  RML = (int * ) &(rcv_buf0[LAM_PKT_WORDS+2]) ;

	  /* switch to send thread while waiting for msg to complete */	 

	  while (!(ISR & RECV_INT_BIT)) {
	    GAM_RESUME_USER;

	    if (ISR & BUFF_INT_BIT) {
	      too_long++;
	      goto drop; 
	    }
	  }

	  /* check ORUN to make sure packet is  multiple of 4 bytes only */ 

	  good_len = (ISR & (ORUN1_INT_BIT | ORUN2_INT_BIT)); 
	  	  
	  /* check CRC */
	  
	  good_crc =((( (unsigned short) (*((unsigned short* ) ((int) RMP -
								2)))
		       >> 8) & 0xff) == 0);

	  if (good_crc && 
	      (good_len == (ORUN1_INT_BIT|ORUN2_INT_BIT))) { /* good crc&len*/
	    /* while DMA is busy, switch to send thread */
	    while (host_DMA_mutex != HOST_DMA_FREE)
	      GAM_RESUME_USER;

	    host_DMA_mutex = HOST_DMA_RECV;	/* mark host DMA as busy */
	    /* move data into host */
	    EAR = (int)   endpt.bulk_d_p[in_num].hi;
	    /* EARL = endpt.bulk_d_p[in_num].low; */
	    LAR = (int) rcv_buf0;
	    DMA_DIR = L2E;
	    DMA_CTR = (in_d->hdr.b_hdr.len);

	    /* while DMA is not done, switch to send thread */
	    TRACE_POINT(__LINE__); /* take out when compiler bug gets fixed */

	    while (!(ISR & DMA_INT_BIT)) {
	      GAM_RESUME_USER; 
	    }
	    host_DMA_mutex = HOST_DMA_FREE;	/* mark host DMA as free */
	    in_d->op_dest = recv_all; 
	  }

	  /* bad CRC */
	  else {
	    in_d->op_dest = recv_all|BAD_CRC;
	    crc_errors++;

	    if (good_len != (ORUN1_INT_BIT|ORUN2_INT_BIT)) 
	      len_errors++;
	  }
	} /* end bulk */
	in_num = (in_num+1) & IN_SIZE_MASK;
	in_d = &(endpt.in[in_num]);
      }
      /* if the input buffer was full, don't advance the pointer, we */
      /* just drop everything on the floor */
      else { 
	dropped++;
      drop:
	RMP = (int *) &(rcv_buf0[0]);
	RML = (int *) &(rcv_buf0[LAM_PKT_WORDS+sizeof(desc_t)+2]) ;
	/* switch to send thread while waiting for msg to complete */

	while (!(ISR & RECV_INT_BIT)) {
	  GAM_RESUME_USER; 
	  if (ISR & BUFF_INT_BIT) {
	    too_long++;
	    goto drop; 
	  }
	}
      }
    } /* anything to receive */
    GAM_RESUME_USER;		/* switch to send thread */
  }
} /* recv_thread */



void 
send_thread()
{
  register desc_t *out_d,*out_d_low,*out_d_high;
  register desc_t *out_d_next;  /* current output descriptor */

  int dest; 
  register unsigned int op_dest; 

  out_d = &(endpt.out[0]);
  out_d_next =  &(endpt.out[1]);

  out_d_low =  &(endpt.out[0]);
  out_d_high = &(endpt.out[OUT_SIZE_MASK]);

  prefetch =0;
  send_buf = snd_buf0;
  prefetch_buf = snd_buf1;

  while (1) {

  send_top: /* switch to receive thread while no work */

#ifdef DEBUG
    send_ticks++;
#endif 
    /* NOT! fixed in lanai3-gcc 2.7.2f + versions bummer!!!!! */
    /*    stall(broken_myricom_stall_time);    */

    /* if host posts a send request */
    if (((op_dest = out_d->op_dest) &OP_MASK) == SHORT)  {  

#ifdef DEBUG
    send_count++;
#endif 

    while (!(ISR & SEND_RDY_BIT)) {
      GAM_RESUME_SYSTEM;

    }

    dest = (op_dest & DEST_MASK) >>  DEST_SHIFT;
    
    switch (routes[dest].len) {
    case 0:
       /* special case refects to self */
       goto skip_wait;
     case 1:
       SB = (routes[dest].path[0]);  
       goto skip_wait;
     case 2:
       SH = (routes[dest].path[0]);
       goto skip_wait;
     case 3:
       SA = 1;
       SMP = (int *)(routes[dest].path);
       SML = (int *)(routes[dest].path);
       break;
     case 4:
       SMP = (int *)(routes[dest].path);
       SML = (int *)(routes[dest].path); 
       break; 
     case 5:
       SA = 3;
       SMP = (int *)(routes[dest].path);
       SML = (int *)(routes[dest].path+2);
       break;
     case 6:
       SA = 2;
       SMP = (int *)(routes[dest].path);
       SML = (int *)(routes[dest].path+2);
       break;
     case 7:
       SA = 1;
       SMP = (int *)(routes[dest].path);
       SML = (int *)(routes[dest].path+2);
       break;
     case 8:
       SMP = (int *)(routes[dest].path);
       SML = (int *)(routes[dest].path+2);
       break;
     default:
       break; 
     }

    while (!(ISR & SEND_INT_BIT)) {
      GAM_RESUME_SYSTEM; 

    } 
   
  skip_wait:

      /* overwrite dest with source word */
      out_d->op_dest =  (p_endpt.src<<DEST_SHIFT) | 
	                (op_dest &(~DEST_MASK));

      SMP = (int *) &(out_d->op_dest);
      SMLT = (int *) &(out_d->hdr.s_data[5]); 

      /* switch to receive thread while waiting to send out header */

      while (!(ISR & SEND_INT_BIT)) {
	GAM_RESUME_SYSTEM; 
      }

      /* clear op-code */
      /* hack for mediums, we have the make sure we always erase this bit */
      out_d->op_dest = out_d->op_dest & (~(OP_MASK|0x08)); 

      /* inc and mod */
      out_d = out_d_next;
      if (out_d_next == out_d_high) 
	out_d_next = out_d_low;
      else 
	out_d_next++;

      GAM_RESUME_SYSTEM;
      goto send_top;

    } /* short message */

    /* this bulk packet had it's data prefetch from last time */
    if (prefetch) { 

#ifdef DEBUG 
    send_count++;
#endif 

    send_prefetch:
      /* swap prefetch and send buffers */
      {  int *temp;
	 temp = send_buf;
	 send_buf = prefetch_buf;
	 prefetch_buf = temp;
	 prefetch =0;
       }

      while (!(ISR & DMA_INT_BIT)) {
	GAM_RESUME_SYSTEM;
      }

      host_DMA_mutex = HOST_DMA_FREE;    /* mark host DMA as free */
      GAM_RESUME_SYSTEM;        /* give receive a shot at DMA */
      
      /* send header out to reduce latency as much as possible */
      touch(ISR);
      while (!(ISR & SEND_RDY_BIT)) {
	GAM_RESUME_SYSTEM;
      }


    dest = (out_d->op_dest & DEST_MASK) >> DEST_SHIFT;
    
    switch (routes[dest].len) {
    case 0:
       goto skip_wait2;  /* special case refects to self */
     case 1:
       SB = (routes[dest].path[0]);  
       goto skip_wait2;
     case 2:
       SH = (routes[dest].path[0]);
       goto skip_wait2;
     case 3:
       SA = 1;
       SMP = (int *)(routes[dest].path);
       SML = (int *)(routes[dest].path);
       break;
     case 4:
       SMP = (int *)(routes[dest].path);
       SML = (int *)(routes[dest].path); 
       break; 
     case 5:
       SA = 3;
       SMP = (int *)(routes[dest].path);
       SML = (int *)(routes[dest].path+2);
       break;
     case 6:
       SA = 2;
       SMP = (int *)(routes[dest].path);
       SML = (int *)(routes[dest].path+2);
       break;
     case 7:
       SA = 1;
       SMP = (int *)(routes[dest].path);
       SML = (int *)(routes[dest].path+2);
       break;
     case 8:
       SMP = (int *)(routes[dest].path);
       SML = (int *)(routes[dest].path+2);
       break;
     default:
       break; 
     }

    while (!(ISR & SEND_INT_BIT)) {
      GAM_RESUME_SYSTEM; 
    } 
   
  skip_wait2:
      /* overwrite dest with source word */
      out_d->op_dest =  (p_endpt.src<<DEST_SHIFT) | 
	                (out_d->op_dest &(~DEST_MASK));

      SMP = (int *) &(out_d->op_dest);
      SML = (int *) &(out_d->hdr.s_data[5]); 
      
      /* try to get next packet */
      if ( (out_d_next->op_dest&(OP_MASK)) == BULK) { 

	/* if we can't grab the mutex abort */
	/* the prefetch, since receive has it */
	/* if (0)   turn this op to dis-able prefetch */
	if  (host_DMA_mutex == HOST_DMA_FREE) {  
	  host_DMA_mutex = HOST_DMA_SEND;   /* mark host DMA as busy */
	  /* move data from host to LANai memory */
	  EAR = (int)  out_d_next->hdr.b_hdr.la;
	  LAR = (int) prefetch_buf;
	  DMA_DIR = E2L;
	  DMA_CTR = (out_d_next->hdr.b_hdr.len);
	  prefetch = 1;
	} /* we got the DMA mutex */
      } /* try to prefetch the next bulk packet */

      /* wait for header */
      while (!(ISR & SEND_INT_BIT)) {
	GAM_RESUME_SYSTEM; 
      }

      SMP = (int *) send_buf;
      SMLT = (int *) ( (int) send_buf + out_d->hdr.b_hdr.len) ;

      /* switch to receive thread while DMA is not done */
      if ( host_DMA_mutex == HOST_DMA_FREE) {
	  SEND_TRACE_POINT(0x1);

	  while (!(ISR & SEND_INT_BIT)) {
	    GAM_RESUME_SYSTEM;
	  }
	}
      else {
	while (!(ISR & SEND_INT_BIT)) {
	  SEND_TRACE_POINT(0x2);
	  if (ISR & DMA_INT_BIT) 
	    host_DMA_mutex = HOST_DMA_FREE;
	  GAM_RESUME_SYSTEM;
	}
      }

    /* clear op-code */
    /* hack for medium messages, since they use an extra bit */
    out_d->op_dest = out_d->op_dest & ((~(OP_MASK|0x8))); 

      /* inc and mod */
    out_d = out_d_next;
    if (out_d_next == out_d_high) 
	out_d_next = out_d_low;
    else 
      out_d_next++;

    GAM_RESUME_SYSTEM;
    SEND_TRACE_POINT(0x3);
    goto send_top;
  } /* end if prefetch */
    
    /* first bulk packet, start the prefetch */
    if ( (out_d->op_dest&OP_MASK) == BULK ) {  
      while (host_DMA_mutex != HOST_DMA_FREE) {
	GAM_RESUME_SYSTEM;
      }

      host_DMA_mutex = HOST_DMA_SEND;   /* mark host DMA as busy */
      /* move data from host to LANai memory */
      EAR = (int) out_d->hdr.b_hdr.la;
      LAR = (int) prefetch_buf;
      DMA_DIR = E2L;
      DMA_CTR = (out_d->hdr.b_hdr.len);

      prefetch = 1;
      SEND_TRACE_POINT(0x5);
      goto send_prefetch;
    }

    GAM_RESUME_SYSTEM;
  } /* end main loop */

}


/* Enable the Myrinet Link Interface */
msg_passing_init() {
  int i;

  if (ISR & NRES_INT_BIT) {
    network_fres_cnt++;	/* count network resets */
  }
  else 
    host_fres_cnt++;     /* count host resets */

  /*  ISR = 0xFFFFFFFF; */

  /* may have change NRES later when mapper is enabled */
  /*  MYRINET = NRES_ENABLE_BIT | CRC_ENABLE_BIT; */
  MYRINET = CRC_ENABLE_BIT; 
	/* Don't allow the myrinet to be forward reset */
	 /* MYRINET = CRC_ENABLE_BIT;*/ 
  VERSION = 3;
  TIMEOUT = 0;
  DMA_STS = dma_sts;  

}

/* small function to drain the network while we are waiting the host 
 * to jerk.
 */

void 
drain_network() {
  register unsigned int bit_bucket;
  
 while (ISR & WORD_RDY_BIT ) {
   bit_bucket = RW;
 
   /* break out of loop if we hit end of packet -- go back and pay attention
    * to the host 
    */
   if (ISR & TAIL_INT_BIT) 
     return;
 }

} 

main()
{
  int j;

  send_ticks = recv_ticks = send_count = recv_count = crc_errors = 
    too_long = 0;

  debug_event =0;
  host_DMA_mutex = 0;    /* 0 = DMA is free */
  dropped=bad_type=0;
  pad0=0xdead; 
  pad1=0xbad; 
  dma_sts = 0x7;   /* if the host wants to fix later, it can */

  /* this code assumes host always zeros lanai mem.     */
  /* doesn't work if we get hit with a forward reset   */

  msg_passing_init();

  broken_myricom_stall_time =10; 

  pad1=0xbeaf; 

  trace_num= pc_trace_num = send_trace_num=send_count= recv_count=0;

  IMR = 0; /* no interrupts */

  /* tell host we are ready */
  lanai_done =1;

  /* host must set up recv queue*/
  TRACE_POINT(__LINE__);

 /*  generates some weird code which does not work on lanai 4.1 !  */
  /*while (! (H_INIT_DONE)) {
   * wait2++;
   *  };  
   */


  /* host tells us it's ready */
  while (! (host_done)) {
    wait++;
  };  
  
  DMA_STS = dma_sts; 

  TRACE_POINT(__LINE__);

  START_USER(send_thread,START_USER_STACK); 

  recv_thread();

} /* end main */




