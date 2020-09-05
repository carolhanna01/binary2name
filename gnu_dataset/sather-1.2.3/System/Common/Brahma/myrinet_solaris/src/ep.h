/*									tab:8
 *
 * ep.h - Endpoint description for the Lanai AM layer.
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
 * Version:		65
 * Creation Date:	Sun Feb  5 21:46:21 1995
 * Filename:		ep.h
 * History:             
 */

/* all test structs assume that shorts don't get packed */

#define DEPTH      4   /* number of outstanding messages between nodes */
#define MAX_REMOTE 8   /* max number of nodes */
#define MAX_HOPS   8

#define MAX_DEPTH 8 /* max depth of *switches* between two nodes for routes */ 

#define IN_Q_SIZE  (2*(DEPTH*MAX_REMOTE))
#define OUT_Q_SIZE (2*(DEPTH*MAX_REMOTE))

#define IN_SIZE_MASK (IN_Q_SIZE-1)    /* QUEUES MUST BE POWERS OF 2 */
#define OUT_SIZE_MASK (OUT_Q_SIZE-1)

#define LAM_PKT_WORDS 1024               /* packet size in words */
#define LAM_PKT_SHORTS (LAM_PKT_WORDS*2) /* in shorts */
#define LAM_PKT_BYTES (LAM_PKT_WORDS*4) /* bytes */

#define MAX_IT_PERIOD 0x7fff 
/* we have two defs foe what's inside a descriptor, one for the LANai
 * and one for the host. 
 * both should end up being the same size 
 */

/* THESE ARE 16 bit versions of the data structures for LANAI 2.X! */

#ifdef LANAI

#define OP_MASK   0x7 /* hack for medium messages  should be 0x8 */
#define SHORT     0x1
#define BULK      0x2
#define BAD_CRC   0x4
#define MEDIUM    0xa     /* mark it as BULK as well */

#define AM_TYPE_MASK 0xFF00
#define AM_TYPE      0x0100

#define DEST_MASK 0x00ff

typedef struct bulk_data {
  short la_hi;     /* local I/O virtual address to start xfer */
  short la_low;    /* has to be 32-bit aligned                */
  short len_hi;    /* length of xfer in bytes                 */
  short len_low;
  short data[8];  /*  bytes of payload                 */

} bulk_t;

typedef struct desc {
  short dest;    /* or source for input possible timeout field? */    
  short op;      /* own bit and op-code set */
  union {
    short s_data[12];  /* six 32 bit-words of payload  */
    bulk_t b_hdr;
  } hdr;
  short crc;    /* byte for tail flit for CRC check */
  short junk; 
} desc_t ;

typedef struct bulk_d {
  short hi;  /* incomming pointer to bulk data xfer */
  short low;
}  bulk_p_t;

/* host uses int for better access */
#else 



#define AM_TYPE_MASK 0xFF000000
#define AM_TYPE      0x01000000

#define DEST_MASK 0x00ff0000
#define DEST_SHIFT 0x10

/* we trick the lanai4 into thinking a medium == a bulk this way */
#ifdef LANAI4 
#define OP_MASK   0x00000007
#else
#define OP_MASK   0x0000000f
#endif 

#define SHORT     0x00000001     /* small packet */
#define BULK      0x00000002     /* bulk xfer    */
#define BAD_CRC   0x00000004
#define MEDIUM    0x0000000a     /* mark it as BULK as well */

#define REQ_MASK 0x00000010     /* request/reply type */
#define REQUEST  0x00000000     
#define REPLY    0x00000010

#define SEQ_MASK 0x0000ff00     /* request sequence number */
#define SEQ_SHIFT 0x8
#define SEQ_SIZE_MASK 0xff;     /* 8 bits of sequence number */

typedef struct bulk_data {
  volatile int *la;           /* local pointer to data */
  volatile int len;           /* length in bytes       */
  volatile int data[4];       /* 5 words of payload */
} bulk_t;

/* a descriptor consists of an owned bit, opcode and dest 
 * for small xfers, we get 7 words of payload 
 * for bulk data, we use two words for the src and len and get
 * get 5 words of payload 
 */

typedef struct desc {
  volatile int op_dest;
  union {
    volatile int s_data[6];      /* small payload */
    volatile bulk_t b_hdr;       /* bulk header   */
  } hdr ;
  int crc;
} desc_t ;

typedef struct bulk_d {
  volatile unsigned int *hi;  /* incomming pointer to bulk data xfer */
}  bulk_p_t;

#define MAX_POLL_COUNT 0x00FFFFFF

#endif /* host */

/* a route is a length followed by at most 8 hops */
typedef struct route {
  volatile unsigned int len;
  volatile unsigned int pad;  
  volatile unsigned short path[4];
} route_t;

/* private data associated with each endpoint */
typedef struct private_ep {
  volatile unsigned int src;
} private_ep_t;

/* Each endpoint is an 8k block with the following structure */
typedef struct ep {

  bulk_p_t bulk_d_p[IN_Q_SIZE];

  /* request descriptors */
  desc_t out[OUT_Q_SIZE];

  /* reply descriptors   */
  desc_t in[IN_Q_SIZE];

} ep_t ; 


/* Support for indirect AM */

#define MAX_HANDLERS 512
#define MAX_SEGMENTS 256
#define NULL_HANDLER 0
#define OFFSET_SHIFT 2
#define OFFSET_MASK (~0x3)

typedef struct s {
  void *base;
	int len;
} seg_t;

#ifdef __GNUC__

#define AM_INIT_LOCK(mem) {mem=1;}
#define AM_SWAP(mem,reg) asm("swap %2,%0" : "=r" (reg) : "0" (reg), "m" (mem));
#define AM_LOCK(mem) {register int reg=0; do {AM_SWAP(mem,reg); } while(reg ==0 );}
#define AM_UNLOCK(mem) { mem = 1; }

#else 

#define AM_INIT_LOCK(mem) {mem=1;}
#define AM_SWAP(mem,val)  (val=am_swap(val,mem)) /* tbd */
#define AM_LOCK(mem) 
#define AM_UNLOCK(mem)  { mem = 1; }

#endif 
