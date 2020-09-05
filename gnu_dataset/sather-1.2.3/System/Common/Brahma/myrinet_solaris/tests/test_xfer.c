/*                                                                      tab:8
 *
 * "Copyright (c) 1993 The Regents of the University of California.
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
 * Author:              Richard P. Martin
 * Version: 		5
 * Creation Date:       Sat Nov 27 12:26:03 PST 1993
 * Filename:            xfer test for HPAM library
 * History:
 *	RPM	5	Wed May  4 22:00:42 1994
 *		fixed to deal with possible re-ordering 
 *	RPM	4	Wed Jan  5 12:07:47 1994
 *		changed names to hpam from pmam
 *	RPM	3	Sun Nov 28 18:02:22 1993
 *		fixed bug which added to non source alignmed data
 *	RPM	2	Sat Nov 27 15:46:32 1993
 *		first version now working
 *	RPM	1	Sat Nov 27 12:26:16 1993
 *		first version still not quite working 
 *
 */

/* this file sends a total of SIZES different sized segments using a
 * bulk put/get interface. For each size, we change the start of 
 * of the offset to a new byte alignment.
 */

#include <stdio.h>
#include <string.h>
#include <sys/types.h>
#include <sys/socket.h>
#include <netinet/in.h>
#include <netdb.h>
#include <fcntl.h>
#include <time.h>
#include <sys/systeminfo.h> 

#include "am.h"

extern int errno;

#define SIZES 10
#define ALIGNMENTS 4
#define CLEAR_TAIL 32

/* #define TEST_PATTERN(x) ((x%255)+1) */
/* #define TEST_PATTERN(x) (0xFF)  */
#define TEST_PATTERN(x) ((x)|(x<<1)|(x<<2)|0x77) 


#define ALL_ALL_SIZE 512

int get_cnt,put_cnt;
int clear_flag;

char buffer1[20*16384];
char buffer2[20*16384];
char *buf1,*buf2;

int sizes[SIZES];
int am_self_address;
int am_max_pkt;

static void incr(int src,volatile int *flag) 
{
  (*flag)++;
}

static void clear(int src,char *buf,int len,void *flag)
{
  memset(buf,0,len);
  gam_reply_4(src,incr,flag);
}

/* get functions
 * on the receiving side, the sink function. Just increment the 
 * get counter 
 */

static void bulk_get_reply_h(int src,void *buf, int len, volatile int *cnt)
{
  (*cnt)+=len;
}

static void bulk_get(int node,void *l_buf,void *r_buf,int len)
{
  int i; 
  
  if (len <= am_max_pkt) { /* fast check, most often < am_max_pkt */
    gam_get(node,r_buf,l_buf,len,bulk_get_reply_h,&get_cnt);
  }
  else {   /* chunk the data into maximum packet sizes */
    for (i=am_max_pkt;i<len;i+=am_max_pkt) {
      gam_get(node,r_buf,l_buf,am_max_pkt,bulk_get_reply_h,&get_cnt);
      (char *) l_buf+= am_max_pkt; (char *) r_buf+= am_max_pkt;
    }
    i-=am_max_pkt;
    /*get the last packet */
    gam_get(node,r_buf,l_buf,len-i,bulk_get_reply_h,&get_cnt);
  }
} 


/* put functions 
 * The protocol is for the sender to continually store into the 
 * node. Each ack returns the amount put. 
 */

static void bulk_put_reply_h(int src,volatile int *cnt,int len)
{
  (*cnt)+=len;
}

static void bulk_put_handler(int src,void *buf, int len,void *cnt)
{
  gam_reply_4(src,bulk_put_reply_h,cnt,len);
}

static void bulk_put(int node,void *l_buf,void *r_buf,int len)
{
  int i;

  if( len <= am_max_pkt) {
    gam_store(node,l_buf,r_buf,len,bulk_put_handler,&put_cnt);
  }
  else {
    /* chunk the data into maximum packet sizes */
    for (i=am_max_pkt;i<len;i+=am_max_pkt) {
      gam_store(node,l_buf,r_buf,am_max_pkt,bulk_put_handler,&put_cnt);
      (char *) l_buf+= am_max_pkt; (char *)r_buf+= am_max_pkt;
    }
    i-=am_max_pkt;
    /*put the last packet */
    gam_store(node,l_buf,r_buf,len-i,bulk_put_handler,&put_cnt);
  }

}

void check_buf(char *data, int a1, int a2, int s)
{
  int i,j;
  char c;
  int start_region, end_region;

  /* check to make sure didn't store something too soon*/

  start_region =-1;

  if (data[-1] !=0 )
    printf("node %d pre condition failed for size %d alignments %d %d\n",
	   am_self_address, s,a1,a2);  

  for (i=a1,j=a2;i<(s+a1);i++,j++) {

    c=(char) (TEST_PATTERN(j));

    if (data[i] !=c ) {
      if (start_region <0) {
	start_region = i;
	printf("node %d local check failed for size %d alignments %d %d at %d\n",
	       am_self_address, s,a1,a2,i);
      }
    }
    else {
      if (start_region >0) {
	end_region =i-1;
	printf("node %d failed region [%d:%d]\n",am_self_address,
	       start_region,end_region);
	start_region = -1;
      }
    }

  }

  /* check the last char, should be zero */
  if (data[i] !=0 )
    printf("node %d end condition failed for size %d alignments %d %d\n",
	   am_self_address, s,a1,a2);


} /* check_buf */

void remote_check_buf(int src,char *data, int a1, int a2, int s)
{
  int i,j;
  char c;

  /* check to make sure didn't store something too soon*/

  if (data[-1] !=0 )
    printf("node %d pre condition failed for size %d alignments %d %d\n",
	   am_self_address, s,a1,a2);  

  for (i=a1,j=a2;i<(s+a1);i++,j++) {

    c=(char) (TEST_PATTERN(j));

    if (data[i] !=c ) {
      printf("node %d remote check failed for size %d alignments %d %d at %d\n",
	     am_self_address, s,a1,a2,i);
      goto end;
    }

  }

  /* check the last char, should be zero */
  if (data[i] !=0 )
    printf("node %d end condition failed for size %d alignments %d %d\n",
	   am_self_address, s,a1,a2);

 end: gam_reply_4(src,incr,&clear_flag);
}

main(int argc,char *argv[],char *envp[])
{
  int i,j,k,s;
  int iterations,reps;
  int mask;
  int align;
  int me,other,procs;  /* node to exchanges xfers with */
  int next;
  static volatile int go;
  struct timeval start,finish,lapsed;
  struct timezone tzone;
  char my_name[256];
  double total_time,rtt;

#ifdef GLUNIX 
  /* lam_glunix_spawn(&argc,argv,envp);  */
  if (!Glib_Initialize()) {
    fprintf(stderr,"Error - GLUnix couldn't initialize!\n");
    exit(-1);
  }
#endif

  gam_enable(0); /* 1= use glunix */

  sysinfo(SI_HOSTNAME,my_name,256);

  printf("Node %d: %s passed gam_enable total nodes %d\n", gam_my_proc(),
	 my_name,gam_procs());  
  fflush(stdout);
  gam_barrier();

  printf("I am node %d \n,",gam_my_proc()); 
  fflush(stdout); 

  am_self_address = gam_my_proc();
  me = am_self_address; 

  other =1; /* default is between 0 and 1 */
  procs = gam_procs();
  am_max_pkt= gam_max_size();

  sizes[0]=1; 
  sizes[1]=4; sizes[2]=16; sizes[3]=37; sizes[4]=am_max_pkt/4; 
  sizes[5]=am_max_pkt; sizes[6]=2*am_max_pkt; sizes[7] = (4*am_max_pkt/3); 
  sizes[8]=(10*am_max_pkt); sizes[9]=1;


  go =0;

  if (argc > 1) {
    other = atoi(argv[1]);
    printf("test get and put from 0 to %d \n",other);
    fflush(stdout);
  }

/* do a series of gets/puts between 0 and other */
  buf1=&buffer1[4];
  buf2=buffer2;

  for (i=0;i<20*am_max_pkt;i++) {
    buf2[i] = (char) (TEST_PATTERN(i));
  }

  gam_barrier();

/* this first set of tests is from 0 to 1 */

  if (me == 0 ) {

    printf("Node %d testing get from %d \n",me,other);
    fflush(stdout);

    for (i=0;i<ALIGNMENTS;i++) {      /* for all alignments and sizes */
      for (k=0;k<ALIGNMENTS;k++) { 
	for (s=0;s<SIZES;s++) {  
	  memset(buffer1,0,sizes[s]+CLEAR_TAIL);
	  get_cnt=0;
	  bulk_get(other,(void *)&buf1[i],(void *) &buf2[k],sizes[s]);
	  gam_wait(&get_cnt,sizes[s]);
	  check_buf(buf1,i,k,sizes[s]);
	}
      }
    }

    printf("Node %d testing put to %d \n",me,other);

    for (i=0;i<ALIGNMENTS;i++) {      /* for all alignments and sizes */
      for (k=0;k<ALIGNMENTS;k++) { 
	for (s=0;s<SIZES;s++) {  
	  clear_flag=0;
	  put_cnt =0;
	  gam_request_4(other,clear,buffer1,sizes[s]+CLEAR_TAIL,&clear_flag);
	  gam_wait(&clear_flag,1);

	  bulk_put(other,(void *)&buf2[k],(void *) &buf1[i],sizes[s]);
	  gam_wait(&put_cnt,sizes[s]);
	  gam_request_4(other,remote_check_buf,buf1,i,k,sizes[s]);
	  gam_wait(&clear_flag,1);
	}
      }
    }

  } /* if me == 0 */
  gam_barrier();

  if (me == other)  {

    printf("Node %d testing get from %d \n",me,0);

    for (i=0;i<ALIGNMENTS;i++) {      /* for all alignments and sizes */
      for (k=0;k<ALIGNMENTS;k++) { 
	for (s=0;s<SIZES;s++) {  
	  memset(buffer1,0,sizes[s]+CLEAR_TAIL);
	  get_cnt =0;
	  bulk_get(0,(void *)&buf1[i],(void *) &buf2[k],sizes[s]);
	  gam_wait(&get_cnt,sizes[s]);
	  check_buf(buf1,i,k,sizes[s]);
	}
      }
    }

    printf("Node %d testing put to %d \n",me,0);

    for (i=0;i<ALIGNMENTS;i++) {      /* for all alignments and sizes */
      for (k=0;k<ALIGNMENTS;k++) { 
	for (s=0;s<SIZES;s++) {  
	  put_cnt =0;
	  clear_flag=0;

	  gam_request_4(0,clear,buffer1,sizes[s]+CLEAR_TAIL,&clear_flag);
	  gam_wait(&clear_flag,1);

	  bulk_put(0,(void *)&buf2[k],(void *) &buf1[i],sizes[s]);
	  gam_wait(&put_cnt,sizes[s]);
	  gam_request_4(0,remote_check_buf,buf1,i,k,sizes[s]);
	  gam_wait(&clear_flag,1);
	}
      }
    }
  }  /* if me == other */

  gam_barrier();

  if (me ==0 ) 
    printf("Node %d testing simultainious get from %d \n",0,other);  
  else 
    printf("Node %d testing simultainious get from %d \n",me,0);      

  fflush(stdout);
  gam_barrier();

  mask = 0x1;
  for (i =0 ; i< SIZES; i++) {
    sizes[i] = 128;
  }

  if ( (me == 0) || (me == other)) {
    for (reps = 0; reps < 100 ; reps++) {
      for (i=0;i<ALIGNMENTS;i++) {      /* for all alignments and sizes */
	for (k=0;k<ALIGNMENTS;k++) { 
	  for (s=0;s<SIZES;s++) {  
	    memset(buffer1,0,sizes[s]+CLEAR_TAIL);
	    get_cnt =0;
	    if (me ==0 ) 
	      bulk_get(other,(void *)&buf1[i],(void *) &buf2[k],sizes[s]);
	    else 
	      bulk_get(0,(void *)&buf1[i],(void *) &buf2[k],sizes[s]);
	    gam_wait(&get_cnt,sizes[s]);
	    check_buf(buf1,i,k,sizes[s]); 
	  }
	}
      }
      if ( (reps & mask) == 0x1) {
	printf(".");
	fflush(stdout);
	if (mask <= 0xff) {
	  mask = (mask<<1) | mask; 
	}
      }
    }
  }
  gam_barrier();
  mask = 0x1;
  next = (me+1)%procs;

  printf("\nNode %d testing all get from 0 \n",me);  
  fflush(stdout);
  gam_barrier();

  if ( me != 0 ) {
    for (reps = 0; reps < 200 ; reps++) {
      for (i=0;i<ALIGNMENTS;i++) {      /* for all alignments and sizes */
	for (k=0;k<ALIGNMENTS;k++) { 
	  for (s=0;s<SIZES;s++) {  
	    memset(buffer1,0,sizes[s]+CLEAR_TAIL);
	    get_cnt =0;
	    bulk_get(0,(void *)&buf1[i],(void *) &buf2[k],sizes[s]);
	    gam_wait(&get_cnt,sizes[s]);
	    check_buf(buf1,i,k,sizes[s]);   

	  }
	}
      }
      if ( (reps & mask) == 0x1) {
	printf(".");
	fflush(stdout);
	if (mask <= 0xff) {
	  mask = (mask<<1) | mask; 
	}
      }
    }
  }


  gam_barrier();
  mask = 0x1;
  next = (me+1)%procs;

  printf("\nNode %d testing all put to 0 \n",me);  
  fflush(stdout);
  gam_barrier();

  if ( me != 0 ) {
    for (reps = 0; reps < 1000 ; reps++) {
      for (i=0;i<ALIGNMENTS;i++) {      /* for all alignments and sizes */
	for (k=0;k<ALIGNMENTS;k++) { 
	  for (s=0;s<SIZES;s++) {  
	    memset(buffer1,0,sizes[s]+CLEAR_TAIL);
	    get_cnt =0;
	    bulk_put(0,(void *)&buf1[i],(void *) &buf2[k],sizes[s]);


	  }
	}
      }
      if ( (reps & mask) == 0x1) {
	printf(".");
	fflush(stdout);
	if (mask <= 0xff) {
	  mask = (mask<<1) | mask; 
	}
      }
    }
  }

  gam_barrier();
  mask = 0x1;
  next = (me+1)%procs;

  printf("\nNode %d testing simultainious get from all \n",me);  
  fflush(stdout);
  gam_barrier();
  memset(buffer2,0,128+CLEAR_TAIL);

#define REP_COUNT 1000 

  get_cnt =0;
  for (reps = 0; reps < REP_COUNT ; reps++) {
   for (i=0;i<ALIGNMENTS;i++) {     
      for (k=0;k<ALIGNMENTS;k++) { 
	for (s=0;s<SIZES;s++) {  
/*	  memset(buffer1,0,sizes[s]+CLEAR_TAIL); */
	  bulk_get(next,(void *)&buf1[0],(void *) &buf2[0],ALL_ALL_SIZE);
  /*	  gam_wait(&get_cnt,sizes[s]); */
  /*	  check_buf(buf1,i,k,sizes[s]);   */
	  next = (next+1)%procs;

	}
     }
    }

   if ( (reps & mask) == 0x1) {
     printf(".");
     fflush(stdout);
     if (mask <= 0xff) {
       mask = (mask<<1) | mask; 
     }
   }

  }

  printf("%d done!\n",me);
  fflush(stdout);
  gam_barrier();
  i =  ALL_ALL_SIZE*(REP_COUNT*ALIGNMENTS*ALIGNMENTS*SIZES); 

  if (get_cnt != i )  {
    printf("Node %d wanted %d bytes got only %d \n",me,i,get_cnt);
  }

}
