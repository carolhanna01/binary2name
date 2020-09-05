/*                                                                      tab:8
 *
 * "Copyright (c) 1996 The Regents of the University of California.
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
 * Creation Date:       Mon Jun 24 14:53:13 PDT 1996
 * Filename:            Medium test for LAM library
 * History:
 */

/* this file sends a total of SIZES different sized segments using a
 * bulk put/get interface. For each size, we change the start of 
 * of the offset to a new byte alignment.
 */

#include <stdio.h>
#include <string.h>
#include <sys/types.h>
#include <sys/socket.h>
#include <sys/time.h>
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

#define ALL_ALL_SIZE 512

/* #define TEST_PATTERN(x) ((x%255)+1)*/ 
/* #define TEST_PATTERN(x) (0xFF)  */

#define TEST_PATTERN(x) ((x)|(x<<1)|(x<<2)|0x77)
int get_cnt,put_cnt;
int clear_flag;

char buffer1[20*16384];
char buffer2[20*16384];
char *buf1,*buf2;

int sizes[SIZES];
int am_self_address;
int am_max_pkt;

void remote_check_buf(int src,char *data, int a1, int a2, int s); 

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

    c=(char) TEST_PATTERN(j);

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

void 
remote_ping(int src,char *data, int s, int a1) {

  
}
void 
remote_check_buf_wrapper(int src,char *data, int s, int a1) {

  data[s] = 0; /* set the last byte to 0 get check to pass */
  remote_check_buf(src,data, 0, a1,s);

}

void remote_check_buf(int src,char *data, int a1, int a2, int s)
{
  int i,j;
  char c;

  /* check to make sure didn't store something too soon*/

  if (data[-1] !=0 )
    printf("node %d pre condition failed for size %d alignments %d %d\n",
	   am_self_address, s,a1,a2);  

  for (i=a1,j=a2;i<(s+a1);i++,j++) {

    c=(char) TEST_PATTERN(j);

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
  unsigned int count;
  static volatile int go;
  struct timeval start,finish,lapsed;
  struct timezone tzone;
  char my_name[256];
  double total_time,rtt;

  /* lam_glunix_spawn(&argc,argv,envp);  */
#ifdef GLUNIX
  if (!Glib_Initialize()) {
    fprintf(stderr,"Error - GLUnix couldn't initialize!\n");
    exit(-1);
  }
#endif
  gam_enable(0); /* 1= use glunix */

  sysinfo(SI_HOSTNAME,my_name,256);


  gam_barrier();

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

  count=0;
  if (argc > 2) {
    count = atoi(argv[2]);
  }


  buf1=&buffer1[4];
  buf2=buffer2;

  for (i=0;i<20*am_max_pkt;i++) {
    buf2[i] = (char) TEST_PATTERN(i);
  }

  gam_barrier();

/* this first set of tests is from 0 to 1 */

  if (me == 0 ) {

    printf("Node %d testing am_request to %d \n",me,other);
    fflush(stdout);

    for (i=0;i<ALIGNMENTS;i++) {      /* for all alignments and sizes */
      for (s=1;s<am_max_pkt-CLEAR_TAIL;s*=2) {  
	clear_flag=0;
	gam_request(other,remote_check_buf_wrapper, 
		   (void *)&buf2[i],s,i);
	gam_wait(&clear_flag,1);
      }
    }
  }
  gam_barrier();
  
  if ( (me == 0) || (me == other)) {
    int remote ; 
    remote = ((me == other) ? 0 : other); 

    printf("Node %d testing simultaneous am_request to %d \n",me,remote);
    fflush(stdout);
    for (j=0; j< 500; j++) {
      for (i=0;i<ALIGNMENTS;i++) {      /* for all alignments and sizes */
	for (s=1;s<am_max_pkt-CLEAR_TAIL;s *=2 ) {  
	  clear_flag=0;
	  gam_request(remote,remote_check_buf_wrapper, 
		     (void *)&buf2[i],s,i);
	  gam_wait(&clear_flag,1);
	}
      }
    }
  }

  gam_barrier(); 

  /* all to all test */

  printf("Node %d testing simultaneous am_request to all \n",me);
  fflush(stdout);

  { 
    int remote; 
    
    remote = ((me +1) % procs); 

    for (j=0; j< 1000; j++) {
      for (i=0;i<ALIGNMENTS;i++) {      /* for all alignments and sizes */
	  clear_flag=0;
	  for (s=140;s<am_max_pkt-CLEAR_TAIL;s *=2 ) {  
	  gam_request(remote,remote_check_buf_wrapper, 
		     (void *)&buf2[i],4000,i);
	  /*	  am_wait(&clear_flag,1); */
	  remote = ((remote+1) % procs);
	}
      }
    }
  }


  gam_barrier();


  if (count > 0) {
    printf("Node %d testing long (%d) am_request to all \n",me,count);
    fflush(stdout);
    { 
      int remote,size; 
      struct timeval start,end,lapsed; 
      long long lapsed_usec;
      double bytes,time,bandwidth; 
      remote = ((me +1) % procs); 
      size = 380; 

      gettimeofday(&start,NULL);
      for (j=1; j<= count; j++) {
	clear_flag=0;
	gam_request(remote,remote_ping,(void *)&buf2[i],size,i);
	  /*	  gam_wait(&clear_flag,1); */
	remote = ((remote+1) % procs);
	if ((j % ( 500000 + (me*10000))) ==0 ) {
	  gettimeofday(&end,NULL);	  

	  if (start.tv_usec > end.tv_usec) {
	    end.tv_usec += 1000000;
	    end.tv_sec--;
	  }
  
	  lapsed.tv_usec = end.tv_usec - start.tv_usec;
	  lapsed.tv_sec = end.tv_sec - start.tv_sec;  
	  lapsed_usec =( (long long)lapsed.tv_usec+
			 (long long)lapsed.tv_sec*1000000);

	  bytes = (double)(((long long) size*(long long)j)/(long long)1000000);
	  time =   (double) (lapsed_usec/(long long)1000000); 
	  bandwidth =   bytes /   time;
	  fprintf(stderr,"%d sent %d size %d in %.3lf usec (%.3lf MB/s)\n",
		  me,j,size,(double)lapsed_usec,bandwidth);
	}
      }
    }
  }

  gam_barrier();
  gam_disable();
}
