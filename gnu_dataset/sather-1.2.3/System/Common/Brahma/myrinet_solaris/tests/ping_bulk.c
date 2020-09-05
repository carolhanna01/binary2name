/*                                                                      tab:8
 *
 * "Copyright (c) 1994 The Regents of the University of California.
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
 * Version: 		1
 * Creation Date:       Thu Oct 28 10:47:28 PDT 1993
 * Filename:            ping.c ping test for HPAM library
 * History:
 */

#include <stdio.h>
#include <stdlib.h>
#include <sys/types.h>
#include <sys/socket.h>
#include <netinet/in.h>
#include <netdb.h>
#include <fcntl.h>
#include <time.h>
#include "am.h"

extern int errno;

int buf[1024];

static void poof_small(int src, volatile int *flag)
{
  (*flag)++; 
}
static void poof(int src, void *lva, int size, volatile int *flag)
{
  (*flag)++; 
}
     
static void pong(int src, void *lva, int size, volatile int *return_flag)
{	
  gam_reply_xfer(src, lva,lva,size,poof, return_flag); 
}


main(int argc,char *argv[],char *envp[])
{
  int i;
  int iterations;
  int me,to;
  int size,count;
  static volatile int go;
  static volatile int done1;
  struct timespec begin,end,lapsed;

  double total_time,rtt;

#ifdef GLUNIX
  lam_glunix_spawn(&argc,argv,envp) 
#endif

  gam_enable(0);
  lam_sync_all();


  me = gam_my_proc();
  to=1;
  count = 500;

  if (argc > 1) {
    size = atoi(argv[1]);
  }

  if (argc > 2) 
    count = atoi(argv[2]);
  
  if (me != to ) {

    printf("Bulk Round trip test from %d to %d\n\n",me,to);

    printf(" Timing full round trip time \n");
    fflush(stdout);

    clock_gettime(CLOCK_REALTIME,&begin);    

    for (i=0;i<count;i++){
      done1 =0;
      gam_store(to, buf,buf,size,pong, &done1);  
      while (done1 ==0) 
	gam_poll(); 
    }    

    clock_gettime(CLOCK_REALTIME,&end);    

    if (begin.tv_nsec > end.tv_nsec) {
      end.tv_nsec += 1000000000;
      end.tv_sec--;
    }


    lapsed.tv_nsec = end.tv_nsec - begin.tv_nsec;
    lapsed.tv_sec =  end.tv_sec - begin.tv_sec;
    
    total_time = (double)lapsed.tv_sec*1000000000.00+(double)lapsed.tv_nsec;
    total_time = total_time/1000.00;

    rtt = total_time/count;
     
    printf("For size %d count %d total %.3lf, average rtt = %.3lf usec \n",
	   size,count,total_time,rtt);

    gam_request_4(to,poof_small,&go);
  }
  else {
    while(go < 1) 
      gam_poll();
  }
  printf("done!\n");
  fflush(stdout);
  gam_disable();
}
