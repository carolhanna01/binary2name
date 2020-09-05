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

char buf1[4096]
char buf2[4096];

static void poof(int src, volatile int *flag)
{
  (*flag)++; 
}
     
static void pong(int src, volatile int *return_flag)
{	
  am_reply_4(src, poof, return_flag); 
}

main(int argc,char *argv[])
{
  int i;
  int iterations;
  int me,to;
  static volatile int go;
  static volatile int done1;
  struct timespec begin,end,lapsed;

  double total_time,rtt;

  am_enable();

  me = am_my_proc();
  to=0;

  if (argc > 1) {
    to = atoi(argv[1]);
  }

  iterations =500;


  if (me != to ) {

    printf("Round trip test from %d to %d\n\n",me,to);

    printf(" Timing full round trip time \n");
    clock_gettime(CLOCK_REALTIME,&begin);    
    for (i=0;i<iterations;i++){
      done1 =0;
      am_store(to, pong, &done1);  
      while (done1 ==0) 
	am_poll(); 
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

    rtt = total_time/iterations;
     
    printf("average full rtt = %.3lf usec \n",rtt);

    am_request_4(to,poof,&go);
  }
  else {
    while(go < 1) 
      am_poll();
  }
  printf("done!\n");
  fflush(stdout);
  am_disable();
}
