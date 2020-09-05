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
#include "brahma.h"

extern int errno;

int buf[8192];
struct timespec begin,end,lapsed;
int size,count;
static volatile int go;
static volatile int done1;


static void pong(int src, void *lva, int size, volatile int *flag)
{	
  if ( (int) flag ==0)
    clock_gettime(CLOCK_REALTIME,&begin);    
  else {
    if ((int) flag == (count-1) ) 
      go =1;
  }
}

main(int argc,char *argv[])
{
  int i;
  int iterations;
  int me,to;
  int bytes;

  double total_time,rtt,thruput;

  BR_init(3, argc, argv);
  BR_start();

  BR_BARRIER();
  me = BR_HERE();


  to=1;
  count = 500;

  if (argc > 1) {
    size = atoi(argv[1]);
  }

  if (argc >= 2) 
    count = atoi(argv[2]);

  bytes=size*count;

  if (me == 0 ) {

    printf("Bulk througput test from %d to %d\n\n",me,to);

    clock_gettime(CLOCK_REALTIME,&begin);    

    for (i=0;i<count;i++){
      done1 =0;
      BR_STORE(to, buf,buf,size,pong, (int *)i);  
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

    thruput = bytes/total_time;
     
    printf("[0]: For size %d count %d total %.3lf, = %.3lf MB/s \n",
	   size,count,total_time,thruput);

  }
  else {
    while(go < 1) 
      BR_POLL();

    clock_gettime(CLOCK_REALTIME,&end);    

    if (begin.tv_nsec > end.tv_nsec) {
      end.tv_nsec += 1000000000;
      end.tv_sec--;
    }

    lapsed.tv_nsec = end.tv_nsec - begin.tv_nsec;
    lapsed.tv_sec =  end.tv_sec - begin.tv_sec;
    
    total_time = (double)lapsed.tv_sec*1000000000.00+(double)lapsed.tv_nsec;
    total_time = total_time/1000.00;
      
    bytes = bytes-size; /* minus one packet */

    thruput = bytes/total_time;
    printf("[1]: For size %d count %d total %.3lf, = %.3lf MB/s \n",
	   size,count,total_time,thruput);

  }
  printf("done!\n");
  fflush(stdout);
  if(BR_HERE()==0){
    BR_exit();
  }
}






