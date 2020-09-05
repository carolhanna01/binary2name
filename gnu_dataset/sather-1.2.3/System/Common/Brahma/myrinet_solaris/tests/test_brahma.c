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
#include <sys/systeminfo.h> 
#include <thread.h>
#include "brahma.h"
/*#include "am.h"*/

extern int errno;

void* poll_thread(void *arg);
BR_sema_t poll_sema;

static void poof(int src, volatile int *flag)
{
  (*flag)++; 
}
     
static void pong(int src, volatile int *return_flag)
{	
  gam_reply_4(src, poof, return_flag); 
}


#define NUM_POLL_THREADS 50
int poll_thread_cntrs[NUM_POLL_THREADS];

void init_ctrs() {
  int i;

  for(i = 0; i < NUM_POLL_THREADS; i++){
    poll_thread_cntrs[i] = 0;
  }
}

void print_cntrs() {
  int i;
  
  for(i = 0; i < NUM_POLL_THREADS; i++){
    printf("[%d]  :  %d\n", i, poll_thread_cntrs[i]);
  }
}

print_msg(char *str){
  printf("%s\n", str);
  fflush(stdout);
}

main(int argc,char *argv[],char *envp[])
{
  int i;
  int iterations;
  int me,to;
  static volatile int go;
  static volatile int done1;
  struct timespec begin,end,lapsed;
  char my_name[64];

  double total_time,rtt;

  fprintf(stderr,"about to call gam_enable() \n");

  gam_enable(0);  

  gam_barrier(); 

  me = gam_my_proc();


  thr_setconcurrency(4);
  i = thr_getconcurrency();
  printf("Concurrency level: %d\n", i);

  poll_sema = BR_SEMA_CREATE(0);
  init_ctrs();

    for(i=0; i < NUM_POLL_THREADS; i++){
    if (thr_create(NULL, 0, poll_thread, &i, 0L, NULL)){
      fprintf(stderr, "thr_create() failed\n");
    }
  }

  
  /* Wait until all threads have been created ... */
  for(i=0; i < NUM_POLL_THREADS; i++){
    printf("finished creation #%d\n", i);
    fflush(stdout);
    BR_WAIT(poll_sema);
  }
  
  print_msg("All threads created\n");
  
#ifdef DEBUG
  sysinfo(SI_HOSTNAME,my_name,64);
  fprintf(stderr,"node %d running on %s\n",me,my_name);
  fprintf(stderr,"main: calling am_enable()\n" );
  fflush(stdout);
#endif 
  

  fprintf(stderr,"node %d passed barrier \n",me,my_name);

  to=1;
  iterations = 1000;

  if (argc > 1) {
    to = atoi(argv[1]);
  }

  if (argc >= 2 ) 
    iterations = atoi(argv[2]);

  gam_barrier();

  fprintf(stderr,"node %d passed 2nd barrier \n",me,my_name);

  if (me != to ) {

    printf("Round trip test from %d to %d\n\n",me,to);
    printf(" Timing full round trip time \n");
    fflush(stdout); 

    clock_gettime(CLOCK_REALTIME,&begin);    
    for (i=0;i<iterations;i++){
      done1 =0;
      gam_request_4(to, pong, &done1);   
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

    rtt = total_time/iterations;
     
    printf("total %.3lf, average full rtt = %.3lf usec \n",total_time,rtt);

    gam_request_4(to,poof,&go); 
  }
  else {
    while(go < 1) {
       gam_poll();
    }
  }
  printf("done!\n");
  fflush(stdout);
  gam_disable(); 
  print_cntrs();
}



void* poll_thread(void *arg){
  int thr_number, i;
  double foo=100.897, bar=2354.32;
  
  thr_number = *((int *)arg);
  
  printf("poll_thread: [%d]\n", thr_number);
  fflush(stdout);

  BR_SIGNAL(poll_sema);
  while(1) {
    for(i = 0; i<thr_number*100; i++){
      foo = foo*foo/(3*bar);
      bar = 8.78*foo ;
    }
    gam_poll(); 
    poll_thread_cntrs[thr_number]++;
    thr_yield();
  }
  return NULL;
}
