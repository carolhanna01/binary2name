/*------------------------->  ANSI C - sourcefile  <-------------------------*/
/* Copyright (C) 199x by International Computer Science Institute            */
/* This file is part of the GNU Sather library. It is free software; you may */
/* redistribute  and/or modify it under the terms of the GNU Library General */
/* Public  License (LGPL)  as published  by the  Free  Software  Foundation; */
/* either version 3 of the license, or (at your option) any later version.   */
/* This  library  is distributed  in the  hope that it will  be  useful, but */
/* WITHOUT ANY WARRANTY without even the implied warranty of MERCHANTABILITY */
/* or FITNESS FOR A PARTICULAR PURPOSE. See Doc/LGPL for more details.       */
/* The license text is also available from:  Free Software Foundation, Inc., */
/* 59 Temple Place, Suite 330, Boston, MA 02111-1307 USA                     */
/*------------>  Please email comments to <bug-sather@gnu.org>  <------------*/

#include <stdio.h>
#include <stdlib.h>
#include <sys/time.h>
#include <math.h>
#include <thread.h>
#include <sys/types.h>
#include <sys/processor.h>
#include <sys/procset.h>
#include <sys/lwp.h>
#include <ucontext.h>
#include <signal.h>

#include "brahma.h"

#define THOUSAND	1000  /*         1,000 */
#define MILLION      1000000  /*     1,000,000 */
#define BILLION   1000000000  /* 1,000,000,000 */

static BR_lock_t lck, pinglck, ponglck, delaylck;
static BR_sema_t sema, pingsema, pongsema;
static BR_SPINLOCK_DEC(slck);
static BR_SPINLOCK_DEC(pingslck);
static BR_SPINLOCK_DEC(pongslck);
static BR_SPINLOCK_DEC(reqslck);

#define BUFSIZE 2048

static char buf1[BUFSIZE], buf2[BUFSIZE];
static int done;

void lock_pingpong_demon(BR_cluster_t ignored) {
   while (1) {
      BR_LOCK(pinglck);
      BR_UNLOCK(ponglck);
   }
}

void sema_pingpong_demon(BR_cluster_t ignored) {
   while (1) {
      BR_WAIT(pingsema);
      BR_SIGNAL(pongsema);
   }
}

static volatile int spinlock_pingpong_demon_die=0;
/* 
   Since spinlocks are not supposed to yield (although some implementation
   may choose to do so after a while ;-), we have to be a bit careful
   here... If we have only a single CPU, a straightward implementation
   simply hangs the machine since it is spinning on a lock.

   We also need to make sure that this thread dies eventually. Otherwise
   it will consume cycle (since it may not block) and mess up all 
   other measurements
   */
void spinlock_pingpong_demon(BR_cluster_t ignored) {
   while (!spinlock_pingpong_demon_die) {
     while(!BR_SPINLOCK_TRY(pingslck)){
       BR_THREAD_YIELD();
       if (spinlock_pingpong_demon_die){
	 break;
       }
     }
     BR_SPINLOCK_UNLOCK(pongslck);
   }
}

void initialize(int clusters, int argc, char **argv) {
  char hostname[256];

  BR_init(clusters, argc, argv);

  BR_start();
   if (BR_HERE()==0) {
     printf("Platform:                         %s\n",BR_ASCII_PLATFORM());
     printf("Clusters:                         %d\n",BR_CLUSTERS());
     printf("Max xfer:                         %d\n",BR_MAX_XFER());
     printf("Cluster local size:               %d\n",BR_CLUSTER_LOCAL_SIZE());
   }
   
   /* Use barrier to print things on different clusters nicely */
   BR_BARRIER();

   fflush(stdout);   
   gethostname(hostname, 256);
   printf("Cluster [%d]: %-10s Procs:    %d\n",BR_HERE(), hostname, BR_PROCESSORS());
   fflush(stdout);

   BR_BARRIER();

   lck = BR_LOCK_CREATE();
   pinglck = BR_LOCK_CREATE();
   BR_LOCK(pinglck);
   ponglck = BR_LOCK_CREATE();
   BR_LOCK(ponglck);
   sema = BR_SEMA_CREATE(1);
   pingsema = BR_SEMA_CREATE(0);
   pongsema = BR_SEMA_CREATE(0);
   BR_SPINLOCK_INIT(slck);
   BR_SPINLOCK_INIT(pingslck);
   BR_SPINLOCK_LOCK(pingslck);
   BR_SPINLOCK_INIT(pongslck);
   BR_SPINLOCK_LOCK(pongslck);
   BR_SPINLOCK_INIT(reqslck);
   BR_SPINLOCK_LOCK(reqslck);
   delaylck = BR_LOCK_CREATE();
   BR_LOCK(delaylck);
   done = 0;

   if (BR_HERE() == 0){
     BR_FORK_0(0, lock_pingpong_demon);
     BR_FORK_0(0, sema_pingpong_demon);
     BR_FORK_0(0, spinlock_pingpong_demon); 
   }
}

void finalize() {
  if (BR_HERE() == 0) {
    BR_exit();
  }
}

/* Don't compile with -O3, or these may get optimized away */
void donothing() {}

void leafcall() {
   donothing();
}

void set_thread_local() {
   BR_SET_THREAD_LOCAL(NULL);
}

void get_thread_local() {
   (void) BR_GET_THREAD_LOCAL();
}

void cluster_local() {
   (void) BR_CLUSTER_LOCAL();
}

void uncontested_lock() {
   BR_LOCK(lck);
   BR_UNLOCK(lck);
}

void uncontested_sema() {
   BR_WAIT(sema);
   BR_SIGNAL(sema);
}

void uncontested_spinlock() {
   BR_SPINLOCK_LOCK(slck);
   BR_SPINLOCK_UNLOCK(slck);
}

void lock_pingpong() {
   BR_UNLOCK(pinglck);
   BR_LOCK(ponglck);
}

void sema_pingpong() {
   BR_SIGNAL(pingsema);
   BR_WAIT(pongsema);
}

/* remember that spinlocks do not have to yield ... */
void spinlock_pingpong() {
   BR_SPINLOCK_UNLOCK(pingslck);
   while(!BR_SPINLOCK_TRY(pongslck)){
     BR_THREAD_YIELD();
   }
}

static volatile BR_thread_t tid;

void threadhandler0(BR_cluster_t ignore) {
}

void thread_local_fork() {
   BR_FORK_0(0, threadhandler0);
}

void delayhandler(void *dummy) {
};

void delayed_call_init() {
   BR_delay_t del;
   del.sec = 0;
   del.nsec = 0;
   BR_delay_function(del, delayhandler, NULL);
}

void delayhandler2(void *dummy) {
   BR_UNLOCK(delaylck);
};

void delayed_0_usec() {
   BR_delay_t del;
   del.sec = 0;
   del.nsec = 0;
   BR_delay_function(del, delayhandler2, NULL);
   BR_LOCK(delaylck);
}

void delayed_100_msec() {
   BR_delay_t del;
   del.sec = 0;
   del.nsec = 100000000;
   BR_delay_function(del, delayhandler2, NULL);
   BR_LOCK(delaylck);
}

void thread_id() {
   (void) BR_THREAD_ID;
}

void thread_here() {
   (void) BR_HERE();
}

void thread_clusters() {
   (void) BR_CLUSTERS();
}

void thread_max_xfer() {
   (void) BR_MAX_XFER();
}

/*
void freeze_thaw() {
   BR_freeze();
   BR_thaw();
}
*/

void local_request_initiate() {
   BR_REQUEST_0(0, threadhandler0);
}

void remote_request_initiate() {
   BR_REQUEST_0(1, threadhandler0);
}

void threadhandler2(BR_cluster_t ignore) {
   BR_SPINLOCK_UNLOCK(reqslck);
}

void threadhandler1(BR_cluster_t c) {
   BR_REPLY_0(c, threadhandler2);
}

void local_reqrep_roundtrip() {
   BR_REQUEST_0(0, threadhandler1);
   BR_SPINLOCK_LOCK(reqslck);
}

void remote_reqrep_roundtrip() {
   BR_REQUEST_0(1, threadhandler1);
   while (!BR_SPINLOCK_TRY(reqslck)){
     BR_POLL();
   }
}

void local_get256_initiate() {
   BR_GET(0, buf1, buf2, BUFSIZE, donothing, 0);
}

void remote_get256_initiate() {
   BR_GET(1, buf1, buf2, BUFSIZE, donothing, 0);
}

void memhandler0(BR_cluster_t ignore, caddr_t x, size_t size, caddr_t y) {
  BR_SPINLOCK_UNLOCK(reqslck);
}

void local_get256_complete() {
   BR_GET(0, buf1, buf2, BUFSIZE, memhandler0, 0);
   BR_SPINLOCK_LOCK(reqslck);
}

void remote_get256_complete() {
   BR_GET(1, buf1, buf2, BUFSIZE, memhandler0, 0);
   while(!BR_SPINLOCK_TRY(reqslck)){
     BR_POLL();
   }
}

void local_asyncstore256_initiate() {
   BR_ASYNC_STORE(0, buf1, buf2, BUFSIZE, donothing, 0, donothing, 0);
}

void remote_asyncstore256_initiate() {
   BR_ASYNC_STORE(1, buf1, buf2, BUFSIZE, donothing, 0, donothing, 0);
}

void local_asyncstore256_complete() {
   BR_ASYNC_STORE(0, buf1, buf2, BUFSIZE, donothing, 0, memhandler0, 0);
   BR_SPINLOCK_LOCK(reqslck);
}

void remote_asyncstore256_complete() {
   BR_ASYNC_STORE(1, buf1, buf2, BUFSIZE, donothing, 0, memhandler0, 0);
   while(!BR_SPINLOCK_TRY(reqslck)){
     BR_POLL();
   }
}

void benchmark_done_handler(BR_cluster_t cl){ printf("cluster [%d]
done\n", BR_HERE()); fflush(stdout); done = 1; }

/*
** Time something in loops of `manytimes', until at least a second
** has been spent doing it.  Return number of usec per item.
*/
double timeit(void (*f)(), unsigned long manytimes) {
   hrtime_t start, finish;
   long iterations=0;
   long i;
   start=gethrtime();
   while (1) {
      for (i=0; i<manytimes; i++) {
	 f();
	 }
      iterations++;
      finish=gethrtime();
      if (finish-start>=BILLION) break;
   }
   return ((double)(finish-start))/(manytimes*iterations)/THOUSAND;
}

/* 
 * A "nice" version of timeit that just executes the thing the number
 * of times passed as an argument. Used to avoid flooding the network
 * for bulk communications tests.
 */
double timeit_nice(void (*f)(), unsigned long manytimes) {
   hrtime_t start, finish;
   long iterations=0;
   long i;
   start=gethrtime();
   for (i=0; i<manytimes; i++) {
     f();
   }
   finish=gethrtime();
   return ((double)(finish-start))/(manytimes)/THOUSAND;
}

void benchmark(char *s, void (*f)(), unsigned long manytimes) {
   printf("   %s\t%10.2f\n",s,timeit(f, manytimes));
   fflush(stdout);
}

void benchmark_nice(char *s, void (*f)(), unsigned long manytimes) {
   printf("   %s\t%10.2f\n",s,timeit_nice(f, manytimes));
   fflush(stdout);
}

int main(int argc, char **argv) {
   int i;

   initialize(0, argc, argv);

   if (BR_HERE() == 0 ){
     printf("Basic (all times are in usecs):\n\n");
     
     benchmark("Timing overhead           ", donothing, 10000);
     benchmark("Leaf routine call         ", leafcall, 10000);
     benchmark("Thread id                 ", thread_id, 10000);
     benchmark("Here                      ", thread_here, 10000);
     benchmark("Clusters                  ", thread_clusters, 10000);
     benchmark("Set thread local          ", set_thread_local, 10000);
     benchmark("Get thread local          ", get_thread_local, 10000);
     benchmark("Cluster local             ", cluster_local, 10000);


     printf("\nSynchronization:\n\n"); 
     benchmark("Local fork                ", thread_local_fork, 1); 
     benchmark("Delayed function initiate ", delayed_call_init, 1000);
     benchmark("Delayed roundtrip         ", delayed_0_usec, 1000);
     benchmark("Delayed 100msec roundtrip ", delayed_100_msec, 10); 
     benchmark("Max xfer                  ", thread_max_xfer, 10000); 
     benchmark("Uncontested lock          ", uncontested_lock, 10000);
     benchmark("Uncontested semaphore     ", uncontested_sema, 10000);
     benchmark("Uncontested spinlock      ", uncontested_spinlock, 10000);

     benchmark("Spinlock ping-pong        ", spinlock_pingpong, 1000); 
     /* Kill the spinlock daemon. Otherwise it will take cycles away
	from other things */
     spinlock_pingpong_demon_die = 1;

     benchmark("Lock ping-pong            ", lock_pingpong, 1000);
     benchmark("Semaphore ping-pong       ", sema_pingpong, 1000);
     /* Commented out until it is fixed */
     /* benchmark("Unloaded freeze/thaw      ", freeze_thaw, 10); */
	
     printf("\nMessaging:\n\n");
     benchmark("Local request initiate    ", local_request_initiate, 100);
     benchmark("Local req/rep round-trip  ", local_reqrep_roundtrip, 100);
     benchmark("Local get 8KB initiate    ", local_get256_initiate, 100);
     benchmark("Local get 8KB complete    ", local_get256_complete, 100);
     benchmark("Local async store 8KB ini ", local_asyncstore256_initiate, 100);
     benchmark("Local async store 8KB com ", local_asyncstore256_complete, 100);
     
     /* Distributed tests */
     if (BR_CLUSTERS() > 1) {
       benchmark_nice("Remote request initiate   ", remote_request_initiate, 100);
       benchmark_nice("Remote req/rep round-trip ", remote_reqrep_roundtrip, 100);
       benchmark_nice("Remote get 8KB initiate   ", remote_get256_initiate, 100); 
       benchmark_nice("Remote get 8KB complete   ", remote_get256_complete, 100);
       benchmark_nice("Remote async store 8KB ini", remote_asyncstore256_initiate, 100);
       benchmark_nice("Remote async store 8KB com", remote_asyncstore256_complete, 100);
     } else {
       printf("\nWith clusters=1, remote tests could not be run.\n");
     }
   }
   else {
     /* just poll for better reaction time ... */
     while(1){
       BR_POLL();
     }
   }
   
   finalize();
}







