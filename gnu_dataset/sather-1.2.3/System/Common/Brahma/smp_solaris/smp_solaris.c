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

/*
** This is the implementation for Brahma implemented using Solaris threads
** and Sparc V7 spinlocks.
*/

#define BR_SMP_SOLARIS_IMPL
#include "smp_solaris.h"
#include <stdio.h>
#include <stdlib.h>

void BR_error(char *s) {
   fprintf(stderr,"Brahma fatal error: %s\n",s); exit(1);
}

#define MOVETO(x) thr_setspecific(BR_here_key, (void *)(x))

/*
** Instead of forking immediately, fork requests are put on a queue.  When
** the queue gets too large, forks do happen immediately, but the
** enqueueing thread defers.  The lower priority background thread can
** only wake up when there is unused CPU power, and it dequeues and forks
** threads in a tight loop.  This background thread is automatically
** preempted by the new thread it forks.
*/

static BR_lock_t qlock;

#define MAX_FORK_QUEUE 1024
typedef struct fork_elem_struct {
   BR_cluster_t c_was;
   BR_cluster_t c;
   BR_handler_t func;
   BR_word_t arg0, arg1, arg2, arg3, arg4;
   } BR_fork_elem_t;
static BR_fork_elem_t BR_fork_queue[MAX_FORK_QUEUE];

static unsigned int BR_fork_head = 0;
static unsigned int BR_fork_tail = 0;

#define next_fork_head ((BR_fork_head==MAX_FORK_QUEUE)?0:(BR_fork_head+1))
#define next_fork_tail ((BR_fork_tail==MAX_FORK_QUEUE)?0:(BR_fork_tail+1))
#define fork_queue_empty (BR_fork_head==BR_fork_tail)
#define fork_queue_full (next_fork_head==BR_fork_tail)

/* Start the thread using args in a queue element */
static void BR_fork_wrapper(BR_fork_elem_t *e) {

   while (1) {

      /* Copy out the args and unlock, quickly. */
      BR_cluster_t c_was = e->c_was;
      BR_cluster_t c = e->c;
      BR_handler_t func = e->func;
      BR_word_t arg0 = e->arg0;
      BR_word_t arg1 = e->arg1;
      BR_word_t arg2 = e->arg2;
      BR_word_t arg3 = e->arg3;
      BR_word_t arg4 = e->arg4;
      BR_UNLOCK(qlock);

      MOVETO(c);
      func(c_was, arg0, arg1, arg2, arg3, arg4);

      /* After thread finishes, we get here.  Try to start next thread. */
      BR_LOCK(qlock);
      if (!fork_queue_empty) {
	 unsigned int t;
	 BR_fork_tail = next_fork_tail;
	 e = &(BR_fork_queue[BR_fork_tail]);
      } else {
	 BR_UNLOCK(qlock);
	 return;
	 /* The thread terminates when nothing left in queue. */
	 /* Right now there is no pooling of idle threads.    */
      }
   }
}

void BR_enqueue_fork(BR_cluster_t c, BR_handler_t func,
	 BR_word_t arg0, BR_word_t arg1, BR_word_t arg2, 
	 BR_word_t arg3, BR_word_t arg4) {

   BR_fork_elem_t *e;
   thread_t thr;

   BR_LOCK(qlock);
   /* If queue is full, create next thread but defer to slow down creation */
   if (fork_queue_full) {

      /* Insert new thread args while we have the spinlock */
      BR_fork_head = next_fork_head;
      e = &(BR_fork_queue[BR_fork_head]);
      e->c_was = BR_HERE();
      e->c = c;
      e->func = func;
      e->arg0 = arg0;
      e->arg1 = arg1;
      e->arg2 = arg2;
      e->arg3 = arg3;
      e->arg4 = arg4;

      /* Dequeue and create thread */
      BR_fork_tail = next_fork_tail;
      thr_create(NULL, 0, (void *(*)(void*)) BR_fork_wrapper, 
		       &(BR_fork_queue[BR_fork_tail]), THR_DETACHED, &thr);
      thr_setprio(thr, 1);
      /* Unlock will be done by created thread */
      thr_yield();
      }
   else {
      BR_fork_head = next_fork_head;
      e = &(BR_fork_queue[BR_fork_head]);
      e->c_was = BR_HERE();
      e->c = c;
      e->func = func;
      e->arg0 = arg0;
      e->arg1 = arg1;
      e->arg2 = arg2;
      e->arg3 = arg3;
      e->arg4 = arg4;
      BR_UNLOCK(qlock);
   }

};

/* These includes are for the proc file system stuff below */
/*
** #include <sys/types.h>
** #include <sys/signal.h>
** #include <sys/fault.h>
** #include <sys/syscall.h>
** #include <sys/procfs.h>
** #include <sys/types.h>
** #include <sys/stat.h>
** #include <fcntl.h>
*/

/* This gets executed at low priority */
static void BR_background() {
/*
**    int fd, retval;
**    char buf[80];
**    sprintf(buf, "/proc/%lu", getpid());
**    fd = open(buf, O_RDONLY);
**    if (fd < 0) { perror("Couldn't open proc file"); exit(1); }
*/
   thr_setprio(thr_self(), 0);  /* Just in case */

   while (1) {
      BR_LOCK(qlock);
      if (!fork_queue_empty) {
	 thread_t thr;
	 unsigned int t;
	 BR_fork_tail = next_fork_tail;
	 thr_create(NULL, 0, (void *(*)(void*)) BR_fork_wrapper,
			  &(BR_fork_queue[BR_fork_tail]), THR_DETACHED, &thr);
	 thr_setprio(thr, 1);
	 /* The created thread is responsible for unlocking qlock. */

      } else {
	 BR_UNLOCK(qlock);
      }
      /* Make sure any created threads get to run as soon as possible. */
      thr_yield();
/*
**       {
** 	 prstatus_t status;
** 
** 	 retval = ioctl(fd, PIOCSTATUS, &status);
** 	 if (retval < 0) { perror("Couldn't PIOCSTATUS"); exit(1); }
**           
** 	 fprintf(stderr, "Num. lwps = %d\n", status.pr_nlwp);
**       }
*/
   }
}

void BR_init(int clusters, int argc, char *argv[]) {
   int i;

   if (clusters==0) {
      if (getenv("CLUSTERS")!=NULL) {
	 sscanf(getenv("CLUSTERS"), "%d", &BR_clusters);
      } else {
	 BR_clusters = 1;
      }
   } else {
      BR_clusters = clusters;
   }

   thr_setconcurrency(BR_PROCESSORS());

   thr_keycreate(&BR_key, NULL);
   thr_keycreate(&BR_here_key, NULL);

   MOVETO(0); /* Start on cluster 0 */

   /* Initialize spinlock protecting thread queue */
   qlock = BR_LOCK_CREATE();

   /* Start the background thread that handles deferred forks */
   thr_create(NULL, 0, (void *(*)(void*)) BR_background,
		    NULL, THR_DETACHED, NULL);

   /* Run self and all forked threads at higher priority */
   thr_setprio(thr_self(), 1); 

   /* Initialize the cluster local array */
   BR_cluster_local_arr = (caddr_t *) malloc(BR_CLUSTERS()*sizeof(caddr_t));
   for (i=0; i<BR_CLUSTERS(); i++)
      BR_cluster_local_arr[i] = calloc(BR_CLUSTER_LOCAL_SIZE(), 1);
};

void BR_start(){
}


void BR_exit() {
}

/*
** Asyncronously send a request active message.  There is no
** failure mode.
*/
void BR_REQUEST_0(BR_cluster_t c, BR_handler_0_t handler) {
   BR_cluster_t was = BR_HERE();
   MOVETO(c);
   handler(was);
   MOVETO(was);
}

void BR_REQUEST_1(BR_cluster_t c, BR_handler_1_t handler, BR_word_t arg0){
   BR_cluster_t was = BR_HERE();
   MOVETO(c);
   handler(was, arg0);
   MOVETO(was);
}

void BR_REQUEST_2(BR_cluster_t c, BR_handler_2_t handler, BR_word_t arg0, BR_word_t arg1){
   BR_cluster_t was = BR_HERE();
   MOVETO(c);
   handler(was, arg0, arg1);
   MOVETO(was);
}

void BR_REQUEST_3(BR_cluster_t c, BR_handler_3_t handler, 
		  BR_word_t arg0, BR_word_t arg1, BR_word_t arg2){
   BR_cluster_t was = BR_HERE();
   MOVETO(c);
   handler(was, arg0, arg1, arg2);
   MOVETO(was);
}

void BR_REQUEST_4(BR_cluster_t c, BR_handler_4_t handler, 
		  BR_word_t arg0, BR_word_t arg1, BR_word_t arg2, BR_word_t arg3){
   BR_cluster_t was = BR_HERE();
   MOVETO(c);
   handler(was, arg0, arg1, arg2, arg3);
   MOVETO(was);
}

void BR_REQUEST_5(BR_cluster_t c, BR_handler_5_t handler, 
		  BR_word_t arg0, BR_word_t arg1, BR_word_t arg2,
		  BR_word_t arg3, BR_word_t arg4){
   BR_cluster_t was = BR_HERE();
   MOVETO(c);
   handler(was, arg0, arg1, arg2, arg3, arg4);
   MOVETO(was);
}

/*
** Asyncronously send a reply active message.  This must only be
** done from within a request handler.  There is no failure mode.
*/
void BR_REPLY_0(BR_cluster_t c, BR_handler_0_t handler){
   BR_cluster_t was = BR_HERE();
   MOVETO(c);
   handler(was);
   MOVETO(was);
}

void BR_REPLY_1(BR_cluster_t c, BR_handler_1_t handler, BR_word_t arg0){
   BR_cluster_t was = BR_HERE();
   MOVETO(c);
   handler(was, arg0);
   MOVETO(was);
}

void BR_REPLY_2(BR_cluster_t c, BR_handler_2_t handler, BR_word_t arg0, BR_word_t arg1){
   BR_cluster_t was = BR_HERE();
   MOVETO(c);
   handler(was, arg0, arg1);
   MOVETO(was);
}

void BR_REPLY_3(BR_cluster_t c, BR_handler_3_t handler, 
		  BR_word_t arg0, BR_word_t arg1, BR_word_t arg2){
   BR_cluster_t was = BR_HERE();
   MOVETO(c);
   handler(was, arg0, arg1, arg2);
   MOVETO(was);
}

void BR_REPLY_4(BR_cluster_t c, BR_handler_4_t handler, 
	       BR_word_t arg0, BR_word_t arg1, BR_word_t arg2, BR_word_t arg3){
   BR_cluster_t was = BR_HERE();
   MOVETO(c);
   handler(was, arg0, arg1, arg2, arg3);
   MOVETO(was);
}

void BR_REPLY_5(BR_cluster_t c, BR_handler_5_t handler, 
	       BR_word_t arg0, BR_word_t arg1, BR_word_t arg2,
	       BR_word_t arg3, BR_word_t arg4){
   BR_cluster_t was = BR_HERE();
   MOVETO(c);
   handler(was, arg0, arg1, arg2, arg3, arg4);
   MOVETO(was);
}


/*
** Transfer size bytes from local memory at address from to remote
** address to; on completion, remote handler is invoked with the
** arguments (<requesting cluster>, to, size, arg0).  Sender
** blocks until memory transfer complete.  There is no failure mode.
*/
void BR_STORE(BR_cluster_t c, caddr_t from, caddr_t to, size_t size,
		  BR_handler_mem_t handler, caddr_t arg0) {

   BR_cluster_t was = BR_HERE();
   MOVETO(c);
   (void) memcpy(to, from, size);
   handler(was, to, size, (void *) arg0);
   MOVETO(was);
}

/*
** Like BR_STORE, but sender does not block.  "on_completion" is
** invoked locally when transfer completes with the arguments
** (c, from, size, arg0b), while "handler" is invoked remotely
** with the arguments (<requesting cluster>, to, size, arg0a).  
** There is no failure mode.
*/
void BR_ASYNC_STORE(BR_cluster_t c, caddr_t from, caddr_t to, size_t size,
		  BR_handler_mem_t handler, BR_word_t arg0a,
		  BR_handler_mem_t on_completion, BR_word_t arg0b){

   BR_cluster_t was = BR_HERE();
   MOVETO(c);
   (void) memcpy(to, from, size);
   handler(was, to, size, (void *) arg0a);
   MOVETO(was);
   on_completion(c, from, size, (void *) arg0b);
}

/*
** Transfer data from remote cluster c to local memory.
** when transfer is complete, handler is invoked locally
** with arguments (c, to, size, arg0).  This may not be called
** from any handler function.  There is no failure mode.
*/
void BR_GET(BR_cluster_t c, caddr_t from, caddr_t to, size_t size,
		  BR_handler_mem_t handler, caddr_t arg0) {

   (void) memcpy(to, from, size);
   handler(c, (void *) to, size, (void *) arg0);
}


/* Does absolutely nothing; convenient no-op handler. */
void BR_dummy() {
}

#include <sys/lwp.h>
#include <sys/types.h>
#include <sys/signal.h>
#include <sys/fault.h>
#include <sys/syscall.h>
#include <sys/procfs.h>

#include <fcntl.h>

static prstatus_t BR_status, *BR_lwpstats;
static lwpid_t BR_my_lwpid;
static sigset_t BR_set, BR_old_mask;

/*
** Forcefully halt all threads on all clusters (other than those
** used by Brahma itself) and wait for any user active messages to
** drain.  On return there is no user activity other than the
** executing thread.  This is useful for debugging and garbage
** collection.
*/
void BR_freeze() {
   char buf[80];
   int fd, nummaps, retval, i;
   void mark_from_region(caddr_t start, size_t len);
   void GC_push_regs();

   int other_lwps_still_running;

   sprintf(buf, "/proc/%lu", getpid());
   fd = open(buf, O_RDONLY);
   if (fd < 0) { perror("Couldn't open proc file"); exit(1); }

   /* Turn off preemption. */
   sigfillset(&BR_set);
   sigprocmask(SIG_SETMASK, &BR_set, &BR_old_mask);

   BR_my_lwpid = _lwp_self();

   /*
   ** Stop all other lwps.  Because lwps may be forked or die out
   ** while doing this, we need to loop until we're sure that we
   ** are the only lwp running.
   */
   other_lwps_still_running = 1;
   while (other_lwps_still_running) {
      retval = ioctl(fd, PIOCSTATUS, &BR_status);
      if (fd < 0) { perror("Couldn't PIOCSTATUS"); exit(1); }

      /*
      ** The number of lwps might not be right here, so we fake
      ** it by making room for some extra lwps (100).  This is
      ** a hack, but there doesn't seem to be any way around it.
      */
      BR_lwpstats = malloc(sizeof(prstatus_t)*(BR_status.pr_nlwp+100));
      retval = ioctl(fd, PIOCLSTATUS, BR_lwpstats);
      if (fd < 0) { perror("Couldn't PIOCLSTATUS"); exit(1); }

      other_lwps_still_running = 0;
      /* Try to suspend the other lwps. */
      for (i=1; i<BR_status.pr_nlwp+1; i++) {
         if (BR_lwpstats[i].pr_who == BR_my_lwpid) continue;
         if (!(BR_lwpstats[i].pr_flags & PR_STOPPED)) {
            if (_lwp_suspend(BR_lwpstats[i].pr_who) < 0) {
               /* Might fail if lwp exited already */
            }
            other_lwps_still_running = 1;
         }
      }

      if (other_lwps_still_running) free(BR_lwpstats);
   }
   close(fd);
}

/*
** Restarts threads halted by "BR_freeze".  This should be executed
** exactly once.
*/
void BR_thaw() {
   int i, retval;

   /* Turn other lwps back on. */
   for (i=1; i<BR_status.pr_nlwp+1; i++) {
      if (BR_lwpstats[i].pr_who == BR_my_lwpid) continue;
      retval = _lwp_continue(BR_lwpstats[i].pr_who);
      if (retval < 0) { perror("_lwp_continue failed"); exit(1); }
   }

   free(BR_lwpstats);

   /* Turn preemption back on. */
   sigprocmask(SIG_SETMASK, &BR_old_mask, NULL);
}


BR_cluster_t BR_HERE() {
   BR_cluster_t res;
   thr_getspecific(BR_here_key, (void **) &res);
   return res;
}



