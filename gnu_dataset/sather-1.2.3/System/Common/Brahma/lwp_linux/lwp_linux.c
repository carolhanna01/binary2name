/*------------------------->  ANSI C - sourcefile  <-------------------------*/
/* Copyright (C) 199x by International Computer Science Institute            */
/* Copyright (C) 1999 by Free Software Foundation, Inc.                      */
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
** This is the implementation for the Brahma platform - version LWP-Linux
*/

#include "lwp_linux.h"

#include <stdio.h>
#include <sys/types.h>
#include <sys/signal.h>
/* #include <sys/fault.h> -- Nobbi: did not find this */
#include <sys/syscall.h>
#include <sys/procfs.h>
#include <assert.h>
#include <sys/sysinfo.h>
#include <stdlib.h>
#include <string.h>


void BR_error(char *s) {
   fprintf(stderr,"Brahma fatal error: %s\n",s); exit(1);
}

caddr_t *BR_cluster_local_arr;
unsigned int BR_clusters;

/* Does absolutely nothing; convenient no-op handler. */
void BR_dummy() {
}

/* Note: The current cluster of a thread is stored in the unused field pcb->argc */

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

   /* Avoid peallocating local per thread memory for now */
   /* Preallocate enough local memory for pSather threads */
   /* Initialize the cluster local array */
   BR_cluster_local_arr = (caddr_t *)malloc(BR_CLUSTERS()*sizeof(caddr_t));
   for (i=0; i<BR_CLUSTERS(); i++)
      BR_cluster_local_arr[i] = calloc(BR_CLUSTER_LOCAL_SIZE(), 1);

   initlp(1); /* initialize LWP - priority 1 */

   BR_here = 0; /* Start on cluster 0 */
};

void BR_exit() {
   assert(BR_HERE()==0);
}

void BR_FORK_mem(BR_cluster_t c, BR_handler_mem_t func,
                    caddr_t to, size_t size,BR_word_t arg0);
       /* defined below */


/*
** Transfer size bytes from local memory at address from to remote
** address to; on completion, remote handler is invoked with the
** arguments (<requesting cluster>, to, size, arg0).  Sender
** blocks until memory transfer complete.  There is no failure mode.
*/
void BR_STORE(BR_cluster_t c, caddr_t from, caddr_t to, size_t size,
		  BR_handler_mem_t handler, BR_word_t arg0) {

   (void) memcpy(to, from, size);
   BR_FORK_mem(c, handler, to, size, arg0);
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

   (void) memcpy(to, from, size);
   BR_FORK_mem(c, handler, to, size, arg0a);
   on_completion(c, from, size, arg0b);
}

/*
** Transfer data from remote cluster c to local memory.
** when transfer is complete, handler is invoked locally
** with arguments (c, to, size, arg0).  This may not be called
** from any handler function.  There is no failure mode.
*/
void BR_GET(BR_cluster_t c, caddr_t from, caddr_t to, size_t size,
		  BR_handler_mem_t handler, BR_word_t arg0) {

   (void) memcpy(to, from, size);
   handler(c, to, size, arg0);
}


/*
 * A number of wrapper functions that do mainly unpack the arguments
 */
void entry_wrapper_0(int argc,char **argv,char *envp) {
    (*(BR_handler_0_t)envp)((BR_cluster_t)(argv[0])); /* calling cluster */
}

void entry_wrapper_1(int argc,char **argv,char *envp) {
    (*(BR_handler_1_t)envp)((BR_cluster_t)(argv[0]), /* calling cluster */
                            (BR_word_t)(argv[1]));   /* arg0 */
}

void entry_wrapper_2(int argc,char **argv,char *envp) {
    (*(BR_handler_2_t)envp)((BR_cluster_t)(argv[0]), /* calling cluster */
                            (BR_word_t)(argv[1]),    /* arg0 */
                            (BR_word_t)(argv[2]));   /* arg1 */
}

void entry_wrapper_3(int argc,char **argv,char *envp) {
    (*(BR_handler_3_t)envp)((BR_cluster_t)(argv[0]), /* calling cluster */
                            (BR_word_t)(argv[1]),    /* arg0 */
                            (BR_word_t)(argv[2]),    /* arg1 */
                            (BR_word_t)(argv[3]));   /* arg2 */
}

void entry_wrapper_4(int argc,char **argv,char *envp) {
    (*(BR_handler_4_t)envp)((BR_cluster_t)(argv[0]), /* calling cluster */
                            (BR_word_t)(argv[1]),    /* arg0 */
                            (BR_word_t)(argv[2]),    /* arg1 */
                            (BR_word_t)(argv[3]),    /* arg2 */
                            (BR_word_t)(argv[4]));   /* arg3 */
}

void entry_wrapper_5(int argc,char **argv,char *envp) {
    (*(BR_handler_5_t)envp)((BR_cluster_t)(argv[0]), /* calling cluster */
                            (BR_word_t)(argv[1]),    /* arg0 */
                            (BR_word_t)(argv[2]),    /* arg1 */
                            (BR_word_t)(argv[3]),    /* arg2 */
                            (BR_word_t)(argv[4]),    /* arg3 */
                            (BR_word_t)(argv[5]));   /* arg4 */
}

void entry_wrapper_mem(int argc,char **argv,char *envp) {
    (*(BR_handler_mem_t)envp)((BR_cluster_t)(argv[0]), /* calling cluster */
                              (caddr_t)(argv[1]),    /* to */
                              (size_t)(argv[2]),    /* size */
                              (BR_word_t)(argv[3]));   /* arg0 */
}

void entry_wrapper_delay(int argc,char **argv,char *envp) {
    delayp((BR_delay_t)argv[0]);
    (*(BR_delay_handler_t)envp)((void *)(argv[1]));
}

/*
 * Create a new thread on cluster "c" executing "func".
 */
void BR_FORK_0(BR_cluster_t c, BR_handler_0_t func) {
    char *argv[1];
    argv[0] = (char *)BR_HERE();
    creatp(1,&entry_wrapper_0,BR_THREAD_STACK_SIZE,c,argv,(char *)func);
}

void BR_FORK_1(BR_cluster_t c, BR_handler_1_t func, BR_word_t arg0) {
    char *argv[2];
    argv[0] = (char *)BR_HERE();
    argv[1] = (char *)arg0;
    creatp(1,&entry_wrapper_1,BR_THREAD_STACK_SIZE,c,argv,(char *)func);
}

void BR_FORK_2(BR_cluster_t c, BR_handler_2_t func, BR_word_t arg0, BR_word_t arg1) {
    char *argv[3];
    argv[0] = (char *)BR_HERE();
    argv[1] = (char *)arg0;
    argv[2] = (char *)arg1;
    creatp(1,&entry_wrapper_2,BR_THREAD_STACK_SIZE,c,argv,(char *)func);
}

void BR_FORK_3(BR_cluster_t c, BR_handler_3_t func, 
	       BR_word_t arg0, BR_word_t arg1, BR_word_t arg2) {
    char *argv[4];
    argv[0] = (char *)BR_HERE();
    argv[1] = (char *)arg0;
    argv[2] = (char *)arg1;
    argv[3] = (char *)arg2;
    creatp(1,&entry_wrapper_3,BR_THREAD_STACK_SIZE,c,argv,(char *)func);
}

void BR_FORK_4(BR_cluster_t c, BR_handler_4_t func, 
	       BR_word_t arg0, BR_word_t arg1, BR_word_t arg2, BR_word_t arg3) {
    char *argv[5];
    argv[0] = (char *)BR_HERE();
    argv[1] = (char *)arg0;
    argv[2] = (char *)arg1;
    argv[3] = (char *)arg2;
    argv[4] = (char *)arg3;
    creatp(1,&entry_wrapper_4,BR_THREAD_STACK_SIZE,c,argv,(char *)func);
}

void BR_FORK_5(BR_cluster_t c, BR_handler_5_t func, 
	       BR_word_t arg0, BR_word_t arg1, BR_word_t arg2, BR_word_t arg3, BR_word_t arg4) {
    char *argv[6];
    argv[0] = (char *)BR_HERE();
    argv[1] = (char *)arg0;
    argv[2] = (char *)arg1;
    argv[3] = (char *)arg2;
    argv[4] = (char *)arg3;
    argv[5] = (char *)arg4;
    creatp(1,&entry_wrapper_5,BR_THREAD_STACK_SIZE,c,argv,(char *)func);
}

/* 
 * This one is only meant internally, since messages are simply forks in this implementation
 */
void BR_FORK_mem(BR_cluster_t c, BR_handler_mem_t func,
                 caddr_t to, size_t size,BR_word_t arg0) {
    char *argv[4];
    argv[0] = (char *)BR_HERE();
    argv[1] = (char *)to;
    argv[2] = (char *)size;
    argv[3] = (char *)arg0;
    creatp(1,&entry_wrapper_mem,BR_THREAD_STACK_SIZE,c,argv,(char *)func);
}

/*
 * Create human-readable form of the thread id in the buffer "buf",
 * for debugging.
 */
char *BR_ascii_id(BR_thread_t id, char *buf, size_t maxlen) {
   snprintf(buf,maxlen,"%i",id->id); /* use the ->id field of the pcb struct */
   return buf;
}

/*
 * Asyncronously call a function after at least n.sec seconds and
 * n.nsec nano seconds, with arg as argument.  May or may not be
 * executed by the thread that called "BR_delay_function".
 */
void BR_delay_function(BR_delay_t n, BR_delay_handler_t func, void *arg) {
    char *argv[2];
    argv[0] = (char *)n;
    argv[1] = (char *)arg;
    creatp(1,&entry_wrapper_delay,BR_THREAD_STACK_SIZE,BR_HERE(),argv,(char *)func);
}

void BR_unlock_handler(BR_cluster_t ignored, BR_word_t lock) {
    BR_UNLOCK((BR_lock_t)lock);
}

void BR_signal_handler(BR_cluster_t ignored, BR_word_t sema) {
    BR_SIGNAL((BR_sema_t)sema);
}

void BR_signal_mem_handler(BR_cluster_t from, caddr_t a, size_t size, BR_word_t sema) {
    BR_SIGNAL((BR_sema_t)sema);
}
