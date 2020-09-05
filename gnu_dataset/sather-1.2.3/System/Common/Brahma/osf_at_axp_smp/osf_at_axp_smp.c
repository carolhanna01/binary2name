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
** This is the implementation for Brahma using Active Threads
*/

#define BR_OSF_AT_AXP_SMP_IMPL
#include "osf_at_axp_smp.h"
#include <stdio.h>
#include <stdlib.h>
#include <assert.h>
#include <fcntl.h>
#include <sys/systeminfo.h>
#include <string.h>
#include <time.h>

void BR_error(char *s) {
   fprintf(stderr,"Brahma fatal error: %s\n",s); exit(1);
}

/* Just set a word in the thread data structure to be "current cluster" */
#define MOVETO(x) at_setdata(x)

static int BR_stack_size;


void BR_init(int clusters, int argc, char *argv[]) {
   int i;
   clock_t start_time;
   
   BR_stack_size = 0x10000;

   clock();
   if (clusters==0) {
      if (getenv("CLUSTERS")!=NULL) {
	 sscanf(getenv("CLUSTERS"), "%d", &BR_clusters);
      } else {
	 BR_clusters = 1;
      }
   } else {
      BR_clusters = clusters;
   }

   /* Initialize Active Threads with concurrency level quial to
      the number of processors on this cluster */
   /* Avoid peallocating local per thread memory for now */
   /* Preallocate enough local memory for pSather threads */
   /* Initialize the cluster local array */
   BR_cluster_local_arr = (caddr_t *) at_malloc(BR_CLUSTERS()*sizeof(caddr_t));
   for (i=0; i<BR_CLUSTERS(); i++)
      BR_cluster_local_arr[i] = at_calloc(BR_CLUSTER_LOCAL_SIZE(), 1);

   at_init(BR_PROCESSORS(), BR_stack_size, 200);
   MOVETO(0); /* Start on cluster 0 */
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


/*
** Forcefully halt all threads on all clusters (other than those
** used by Brahma itself) and wait for any user active messages to
** drain.  On return there is no user activity other than the
** executing thread.  This is useful for debugging and garbage
** collection.
*/
void BR_freeze() {
	/* Well, we currently support only uniprocessor ALPHAs,
	so do not need to do anything here */
}

/*
** Restarts threads halted by "BR_freeze".  This should be executed
** exactly once.
*/
void BR_thaw() {
}


BR_cluster_t BR_HERE() {
   return at_getdata();
}



