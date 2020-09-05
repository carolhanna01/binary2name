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
** This is the implementation for the serial Brahma platform
*/

#define BR_SERIAL_IMPL
#include "serial.h"
#include <stdio.h>

void BR_error(char *s) {
   fprintf(stderr,"Brahma fatal error: %s\n",s); exit(1);
}

/* Things necessary to determine the number of procs for a cluster */

#include <assert.h>
#include <sys/sysinfo.h>
#include <stdlib.h>
#include <string.h>

#define MOVETO(x) BR_here=(x)

void BR_init(int clusters, int argc, char *argv[]) {

   if (clusters==0) {
      if (getenv("CLUSTERS")!=NULL) {
	 sscanf(getenv("CLUSTERS"), "%d", &BR_clusters);
      } else {
	 BR_clusters = 1;
      }
   } else {
      BR_clusters = clusters;
   }

   MOVETO(0); /* Start on cluster 0 */
};

void BR_exit() {
   assert(BR_HERE()==0);
}

void BR_start() {}

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


/*
** Transfer size bytes from local memory at address from to remote
** address to; on completion, remote handler is invoked with the
** arguments (<requesting cluster>, to, size, arg0).  Sender
** blocks until memory transfer complete.  There is no failure mode.
*/
void BR_STORE(BR_cluster_t c, caddr_t from, caddr_t to, size_t size,
		  BR_handler_mem_t handler, BR_word_t arg0) {

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
		  BR_handler_mem_t handler, BR_word_t arg0) {

   (void) memcpy(to, from, size);
   handler(c, (void *) to, size, (void *) arg0);
}


/* Does absolutely nothing; convenient no-op handler. */
void BR_dummy() {
}

#include <sys/types.h>
#include <sys/signal.h>
/* #include <sys/fault.h> -- Nobbi: did not find this */
#include <sys/syscall.h>
#include <sys/procfs.h>

void BR_freeze() {
}

void BR_thaw() {
}

/* Create a new thread on cluster "c" executing "func". */
void BR_FORK_0(BR_cluster_t c, BR_handler_0_t func) {
   BR_error("Thread creation is illegal on the serial Brahma platform.");
}

void BR_FORK_1(BR_cluster_t c, BR_handler_1_t func, BR_word_t arg0) {
   BR_error("Thread creation is illegal on the serial Brahma platform.");
}

void BR_FORK_2(BR_cluster_t c, BR_handler_2_t func, BR_word_t arg0, BR_word_t arg1) {
   BR_error("Thread creation is illegal on the serial Brahma platform.");
}

void BR_FORK_3(BR_cluster_t c, BR_handler_3_t func, 
	       BR_word_t arg0, BR_word_t arg1, BR_word_t arg2) {
   BR_error("Thread creation is illegal on the serial Brahma platform.");
}

void BR_FORK_4(BR_cluster_t c, BR_handler_4_t func, 
	       BR_word_t arg0, BR_word_t arg1, BR_word_t arg2, BR_word_t arg3) {
   BR_error("Thread creation is illegal on the serial Brahma platform.");
}


/*
** Create human-readable form of the thread id in the buffer "buf",
** for debugging.
*/
char *BR_ascii_id(BR_thread_t id, char *buf, size_t maxlen) {
   BR_error("BR_ascii_id not yet implemented.");
}

/*
** Asyncronously call a function after at least n.sec seconds and
** n.nsec nano seconds, with arg as argument.  May or may not be
** executed by the thread that called "BR_delay_function".
*/
void BR_delay_function(BR_delay_t n, BR_delay_handler_t func, void *arg) {
   BR_error("BR_delay_function not meaningful for serial Brahma platform.");
}

void BR_unlock_handler(BR_cluster_t ignored, BR_word_t lock) {
}

void BR_signal_handler(BR_cluster_t ignored, BR_word_t sema) {
}

void BR_signal_mem_handler(BR_cluster_t from, void *a, int size, BR_word_t sema)
 { }

