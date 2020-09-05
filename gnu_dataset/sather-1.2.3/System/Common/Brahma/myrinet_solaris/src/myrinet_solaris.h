/*------------------------->  ANSI C - headerfile  <-------------------------*/
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
** This is the header for distributed Myrinet based Brahma (nodes run
** Solaris
*/
#ifndef _MYRINET_SOLARIS_H_
#define _MYRINET_SOLARIS_H_

#ifdef  __cplusplus
extern "C" {
#endif


#ifndef _REENTRANT
#define _REENTRANT
#endif

#if defined(BR_MYRINET_SOLARIS_IMPL) || defined(BR_MYRINET_SOLARIS_AT_IMPL)
# define EXTERNAL
#else
# define EXTERNAL extern
#endif


#ifdef __GNUG__ 
    #define CFUNC "C"
#else
    #define CFUNC  
#endif

#include <synch.h>
#include <thread.h>
#include <unistd.h>
#include <limits.h>
#include <time.h>
#include <sys/types.h>
#include <string.h>
#include <tnf/probe.h>  /* Needed for tracing with Sun's TNF utility */

#if defined(BR_MYRINET_SOLARIS)
  #include "../../solaris_common/solaris_types.h"
#elif defined(BR_MYRINET_SOLARIS_AT)
  #include "../../at_common/at_types.h"
#endif


/* Standard Active Message handler types */
typedef void (*BR_handler_0_t)(BR_cluster_t);
typedef void (*BR_handler_1_t)(BR_cluster_t,BR_word_t);
typedef void (*BR_handler_2_t)(BR_cluster_t,BR_word_t,BR_word_t);
typedef void (*BR_handler_3_t)(BR_cluster_t,BR_word_t,BR_word_t,BR_word_t);
typedef void (*BR_handler_4_t)(BR_cluster_t,BR_word_t,BR_word_t,BR_word_t,BR_word_t);
typedef void (*BR_handler_5_t)(BR_cluster_t,BR_word_t,BR_word_t,BR_word_t,BR_word_t, BR_word_t);

/* Bulk xfer */
typedef void (*BR_handler_mem_t)(BR_cluster_t,caddr_t,size_t,caddr_t);
typedef void (*BR_signal_handler_t)(void);
typedef void (*BR_handler_t)();


/* Prototypes and definitions... */

EXTERNAL void BR_init(int clusters, int argc, char *argv[]);
EXTERNAL void BR_start();

EXTERNAL void BR_exit();
EXTERNAL void BR_exit_remote();
EXTERNAL int BR_exit_reached();

EXTERNAL unsigned int BR_PROCESSORS();

extern int lam_myproc;
#define BR_HERE() lam_myproc

extern int lam_procs;
#define BR_CLUSTERS() lam_procs

EXTERNAL int BR_MAX_XFER();

EXTERNAL int BR_REQUEST_4(BR_cluster_t, BR_handler_4_t,BR_word_t,BR_word_t,BR_word_t,BR_word_t);
#define BR_REQUEST_0(dest,handler) BR_REQUEST_4(dest, \
					(BR_handler_4_t)handler,0,0,0,0)
#define BR_REQUEST_1(dest,handler,a0) BR_REQUEST_4(dest, \
					(BR_handler_4_t)handler,a0,0,0,0)
#define BR_REQUEST_2(dest,handler,a0,a1) BR_REQUEST_4(dest, \
					(BR_handler_4_t)handler,a0,a1,0,0)
#define BR_REQUEST_3(dest,handler,a0,a1,a3) BR_REQUEST_4(dest, \
					(BR_handler_4_t)handler,a0,a1,a3,0)

EXTERNAL int BR_REQUEST_5(BR_cluster_t, BR_handler_5_t,BR_word_t,BR_word_t,BR_word_t,BR_word_t,BR_word_t);


EXTERNAL void BR_REPLY_4(BR_cluster_t, BR_handler_4_t, BR_word_t,
BR_word_t,BR_word_t,BR_word_t); 
#define BR_REPLY_0(dest,handler) BR_REPLY_4(dest, \
					(BR_handler_4_t)handler,0,0,0,0)
#define BR_REPLY_1(dest,handler,a0) BR_REPLY_4(dest, \
					(BR_handler_4_t)handler,a0,0,0,0)
#define BR_REPLY_2(dest,handler,a0,a1) BR_REPLY_4(dest, \
					(BR_handler_4_t)handler,a0,a1,0,0)
#define BR_REPLY_3(dest,handler,a0,a1,a3) BR_REPLY_4(dest, \
					(BR_handler_4_t)handler,a0,a1,a3,0)
EXTERNAL void BR_REPLY_5(BR_cluster_t, BR_handler_5_t, BR_word_t,
BR_word_t,BR_word_t,BR_word_t,BR_word_t); 



EXTERNAL void BR_STORE(BR_cluster_t, caddr_t,caddr_t,size_t,BR_handler_mem_t,caddr_t);

EXTERNAL void BR_ASYNC_STORE(BR_cluster_t, caddr_t, caddr_t, size_t,
		  BR_handler_mem_t, BR_word_t, BR_handler_mem_t, BR_word_t);

EXTERNAL void BR_GET(BR_cluster_t, caddr_t, caddr_t, size_t,BR_handler_mem_t,caddr_t);

EXTERNAL void BR_dummy();

EXTERNAL void BR_freeze();

EXTERNAL void BR_thaw();

EXTERNAL void BR_POLL();

/*
** These are macros now...
** EXTERNAL void BR_FORK_0(BR_cluster_t, BR_handler_0_t);
** EXTERNAL void BR_FORK_1(BR_cluster_t, BR_handler_1_t, BR_word_t);
** EXTERNAL void BR_FORK_2(BR_cluster_t, BR_handler_2_t, BR_word_t, BR_word_t);
** EXTERNAL void BR_FORK_3(BR_cluster_t, BR_handler_3_t, BR_word_t, BR_word_t, BR_word_t);
** EXTERNAL void BR_FORK_4(BR_cluster_t, BR_handler_4_t, BR_word_t, BR_word_t, BR_word_t,BR_word_t);
*/

#if defined(BR_MYRINET_SOLARIS_AT)
EXTERNAL void BR_FORK_4(BR_cluster_t dest, BR_handler_t func,
	 BR_word_t arg0, BR_word_t arg1, BR_word_t arg2, BR_word_t arg3);
EXTERNAL void BR_FORK_3(BR_cluster_t dest, BR_handler_t func,
	 BR_word_t arg0, BR_word_t arg1, BR_word_t arg2);
EXTERNAL void BR_FORK_2(BR_cluster_t dest, BR_handler_t func,
	 BR_word_t arg0, BR_word_t arg1);
EXTERNAL void BR_FORK_1(BR_cluster_t dest, BR_handler_t func,
	 BR_word_t arg0);
EXTERNAL void BR_FORK_0(BR_cluster_t dest, BR_handler_t func);
#else
/* Helper for the macros below */
EXTERNAL void BR_enqueue_fork(BR_cluster_t, BR_handler_t, BR_word_t, BR_word_t, BR_word_t,BR_word_t);

#define BR_FORK_0(c,f) 	        BR_FORK_4(c,f,0,0,0,0)
#define BR_FORK_1(c,f,a) 	BR_FORK_4(c,f,a,0,0,0)
#define BR_FORK_2(c,f,a,b) 	BR_FORK_4(c,f,a,b,0,0)
#define BR_FORK_3(c,f,a,b,d) 	BR_FORK_4(c,f,a,b,d,0)

EXTERNAL void BR_FORK_4(BR_cluster_t dest, BR_handler_t func,
	 BR_word_t arg0, BR_word_t arg1, BR_word_t arg2, BR_word_t arg3);
#endif /* Done with FORKS */

#if defined(BR_MYRINET_SOLARIS)
  #define BR_ASCII_PLATFORM() "solaris_myrinet"
#elif defined(BR_MYRINET_SOLARIS_AT)
  #define BR_ASCII_PLATFORM() "solaris_at_myrinet"
#endif

EXTERNAL char * BR_ascii_id(BR_thread_t, char *, size_t);

/*
** Get the local memory associated with this cluster.  For truly
** distributed systems, this may just be a pointer to a static
** region at the same address everywhere.  For systems which emulate
** distribution in a single address space, this will be a zeroed
** region allocated in BR_init(...).
*/

#define BR_CLUSTER_LOCAL() BR_cluster_local

/* Size of the cluster local memory in bytes, at least 1KB. */

#define BR_CLUSTER_LOCAL_SIZE() 1024
EXTERNAL caddr_t *BR_cluster_local;

EXTERNAL void BR_BARRIER();

#ifdef  __cplusplus
}
#endif


#endif

