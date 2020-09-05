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
** This is the header for distributed TCP based Brahma (nodes run
** Solaris
*/
#ifndef _TCP_WIN32_H_
#define _TCP_WIN32_H_

#ifndef _REENTRANT
#define _REENTRANT
#endif

#ifdef BR_TCP_WIN32_IMPL
# define EXTERNAL
#else
# define EXTERNAL extern
#endif

#include <thread.h>
#include <unistd.h>
#include <limits.h>
#include <time.h>

/*#define TCP
#define QQ_THREADS
#define SOLARIS_THREADS
#define _REENTRANT
*/
#include "../win32_common/win32_types.h"
/* Warning: the order of the two includes is important. To see why
 * try swapping them ;-) 
 */
#include "br_tcpam.h"     /* AM, must be included _before_ brahma2am.h */
#include "../win32_common/brahma2am.h"    /* Communication stuff */
#include "../win32_common/win32_sync.h" /* Threads and synchronization stuff */


/* Define BR_POLL to do nothing */
#define BR_POLL() 

/* poll that blocks on select */
#define BR_BLOCK_POLL() qq_poll(1)
#define BR_ASYNC_POLL() qq_poll(0)

/* Prototypes and definitions... */

EXTERNAL void BR_init(int clusters, int argc, char *argv[]);
EXTERNAL void BR_start();
EXTERNAL void BR_exit();

EXTERNAL unsigned int BR_PROCESSORS();

EXTERNAL void BR_freeze();

EXTERNAL void BR_thaw();

EXTERNAL void BR_unlock_handler(BR_cluster_t, BR_word_t);
EXTERNAL void BR_signal_handler(BR_cluster_t, BR_word_t);
EXTERNAL void BR_signal_mem_handler(BR_cluster_t, caddr_t , size_t, caddr_t);

#define BR_ASCII_PLATFORM "tcp_solaris"
EXTERNAL char * BR_ascii_id(BR_thread_t, char *, size_t);

EXTERNAL int BR_thread_may_poll();

/* These things need to change */
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

/* this forks off a local thread and executes the passed function */
/* Pretty dumb for now - no pooling */
#define BR_FORK_LOCAL_1(func,arg) thr_create(NULL,0,(void *(*)(void*))func,\
					     (void*)arg,THR_DETACHED,NULL)

void BR_BARRIER();

#endif

