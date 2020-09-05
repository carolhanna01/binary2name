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
** This is the header for Brahma implemented using Active Threads
*/

#ifndef _AT_LINUX_SMP_H_
#define _AT_LINUX_SMP_H_


#ifdef BR_AT_LINUX_SMP_IMPL
# define EXTERNAL
#else
# define EXTERNAL extern
#endif

#include <unistd.h>
#include <limits.h>
#include <time.h>

#include "at.h"
#include "at_common/at_types.h"
/* include Active Thread thread and synchronization stuff common across all
 * Active Thread based platforms 
 */
#include "at_common/at_sync.h"

/* Deal with active messages and things that vary across different AT
 * platforms (such BR_PROCESSORS, BR_freeze and BR_thaw, BR_init and BR_exit, etc.  
 */

/* Standard Active Message handler types */
typedef void (*BR_handler_0_t)(BR_cluster_t);
typedef void (*BR_handler_1_t)(BR_cluster_t,BR_word_t);
typedef void (*BR_handler_2_t)(BR_cluster_t,BR_word_t,BR_word_t);
typedef void (*BR_handler_3_t)(BR_cluster_t,BR_word_t,BR_word_t,BR_word_t);
typedef void (*BR_handler_4_t)(BR_cluster_t,BR_word_t,BR_word_t,BR_word_t,BR_word_t);
typedef void (*BR_handler_5_t)(BR_cluster_t,BR_word_t,BR_word_t,BR_word_t,BR_word_t,BR_word_t);

/* Bulk xfer */
typedef void (*BR_handler_mem_t)(BR_cluster_t,caddr_t,size_t,caddr_t);
typedef void (*BR_signal_handler_t)(void);
typedef void (*BR_handler_t)(BR_cluster_t,...);


/* Prototypes and definitions... */

EXTERNAL void BR_init(int clusters, int argc, char *argv[]);
EXTERNAL void BR_start();
EXTERNAL void BR_exit();

EXTERNAL unsigned int BR_clusters;
#define BR_CLUSTERS() BR_clusters

/*EXTERNAL unsigned int BR_PROCESSORS(); */

EXTERNAL BR_cluster_t BR_HERE();
#define BR_MAX_XFER() INT_MAX

EXTERNAL void BR_REQUEST_0(BR_cluster_t, BR_handler_0_t);
EXTERNAL void BR_REQUEST_1(BR_cluster_t, BR_handler_1_t, BR_word_t);
EXTERNAL void BR_REQUEST_2(BR_cluster_t, BR_handler_2_t, BR_word_t, BR_word_t);
EXTERNAL void BR_REQUEST_3(BR_cluster_t, BR_handler_3_t, BR_word_t, BR_word_t, BR_word_t);
EXTERNAL void BR_REQUEST_4(BR_cluster_t, BR_handler_4_t,BR_word_t,BR_word_t,BR_word_t,BR_word_t);
EXTERNAL void BR_REQUEST_5(BR_cluster_t, BR_handler_5_t,BR_word_t,BR_word_t,BR_word_t,BR_word_t,BR_word_t);

EXTERNAL void BR_REPLY_0(BR_cluster_t, BR_handler_0_t);
EXTERNAL void BR_REPLY_1(BR_cluster_t, BR_handler_1_t, BR_word_t);
EXTERNAL void BR_REPLY_2(BR_cluster_t, BR_handler_2_t, BR_word_t, BR_word_t);
EXTERNAL void BR_REPLY_3(BR_cluster_t, BR_handler_3_t, BR_word_t, BR_word_t, BR_word_t);
EXTERNAL void BR_REPLY_4(BR_cluster_t, BR_handler_4_t, BR_word_t, BR_word_t,BR_word_t,BR_word_t);
EXTERNAL void BR_REPLY_5(BR_cluster_t, BR_handler_5_t, BR_word_t, BR_word_t,BR_word_t,BR_word_t,BR_word_t);

EXTERNAL void BR_STORE(BR_cluster_t, caddr_t,caddr_t,size_t,BR_handler_mem_t,caddr_t);

EXTERNAL void BR_ASYNC_STORE(BR_cluster_t, caddr_t, caddr_t, size_t,
		  BR_handler_mem_t, BR_word_t, BR_handler_mem_t, BR_word_t);

EXTERNAL void BR_GET(BR_cluster_t, caddr_t, caddr_t, size_t,BR_handler_mem_t,caddr_t);

EXTERNAL void BR_dummy();

EXTERNAL void BR_freeze();

EXTERNAL void BR_thaw();

/* 
 *  We ignore the ability to bind Active Threads to particular processors
 * and create all threads ubound.
 */

#define BR_FORK_0(c,f) 		 at_create_1(at_get_focus(),AT_UNBOUND,(at_userf_1_t*)f,BR_HERE())
#define BR_FORK_1(c,f,a) 	 at_create_2(at_get_focus(),AT_UNBOUND,(at_userf_2_t*)f,BR_HERE(),a)
#define BR_FORK_2(c,f,a,b) 	 at_create_3(at_get_focus(),AT_UNBOUND,(at_userf_3_t*)f,BR_HERE(),a,b)
#define BR_FORK_3(c,f,a,b,d) 	 at_create_4(at_get_focus(),AT_UNBOUND,(at_userf_4_t*)f,BR_HERE(),a,b,d)
#define BR_FORK_4(c,f,a,b,d,e) 	at_create_5(at_get_focus(),AT_UNBOUND,(at_userf_5_t*)f,BR_HERE(),a,b,d,e)
#define BR_FORK_5(c,f,a,b,d,e,g) at_create_6(at_get_focus(),AT_UNBOUND,(at_userf_6_t*)f,BR_HERE(),a,b,d,e,g)

#define BR_THREAD_YIELD()               at_yield()

#define BR_ASCII_PLATFORM() "ActiveThread_linux_smp"
EXTERNAL char * BR_ascii_id(BR_thread_t, char *, size_t);

#define BR_POLL()

/*
** Get the local memory associated with this cluster.  For truly
** distributed systems, this may just be a pointer to a static
** region at the same address everywhere.  For systems which emulate
** distribution in a single address space, this will be a zeroed
** region allocated in BR_init(...).
*/

#define BR_CLUSTER_LOCAL() (BR_cluster_local_arr[BR_HERE()])

/* Size of the cluster local memory in bytes, at least 1KB. */

#define BR_CLUSTER_LOCAL_SIZE() 1024
EXTERNAL caddr_t *BR_cluster_local_arr;

#endif

/* This platform only supports one cluster. */
#define BR_BARRIER()







