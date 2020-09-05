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
** This is the serial header for Brahma
*/

#ifdef BR_SERIAL_IMPL
# define EXTERNAL
#else
# define EXTERNAL extern
#endif

#include <unistd.h>
#include <limits.h>
#include <sys/types.h>

/* Basic types. */
typedef unsigned long BR_word_t;
typedef unsigned long long BR_doubleword_t;

/* Integral cluster id type. */
typedef unsigned long BR_cluster_t;

/* Standard Active Message handler types */
typedef void (*BR_handler_0_t)(BR_cluster_t);
typedef void (*BR_handler_1_t)(BR_cluster_t,BR_word_t);
typedef void (*BR_handler_2_t)(BR_cluster_t,BR_word_t,BR_word_t);
typedef void (*BR_handler_3_t)(BR_cluster_t,BR_word_t,BR_word_t,BR_word_t);
typedef void (*BR_handler_4_t)(BR_cluster_t,BR_word_t,BR_word_t,BR_word_t,BR_word_t);

/* Bulk xfer */
typedef void (*BR_handler_mem_t)(BR_cluster_t,caddr_t,size_t,caddr_t);
typedef void (*BR_signal_handler_t)(void);
typedef void (*BR_handler_t)(BR_cluster_t,...);

/* ID of a thread */
typedef int BR_thread_t;

/* Synchronization types */
typedef void *BR_lock_t;
typedef void *BR_sema_t;
typedef volatile unsigned char BR_spinlock_t;

/* A unit of delay */
typedef struct {
   unsigned long sec;
   unsigned long nsec;
} BR_delay_t;

/* Handler for delayed function calls. */
typedef void (*BR_delay_handler_t)(void *);



/* Prototypes and definitions... */

EXTERNAL void BR_init(int clusters, int argc, char *argv[]);
EXTERNAL void BR_exit();
EXTERNAL void BR_start();

EXTERNAL unsigned int BR_clusters;
#define BR_CLUSTERS() BR_clusters

EXTERNAL BR_cluster_t BR_here;
#define BR_PROCESSORS() 1
#define BR_HERE() BR_here

#define BR_MAX_XFER() INT_MAX

EXTERNAL void BR_REQUEST_0(BR_cluster_t, BR_handler_0_t);
EXTERNAL void BR_REQUEST_1(BR_cluster_t, BR_handler_1_t, BR_word_t);
EXTERNAL void BR_REQUEST_2(BR_cluster_t, BR_handler_2_t, BR_word_t, BR_word_t);
EXTERNAL void BR_REQUEST_3(BR_cluster_t, BR_handler_3_t, BR_word_t, BR_word_t, BR_word_t);
EXTERNAL void BR_REQUEST_4(BR_cluster_t, BR_handler_4_t,BR_word_t,BR_word_t,BR_word_t,BR_word_t);

EXTERNAL void BR_REPLY_0(BR_cluster_t, BR_handler_0_t);
EXTERNAL void BR_REPLY_1(BR_cluster_t, BR_handler_1_t, BR_word_t);
EXTERNAL void BR_REPLY_2(BR_cluster_t, BR_handler_2_t, BR_word_t, BR_word_t);
EXTERNAL void BR_REPLY_3(BR_cluster_t, BR_handler_3_t, BR_word_t, BR_word_t, BR_word_t);
EXTERNAL void BR_REPLY_4(BR_cluster_t, BR_handler_4_t, BR_word_t, BR_word_t,BR_word_t,BR_word_t);

EXTERNAL void BR_STORE(BR_cluster_t, caddr_t,caddr_t,size_t,BR_handler_mem_t,BR_word_t);

EXTERNAL void BR_ASYNC_STORE(BR_cluster_t, caddr_t, caddr_t, size_t,
		  BR_handler_mem_t, BR_word_t, BR_handler_mem_t, BR_word_t);

EXTERNAL void BR_GET(BR_cluster_t, caddr_t, caddr_t, size_t,BR_handler_mem_t,BR_word_t);

EXTERNAL void BR_dummy();

EXTERNAL void BR_freeze();

EXTERNAL void BR_thaw();

EXTERNAL void BR_FORK_0(BR_cluster_t, BR_handler_0_t);
EXTERNAL void BR_FORK_1(BR_cluster_t, BR_handler_1_t, BR_word_t);
EXTERNAL void BR_FORK_2(BR_cluster_t, BR_handler_2_t, BR_word_t, BR_word_t);
EXTERNAL void BR_FORK_3(BR_cluster_t, BR_handler_3_t, BR_word_t, BR_word_t, BR_word_t);
EXTERNAL void BR_FORK_4(BR_cluster_t, BR_handler_4_t, BR_word_t, BR_word_t, BR_word_t,BR_word_t);

#define BR_THREAD_ID() 0
#define BR_SAME_THREAD(x, y) 1
#define BR_INVALID_ID() "BR_INVALID_ID not valid in serial platform"

EXTERNAL caddr_t BR_local_mem;

#define BR_SET_THREAD_LOCAL(x) BR_local_mem=(x)
#define BR_GET_THREAD_LOCAL() (BR_local_mem)

EXTERNAL void BR_delay_function(BR_delay_t, BR_delay_handler_t, void *);

#define BR_LOCK_CREATE() NULL
#define BR_LOCK_DELETE(l)
#define BR_SEMA_CREATE(c) NULL
#define BR_SEMA_DELETE(s)

#define BR_LOCK(l)
#define BR_UNLOCK(l)

#define BR_WAIT(s)
#define BR_SIGNAL(s)

#define BR_TRY_LOCK(l) 1
#define BR_TRY_WAIT(s) 1

EXTERNAL void BR_unlock_handler(BR_cluster_t, BR_word_t);
EXTERNAL void BR_signal_handler(BR_cluster_t, BR_word_t);
EXTERNAL void BR_signal_mem_handler(BR_cluster_t, void *, int, BR_word_t);

#define BR_SPINLOCK_DEC(s) unsigned char s
#define BR_SPINSEMA_DEC(s) unsigned char s
#define BR_SPINLOCK_INIT(s) 
#define BR_SPINSEMA_INIT(s, c)

#define BR_SPINLOCK_LOCK(x)
#define BR_SPINLOCK_UNLOCK(x)
#define BR_SPINLOCK_TRY(x) 0

#define BR_SPINSEMA_WAIT(s)
#define BR_SPINSEMA_SIGNAL(s)

#define BR_ASCII_PLATFORM "serial"
EXTERNAL char *BR_ascii_id(BR_thread_t, char *, size_t);


#define BR_BARRIER()

#define BR_POLL()
#define BR_THREAD_YIELD()

#define BR_REAL_SPINLOCK_LOCK(x)
#define BR_REAL_SPINLOCK_UNLOCK(x)
#define BR_CLUSTER_LOCAL() 0