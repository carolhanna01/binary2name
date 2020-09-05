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

#ifndef _AT_H_
#define _AT_H_

#ifdef __cplusplus
extern "C" {
#endif

/* 
 * Disable TNF - it uses Solaris Threads locks and currentrly cannot
 * be used for tracing of Active Threads.
*/
#define NPROBE


/* Need to our version of HPUX */
/* #define int32_t int */

#if defined( __GNUC__) 
 #ifndef AT_INLINE 
   #ifndef AT_NO_INLINE
      #define AT_INLINE inline extern
   #else
     /* The following can be used for debugging to get rid of inlining */
      #define AT_INLINE static 
   #endif
 #endif
#else
  #define AT_INLINE 
#endif

#include "thread.h"

/*typedef struct at_t at_t; */

/* Each thread starts by calling a user-supplied function of this
   type. */

/*typedef void (at_userf_t)(void*);*/
typedef void (at_userf_0_t)(void);
typedef void (at_userf_1_t)(at_word_t a1);
typedef void (at_userf_2_t)(at_word_t a1, at_word_t a2);
typedef void (at_userf_3_t)(at_word_t a1, at_word_t a2, at_word_t a3);
typedef void (at_userf_4_t)(at_word_t a1, at_word_t a2, at_word_t a3, at_word_t a4);
typedef void (at_userf_5_t)(at_word_t a1, at_word_t a2, at_word_t a3, at_word_t a4, at_word_t a5);
typedef void (at_userf_6_t)(at_word_t a1, at_word_t a2, at_word_t a3, at_word_t a4, at_word_t a5, at_word_t a6);
/* Call this before any other primitives. */
extern void at_init(unsigned int concurrency, unsigned int stack_size, 
		    unsigned int local_size);

#ifdef __linux__
extern void at_stop(void) ;
#endif

/* Include inline stuff */
#include "at-inline.h"
/* Include wrappers for I/O, system calls, etc */
#include "wrappers.h"

#include "mutex.h"
#include "sema.h"
#include "barrier.h"
#include "rw.h"

#include "bundle.h"
#include "../schedulers/scheduler.h"

extern volatile at_bundle_t *at_bundle;


/* Like `at_yield' but the thread is discarded.  Any intermediate
   state is lost.  The thread can also terminate by simply
   returning. */

extern void at_exit(void);
extern void at_do_when_idle(void (*func)());
extern void at_stop();
extern void at_continue();
extern void at_bundle_destroy(at_bundle_t *b);
extern void at_join_all();

#ifdef __cplusplus
}
#endif

#endif /* _AT_H_ */






