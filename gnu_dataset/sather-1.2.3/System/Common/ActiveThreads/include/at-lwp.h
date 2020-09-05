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

#ifndef _AT_LWP_
#define _AT_LWP_

#include "thread.h"
#include "lwp.h" 
/*
 *This must be private per lightweight process. The underlying
 * implementation must provide a way to arrange this. Possible
 * solutions are per process private memory if it exists or reserving
 * the register for just this purpose (such as %g7 on the SPARCs
 */
typedef struct lwp_private_t
{
	at_thread_t *at_main;      /* "main" thread for this LWP */
	at_thread_t *at_current;  /* a thread currently executing on this LWP */
	int id;            /* this lwp's id */
}
lwp_private_t;

extern volatile unsigned int at_stack_size;
extern volatile unsigned int at_local_size;
/*
 * If the processor does not have private memory, we use the thread
 * stack to store the pointer to LWP structure. In this case, the stack
 * has to be aligned on the stack size boundary and the pointer to 
 * LWP data goes at the base of the stack. This way, given sp at any
 * point in thread's lifetime, we can recover the address of the
 * stack base and extract the pointer to LWP structure
 */

/*
 * Well, this needs some more work: I could get around passing 
 * a pointer to lwp_private on the thread stack with sparcs and i386
 * running Solaris. Will probably complete this at some later
 * point when there is a need for it
 */

/*
#ifndef AT_PROC_MEM
  #ifdef QT_GROW_DOWN 
    #define AT_STKTOP(sp) \
       (((unsigned)(sp) & ~(AT_STKBASE_ALIGNMENT-1)) + sizeof(qt_word_t))
  #endif
 
  #ifdef QT_GROW_UP
    #define AT_STKTOP(sp) \
     ((((sp)+AT_STKSIZE) & ~(AT_STKBASE_ALIGNMENT-1)) - sizeof(qt_word_t))
  #endif
 
  #define LWP_SET_PRIVATE(val) \
     ((*(qt_word_t *)AT_STKTOP(AT_CURRENT_SP))=(qt_word_t)(val))
 
  #define LWP_GET_PRIVATE    \
     ((lwp_private_t *)(*(qt_word_t *)AT_STKTOP(AT_CURRENT_SP)))
 
 
#endif
*/

/*----------------------------------------------------------------*/

#define AT_SET_CURRENT(x)       LWP_GET_PRIVATE->at_current=(x);
#define AT_GET_CURRENT          LWP_GET_PRIVATE->at_current

#define AT_SET_MAIN(x)          LWP_GET_PRIVATE->at_main=(x)
#define AT_GET_MAIN             LWP_GET_PRIVATE->at_main

#define AT_SET_MAIN_STACK(x)    LWP_GET_PRIVATE->at_main->sp=(x)
#define AT_GET_MAIN_STACK       LWP_GET_PRIVATE->at_main->sp

#define at_cpu()                LWP_GET_PRIVATE->id
#define at_vproc()              LWP_GET_PRIVATE->at_current->affinity

/*----------------------------------------------------------------*/

#endif  /* _AT_LWP_ */
