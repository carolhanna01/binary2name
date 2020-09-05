/*------------------------->  ANSI C - sourcefile  <-------------------------*/
/* Copyright (C) 1997 by International Computer Science Institute            */
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

/* ***************************************************************************
 * Active Threads implementation: sparc/solaris impl.                        *
 * written by:    Boris Weissman                                             *
 * on:            Feb 20, 1997                                               *
 *****************************************************************************/

#define NUMBER_OF_PROCESSORS 2

#include "lwp.h"
#include "wrappers.h"
#include "const.h"

#include <windows.h>
#include <unistd.h>

#define MAX_LWPS NUMBER_OF_PROCESSORS

static DWORD thread_id_map[MAX_LWPS] ;
static lwp_private_t *lwp_map[MAX_LWPS] ;

#define GET_THREAD_ID GetCurrentThreadId()

lwp_private_t *_lwp_getprivate()
{
  DWORD thread_id = GET_THREAD_ID ;
  int i = 0 ;
  while(i<MAX_LWPS) {
    if (thread_id == thread_id_map[i]) return lwp_map[i] ;
    i++ ;
  }
  fprintf(stderr,"Lost a  thread id [%d]\n", thread_id) ;
  fflush(stderr) ;
  return NULL ;
}

void _lwp_setprivate(lwp_private_t *lwp_private)
{
  lwp_map[lwp_private->id] = lwp_private ;
}

int at_nprocs = 0 ;

static int procs(void){
  char *num_procs = getenv("NUMBER_OF_PROCESSORS") ;
  if (num_procs == NULL) {
    fprintf(stderr,
	    "Missing system environment variable \"NUMBER_OF_PROCESSORS\"\n") ;
    fflush(stderr) ;
    exit(1) ;
  }
  return atoi(num_procs) ;
}

static char *stk_org[AT_MAX_PROCS];
static int stk_index[AT_MAX_PROCS];

int LWP_INIT(int concurrency){
  int i ;

  /* Figure out the number of processors */
  at_nprocs = procs();
  if(!at_nprocs){
    /* we are are in trouble */
    return 1;
  }

  if(concurrency <=0){
    concurrency = at_nprocs;
  }

  thread_id_map[0] = GET_THREAD_ID ;
  
  return 0;
}

static HANDLE thread_array[MAX_LWPS-1] ;

void LWP_START(void start_routine(void *), void *arg, caddr_t stack_base, 
	      size_t stack_size, lwp_private_t *lwp_private)
{
  int iID, error = 0 ;
  HANDLE main_thread ;

  main_thread = CreateThread(NULL, 0, (LPTHREAD_START_ROUTINE)start_routine,
			     (LPVOID)arg, 0, &iID) ;

  if (error) {
    fprintf(stderr, "LWP_START:  unable to create thread\n") ;
    fflush(stderr) ;
    exit(1) ;
  }

  thread_id_map[lwp_private->id] = iID ;
  thread_array[lwp_private->id - 1] = main_thread ;

#ifdef AT_DEBUG 
  at_printf("START_LWP arg: %x\n", (int)arg);
#endif 
}

void LWP_STOP(void)
{
  int i ;
  /*  for(i=0;i<MAX_LWPS-1;i++) pthread_cancel(thread_array[i]) ; */
}

extern void* at_malloc();
/* 
 * This function probably belongs in some other file, but we will keep
 * it here (lack of imagination)
 */

/* 
 * Allocate stacks in chunks, pnum is the physical processor number, 
 * starts with 0.
 *
 * Current implementation is very simple. Stacks are allocated in chunks
 * with the number of element in the chunk equal to that of the
 * current stack pool size. Requests from different processors are 
 * satisfied from per-processor structures.
 */




void *MALLOC_THREAD_STACK(int pnum){
  void *st;
  if (stk_index[pnum] == AT_THREAD_POOL_SIZE){
    stk_index[pnum] = 0;
    /* Allocate space for AT_STK_BLOCK_SIZE at one go */
    stk_org[pnum] = (char *) at_malloc(AT_STKSIZE*AT_THREAD_POOL_SIZE);
    /* Align things properly */
    stk_org[pnum] = AT_STKALIGN(stk_org[pnum],AT_STKBASE_ALIGNMENT);
  }
  st = (void *)stk_org[pnum];
  stk_org[pnum] += AT_STKSIZE;
  stk_index[pnum]++;
  return st;
}
