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

#include <pthread.h>
#include <unistd.h>

#define MAX_LWPS NUMBER_OF_PROCESSORS

static pid_t pid_map[MAX_LWPS] ;
static lwp_private_t *lwp_map[MAX_LWPS] ;

static pthread_mutex_t lwp_mutex = PTHREAD_MUTEX_INITIALIZER ;

static pid_t prev_pid = 99999 ;

static inline pid_t atomic_getpid()
{
  pid_t id ;
  pthread_mutex_lock(&lwp_mutex) ;
  id = getpid() ;
  pthread_mutex_unlock(&lwp_mutex) ;
  return id ;
}

#define GET_PID atomic_getpid()

void set_pid(lwp_private_t *lwp_private)
{
  pid_map[lwp_private->id] = GET_PID ;
}

lwp_private_t *_lwp_getprivate()
{
  pid_t pid = GET_PID ;
  int i = 0 ;
  while(i<MAX_LWPS) {
    if (pid == pid_map[i]) return lwp_map[i] ;
    i++ ;
  }
  fprintf(stderr,"Lost a pid [%d]\n", (int)pid) ;
  fflush(stderr) ;
  return NULL ;
}

void _lwp_setprivate(lwp_private_t *lwp_private)
{
  lwp_map[lwp_private->id] = lwp_private ;
}

int at_nprocs = 0 ;

static int procs(void){
  return NUMBER_OF_PROCESSORS ;
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

  pid_map[0] = GET_PID ;
  
  return 0;
}

static pthread_t pthread_array[MAX_LWPS-1] ;

void LWP_START(void *start_routine(void *), void *arg, caddr_t stack_base, 
	      size_t stack_size, lwp_private_t *lwp_private)
{
  int error ;
  pthread_t main_thread ;
  pthread_attr_t attr ;

  pthread_attr_init(&attr);
  pthread_attr_setinheritsched(&attr, PTHREAD_INHERIT_SCHED) ;
  error = pthread_create(&main_thread, &attr, start_routine, arg) ;

  if (error) {
    fprintf(stderr, "LWP_START:  unable to create thread\n") ;
    fflush(stderr) ;
    exit(1) ;
  }

  pthread_detach(main_thread) ;
  pthread_array[lwp_private->id - 1] = main_thread ;

#ifdef AT_DEBUG 
  at_printf("START_LWP arg: %x\n", (int)arg);
#endif 
}

void LWP_STOP(void)
{
  int i ;
  for(i=0;i<MAX_LWPS-1;i++) pthread_cancel(pthread_array[i]) ;
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
