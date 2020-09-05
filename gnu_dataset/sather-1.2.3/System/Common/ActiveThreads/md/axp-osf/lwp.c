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
#include <stdio.h>
#include "lwp.h"
#include "wrappers.h"
#include "const.h"


lwp_private_t *lwp_private;


int at_nprocs = 0;

static int procs(void){
  return 1;
}

static char *stk_org[AT_MAX_PROCS];
static int stk_index[AT_MAX_PROCS];

int LWP_INIT(int concurrency){
  int error;
  int i;

  /* Figure out the number of processors */
  at_nprocs = procs();
  if(!at_nprocs){
    /* wre are in trouble */
    return 1;
  }

  if(at_nprocs>1){
    printf("Can't deal with multiprocessors yet\n");
    exit(1);
  }

  if(concurrency <=0){
    concurrency = at_nprocs;
  }

  /*we are ok if we got here */
  /* Allocate initial stacks for threads */
  for(i=0; i<concurrency; i++){
    stk_index[i] = 0;
    /* Allocate space for AT_STK_BLOCK_SIZE at one go */
    stk_org[i] = (char *) at_malloc(AT_STKSIZE*AT_THREAD_POOL_SIZE);
    /* Align things properly */
    stk_org[i] = AT_STKALIGN(stk_org[i],AT_STKBASE_ALIGNMENT);
  }

  return 0;
}


void LWP_START(void start_routine(void *), void *arg, caddr_t stack_base, 
	      size_t stack_size, lwp_private_t *lwp_private )
{
  /* This should not be called for now */
  printf("Ouch, LWP_START called!\n");
  exit(1);
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












