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
#include <sys/types.h>
#include <unistd.h>
#include <fcntl.h>
#include <sys/mman.h>

#include <sys/processor.h>
#include <sys/procset.h>

#include "lwp.h"
#include "wrappers.h"
#include "const.h"

#ifdef AT_PROC_MEM
   /* Use a solaris call to obtain lwp private storage */
#else
   #define MAX_LWPS 32
   lwp_private_t *lwp_private_map[MAX_LWPS];
#endif

int at_nprocs = 0;

static int procs(void){
  int ncpu = 0;

  ncpu = sysconf( _SC_NPROCESSORS_ONLN);
  if (ncpu==-1){
    perror("sysconf");
  }
  printf("AT: Number of SPARC CPUs: %d\n", ncpu);
  return ncpu;
}



/* Do machine dependent initialization here. return 0 if successflul */

static int lwp_num=1;  /* current number of lwp (initial number 0 existed)*/
static processorid_t proc_id=0; /* proc number - will be used in init. */

processorid_t  next_processor_id(){
  processor_info_t info;

  while(1){
    if (processor_info(proc_id, &info) == 0){
      if(info.pi_state==P_ONLINE){
	/* We got the proc */
	printf("PROCESSOR %d FOUND\n", proc_id);
	break;
      }
      else{
	printf("Warning: processor %d is offline. Skipping...\n", proc_id);
      }
    }
    proc_id++; /* try again */
    if(proc_id==200){
      perror("Not enough online processors");
      exit(1);
    }
  }
  proc_id++;
  return (proc_id-1);
}

static char *stk_org[AT_MAX_PROCS];
static int stk_index[AT_MAX_PROCS];

int LWP_INIT(int concurrency){
  int error;
  int i;
  processorid_t pid;

  /* Figure out the number of processors */
  at_nprocs = procs();
  if(!at_nprocs){
    /* wre are in trouble */
    return 1;
  }

  if(concurrency <=0){
    concurrency = at_nprocs;
  }

  /* Bind the initial lwp to a processor */
  pid = next_processor_id();
  error = processor_bind(P_LWPID, P_MYID, pid, NULL);
  if(error){
    fprintf(stderr, "processor_bind failed for proc %d\n", pid);
    return 1;
  }
  
  /*we are ok if we got here */
  /* Allocate initial stacks for threads */
  /*  for(i=0; i<concurrency; i++){
    stk_index[i] = 0;
    stk_org[i] = (char *) at_malloc(AT_STKSIZE*AT_THREAD_POOL_SIZE);
    stk_org[i] = AT_STKALIGN(stk_org[i],AT_STKBASE_ALIGNMENT);
  }*/

  return 0;
}


void LWP_START(void *start_routine(void *), void *arg, caddr_t stack_base, 
	      size_t stack_size, lwp_private_t *lwp_private )
{
  int error;
  ucontext_t *contextp;
  lwpid_t new_lwp;
  processorid_t pid;
  
  contextp = (ucontext_t *)malloc(sizeof(ucontext_t));
  sigprocmask(SIG_SETMASK, NULL, &contextp->uc_sigmask);

  _lwp_makecontext(contextp, start_routine, arg, (void *)lwp_private, 
		   stack_base, stack_size);
  error = _lwp_create(contextp, 0L, &new_lwp);
  if(error){
    printf("_lwp_create failed\n");
    exit(1);
  }

  /* Bind new lwp to a processor */
  /* 
   * The only trick here is that processors do not necessarily have
   * consecutive numbers. Need first to get the number of the
   * next available processor 
   */
  pid = next_processor_id();
  error = processor_bind(P_LWPID, new_lwp, pid, NULL);
  if(error){
    printf("processor_bind failed for proc %d\n", pid);
    exit(1);
  }

  lwp_num++;

#ifdef AT_DEBUG 
  at_printf("START_LWP arg: %x\n", (int)arg);
#endif 
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
  /* 
  ** Reserve the virtual address range for the thread stack without 
  ** reserving the physical space. This will never happen if the thread is 
  ** not used. The actual mapping is create on first reference. 
  */

  /*
  ** Open descriptor to use to establish the dummy mapping. All we really
  ** do is reserving the address range... 
  */

  /*  
      int zero_filedes; 
      zero_filedes = open("/dev/zero", O_RDWR);
      if (zero_filedes == -1) { 
      perror("Unable to open /dev/zero for virtual map1");
      exit(1);
      }
      
      s = (void *) mmap(NULL, 
      stack_size,
      PROT_READ | PROT_WRITE,
      MAP_PRIVATE | MAP_NORESERVE,
      zero_filedes,
      (off_t) 0);
      if (s == MAP_FAILED) {
      fprintf(stderr, "mmap of /dev/zero for thread stack failed\n");
      exit(1);
      }
      
      close(zero_filedes);
      return s;
      */

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












