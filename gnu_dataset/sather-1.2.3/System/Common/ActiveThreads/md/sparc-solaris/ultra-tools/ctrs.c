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
 * Clear or report the values of the UltraSPARC performance instrumentation  *
 * counters 0 and 1 (PIC0 and PIC1) for all processors                       *
 * No attempts to handle counter overflow yet - be careful !!!               *
 *                                                                           *
 * written by:    Boris Weissman                                             *
 * on:            May 1, 1998                                                *
 *****************************************************************************/

#include <stdio.h>
#include <sys/types.h>
#include <unistd.h>
#include <fcntl.h>
#include <sys/mman.h>

#include <sys/processor.h>
#include <sys/procset.h>
#include <sys/lwp.h>
#include <ucontext.h>
#include <signal.h>

/* The PIC stuff */
extern int   sub_spf_pic01();
extern void  clr_spf_pic01();
extern int   get_spf_pic0();
extern int   get_spf_pic1();

#define ARG_ERROR -1
#define CLEAR 0
#define REPORT 1

#define STACK_SIZE 66536

void ctrs_clear(void *arg);
void ctrs_report(void *arg);
processorid_t  next_processor_id();

static lwp_sema_t sema;
static lwp_mutex_t mutex;  /* protects printf */
static processorid_t start_pid;

static int total_refs, total_hits, total_misses;



void main(int argc, char **argv){
  int i;
  int action=ARG_ERROR;  /* -1 error, 0 clear, 1 report */
  int ncpus;
  void (*func)(void *);
  ucontext_t *contextp;
  lwpid_t lwp;
  processorid_t pid, new_pid;
  char *stack_base;

  for(i=0; i<argc; i++){
    if(!strcmp(argv[i], "-clear")){
      action = CLEAR;
      func = ctrs_clear;
    }
    else 
      if (!strcmp(argv[i], "-report")){
	action = REPORT;
	func = ctrs_report;
      }
  }
  if(action == ARG_ERROR){
    printf("Use either -clear or -report\n");
    exit(1);
  }

  ncpus = sysconf( _SC_NPROCESSORS_ONLN);
  if (ncpus==-1){
    perror("sysconf error");
  }

  /* Bind the original lwp to a processor */
  start_pid = next_processor_id();
  if(processor_bind(P_LWPID, P_MYID, start_pid, NULL)){
    perror("processor_bind error (original lwp)");
  }

  /* Initialize the lwp semaphore */

  if(action == REPORT){
    printf("\n\n==========================  Stats for %d processors =======================\n", ncpus);
  }

  if(_lwp_sema_init(&sema, 0)){
    perror("_lwp_sema_init");
  }

  if(action == REPORT){
    func((void *)start_pid);
  }
  /* create lwps for each processor */
  /* and bind them accordingly */
  for(i=1; i<ncpus; i++) {
    stack_base = (char *)malloc(STACK_SIZE);
    contextp = (ucontext_t *)malloc(sizeof(ucontext_t));
    sigprocmask(SIG_SETMASK, NULL, &contextp->uc_sigmask);
    new_pid = next_processor_id();

    _lwp_makecontext(contextp, func, (void *)new_pid, NULL, 
		     stack_base, STACK_SIZE);
    if( _lwp_create(contextp, LWP_SUSPENDED, &lwp)){
      perror("_lwp_create error\n");
    }
    /* Bind new lwp to a processor */
    if(processor_bind(P_LWPID, lwp, new_pid, NULL)){
      perror("processor bind error (new lwp)");
    }

    /* Now, can actually start */
    if( _lwp_continue(lwp) ){
      perror("_lwp_continue error");
    }
  }

  if(action == CLEAR){
    func((void *)start_pid);
  }
    
  /* Wait until all lwps are done as well */
  for(i=0; i<ncpus; i++){
    _lwp_sema_wait(&sema);
  }

  if(action == REPORT){
    printf("==========================================================================\n");
    printf("total references: %15d\n", total_refs);
    printf("total hits:       %15d\n", total_hits);
    printf("total misses:     %15d\n", total_misses);  
  }
  exit(0);
}

void ctrs_clear(void *arg){
  int pid;
  
  pid = (int) arg;

  _lwp_mutex_lock(&mutex);
  /* printf("[%d] clear\n", pid);
  fflush(stdout);*/
  _lwp_mutex_unlock(&mutex);

  clr_spf_pic01();
  _lwp_sema_post(&sema);

}

void ctrs_report(void *arg){
  int pid;
  int refs, hits, misses;
  
  pid = (int) arg;

  refs = get_spf_pic0();
  hits = get_spf_pic1();

  misses = refs - hits;
  
  _lwp_mutex_lock(&mutex);  
  printf("CPU %3d   refs: %13d   hits: %13d  misses: %13d\n",
	 pid, refs, hits, misses);
  total_refs += refs;
  total_hits += hits;
  total_misses += misses;
  
  fflush(stdout);  
  _lwp_mutex_unlock(&mutex);

  _lwp_sema_post(&sema);

}

processorid_t  next_processor_id(){
  processor_info_t info;
  static  processorid_t proc_id=0;

  while(1){
    if (processor_info(proc_id, &info) == 0){
      if(info.pi_state==P_ONLINE){
	/* We got the proc */
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






