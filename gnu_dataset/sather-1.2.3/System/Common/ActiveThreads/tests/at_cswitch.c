/*------------------------->  ANSI C - sourcefile  <-------------------------*/
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

#include <sys/types.h>
#include <sys/time.h>

#include "at.h"

#define MAX_THREADS 1000

int concurrency = 1;
int threads = 2;
int yields = 500000;

volatile int threads_finished = 0;
at_spinlock_t lck;
/*hrtime_t start, finish;*/
clock_t start, finish;
at_mutex_t done;

void the_thread(at_word_t arg){
  int i;
  
  i = *(int *)arg;

  /*  at_printf("# threads: %d\n", at_thread_count());*/

  while(i){
    i--;
    /*TNF_PROBE_0(AT_YIELD, "AT AT_YIELD AT_YIELD_start", ""); */
    at_yield();
    /*    SYNC(TNF_PROBE_0(AT_YIELD_end, "AT AT_YIELD AT_YIELD_end", ""));*/
  }

  AT_SPINLOCK_LOCK(lck);
  threads_finished++;
  AT_SPINLOCK_UNLOCK(lck);
  at_mutex_unlock(&done);
}


int main(int argc, char **argv){
  int i;
  at_bundle_t *bundle;

  if(argc>1){
    concurrency = atoi(argv[1]);
  }
  if(argc>2){
    threads = atoi(argv[2]);
  }
  if(argc>3){
    yields = atoi(argv[3]);
  }

  AT_SPINLOCK_INIT(lck);


  /*Initialize the thread package */
  at_init(concurrency, 0x2000, 0);
  at_mutex_init(&done);
  at_mutex_lock(&done);

  bundle = at_get_focus();

  start = clock();
  for(i=0; i<threads; i++){
    at_create_1(bundle, AT_UNBOUND, the_thread, (at_word_t)&yields);
  }

  at_mutex_lock(&done);
  finish=clock();


  at_printf("Thread context switch time  %e us\n", 
	 (double)(finish-start)/1.0e+6); 

  at_join_all();
  exit(0);
}















