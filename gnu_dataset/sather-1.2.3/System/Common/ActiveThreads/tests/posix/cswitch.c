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

#include <time.h>

#ifdef HPUX
   #include "hp_pthread.h"
#else
   #include <pthread.h>
#endif

clock_t start, finish;

int concurrency = 0;
int threads = 1000;
int yields = 10;
volatile int threads_finished = 0;

void* the_thread(void* arg);

void main(int argc, char **argv){
  int i;
  pthread_t tid;

  if(argc>1){
    concurrency = atoi(argv[1]);
  }
  if(argc>2){
    threads = atoi(argv[2]);
  }
  if(argc>3){
    yields = atoi(argv[3]);
  }

  
  for(i=0; i<threads-1; i++){
    pthread_create(&tid, NULL, the_thread, &yields);
  }

  start = clock();
  the_thread(&yields);
  finish = clock();
  
  printf("time in us %e\n", ((double)finish-(double)start));  
  exit(0);
}

void* the_thread(void* arg){
  int i;
  
  i = *((int *)arg);
  
  while(i){
    i--;
    /* sched_yield(); -- not supported under Solaris */
    thr_yield();
  }

  /*  AT_SPINLOCK_LOCK(lck);
  threads_finished++;
  AT_SPINLOCK_UNLOCK(lck); */
}














