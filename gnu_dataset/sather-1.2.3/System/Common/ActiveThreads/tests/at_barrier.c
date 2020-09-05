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

/* This is a pretty dumn test of a blocking barrier - just tests
   mutexes */

#include "at.h"

int num_barriers = 100;
int num_threads = 100;
int concurrency = 0;

volatile int threads_finished = 0;
int *thr_nums;

at_mutex_t *mx;
at_mutex_t *barrier_mx;
at_mutex_t *end_mx;
at_mutex_t **thr_mxs;

void Barrier(void *arg){
  int i;
  int thr_num;

  thr_num = *(int *)arg;

  at_mutex_lock(mx);
  threads_finished++;
  if (threads_finished == num_threads){
    threads_finished = 0;
    for(i=0; i<num_threads; i++){
      at_mutex_unlock(thr_mxs[i]);
    }
    at_mutex_unlock(barrier_mx);
  }
  at_mutex_unlock(mx);

  at_mutex_lock(thr_mxs[thr_num]);
}

void the_thread(at_word_t arg){
  /* Just do something for a while, than a barrier */
  int i;
  float a = 1.0, b=2.0;

  for(i=0; i<1000; i++){
    a *= b;
    a /= b;
  }
  a = a+b;
  b = b+a;
  Barrier((void *)arg);
}

int main(int argc, char **argv){
  int i, j;

  if(argc>1){
    concurrency = atoi(argv[1]);
  }
  if(argc>2){
    num_threads = atoi(argv[2]);
  }

  if(argc>3){
    num_barriers = atoi(argv[3]);
  }

  thr_nums = (int *)malloc(num_threads*sizeof(int));
  for(i=0; i< num_threads; i++){
    thr_nums[i] = i;
  }

  thr_mxs = (at_mutex_t **)malloc(num_threads*sizeof(at_mutex_t *));
  
  /*Initialize the thread package */
  at_init(concurrency, 0x2000, 0);

  for(i=0; i<num_threads; i++){
    thr_mxs[i] = at_mutex_create();
    at_mutex_lock(thr_mxs[i]);
  }

  mx = at_mutex_create();
  barrier_mx = at_mutex_create();

  at_mutex_lock(barrier_mx);
  
  for(i=0; i<num_barriers; i++){
    for(j=0; j<num_threads; j++){
      at_create_1(at_get_focus(), AT_UNBOUND, the_thread,
		  (at_word_t)&thr_nums[j]);
    }

    /* wait until all done */
    at_mutex_lock(barrier_mx);
  }
  

  /*at_printf("Mutex pool size: %d\n", at_pool_size(at_mutex_pool));*/
  at_printf("barrier test worked\n");
  exit(0);
}





