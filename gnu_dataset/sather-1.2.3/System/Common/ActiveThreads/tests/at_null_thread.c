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

#include "at.h"
/*#include <time.h>
Commented out for HPPA */
#include <sys/time.h>
/*#include <sys/types.h> */
#include <stdio.h>

#define MAX_THREADS 1000

int thr_numbers[MAX_THREADS];

int concurrency = 1;
int threads = 1000;
AT_SPINLOCK_DEC(slck);
int counter;
at_mutex_t *done;
at_bundle_t *bundle;

void the_thread(at_word_t arg1);

/*hrtime_t start, finish;*/
clock_t start, finish;

void the_thread(at_word_t threads){
  int i;

  for(i=0; i<threads; i++){
    at_create_1(bundle, 0, the_thread, i); 
    at_yield();
  }
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

  /*Initialize the thread package */
  at_init(concurrency, 0x1000, 0);
  done = at_mutex_create();
  at_mutex_lock(done);

  bundle = at_mcs_bundle_create(at_get_focus());
  at_printf("Initialization finished \n");
  
  /* Worm up cache */
  at_create_1(bundle, 0, the_thread, threads);

  /*  start=gethrtime();*/
  start = clock();
  at_create_1(bundle, 0, the_thread, threads);  
  /*finish=gethrtime();*/
  finish = clock();
  printf("Null thread, us %e\n", (double)(finish-start)/(threads*1000));


  exit(0);
}

  
  










