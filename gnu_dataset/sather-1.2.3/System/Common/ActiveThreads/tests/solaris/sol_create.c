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

#include <sys/time.h>
#include <sys/types.h>
#include <stdio.h>
#include <thread.h>
#include <tnf/probe.h>

int concurrency = 0;
int threads = 1000;

void* the_thread(void* arg);

void *stk_base[1000];
int stk_size = 0x1000;

hrtime_t start, finish;

void main(int argc, char **argv){
  int i;

  if(argc>1){
    concurrency = atoi(argv[1]);
  }
  if(argc>2){
    threads = atoi(argv[2]);
  }

  thr_setconcurrency(concurrency);

  for(i=0; i<threads; i++){
    stk_base[i] = (void *)malloc(stk_size);
  }
  /* worm up the cache */
  for(i=0; i<threads; i++){
    /*    thr_create(stk_base[i], stk_size, the_thread, NULL, 0, NULL);*/
        thr_create(NULL, 0, the_thread, NULL, 0, NULL);
  }
  for(i=0; i<threads; i++){
    thr_yield();
  }

  
  start=gethrtime();
  for(i=0; i<threads; i++){
    /*thr_create(stk_base[i], stk_size, the_thread, NULL, 0, NULL);*/
    thr_create(NULL, 0, the_thread, NULL, 0, NULL);
  }
  finish=gethrtime();
  printf("time in us %e\n", (double)(finish-start)/(1000.0));
  exit(0);
}

void* the_thread(void* arg){
  int i;

}
  











