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

int concurrency = 0;
int iterations = 100000;

/*hrtime_t start, finish;*/
clock_t start, finish;

int my_condition = 0;
at_mutex_t *mx_go;

void the_thread(at_word_t iters, at_word_t c, at_word_t m){
  int i;
  at_cond_t *cond;
  at_mutex_t *mx;

  i = iters;
  cond = (at_cond_t *)c;
  mx = (at_mutex_t *)m;

  while(i){
    i--;

    at_mutex_lock(mx);
    my_condition = 1;
    at_cond_signal(cond);
    at_mutex_unlock(mx);

    /* wait on the go lock */
    at_mutex_lock(mx_go);
  }
}


int main(int argc, char **argv){
  int i;
  at_bundle_t *bundle;
  at_mutex_t *mx;
  at_cond_t *cond;

  if(argc>1){
    concurrency = atoi(argv[1]);
  }

  if(argc>2){
    iterations = atoi(argv[2]);
  }

  /*Initialize the thread package */
  at_init(concurrency, 0x2000, 0);

  mx = at_mutex_create();
  cond = at_cond_create();

  mx_go = at_mutex_create();
  at_mutex_lock(mx_go);

  /*bundle = at_mcs_bundle_create(at_get_focus());*/
  bundle = at_get_focus();

  at_set_focus(bundle);

  /* Create the signalling thread */
  at_create_3(bundle, AT_UNBOUND, the_thread, (at_word_t)iterations,
	    (at_word_t) cond, (at_word_t) mx);

  start = clock();

  /* wait on condition variable ... */
  for(i=0; i<iterations; i++){
    /*---------------------*/
    at_mutex_lock(mx);
    /*---------------------*/
    while(!my_condition){
      at_cond_wait(cond, mx);
    }

    /* body */
    /*at_printf("Iteration: %d\n", i);*/
    my_condition = 0;

    /*---------------------*/
    at_mutex_unlock(mx);
    /*---------------------*/
    /* Let the other thread proceed */
    at_mutex_unlock(mx_go);
  }

  finish=clock();
  at_printf("\ntime  %e us\n", 
	 (double)(finish-start)/1.0e+6); 
  exit(0);
}















