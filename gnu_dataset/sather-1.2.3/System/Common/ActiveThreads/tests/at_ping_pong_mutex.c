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
#include "at.h"

int num_pongs = 100000;
int concurrency = 1;

at_mutex_t *ping_mx, *pong_mx;
at_mutex_t *end_mx;

clock_t start, finish;
/*hrtime_t start, finish;*/


void ping_pong(at_word_t arg){
  int i;
  
  i = *(int *)arg;
  while(i--){
    at_mutex_lock(ping_mx);
    at_mutex_unlock(pong_mx);
  }
  at_mutex_unlock(end_mx);
}

void yield_thread()
{
  at_spinlock_t slck;
  AT_SPINLOCK_LOCK(slck);
  AT_SPINLOCK_LOCK(slck);
}
void pong_ping(at_word_t arg){
  int i;
  
  i = *(int *)arg;
  while(i--){
    at_mutex_unlock(ping_mx);
    at_mutex_lock(pong_mx);
  }
  at_mutex_unlock(end_mx);
}

extern at_pool_t *at_mutex_pool;

int main(int argc, char **argv){
  int i;
  at_bundle_t *bundle;
  if(argc>1){
    concurrency = atoi(argv[1]);
  }
  if(argc>2){
    num_pongs = atoi(argv[2]);
  }

  /*Initialize the thread package */
  at_init(concurrency, 0x2000, 0);

  ping_mx = at_mutex_create();
  pong_mx = at_mutex_create();

  at_mutex_lock(ping_mx);
  at_mutex_lock(pong_mx);

  end_mx = at_mutex_create();
  at_mutex_lock(end_mx);

  for(i=1;i<concurrency;i++){
    at_create_0(at_get_focus(), i, yield_thread);
  }

  start = clock();
  
  /*start = gethrtime();*/
  /* Well, things are essentially serial here, so 
     bind everything to the same virtual CPU */
  /*bundle = at_mcs_lazy_bundle_create(at_get_focus());
  at_set_focus(bundle);*/
  bundle = at_get_focus();
  at_create_1(bundle, 0, ping_pong, (at_word_t)&num_pongs);
  at_create_1(bundle, 0, pong_ping, (at_word_t)&num_pongs);

  /* Wait until done */
  at_mutex_lock(end_mx);
  finish = clock();
  /*finish=gethrtime();*/
  at_mutex_destroy(ping_mx);
  at_mutex_destroy(pong_mx);
  at_mutex_destroy(end_mx);
  printf("Mutex ping-pong time, us %e\n", (double)(finish-start)/(num_pongs));  
  exit(0);
}






