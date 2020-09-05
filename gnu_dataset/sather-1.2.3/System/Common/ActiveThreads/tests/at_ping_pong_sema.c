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

at_sema_t *ping_sema, *pong_sema;
at_mutex_t *end_mx;
clock_t start, finish;

void ping_pong(at_word_t arg){
  int i;
  
  i = *(int *)arg;
  
  while(i--){
    at_sema_wait(ping_sema);
    at_sema_signal(pong_sema);
  }
}

void pong_ping(at_word_t arg){
  int i;
  
  i = *(int *)arg;
  
  while(i--){
    at_sema_signal(ping_sema);
    at_sema_wait(pong_sema);
  }
}

int main(int argc, char **argv){
  if(argc>1){
    concurrency = atoi(argv[1]);
  }
  if(argc>2){
    num_pongs = atoi(argv[2]);
  }

  /*Initialize the thread package */
  at_init(concurrency, 0x2000, 0);

  ping_sema = at_sema_create(0);
  pong_sema = at_sema_create(0);
  

  /* Things are serial anyway - no need to cause false sharing
     and other horrible cahche effect. Bind everything to vproc 0 */
  start=clock();
  at_create_1(at_get_focus(), AT_UNBOUND, ping_pong, (at_word_t) &num_pongs); 
  /*  at_create_1(at_get_focus(), AT_UNBOUND, pong_ping, (int) &num_pongs); */
  pong_ping((at_word_t)&num_pongs);

  finish=clock();
  printf("Semaphore ping-pong time, us %e\n", (double)(finish-start)/(num_pongs));
  
  at_sema_destroy(ping_sema);
  at_sema_destroy(pong_sema); 
  exit(0);
}





