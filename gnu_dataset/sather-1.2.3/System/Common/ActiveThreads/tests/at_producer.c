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

/* Producer/Consumer problem solved with semaphores */

#include <stdio.h>
#include "at.h"

#define BSIZE 10

typedef struct {
  char buf[BSIZE];
  at_sema_t *occupied;
  at_sema_t *empty;
  int  nextin;
  int  nextout;
  at_sema_t *pmut;
  at_sema_t *cmut;
} buffer_t;

buffer_t *b;


void producer(char item){
  at_sema_wait(b->empty);
  
  at_sema_wait(b->pmut);
  
  b->buf[b->nextin] = item;
  b->nextin++;
  b->nextin %= BSIZE;

  at_sema_signal(b->pmut);
  
  at_sema_signal(b->occupied);
}


char consumer(){
  char item;

  at_sema_wait(b->occupied);

  at_sema_wait(b->cmut);

  item = b->buf[b->nextout];
  b->nextout++;
  b->nextout %= BSIZE;
  
  at_sema_signal(b->cmut);
  
  at_sema_signal(b->empty);
  
  return item;
}

void producer_driver(){
  int item;

  while(1) {
    /* item=random()%256; */
    item = getchar();
    producer((char)item);
  }
}

void consumer_driver(){
  char item;

  while(1){
    item = consumer();
    putchar(item); 
  }
}

int concurrency;
int num_producers;
int num_consumers;
int threads_finished;

at_mutex_t *end_mx;

int main(int argc, char **argv)
{
  int i;

  if(argc>1){
    concurrency = atoi(argv[1]);
  }

  if(argc>2){
    num_producers = atoi(argv[2]);
  }

  if(argc>3){
    num_consumers = atoi(argv[3]);
  }

  at_init(concurrency, 0x2000, 0);

  b = (buffer_t *)malloc(sizeof(buffer_t));

  b->occupied = at_sema_create(0);
  b->empty = at_sema_create(BSIZE);
  b->pmut = at_sema_create(1);
  b->cmut = at_sema_create(1);

  b->nextin = b->nextout = 0;

  end_mx = at_mutex_create();
  at_mutex_lock(end_mx);
  
  for(i = 0; i< num_producers; i++){
    at_create_0(at_get_focus(), AT_UNBOUND, producer_driver);
  }

  for(i = 0; i< num_consumers; i++){
    at_create_0(at_get_focus(), AT_UNBOUND, consumer_driver);
  }
  
  /* Just go to sleep */
  at_mutex_lock(end_mx);
}

