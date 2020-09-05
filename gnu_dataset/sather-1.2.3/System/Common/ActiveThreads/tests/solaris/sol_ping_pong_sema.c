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

#include <tnf/probe.h>
#include <thread.h>

int num_pongs = 1000;
int concurrency = 1;
hrtime_t start, finish;

sema_t *ping_sema, *pong_sema;

void* ping_pong(void* arg){
  int i;
  
  i = *(int *)arg;
  
  while(i--){
    sema_wait(ping_sema);
    sema_post(pong_sema);
  }
}

void* pong_ping(void* arg){
  int i;
  
  i = *(int *)arg;
  
  while(i--){
    sema_post(ping_sema);
    sema_wait(pong_sema);
  }
}

void main(int argc, char **argv){
  if(argc>1){
    concurrency = atoi(argv[1]);
  }
  if(argc>2){
    num_pongs = atoi(argv[2]);
  }

  /*Initialize the thread package */
  thr_setconcurrency(concurrency);

  ping_sema = (sema_t *)malloc(sizeof(sema_t));
  sema_init(ping_sema, 0, USYNC_THREAD, NULL);
  pong_sema = (sema_t *)malloc(sizeof(sema_t));
  sema_init(pong_sema, 0, USYNC_THREAD, NULL);
  
  thr_create(NULL, 0, ping_pong, &num_pongs, 0, NULL);
  
  start=gethrtime();
  pong_ping(&num_pongs);
  finish=gethrtime();
  printf("Time  %e us\n", 
	 (double)(finish-start)/(1000.0));
  exit(0);
}





