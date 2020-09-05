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

mutex_t *ping_mx, *pong_mx;

void* ping_pong(void* arg){
  int i;
  
  i = *(int *)arg;
  
  while(i--){
    mutex_lock(ping_mx);
    mutex_unlock(pong_mx);
  }
}

void* pong_ping(void* arg){
  int i;
  
  i = *(int *)arg;
  
  while(i--){
    mutex_unlock(ping_mx);
    mutex_lock(pong_mx);
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

  ping_mx = (mutex_t *)malloc(sizeof(mutex_t));
  mutex_init(ping_mx, USYNC_THREAD, NULL);
  pong_mx = (mutex_t *)malloc(sizeof(mutex_t));
  mutex_init(pong_mx, USYNC_THREAD, NULL);
  
  mutex_lock(ping_mx);
  mutex_lock(pong_mx);


  thr_create(NULL, 0, ping_pong, &num_pongs, 0, NULL);

  pong_ping(&num_pongs);
  
  exit(0);
}





