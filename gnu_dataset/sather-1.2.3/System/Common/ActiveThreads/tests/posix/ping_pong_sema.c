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

/* On DEC Alpha compile: cc -threads -lrt */
#include <time.h>
#include <semaphore.h>

#include <pthread.h>


int num_pongs = 1000;
int concurrency = 1;

sem_t *ping_sem, *pong_sem;
clock_t start, finish;

void* ping_pong(void *arg){
  int i;
  
  i = *(int *)arg;

  printf("ping_pong\n");
	 
  while(i--){
    sem_wait(ping_sem);
    sem_post(pong_sem);
  }
}

void* pong_ping(void* arg){
  int i;
  
  i = *(int *)arg;

  printf("pong_ping\n");  
  while(i--){
    sem_post(ping_sem);
    sem_wait(pong_sem);
  }
}

void main(int argc, char **argv){
  pthread_t tid;
  
  if(argc>1){
    concurrency = atoi(argv[1]);
  }
  if(argc>2){
    num_pongs = atoi(argv[2]);
  }

  ping_sem = (sem_t *)malloc(sizeof(sem_t));
  if(sem_init(ping_sem, 0, 0)){
    printf("sem_init failed...\n");
    exit(1);
  }
  
  pong_sem = (sem_t *)malloc(sizeof(sem_t));
  sem_init(pong_sem, 0, 0);
  
  /*  if(pthread_create(&tid, pthread_attr_default, ping_pong, &num_pongs)){
    printf("Thread creation failed\n");
  }*/

  start = clock();
  pong_ping(&num_pongs);
  finish = clock();
  printf("Time (us): %e\n", ((double)finish - (double)start)/num_pongs);    
  exit(0);
}







