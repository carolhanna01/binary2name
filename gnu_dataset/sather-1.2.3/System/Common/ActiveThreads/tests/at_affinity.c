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

/* Test thread affinity. The thread is created bound to vproc 0. It then
   migrates a few times around virtual processors in a round robin fashion */

#include <assert.h>
#include <stdlib.h>
#include "at.h"

int iterations = 10;
int num_threads = 1000;
int concurrency = 1;

at_sema_t *end_sema;

void the_thread(int arg){
  int i;
  int dest;

  for(i=0; i<iterations; i++){
    /* Jump to a random processor */
    dest = rand()%at_vprocs();
    at_set_affinity(dest);
    /* Now make sure we are at the right destination */
    if(at_vproc() !=dest){
      at_printf("at_vproc(): %d  dest: %d\n", at_vproc(), dest);
    }
  }

  /* Signal termination */
  at_sema_signal(end_sema);
}

int main(int argc, char **argv){
  int i;

  if(argc>1){
    concurrency = atoi(argv[1]);
  }

  if(argc>2){
    num_threads = atoi(argv[2]);
  }
  
  if(argc>3){
    iterations = atoi(argv[3]);
  }

  at_init(concurrency, 0x2000, 0);
  
  end_sema = at_sema_create(0);


  for(i=0; i< num_threads; i++){
    at_create_1(at_get_focus(), AT_UNBOUND, the_thread, NULL);
  }
  
  /* wait until all threads terminate */
  for(i=0; i<num_threads; i++){
    at_sema_wait(end_sema);
  }

  exit(0);
}





