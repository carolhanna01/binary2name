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

int concurrency = 1;
int iterations = 1000000;

at_sema_t *sema;
clock_t start, finish;

int main(int argc, char **argv){
  int i;
  if(argc>1){
    concurrency = atoi(argv[1]);
  }
  if(argc>2){
    iterations = atoi(argv[2]);
  }

  /*Initialize the thread package */
  at_init(concurrency, 0x2000, 0);

  sema = at_sema_create(1);
  
  start = clock();
  for(i=0; i<iterations; i++){
    at_sema_wait(sema);
    at_sema_signal(sema);
  }
  finish = clock();
  printf("Uncontested sema, us: %e\n", ((double)finish - (double)start)/iterations);
  exit(0);

}





