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

#include "at.h"
#include <sys/time.h>
#include <sys/types.h>
#include <stdio.h>

/* Recursively create threads until hit the specified depth of the tree */

#define MAX_THREADS 1000

int thr_numbers[MAX_THREADS];

void the_thread(at_word_t cur_level, at_word_t depth, at_word_t child_number);
int concurrency = 0;
at_mutex_t *mutex;

int main(int argc, char **argv){
  int depth=12;

  if(argc>1){
    concurrency = atoi(argv[1]);
  }
  if(argc>2){
    depth = atoi(argv[2]);
  }

  /*Initialize the thread package */
  at_init(concurrency, 0x2000, 0);
  mutex = at_mutex_create();
  at_mutex_lock(mutex);

  /* Create root and go to sleep */
  at_create_3(at_get_focus(), AT_UNBOUND, the_thread, 0, depth,0);
  
  /* Sleep on the end mutex */
  at_mutex_lock(mutex);
  at_printf("at_rec_create worked\n");
  exit(0);
}

  
void the_thread(at_word_t cur_level, at_word_t depth, at_word_t child_number){
  if (cur_level==depth){
    at_mutex_unlock(mutex);
  }
  else {
    /* Create left child */
    at_create_3(at_get_focus(), AT_UNBOUND, the_thread, cur_level+1, depth,0);
    /* Create right child */
    at_create_3(at_get_focus(), AT_UNBOUND, the_thread, cur_level+1, depth,1);
  }
}
  










