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

#include <stdio.h>
#include <assert.h>
#include "at.h"

#define ARR_LEN 100
#define THRESHOLD (3)

at_mutex_t *lock_total;
int total = 0;
int *arr;

int counters[4];

void lp(int lower, int upper, at_sema_t *prev_sema, int level)
{
  int i, k;
  at_thread_t *t1;
  at_thread_t *t2;
  at_sema_t sema;

  at_sema_init(&sema, 0);

  if ((upper-lower)<THRESHOLD) {
    at_mutex_lock(lock_total);
    for (i=lower; i<=upper; i++) {
      total += arr[i];
    }
    at_mutex_unlock(lock_total);
  } else {
    k = (lower+upper)/2;
    t1 = at_create_4(at_get_focus(), AT_UNBOUND, lp, 
		     (at_word_t)lower, (at_word_t)k, (at_word_t)&sema, level+1);
    t2 = at_create_4(at_get_focus(), AT_UNBOUND, lp, 
		     (at_word_t)k+1, (at_word_t)upper,(at_word_t)&sema,level+1);
    at_sema_wait(&sema);
    at_sema_wait(&sema);
    /*at_destroy_local(t1);*/
    /*at_destroy_local(t2);*/
  }
  at_sema_signal(prev_sema);
  /*  at_printf("%d  ", level);*/

}

at_sema_t *sema;

int main(void)
{
  int i;
  at_thread_t *t;
  at_bundle_t *b;

  at_init(0, 0x4000, 0);
  sema = at_sema_create(0);
  lock_total = at_mutex_create();
  arr = (int *)at_malloc(sizeof(int)*ARR_LEN);
  for (i=0; i<ARR_LEN; i++) {
    arr[i] = i*2;
  }
  i = 0;
  while (1) {
    total = 0;
    t = at_create_4(at_get_focus(), AT_UNBOUND, lp, 
		    (at_word_t)0, (at_word_t)ARR_LEN-1, (at_word_t)sema, 0);
    at_sema_wait(sema); 

    i++;
    at_printf("\nTotal = %d   %d\n", total, i);
  }
  /*at_sema_destroy(sema);*/
}









