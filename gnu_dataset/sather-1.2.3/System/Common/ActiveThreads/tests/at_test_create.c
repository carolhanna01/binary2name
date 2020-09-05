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

int concurrency = 0;
at_sema_t *done_sema;

void thread0(void){
  at_printf("thread 0\n");
  at_sema_signal(done_sema);
}

void thread1(at_word_t a1){
  at_printf("thread 1: %d\n", a1);
  at_sema_signal(done_sema);
}

void thread2(at_word_t a1, at_word_t a2){
  at_printf("thread 2: %d  %d\n", a1, a2);
  at_sema_signal(done_sema);
}

void thread3(at_word_t a1, at_word_t a2, at_word_t a3){
  at_printf("thread 3: %d  %d  %d\n", a1, a2, a3);
  at_sema_signal(done_sema);
}

void thread4(at_word_t a1, at_word_t a2, at_word_t a3, at_word_t a4){
  at_printf("thread 4: %d  %d  %d  %d\n", a1, a2, a3, a4);
  at_sema_signal(done_sema);
}


void thread5(at_word_t a1, at_word_t a2, at_word_t a3, at_word_t a4, at_word_t a5){
  at_printf("thread 5: %d  %d  %d  %d  %d\n", a1, a2, a3, a4, a5);
  at_sema_signal(done_sema);
}

void thread6(at_word_t a1, at_word_t a2, at_word_t a3, at_word_t a4, at_word_t a5, at_word_t a6){
  at_printf("thread 6: %d  %d  %d  %d  %d  %d\n", a1, a2, a3, a4, a5, a6);
  at_sema_signal(done_sema);
}


int main(int argc, char **argv){
  if(argc>1){
    concurrency = atoi(argv[1]);
  }
  
  at_init(concurrency, 0x2000, 0);

  done_sema = at_sema_create(-6);
  
  at_create_0(at_get_focus(), AT_UNBOUND, thread0);
  at_create_1(at_get_focus(), AT_UNBOUND, thread1, 1);
  at_create_2(at_get_focus(), AT_UNBOUND, thread2, 1, 2);
  at_create_3(at_get_focus(), AT_UNBOUND, thread3, 1, 2, 3);
  at_create_4(at_get_focus(), AT_UNBOUND, thread4, 1, 2, 3, 4);
  at_create_5(at_get_focus(), AT_UNBOUND, thread5, 1, 2, 3, 4, 5);
  at_create_6(at_get_focus(), AT_UNBOUND, thread6, 1, 2, 3, 4, 5, 6);

  /* Wait until all done */
  at_sema_wait(done_sema);
  at_sema_destroy(done_sema);
  at_printf("Test create finished\n");
  exit(0);
}
    


  











