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
#include <stdlib.h>
#include "at.h"
#include "myrinet_solaris.h"

/* Gets set by fork request handlers */
int BR_fork_handler=0;

void BR_FORK_4_handler(BR_cluster_t src, BR_handler_t func,
	 BR_word_t arg0, BR_word_t arg1, BR_word_t arg2, BR_word_t arg3) {
  BR_fork_handler=1;
  at_create_5(at_get_focus(), AT_URGENT, (at_userf_5_t *)func, src,arg0,arg1,arg2,arg3);
}
void BR_FORK_4(BR_cluster_t dest, BR_handler_t func,
	 BR_word_t arg0, BR_word_t arg1, BR_word_t arg2, BR_word_t arg3) {
  if (dest != lam_myproc) {
    /* Remote fork */
    BR_REQUEST_5(dest, (BR_handler_5_t) BR_FORK_4_handler, (BR_word_t)func, 
		 arg0, arg1, arg2, arg3);
  }
  else {
    /* Local fork */
    at_create_5(at_get_focus(), AT_URGENT, (at_userf_5_t *)func, BR_HERE(),arg0,arg1,arg2,arg3);
  }
}

void BR_FORK_3_handler(BR_cluster_t src, BR_handler_t func,
	 BR_word_t arg0, BR_word_t arg1, BR_word_t arg2) {
  BR_fork_handler=1;
  at_create_4(at_get_focus(), AT_URGENT, (at_userf_4_t *)func, src,arg0,arg1,arg2);
}
void BR_FORK_3(BR_cluster_t dest, BR_handler_t func,
	 BR_word_t arg0, BR_word_t arg1, BR_word_t arg2) {
  if (dest != lam_myproc) {
    /* Remote fork */
    BR_REQUEST_4(dest, (BR_handler_4_t) BR_FORK_3_handler, (BR_word_t)func, 
		 arg0, arg1, arg2);
  }
  else {
    /* Local fork */
    at_create_4(at_get_focus(), AT_URGENT, (at_userf_4_t *)func, BR_HERE(),arg0,arg1,arg2);
  }
}

void BR_FORK_2_handler(BR_cluster_t src, BR_handler_t func,
	 BR_word_t arg0, BR_word_t arg1) {
  BR_fork_handler=1;
  at_create_3(at_get_focus(), AT_URGENT, (at_userf_3_t *)func, src,arg0,arg1);
}
void BR_FORK_2(BR_cluster_t dest, BR_handler_t func,
	 BR_word_t arg0, BR_word_t arg1) {
  if (dest != lam_myproc) {
    /* Remote fork */
    BR_REQUEST_3(dest, (BR_handler_3_t) BR_FORK_2_handler, (BR_word_t)func, 
		 arg0, arg1);
  }
  else {
    /* Local fork */
    at_create_3(at_get_focus(), AT_URGENT, (at_userf_3_t *)func, BR_HERE(),arg0,arg1);
  }
}

void BR_FORK_1_handler(BR_cluster_t src, BR_handler_t func, BR_word_t arg0) {
  BR_fork_handler=1;
  at_create_2(at_get_focus(), AT_URGENT, (at_userf_2_t *)func, src,arg0);
}
void BR_FORK_1(BR_cluster_t dest, BR_handler_t func, BR_word_t arg0) {
  if (dest != lam_myproc) {
    /* Remote fork */
    BR_REQUEST_2(dest, (BR_handler_2_t) BR_FORK_1_handler, (BR_word_t)func, 
		 arg0);
  }
  else {
    /* Local fork */
    at_create_2(at_get_focus(), AT_URGENT, (at_userf_2_t *)func, BR_HERE(),arg0);
  }
}

void BR_FORK_0_handler(BR_cluster_t src, BR_handler_t func) {
  BR_fork_handler=1;
  at_create_1(at_get_focus(), AT_URGENT, (at_userf_1_t *)func, src);
}
void BR_FORK_0(BR_cluster_t dest, BR_handler_t func) {
  if (dest != lam_myproc) {
    /* Remote fork */
    BR_REQUEST_1(dest, (BR_handler_1_t) BR_FORK_0_handler, (BR_word_t)func);
  }
  else {
    /* Local fork */
    at_create_1(at_get_focus(), AT_URGENT, (at_userf_1_t *)func, BR_HERE());
  }
}



