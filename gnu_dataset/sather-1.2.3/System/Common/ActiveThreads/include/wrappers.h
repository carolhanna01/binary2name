/*------------------------->  ANSI C - headerfile  <-------------------------*/
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

#ifndef _WRAPPERS_H_
#define _WRAPPERS_H_

#include "spinlock.h"

#define at_printf(format, args...)            \
  {                                           \
      AT_KERNEL_LOCK;                         \
      fprintf(stdout, format, ## args);       \
      fflush(stdout);                         \
      AT_KERNEL_UNLOCK;                       \
  }

#define at_fprintf(_fp_,format, args...)      \
  {                                           \
      AT_KERNEL_LOCK;                         \
      fprintf(_fp_, format, ## args);         \
      fflush(_fp_);                           \
      AT_KERNEL_UNLOCK;                       \
  }

extern at_spinlock_t kernel_lck;

extern int mem_usage;
extern int max_mem_usage;

void *at_malloc(long size);
void at_free(void *ptr);
void *at_calloc(long nelem, long elsize);
void *at_realloc(void* ptr, long size);


/* Remap all things that need serialization to their syncronized versions */
#define malloc(x)       at_malloc(x)
#define free(x)         at_free(x)
#define realloc(a,b)  at_realloc(a,b)
#define calloc(a,b)   at_calloc(a,b)
/*#define alloca(a)     at_alloc(a)*/

#endif /* _WRAPPERS_H_ */

