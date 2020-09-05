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

/* 
 * This file cannot include wrappers.h because it needs original
 * versions of C library and sys call functions
 */

#include <stdio.h>
#include <malloc.h>
#include <stdlib.h>
#include <dirent.h>
#include "spinlock.h"

int mem_usage=0;
int max_mem_usage=0;

AT_SPINLOCK_DEC(kernel_lck);  /* Serializes kernel calls */

/* For now, serialize all allocation */
void *at_malloc (long size)
{
  void *sto;

  AT_KERNEL_LOCK;
  sto = malloc (size);
  mem_usage+=size;
  if (mem_usage > max_mem_usage) max_mem_usage = mem_usage;
  AT_KERNEL_UNLOCK;
  if (!sto) {
    perror ("at_malloc");
    exit (1);
  }
  return (sto);
}

void *at_calloc(long nelem, long elsize){
  void *s;

  AT_KERNEL_LOCK;
  s = malloc(nelem*elsize);
  mem_usage+=nelem*elsize;
  if (mem_usage > max_mem_usage) max_mem_usage = mem_usage;  
  AT_KERNEL_UNLOCK;
  if (!s) {
    perror("at_calloc");
    exit (1);
  }
  return (s);
}


void *at_realloc(void *a, long size){
  void *s;

  AT_KERNEL_LOCK;
  s = realloc(a, size);
  mem_usage+=size;
  if (mem_usage > max_mem_usage) max_mem_usage = mem_usage;  
  AT_KERNEL_UNLOCK;
  if (!s) {
    perror("at_realloc");
    exit (1);
  }
  return (s);
}



void at_free(void* p){
  AT_KERNEL_LOCK;
  free(p);
  AT_KERNEL_UNLOCK;
}

extern void *alloca(size_t size);

void *at_alloca(long size){
  void *s;

  AT_KERNEL_LOCK;
  s = alloca(size);
  AT_KERNEL_UNLOCK;
  if (!s) {
    perror("at_alloca");
    exit (1);
  }
  return (s);
}




