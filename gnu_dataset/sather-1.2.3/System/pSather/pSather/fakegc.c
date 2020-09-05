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

#ifdef AT_THREADS
#include "at.h"
#else
#include <memory.h>
#include <malloc.h>
#endif
#include <string.h>
char *GC_malloc(long size) { char *a=(char *)malloc(size);memset(a,0,size);return a; }
char *GC_malloc_atomic(long size) { char *a=(char *)malloc(size);memset(a,0,size);return a; }
void GC_free(char *p) { free(p); }
char *GC_calloc(long a,long b) { char *p=(char *)calloc(a,b);memset(p,0,a*b);return p; }
char *GC_realloc(char *a,long size) { return (char *)realloc(a,size); }
char *GC_malloc_atomic_ignore_off_page(long size) { char *a=(char *)calloc(size,1);memset((void *)a,0,size); return a; }
char *GC_malloc_ignore_off_page(long size) { char *a= (char *)calloc(size,1);memset((void *)a,0,size); return a; }
