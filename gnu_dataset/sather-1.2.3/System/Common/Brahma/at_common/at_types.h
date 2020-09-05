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

#ifndef _AT_TYPES_H_
#define _AT_TYPES_H_

#include "at.h"

/* Basic types. */
#define word_t long
typedef long BR_word_t;
typedef unsigned long long BR_doubleword_t;
/* Integral cluster id type. */
typedef unsigned long BR_cluster_t;

/* ID of a thread */
typedef struct BR_thread_t_struct {
  unsigned int t;
} BR_thread_t; 


/* Synchronization types */
typedef at_mutex_t *BR_lock_t;
typedef at_sema_t  *BR_sema_t;
typedef at_rw_t    *BR_rw_t;
typedef at_spinlock_t BR_spinlock_t;



#endif
