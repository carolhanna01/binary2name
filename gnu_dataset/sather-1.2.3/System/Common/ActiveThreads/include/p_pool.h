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

#ifndef _P_POOL_H_
#define _P_POOL_H_

#include <stdio.h>
#include <stdlib.h>
#ifdef AT_DEBUG
#include <assert.h>
#endif

#include "spinlock.h"  /* Needed for locks */
#include "wrappers.h"
#include "pool.h"

typedef struct {
  at_pool_t **local_pools;    /* local pools - one per processor */
} at_p_pool_t;


/* 
 * Pool external interface, includes all inline functions 
 * All interface functions are properly synchronized 
 */

at_p_pool_t* at_p_pool_create(int size, int elt_size);
void at_p_pool_destroy(at_p_pool_t *p);
void *at_p_pool_get(at_p_pool_t *pool);
void at_p_pool_put(at_p_pool_t *p, void *elt);


#define AT_P_POOL_LOCK_INIT(p)  AT_SPINLOCK_INIT(p->slck)
#define AT_P_POOL_LOCK(p)       AT_SPINLOCK_LOCK(p->slck) 
#define AT_P_POOL_UNLOCK(p)     AT_SPINLOCK_UNLOCK(p->slck) 





#endif  /* _P_POOL_H_ */







