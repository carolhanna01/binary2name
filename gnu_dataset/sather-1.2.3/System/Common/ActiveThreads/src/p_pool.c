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

#include "at-int.h"
#include "p_pool.h"


at_p_pool_t *at_p_pool_create(int size, int elt_size){
  at_p_pool_t *p;
  int i;

  p = (at_p_pool_t *)at_malloc(sizeof(at_p_pool_t));
  
  /* create local pools - one per processor */
  p->local_pools = (at_pool_t **)at_malloc(sizeof(at_pool_t *)*at_ncpus());
  for(i=0; i<at_ncpus(); i++){
    p->local_pools[i] = at_pool_create(size, elt_size);
  }
  return p;
}

void at_p_pool_destroy(at_p_pool_t *p){
  int i;
  
  for(i=0; i<at_ncpus(); i++){
    at_pool_destroy(p->local_pools[i]);
  }
  
  free(p);
}

/* 
 * If local pool is non-empty, return an element 
 * If it is empty, try to split the left or right neighbor if
 * one has more than twice of block size.
 * If no such local pools exist, allocate a new chunk of elements
 */
 
void *at_p_pool_get(at_p_pool_t *p){
  at_pool_elt_t *e;
  int size;
  at_pool_t *my_pool;
  int i, j;
  int max_size=0, max_proc, sz;
  
  my_pool = p->local_pools[at_cpu()];

  e = at_pool_get_no_alloc(my_pool);
  if(e==NULL){
    /* 
     * Find the proc with the maximum number of elements 
     * Note that here we do not lock all pools. Thus we
     * end up with a hint, rather than an exact number
     * It is perfectly ok for what we are trying to do 
     */
    if(at_ncpus()>1){
    j = at_cpu();
      for(i=1; i<at_ncpus(); i++){
	j = (j+1)%at_ncpus();
	sz = at_pool_size(p->local_pools[j]);
	if (sz > max_size){
	  max_size = sz;
	  max_proc = j;
	}
      }
      /* Try to split the proc pool with max number of elements */
      if(max_size>0){
	e = at_pool_split(p->local_pools[max_proc], &size);
	if(e){
	  at_pool_init(my_pool, e, size);
	}
      }
    }
    /* 
       Now try to get a element again. If one of the local pools
       has been split, no allocation will take place. Otherwise,
       a new chunk of elements is allocated by the local pool
       */
    e = at_pool_get(my_pool);
  }
  return e;
}


void at_p_pool_put(at_p_pool_t *p, void *e){
  at_pool_put(p->local_pools[at_cpu()], e);
}


