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

#ifndef _STATE_H_
#define _STATE_H_

#include "bundle.h"
#include "thread_queue.h"
#include "bundle_queue.h"
#include "const.h"
#include "heap.h"
/* 
 * A simple bundle state datastructure. Bundles can use this
 * or define anything they need and attach it to bundle_state
 */

typedef struct at_bundle_state {
  at_bundle_t *parent;
  at_thread_queue_t threads;  /* child threads */
  at_bundle_queue_t bundles;  /* child bundles */
  int thread_counter;
  int bundle_counter;
  float *misses;    
} at_bundle_state_t;


typedef struct at_mig_bundle_state {
  at_bundle_t *parent;
  at_thread_queue_t local_threads;  /* cannot migrate yet */
  at_thread_queue_t global_threads;  /* can migrate */
  at_bundle_queue_t bundles;  /* child bundles */
  int thread_counter;
  int bundle_counter;
} at_mig_bundle_state_t;


#define LFF_MAXPRIORITY 100
/* Max # of children before a call to flush */
#define MAX_CHILDREN 5000
typedef struct at_lff_bundle_state {
  at_bundle_t *parent;

  /* One of each of these per cpu */
  at_heap_t **heaps;
  at_thread_dqueue_t global_threads; 
  float *misses;               /* Total # of misses since creation */

  at_bundle_queue_t bundles;  /* child bundles */
  int thread_counter;
  int bundle_counter;
  float *logs;                /* precomputed logarithms (for priorities)*/
  float *pows;                /* Precomputed (int) powers of k */
  at_thread_t *children[AT_MAX_PROCS][MAX_CHILDREN];
  int          num_children[AT_MAX_PROCS];
} at_lff_bundle_state_t;

#endif /* _STATE_H_ */
