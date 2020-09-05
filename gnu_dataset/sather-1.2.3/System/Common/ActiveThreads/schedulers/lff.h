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

#ifndef _LFF_H_
#define _LFF_H_

#include "at-int.h"
#include "state.h"

#define AT_N      16384.0
#define AT_k      ((AT_N-1.0)/AT_N)
#define AT_logk   log(AT_k)


void lff_thread_created(at_bundle_t *b, at_thread_t *t);
void lff_thread_started(at_bundle_t *b, at_thread_t *t);
at_bundle_t *at_lff_bundle_create(at_bundle_t *p);
/* The rest of the handdlers are reused from "standrad" LFF scheduler */

extern void lff_thread_terminated(at_bundle_t *b, at_thread_t *t);
extern void lff_thread_blocked(at_bundle_t *b, at_thread_t *t);
extern void lff_thread_unblocked(at_bundle_t *b, at_thread_t *t);
extern void lff_processor_idle(at_bundle_t *b, int proc);
extern void bundle_created(at_bundle_t *parent, at_bundle_t *b);
extern void bundle_terminated(at_bundle_t *parent, at_bundle_t *b);
extern void lff_bundle_terminated(at_bundle_t *b, at_bundle_t *child);

void at_lff_share(at_thread_t *t1, at_thread_t *t2, float q);

void lff_update(at_bundle_t *b, at_thread_t *t);
void at_lff_flush(at_bundle_t *b);
void lff_flush_children(at_lff_bundle_state_t *bs);
void lff_stats(at_bundle_t *b);

#endif /* _LFF_H_ */
