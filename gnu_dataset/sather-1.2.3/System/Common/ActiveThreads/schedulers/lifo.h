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

#ifndef _LIFO_H_
#define _LIFO_H_

#include "at-int.h"

void lifo_thread_created(at_bundle_t *b, at_thread_t *t);
void lifo_thread_started(at_bundle_t *b, at_thread_t *t);
void lifo_thread_terminated(at_bundle_t *b, at_thread_t *t);
void lifo_thread_blocked(at_bundle_t *b, at_thread_t *t);
void lifo_thread_unblocked(at_bundle_t *b, at_thread_t *t);
void lifo_processor_idle(at_bundle_t *b, int proc);
extern void bundle_created(at_bundle_t *parent, at_bundle_t *b);
extern void bundle_terminated(at_bundle_t *parent, at_bundle_t *b);
at_bundle_t *at_lifo_bundle_create(at_bundle_t *p);

#endif /* _LIFO_H_ */
