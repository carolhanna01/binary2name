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

#ifndef _AT_INT_H_
#define _AT_INT_H_

/* The AT "internal" headers */
/* This (rather than at.h) should be included by AT source files */

#include <stdio.h>
#include <stdlib.h>
#include <sys/types.h>
#include <errno.h>

#include "qt.h"
#include "const.h"
#include "spinlock.h"
#include "at.h"
#include "at-lwp.h"
#include "mutex.h"
#include "cond.h"
#include "rw.h"
#include "pool.h"
#include "p_pool.h"
#include "sema.h"
#include "barrier.h"
#include "utils.h"
#include "thread.h"
#include "wrappers.h"
#include "lwp.h"
#include "at-inline.h"

#endif /* AT_INT_H_ */
