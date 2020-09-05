/*------------------------->  ANSI C - sourcefile  <-------------------------*/
/* Copyright (C) 1995/96 by International Computer Science Institute         */
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
 * Memory allocation and deallocation
 *
 * Version 1.0 (released for 1.0.9) by Claudio Fleiner (fleiner@icsi.berkeley.edu)
 */
#include <stdio.h>
#include <malloc.h>
#include "pSather.h"
#include "memory.h"


static void reply_alloc(vnn_t from,void **p,void *mem,ta_sema_t ta)
{
	*p=mem;
	TA_SEMA_SIGNAL(ta);
}

static void request_alloc(vnn_t from,void **p,int size,BR_sema_t ta)
{
	void *n;
	MALLOC(n,size);
	BR_REPLY_3(from,(BR_handler_3_t)reply_alloc,(long)p,(long)n,(long)ta);
}

FOB r_alloc(vnn_t where,int size)
{
	void *p;
	TA_SEMAPHORE(ta);
	BR_REQUEST_3(where,(BR_handler_3_t)request_alloc,(long)&p,(long)size,(long)ta);
	TA_SEMA_WAIT(ta);
	return RECVFOB(p,where);
}

static void request_free(vnn_t from,void *p)
{
	FREE(p);
}

void r_free(FOB p)
{
	BR_REQUEST_1(WHERE(p),(BR_handler_1_t)request_free,(long)SENDFOBHOME(p));
}
