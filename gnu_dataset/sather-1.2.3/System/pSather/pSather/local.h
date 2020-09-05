/*------------------------->  ANSI C - headerfile  <-------------------------*/
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
 * Several defines used in the runtime
 *
 * Version 1.0 (released for 1.0.9) by Claudio Fleiner (fleiner@icsi.berkeley.edu)
 */
#ifndef _LOCAL_PSATHER_H_
#define _LOCAL_PSATHER_H_

struct request_lock {
	BR_lock_t lck;
	short res;
	long plate;
};

void standard_signal_handler();

FOB r_alloc(vnn_t where,int size);
void r_free(FOB p);

typedef void (*handler_exec_t)(vnn_t,void*,long);
void execp_mem(vnn_t where,handler_exec_t func,void *ptr,long size);

#endif
