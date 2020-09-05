/*------------------------->  ANSI C - headerfile  <-------------------------*/
/* Copyright (C) 1994 by International Computer Science Institute            */
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

#ifndef _TIMING_H_
#define _TIMING_H_

#define DO_TIMING 1

#define MAX_TIME_TYPE 100

#define TM_IGNORE       (-1)
#define TM_IDLE         0
#define TM_THREADS	1

extern long time_elapsed[MAX_TIME_TYPE];

#ifdef __cplusplus
extern "C" {
#endif
short TM_timing_type(int new_type);
long TM_now();
void TM_print(void);
#ifdef __cplusplus
}
#endif

#endif
