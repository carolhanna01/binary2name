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

#ifndef _SORT_H_
#define _SORT_H_

typedef struct RECORD_struct {
	long id;
	float ttt;
	FOB name;
} *RECORD;

typedef struct NAME_struct {
	int asize;
	char arr_part[1];
} *NAME;

typedef struct SORT_struct {
	int asize;
	FOB arr_part[1];
} *SORT;

void sort_local(FOB table);
void print_table(FOB table);

#endif
