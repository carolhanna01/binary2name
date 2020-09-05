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
#include <pSather.h>
#include "sort.h"


int comp_RECORD_name(const void *a,const void *b)
{
	NAME na,nb;
	RECORD ra,rb;
	ra=MAKENEAR(*(FOB *)a);
	rb=MAKENEAR(*(FOB *)b);
	na=MAKENEAR(ra->name);
	nb=MAKENEAR(rb->name);
	return strcmp(na->arr_part,nb->arr_part);
}

static int comp_RECORD_ttt(const void *a,const void *b)
{
	RECORD ra,rb;
	ra=MAKENEAR(*(FOB *)a);
	rb=MAKENEAR(*(FOB *)b);
	if(ra->ttt>rb->ttt) return 1;
	if(ra->ttt<rb->ttt) return -1;
	return 0;
}

void sort_local(FOB table)
{
	SORT tbl;
	tbl=MAKENEAR(table);
	qsort(tbl->arr_part,tbl->asize,sizeof(FOB),comp_RECORD_ttt);
}

static int comp_remote_RECORD_ttt(const void *a,const void *b)
{
	float ra,rb;
	F_RATTR_NA(ra,RECORD,*(FOB *)a,ttt);
	F_RATTR_NA(rb,RECORD,*(FOB *)a,ttt);
	if(ra>rb) return 1;
	if(ra<rb) return -1;
	return 0;
}

static void do_sort_remote(FOB table)
{
	SORT tbl;
	tbl=MAKENEAR(table);
	qsort(tbl->arr_part,tbl->asize,sizeof(FOB),comp_remote_RECORD_ttt);
}

typedef struct SORT_THREAD_struct {
	FOB table;
	int from;
	int to;
} *SORT_THREAD;

static void sort_remote_thread(FOB self,FOB arg,GATE gate,int pos)
{
	int to,from;
	FOB table;
	SORT ltable;
	int i;

	F_RATTR_NN(to,SORT_THREAD,arg,to);
	F_RATTR_NN(from,SORT_THREAD,arg,from);
	F_RATTR_NN(table,SORT_THREAD,arg,table);
	ltable=(SORT)malloc(sizeof(struct SORT_struct)+(to-from)*sizeof(FOB));
	for(i=0;i<to-from;i++) 
		F_RARR_NN(ltable->arr_part[i],SORT,table,i+from);

	do_sort_remote(MAKEFAR(ltable));

	for(i=0;i<to-from;i++) 
		F_WARR_NN(SORT,table,i+from,ltable->arr_part[i]);
}

void sort_remote(FOB table)
{
	int size;
	int i;
	GATE g;
	SORT_THREAD t;
	F_RATTR_NN(size,SORT,table,asize);
	g=GATE_CREATE;
	for(i=0;i<CLUSTERS;i++) {
		t=(SORT_THREAD)malloc(sizeof(struct SORT_THREAD_struct));
		t->table=table;
		t->from=(size/CLUSTERS)*i;
		t->to=(size/CLUSTERS)*(i+1)-1;
		ATTACH(sort_remote_thread,FNULL,MAKEFAR(t),g,i);
	}
	DECLARE_LOCK(1,1,0);
	ADD_BRANCH1(1,GATE_NO_THREADS(g));
	SELECT_LOCK
	BRANCH(1)
	LOCK_END
}
