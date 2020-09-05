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

char *v="aeiou";
char *c="bcdfghjklmnpqrstvwxyz";

char *rand_name()
{
	int s,i;
	static char p[12];
	s=5+rand()%5;
	for(i=0;i<s;i++) {
		if((i-1)%3==0) p[i]=v[rand()%strlen(v)];
		else p[i]=c[rand()%strlen(c)];
	}
	p[i]=0;
	return p;
}

FOB mktable(int size)
{
	FOB res;
	SORT tbl;
	int i;

	res=MAKEFAR(malloc(sizeof(struct SORT_struct)+size*sizeof(FOB)));
	tbl=MAKENEAR(res);
	tbl->asize=size;
	for(i=0;i<size;i++) {
		RECORD r;
		char *n;
		NAME na;
		tbl->arr_part[i]=MAKEFAR(malloc(sizeof(struct RECORD_struct)));
		r=MAKENEAR(tbl->arr_part[i]);
		r->id=i+1;
		r->ttt=(double)rand()/(double)rand();
		n=rand_name();
		na=(NAME)malloc(sizeof(struct NAME_struct)+strlen(n));
		strcpy(na->arr_part,n);
		na->asize=strlen(n);
		r->name=MAKEFAR(na);
	}
	return res;
}

void print_table(FOB table)
{
	long i;
	SORT tbl;

	tbl=MAKENEAR(table);

	for(i=0;i<tbl->asize;i++) {
		RECORD r;
		NAME n;
		r=MAKENEAR(tbl->arr_part[i]);
		n=MAKENEAR(r->name);
		printf("%4ld. %10.5f %s\n",r->id,r->ttt,n->arr_part);
	}
}


int main(int argc,char *argv[])
{
	FOB table;
	PSATHER_START(argv[0]);
		table=mktable(argc==1?25:atoi(argv[1]));
		print_table(table);
		sort_remote(table);
		print_table(table);
	PSATHER_STOP;
	return 0;
}

