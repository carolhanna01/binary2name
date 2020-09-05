/*------------------------->  ANSI C - sourcefile  <-------------------------*/
/* Copyright (C) 1995 by International Computer Science Institute            */
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
-- No, I won't tell you how you can use those classes
-- because you should not use them anyway. They break encapsulation,
-- and type safty, but you can make some wonderful things
-- you could not otherwise....
-- Version 1.0, by CMF (fleiner@icsi.berkeley.edu)
*/
#include <sather.h>
#ifndef PRINT_PO
#include "../Debug/print.c"
#endif

#ifdef PRINT_BACKTRACE
static struct _func_frame FF;
#endif

OB create_object(int tag,int array_size) {
	struct sather_type_description *st=sather_types[tag];
	char *p;
	struct sather_type_description *sa;
	int so;
	if(st->is_immutable) {
		return (OB)rt_alloc_atomic(st->size+st->boxed, tag);
	} else {
		if(!st->is_aref) {
			return (OB)rt_alloc(st->size, tag);
		} else {
			sa=sather_types[st->attr[st->attrs+1].type];
			p=(char *)rt_alloc(st->size+(array_size+1)*sa->size, tag);
			so=st->attr[st->attrs].offset;
			*(long *)(p+so)=array_size;
			return (OB)p;
		}
	}
}

int  tp_for_str(char* typename) {
  /* 
     Return the type tag that corresponds to the string "typename 
     This is the converse of SYS::str_for_tp, but cannot go into SYS 
     since the type is not known unless -reflect is used
     */
  int i;
  for(i=1;sather_types[i]!=NULL;i++)
    if(strcmp(sather_types[i]->sather_name,typename)==0) return i;
  for(i=-1;sather_types[i]!=NULL;i--)
    if(strcmp(sather_types[i]->sather_name,typename)==0) return i;
  return 0;
}

OB get_attr(OB object,int i) {
	int tg;
	int attr_tg,attr_offset;
	char *p=(char *)object;
	if(object==NULL) return NULL;
	tg=TAG(object);
	attr_tg=sather_types[tg]->attr[i].type;
	attr_offset=sather_types[tg]->attr[i].offset;
	if(sather_types[tg]->is_immutable) attr_offset+=sather_types[tg]->boxed;
	if(sather_types[attr_tg]->is_immutable) {
		char  *n;
		n=(char *)create_object(attr_tg,0);
		memcpy(n+sather_types[attr_tg]->boxed,p+attr_offset,sather_types[attr_tg]->size);
		return (OB)n;
	} else {
		return *(OB*)(p+attr_offset);
	}
}

char *get_attr_name(OB object,int i) {
	int tg;
	if(object==NULL) return NULL;
	tg=TAG(object);
	return sather_types[tg]->attr[i].sather_name;
}

int get_attrs(OB object) {
	int tg;
	if(object==NULL) return -1;
	tg=TAG(object);
	return sather_types[tg]->attrs;
}

void set_attr(OB object,int i,OB n) {
	int tg;
	int attr_tg,attr_offset;
	char *p=(char *)object;
	if(object==NULL) return;
	tg=TAG(object);
	attr_tg=sather_types[tg]->attr[i].type;
	attr_offset=sather_types[tg]->attr[i].offset;
	if(sather_types[tg]->is_immutable) attr_offset+=sather_types[tg]->boxed;
	if(sather_types[attr_tg]->is_immutable) {
		memcpy(p+attr_offset,(char *)n+sather_types[attr_tg]->boxed,sather_types[attr_tg]->size);
	} else {
		*(void **)(p+attr_offset)=(void *)n;
	}
}

int get_array_size(OB object) {
	int tg;
	int size_offset;
	char *p=(char *)object;
	if(object==NULL) return -1;
	tg=TAG(object);
	size_offset=sather_types[tg]->attr[sather_types[tg]->attrs].offset;
	if(sather_types[tg]->is_aref) {
		if(sather_types[tg]->is_immutable) {
			return size_offset;
		} else {
			return *(long *)(p+size_offset);
		}
	} else return -1;
}

OB get_array_element(OB object,int i) {
	int tg;
	int attr_tg,attr_offset;
	char *p=(char *)object;
	if(object==NULL) return NULL;
	tg=TAG(object);
	attr_tg=sather_types[tg]->attr[sather_types[tg]->attrs+1].type;
	attr_offset=sather_types[tg]->attr[sather_types[tg]->attrs+1].offset;
	if(sather_types[tg]->is_immutable) attr_offset+=sather_types[tg]->boxed;
	if(sather_types[attr_tg]->is_immutable) {
		char  *n;
		n=(char *)create_object(attr_tg,0);
		memcpy(n+sather_types[attr_tg]->boxed,p+attr_offset+i*sather_types[attr_tg]->size,sather_types[attr_tg]->size);
		return (OB)n;
	} else {
		return *(OB*)(p+attr_offset+i*sizeof(void*));
	}
}

OB set_array_element(OB object,int i,OB n) {
	int tg;
	int attr_tg,attr_offset;
	char *p=(char *)object;
	if(object==NULL) return NULL;
	tg=TAG(object);
	attr_tg=sather_types[tg]->attr[sather_types[tg]->attrs+1].type;
	attr_offset=sather_types[tg]->attr[sather_types[tg]->attrs+1].offset;
	if(sather_types[tg]->is_immutable) attr_offset+=sather_types[tg]->boxed;
	if(sather_types[attr_tg]->is_immutable) {
		memcpy(p+attr_offset+i*sather_types[attr_tg]->size,(char *)n+sather_types[attr_tg]->boxed,sather_types[attr_tg]->size);
	} else {
		*(void **)(p+attr_offset+i*sizeof(void *))=(void *)n;
	}
}

