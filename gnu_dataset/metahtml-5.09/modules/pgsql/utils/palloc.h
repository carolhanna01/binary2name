/*-------------------------------------------------------------------------
 *
 * palloc.h--
 *	  POSTGRES memory allocator definitions.
 *
 *
 * Copyright (c) 1994, Regents of the University of California
 *
 * $Id: palloc.h,v 1.1.1.1 1998/11/07 21:16:25 bfox Exp $
 *
 *-------------------------------------------------------------------------
 */
#ifndef PALLOC_H
#define PALLOC_H

#include <c.h>

extern void *palloc(Size size);
extern void pfree(void *pointer);
extern void *repalloc(void *pointer, Size size);

/* like strdup except uses palloc */
extern char *pstrdup(char *pointer);

#endif							/* PALLOC_H */
