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
 * Local memory allocation 
 *
 * Version 1.0 (released for 1.0.9) by Claudio Fleiner (fleiner@icsi.berkeley.edu)
 * Version 1.1 (released for 1.2.1) by Abelardo Gutierrez (fleiner@icsi.berkeley.edu)
 */
#ifndef _MEMORY_H_
#define _MEMORY_H_

#ifdef AT_THREADS
#include "at.h"
#else
#include <malloc.h>
#endif

/* Try zeroing memory everywhere */

#ifndef PSATHER1

#if 0

#define MALLOC(r,s) \
do { \
	(void*)r=(void*)malloc((s)); \
	if(r==NULL || ((long)r&CLUSTER_MASK)) { \
		fprintf(stderr,"%s:%d: out of memory (malloc(%ld))\n", \
		               __FILE__,__LINE__,(long)(s)); \
		abort(); \
	} else { \
		memset((void *)r,0,s); \
	}  \
} while(0)

#else

#define MALLOC(r, s) \
do { \
	r = (void*)malloc((s)); \
	if (r == NULL || ((long)r & CLUSTER_MASK)) { \
		fprintf(stderr, "%s:%d: out of memory (malloc(%ld))\n", \
		                __FILE__, __LINE__, (long)(s)); \
		abort(); \
	} else { \
		memset((void *)r, 0, s); \
	}  \
} while(0)

#endif

#define FREE(s)   \
do {  \
	if(s==NULL) { \
		fprintf(stderr,"%s:%d: free(NULL)\n",__FILE__,__LINE__); \
		abort(); \
	} \
	free(s);  \
} while(0)

#define REALLOC(r,a,b) \
do { \
	(void*)r=(void*)realloc((a),(b)); \
	if(r==NULL || ((long)r&CLUSTER_MASK)) { \
		fprintf(stderr,"%s:%d: out of memory (realloc(%ld))\n", \
		               __FILE__,__LINE__,(long)(b)); \
		abort(); \
	} \
} while(0)

#if 0

#define CALLOC(r,a,b) \
do { \
	(void*)r=(void*)calloc((a),(b)); \
	if(r==NULL || ((long)r&CLUSTER_MASK)) { \
		fprintf(stderr,"%s:%d: out of memory (calloc(%ld,%ld))\n", \
		               __FILE__,__LINE__,(long)(a),(long)(b)); \
		abort(); \
/*	Should not be needed, since calloc (unlike malloc) \
	does this by itself \
	} else { \
		memset((void *)r,0,a*b);	*/ \
	} \
} while(0)

#else

#define CALLOC(r, a, b) \
do { \
	r = (void*)calloc((a), (b)); \
	if (r == NULL || ((long)r & CLUSTER_MASK)) { \
		fprintf(stderr, "%s:%d: out of memory (calloc(%ld, %ld))\n", \
		                __FILE__, __LINE__, (long)(a), (long)(b)); \
		abort(); \
/* Should not be needed, since calloc (unlike malloc) \
	does this by itself \
	} else { \
		memset(r, 0, a*b); */ \
	} \
} while(0)

#endif

#else

#if 0

#define MALLOC(r,s) \
do { \
	(void*)r=(void*)malloc((s)); \
	if(r==NULL) { \
		fprintf(stderr,"%s:%d: out of memory (malloc(%ld))\n", \
		               __FILE__,__LINE__,(long)(s)); \
		abort(); \
	} else { \
		memset((void *)r,0,s); \
	} \
} while(0)

#else

#define MALLOC(r, s) \
do { \
	r = (void*)malloc((s)); \
	if (r == NULL) { \
		fprintf(stderr, "%s:%d: out of memory (malloc(%ld))\n", \
		               __FILE__, __LINE__, (long)(s)); \
		abort(); \
	} else { \
		memset((void *)r, 0, s); \
	} \
} while(0)

#endif

#define FREE(s) \
do { \
	if(s==NULL) { \
		fprintf(stderr,"%s:%d: free(NULL)\n",__FILE__,__LINE__); \
		abort(); \
	} \
	free(s); \
} while(0)

#define REALLOC(r,a,b) \
do { \
	(void*)r=(void*)realloc((a),(b)); \
	if(r==NULL) { \
		fprintf(stderr,"%s:%d: out of memory (realloc(%ld))\n", \
		               __FILE__,__LINE__,(long)(b)); \
		abort(); \
	} \
} while(0)

#if 0

#define CALLOC(r,a,b) \
do { \
	(void*)r=(void*)calloc((a),(b)); \
	if(r==NULL) { \
		fprintf(stderr,"%s:%d: out of memory (calloc(%ld,%ld))\n", \
		               __FILE__,__LINE__,(long)(a),(long)(b)); \
		abort(); \
/*	Should not be needed, since calloc (unlike malloc) \
	does this by itself \
	} else { \
		memset((void *)r,0,a*b);	*/ \
	} \
} while(0)

#else

#define CALLOC(r, a, b) \
do { \
	r = (void*)calloc((a), (b)); \
	if (r == NULL) { \
		fprintf(stderr, "%s:%d: out of memory (calloc(%ld, %ld))\n", \
		                __FILE__, __LINE__, (long)(a), (long)(b)); \
		abort(); \
/* Should not be needed, since calloc (unlike malloc) \
	does this by itself \
	} else { \
		memset((void *)r, 0, a*b); */ \
	} \
} while(0)

#endif

#endif

#endif
