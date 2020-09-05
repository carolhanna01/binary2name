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
 * Defines an atomic memcpy, used in cache.c
 *
 * Version 1.0 (released for 1.0.9) by Claudio Fleiner (fleiner@icsi.berkeley.edu)
 */
#ifndef _ATOMIC_H_
#define _ATOMIC_H_

#include "pSather.h"

/* ATOMIC_MEMCPY here defines a memcpy for
 * the sizes 1,2,4,8. This does not 
 * necessarly mean that a memcpy of those
 * sizes is indeed atomic. This is 
 * only the case if an assignment of a type
 * of this size is atomic too
 */

#if defined(MEMCPY_IS_ATOMIC) || defined(ATOMIC_ALL)
#define ATOMIC_MEMCPY(a,b,c) memcpy((a),(b),(c))
#else

#define IS_ALIGNED(all,p)	(!(((long)(p))%(all)))
#define AMC1(x,y)							\
	*(ATOMIC_1_TYPE*)(x)= *(ATOMIC_1_TYPE*)(y);
#define AMC2(x,y)							\
	if(IS_ALIGNED(ALIGNMENT_2,x) && IS_ALIGNED(ALIGNMENT_2,y)) 	\
		*(ATOMIC_2_TYPE*)(x)= *(ATOMIC_2_TYPE*)(y);		\
	else {								\
		AMC1(x,y);						\
		AMC1(((char *)x)+1,((char *)y)+1);			\
	} 
#define AMC4(x,y)							\
	if(IS_ALIGNED(ALIGNMENT_4,x) && IS_ALIGNED(ALIGNMENT_4,y)) 	\
		*(ATOMIC_4_TYPE*)(x)= *(ATOMIC_4_TYPE*)(y);		\
	else {								\
		AMC2(x,y);						\
		AMC2(((char *)x)+2,((char *)y)+2);			\
	} 
#define AMC8(x,y)							\
	if(IS_ALIGNED(ALIGNMENT_8,x) && IS_ALIGNED(ALIGNMENT_8,y)) 	\
		*(ATOMIC_8_TYPE*)(x)= *(ATOMIC_8_TYPE*)(y);		\
	else {								\
		AMC4(x,y);						\
		AMC4(((char *)x)+4,((char *)y)+4);			\
	} 
#define AMC16(x,y)							\
	if(IS_ALIGNED(ALIGNMENT_16,x) && IS_ALIGNED(ALIGNMENT_16,y)) 	\
		*(ATOMIC_16_TYPE*)(x)= *(ATOMIC_16_TYPE*)(y);		\
	else {								\
		AMC8(x,y);						\
		AMC8(((char *)x)+8,((char *)y)+8);			\
	} 

#if LARGEST_ATOMIC==1
# define ATOMIC_MEMCPY(x,y,size) do { switch(size) { \
			case 1: AMC1(x,y); break;			\
			default: fprintf(stderr,"impossible to make an atomic assignment of size %d\n",(int)size);p_rfatalv("RUNTIME ERROR"); \
			} } while(0)
#else
# if LARGEST_ATOMIC==2
#  define ATOMIC_MEMCPY(x,y,size) do { switch(size) { \
			case 1: AMC1(x,y); break;			\
			case 2: AMC2(x,y); break;			\
			default: fprintf(stderr,"impossible to make an atomic assignment of size %d\n",(int)size);p_rfatalv("RUNTIME ERROR"); \
			} } while(0)
# else
#  if LARGEST_ATOMIC==4
#   define ATOMIC_MEMCPY(x,y,size) do { switch(size) { \
			case 1: AMC1(x,y); break;			\
			case 2: AMC2(x,y); break;			\
			case 4: AMC4(x,y); break;			\
			default: fprintf(stderr,"impossible to make an atomic assignment of size %d\n",(int)size);p_rfatalv("RUNTIME ERROR"); \
			} } while(0)
#  else
#   if LARGEST_ATOMIC==8
#    define ATOMIC_MEMCPY(x,y,size) do { switch(size) { \
			case 1: AMC1(x,y); break;			\
			case 2: AMC2(x,y); break;			\
			case 4: AMC4(x,y); break;			\
			case 8: AMC8(x,y); break;			\
			default: fprintf(stderr,"impossible to make an atomic assignment of size %d\n",(int)size);p_rfatalv("RUNTIME ERROR"); \
			} } while(0)
#   else
#    if LARGEST_ATOMIC==16
#     define ATOMIC_MEMCPY(x,y,size) do { switch(size) { \
			case 1: AMC1(x,y); break;			\
			case 2: AMC2(x,y); break;			\
			case 4: AMC4(x,y); break;			\
			case 8: AMC8(x,y); break;			\
			case 16: AMC16(x,y); break;			\
			default: fprintf(stderr,"impossible to make an atomic assignment of size %d\n",(int)size);p_rfatalv("RUNTIME ERROR"); \
			} } while(0)
#    endif
#   endif
#  endif
# endif
#endif

#endif

#endif
