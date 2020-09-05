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

#include <stdio.h>
#include <sys/types.h>
#include <sys/signal.h>
#include <sys/fault.h>
#include <sys/syscall.h>
#include <sys/procfs.h>
#include <unistd.h>
#include <sys/stat.h>
#include <fcntl.h>
#include <malloc.h>
#include <stdlib.h>
#include <sys/mman.h>
#include <sys/types.h>
#include <setjmp.h>

/*
** These defines must be set to be appropriate for the system.  The
** other macros are computed in terms of them.  These settings are
** for Ultrasparc 1's, but pretty similar to most RISC machines.
*/

typedef unsigned long word;
typedef double doubleword;

/* This is acceptable only with 32 byte cache lines and 8 byte alignment */
#define SI_USE_PARCEL_LOOKUP

#define SI_BYTES_PER_WORD	sizeof(word)        /* 4 */
#define SI_MIN_ALIGNMENT	sizeof(doubleword)  /* 8 */
#define SI_BYTES_PER_BLOCK	32
#define SI_BYTES_PER_PAGE	8192
#define SI_MAX_NODES		1	/* 1 = no distributed stuff	     */
#define SI_MINIMUM_HEAP		4*M	/* Heap to start out with 	     */
#define SI_MAXIMUM_HEAP		256*M	/* Heap to reserve address space for */
#define SI_MAX_FINALIZATION	64*K	/* Max objects needing finalization */
#define SI_MAX_MARK		M	/* Max size of mark stack           */
#define SI_MAX_SPLAY		8*M     /* Max splay nodes		    */

/*
** Zero out region of memory starting at x of y bytes.  `x' is assumed to
** start aligned to SI_MIN_ALIGNMENT, and `y' is assumed to be a proper
** multiple of SI_BYTES_PER_BLOCK.  This implementation assumes IEEE zero
** representation (all bits zero).
*/

/* This implementation unrolls the loop over each block. */

#define SI_ZERO_OUT(x,y) {		\
 register int i, its=(y)/sizeof(double);\
 register double zerod=0.0;		\
 register double *start=(double*)(x);	\
 switch (its) {				\
  case 4: *(start+3)=zerod;		\
  case 3: *(start+2)=zerod;		\
  case 2: *(start+1)=zerod;		\
  case 1: *(start)=zerod; break;	\
  default: for (i=0; i<its; i++) *(start+i)=zerod; } }

/* Defined in serial_hyper.c */
unsigned int first_bit(word arg);
#define SI_FIRST_BIT(x) first_bit(x)  
