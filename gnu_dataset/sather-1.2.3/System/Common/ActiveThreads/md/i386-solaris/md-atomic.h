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

#ifndef _MD_ATOMIC_H_
#define _MD_ATOMIC_H_

/* x must be an address of a word, and y must be a register local */
# define TEST_AND_SET(x,y) \
        asm volatile ("movl $1, %0" :  "=r" (y));  \
        asm volatile ("xchgl %0, 0(%1)" : "=r" (y) : "r" (x)); 
	

#define MD_UNLOCKED_VAL   0
#define MD_LOCKED_VAL     1
#define MD_READ_AND_MODIFY(addr,r) \
        asm volatile ("movl $1, %0" :  "=r" (r));  \
        asm volatile ("xchgl %0, 0(%1)" : "=r" (r) : "r" (addr)); 
#define MD_OBTAINED_LOCK(r) (r==MD_UNLOCKED_VAL)
#define MD_IS_LOCKED(addr)  (*addr==MD_LOCKED_VAL)
#define MD_UNLOCK(addr)     *addr=MD_UNLOCKED_VAL;

#endif /* _MD_ATOMIC_  */
