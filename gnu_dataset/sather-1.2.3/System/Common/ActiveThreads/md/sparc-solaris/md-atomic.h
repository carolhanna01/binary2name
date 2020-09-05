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

#ifndef _MD_ATOMIC_
#define _MD_ATOMIC_

/* addr must be an address of a word, and r must be a register local */

#define MD_UNLOCKED_VAL   0
#define MD_LOCKED_VAL     0xff000000
#define MD_READ_AND_MODIFY(addr,r) asm volatile \
     ("ldstub [%1], %0" : "=r" (r) : "r" (addr))
#define MD_OBTAINED_LOCK(r) (r==MD_UNLOCKED_VAL)
#define MD_IS_LOCKED(addr)  (*addr==MD_LOCKED_VAL)
#define MD_UNLOCK(addr)     asm volatile ("stbar"); \
     (volatile) *addr=MD_UNLOCKED_VAL;

#endif /* _MD_ATOMIC_  */
