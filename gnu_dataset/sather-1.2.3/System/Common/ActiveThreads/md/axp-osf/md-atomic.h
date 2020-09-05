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

/* 
   Until better times (when I have time to look into the multiprocessing
   version), we do not really need atomic operations here. The only trouble
   can come from DMA, but we do not allow this. Active Threads have to be
   called specifically for synchronization. 

   This will be replaced with proper atomic operations when we have a
   true multiprocessing version of Active Threads for the ALPHA AXP
*/ 	


/* 
 * Reads a value from at the address "addr" into register "r" and modifies
 * location with address "addr". The nature of the modification may be
 * different on different architectures. For now, we simply clear the
 * location (for ALPHA AXP multiprocessing version we will use the native 
 * synchronization instructions (in the future)
 */
#define MD_UNLOCKED_VAL   0
#define MD_LOCKED_VAL     1
#define MD_READ_AND_MODIFY(addr,r) r = (*addr); (*addr) = MD_LOCKED_VAL;
#define MD_OBTAINED_LOCK(r) (r==MD_UNLOCKED_VAL)
#define MD_IS_LOCKED(addr)  (*addr==MD_LOCKED_VAL)
#define MD_UNLOCK(addr)     *addr=MD_UNLOCKED_VAL;

#endif 

