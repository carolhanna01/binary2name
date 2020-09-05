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

#ifndef _SPINLOCK_H_
#define _SPINLOCK_H_

#include "md-atomic.h"

/*------------------------------------------------------------------*/
/*                          Spinlok                                 */
/*------------------------------------------------------------------*/

typedef volatile unsigned int at_spinlock_t;
/* Just make it a word to avoid padding everywhere */
/*typedef md_atomic_t at_spinlock_t;*/

#define AT_SPINLOCK_DEC(s) at_spinlock_t s;
#define AT_SPINLOCK_INIT(s) MD_UNLOCK(&(s));


/* T&TSET locks. Initially spin on the local copy */
/*     while(x){};                       */

# define AT_SPINLOCK_LOCK(x)           	\
{  register int XyZ;                    \
   register at_spinlock_t *saddr=&(x);  \
   do {                                 \
     while(MD_IS_LOCKED(saddr)){};      \
     MD_READ_AND_MODIFY( saddr, XyZ);   \
   } while (!MD_OBTAINED_LOCK(XyZ));    \
}				  

# define AT_SPINLOCK_UNLOCK(x) MD_UNLOCK(&(x))


/* in GCC statements and declarations may act as expressions: compound stmts*/
/*#ifdef __GNUC__
# define AT_SPINLOCK_TRY(x)             \
({                                      \
   register int XyZ;                    \
   MD_READ_AND_MODIFY( &(x), XyZ);      \
   MD_OBTAINED_LOCK(XyZ);		\
})	   
#else					  
# define AT_SPINLOCK_TRY(x) at_spinlock_try(&x)              
int at_spinlock_try(at_spinlock_t* x);
AT_INLINE int at_spinlock_try(at_spinlock_t* x) {
  register int XyZ;            
  MD_READ_AND_MODIFY(x, XyZ);
  return MD_OBTAINED_LOCK(XyZ);
}
#endif */

# define AT_SPINLOCK_TRY(x) at_spinlock_try(&x)              


/*------------------------------------------------------------------*/
/*                     Hybrid Lock                                  */
/*------------------------------------------------------------------*/

typedef at_spinlock_t at_hybridlock_t;

#define AT_HYBRIDLOCK_DEC(s) at_hybridlock_t s;
#define AT_HYBRIDLOCK_INIT(s) MD_UNLOCK(&(s));

# define AT_HYBRIDLOCK_LOCK(x)         	\
{  register int XyZ, XyZd;              \
   MD_READ_AND_MODIFY(&(x),XyZ);        \
   if (!MD_OBTAINED_LOCK(XyZ)) {        \
      XyZd=1;                           \
      while (1) {                       \
         if (XyZd>100000) at_yield();   \
         else {                         \
            for (XyZ=XyZd; XyZ--;);     \
            XyZd*=2;                    \
         }                              \
         MD_READ_AND_MODIFY(&(x),XyZ);  \
         if (MD_OBTAINED_LOCK(XyZ)) break;  \
      }                                 \
   }                                    \
}

# define AT_HYBRIDLOCK_UNLOCK(x) MD_UNLOCK(&(x))

/* Try operation is exactly the same as that applied to spinlock */
# define AT_HYBRIDLOCK_TRY(x) AT_SPINLOCK_TRY(x)


#define AT_KERNEL_LOCK    AT_SPINLOCK_LOCK(kernel_lck)
#define AT_KERNEL_UNLOCK  AT_SPINLOCK_UNLOCK(kernel_lck)

#endif   /* _SPINLOCK_H_ */










