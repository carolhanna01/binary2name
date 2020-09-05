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

#ifndef _SIMPLE_PAR_H_ 
#define _SIMPLE_PAR_H_

/* 
 * This file contains macros to be emitted by the compiler. It attempts
 * to drastcally optimize special, but very important cases when a single
 * simple function is forked off or a simple call expression is evaluated
 * remotely. More expensive general cases are handled elsewhere.
 */
 
/* 
 * The following is a "simple" par/fork interface. 
 */

typedef struct SIMPLE_PAR_ATTACH_struct {
  BR_spinlock_t slck;
  int counter;
  BR_lock_t     end_mutex;
} *SIMPLE_PAR_ATTACH;
  
void par_decrement_handler_1(BR_cluster_t ignore, BR_word_t arg);

#define PAR_DEC(name)        struct SIMPLE_PAR_ATTACH_struct name; \
                             BR_spinlock_t *name ## _slck = &(name.slck);

#define PAR_START(name)      BR_REAL_SPINLOCK_INIT(* name ## _slck); \
                             BR_REAL_SPINLOCK_LOCK(* name ## _slck); \
                             name.counter = 1; /* for par body */ \
                             BR_REAL_SPINLOCK_UNLOCK(* name ## _slck); \
			     name.end_mutex = BR_LOCK_CREATE(); \
			     BR_LOCK(name.end_mutex); \
			     SYS_EXPORT;
/*			     fprintf(stderr, "START: 0x%x\n",&par_ob.counter_slck); */



#define PAR_END(name)        BR_REAL_SPINLOCK_LOCK(*name ## _slck); \
                             name.counter--; /* for par body */ \
                             if(name.counter > 0) { \
			        BR_REAL_SPINLOCK_UNLOCK(*name ## _slck); \
				BR_LOCK(name.end_mutex); \
			     } else { \
				BR_REAL_SPINLOCK_UNLOCK(*name ## _slck); \
			     } \
                             BR_LOCK_DELETE(name.end_mutex); \
			     SYS_IMPORT;
/*fprintf(stderr, "END: 0x%x\n",&par_ob.counter_slck); */

#define PAR_INCREMENT(name) \
                             BR_REAL_SPINLOCK_LOCK(*name ## _slck); \
                             name.counter++; \
                             BR_REAL_SPINLOCK_UNLOCK(*name ## _slck); 
/*fprintf(stderr, "INCREMENT: 0x%x\n",&(par_ptr)->counter_slck); */

#define PAR_DECREMENT(src,p) SYS_EXPORT; \
                             BR_REQUEST_1(src,par_decrement_handler_1,(BR_word_t)p)

/*                             BR_REAL_SPINLOCK_LOCK((p)->slck); \
                             (p)->counter--; \
			     if((p)->counter==0) {\
				BR_REAL_SPINLOCK_UNLOCK((p)->slck); \
                                BR_UNLOCK((p)->end_mutex); \
			     } else { \
				BR_REAL_SPINLOCK_UNLOCK((p)->slck); \
			     } 
			     */
/*                             fprintf(stderr, "DECREMENT: 0x%x\n",&(par_ptr)->counter_slck); */





/* Simple "at" expression interface */
typedef struct AT_ATTACH_struct {
  BR_spinlock_t     end_slck;
  BR_word_t         result;
} *AT_ATTACH;
  
void at_expr_finished_handler_2(BR_cluster_t ignore, BR_word_t at_attach_p, 
		       BR_word_t res);

#define AT_ATTACH_DEC(name)  struct AT_ATTACH_struct name; \
                             BR_spinlock_t *name ## _end_slck=&(name.end_slck);

#define AT_ATTACH_START(name) BR_REAL_SPINLOCK_INIT(* name ## _end_slck); \
                              BR_REAL_SPINLOCK_LOCK(* name ## _end_slck); 

/*#define AT_ATTACH_WAIT(name)  BR_LOCK(name.end_mutex); */

/* In the absence of other threads, try to spin to avoid contex switch */
#define AT_ATTACH_WAIT(name)  while(!BR_SPINLOCK_TRY(*name ## _end_slck)){ \
                                 BR_POLL(); \
                                 BR_THREAD_YIELD(); \
			      }

#define AT_ATTACH_END(name)   

/* 
 * This is used to signal termination to the calling cluster if "@" is
 * implemented with Brahma FORK
 */

#define AT_ATTACH_FORK_DONE(src,p,res) \
     BR_REQUEST_2((BR_cluster_t)(src),at_expr_finished_handler_2,(BR_word_t)(p),(BR_word_t)(res))

/* 
 * This is used to signal termination to the calling cluster if "@" is
 * implemented with Brahma REQUEST
 */
#define AT_ATTACH_REQUEST_DONE(src,p,res) \
     BR_REPLY_2((BR_cluster_t)(src),at_expr_finished_handler_2,(BR_word_t)(p),(BR_word_t)(res))

     /* 
      * The following macros are needed to establish a new exception
      * stack for light threads.
      */
     /* 
      *Basically, this creates a root of an exception tree for a new 
      * thread
      */
#define THREAD_PROTECT_BEGIN                     \
   { struct EXCEPT_PROTECT_ELEMENT p_n;		 \
     p_n.c.type=EXCEPT_PROTECT;			 \
     p_n.c.heap=0;				 \
     p_n.c.prev_frame=p_n.c.next_frame=NULL;	 \
     EXCEPTION_STACK=(struct EXCEPT_ELEMENT_COMMON *)&p_n;

#define THREAD_PROTECT_END                       \
   }

#endif

     




