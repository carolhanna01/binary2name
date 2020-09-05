/*------------------------->  ANSI C - headerfile  <-------------------------*/
/* Copyright (C) 1994 by International Computer Science Institute            */
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

#ifndef PSATHER 

#ifndef _EXC_SATHER_H_
#define _EXC_SATHER_H_

#include <stddef.h>
#include <stdlib.h>
#include <sys/utsname.h>
#if defined(V_RISCOS) && defined(R_5_0) /* yoshida@agusa.nuie.nagoya-u.ac.jp */
# ifndef SETJMP_H
#  define SETJMP_H
#  include <setjmp.h>
# endif
#else
# include <setjmp.h>
#endif
#include <assert.h>

/*
 * the structure EXCEPT_ELEMENT should not be used anywhere in the code
 */
struct EXCEPT_ELEMENT {
        struct EXCEPT_ELEMENT *next;
        jmp_buf jmp;
        OB exception;
	short mark;
};

/*
 * to use this code, you must define in ONE *.c file
 * DEFINE_EXCEPTION_STACK outside any function to declare a global variable.
 * or include this file in one *.c file after you've defined RUNTIME
 */
#define DEFINE_EXCEPTION_STACK static struct EXCEPT_ELEMENT exp_bottom;struct EXCEPT_ELEMENT *EXCEPTION_STACK=&exp_bottom
#ifndef RUNTIME
extern struct EXCEPT_ELEMENT *EXCEPTION_STACK;
#else
DEFINE_EXCEPTION_STACK;
#endif

/*
 * While executing a lock, try or protect statement, an exception stack
 * is build. You can mark specific stack frames and unwind the stack to the
 * first mark at any time. You have 16 different mark (0-15), but MARK(0)
 * and MARK(1) are predefined. The first is used to mark the stack at the
 * beginning of a function, while the second can be used for loops.
 * Be warned however, that while ESCAPE unwinds the stack end ends the
 * function, JUMP_OUT_OF_LOOP does NOT terminate the loop. You have to 
 * manually end the loop by either a break or a goto.
 */
#define MARK(n)	(1<<(n))
#define MARK_FUNCTION			MARK(0)
#define MARK_LOOP			MARK(1)
#define SET_EXCEPTION_MARK(n)		EXCEPTION_STACK->mark|=(n)
#define REMOVE_EXCEPTION_MARK(n)	EXCEPTION_STACK->mark&= ~(n)
#define POP_TO(m) do { while(EXCEPTION_STACK->mark&(m)==0) EXCEPTION_STACK=EXCEPTION_STACK->next; REMOVE_EXCEPTION_MARK(m) } while(0)

/*
 * each function using ESCAPE MUST use EXCEPTION_PROLOGUE at the 
 * beginning, and EXCEPTION_EPILOGUE at the end. Such a function may
 * NOT use return.
 */
#define EXCEPTION_PROLOGUE 	SET_EXCEPTION_MARK(MARK_FUNCTION);
#define EXCEPTION_EPILOGUE 	REMOVE_EXCEPTION_MARK(MARK_FUNCTION);
#define ESCAPE			POP_TO(MARK_FUNCTION);return 

/*
 * If you want to use mark on the stack to end loops, use
 * LOOP_BEGIN before the loop and LOOP_END at its end.
 * Note that JUMP_OUT_OF_LOOP must be followed by a break or goto,
 * as it will not end the loop.
 */
#define LOOP_BEGIN		SET_EXCEPTION_MARK(MARK_LOOP);
#define LOOP_END		REMOVE_EXCEPTION_MARK(MARK_LOOP);
#define JUMP_OUT_OF_LOOP	POP_TO(MARK_LOOP)

/*
 * To pop some frames from the exception stack, you may use POP_EXCEPTION.
 * This macro can be used instead of LOOP_BEGIN and LOOP_END. The only
 * problem is that you must know exactly how many frames to pop
 * when exiting a loop.
 */
#define POP_EXCEPTION1  EXCEPTION_STACK=EXCEPTION_STACK->next
#define POP_EXCEPTION(n) do { int i=n;while(i-->0) POP_EXCEPTION1; } while(0)

/*
 * RAISE(x) raises an exception. The value x (actually a pointer) is passed
 * to the exception handling routine, which can use it through the
 * macro EXCEPTION. If this routine needs to propagate the exception,
 * it can use RAISE(EXCEPTION)
 */
#define UNCAUGHT_EXCEPTION 0
/* This macro is somewhat tricky since ex could be instantiated with EXCEPTION
 * when propagating the exception to the handler above. If so, 
 * EXCEPTION is expanded into EXCEPTION_STACK->exception, but EXCEPTION_STACK
 * is changed in the macro and a horrible name capture happens. To avoid it,
 * we need to copy "ex" into local "the_ex" immediately and use "the_ex"
 * everywhere.
 * Fixed on 8/23/96
 * Boris
 */

#define RAISE(ex) do { OB the_ex = ex;                                  \
        struct EXCEPT_ELEMENT *PREV_EXCEPTION_STACK=EXCEPTION_STACK;	\
                       EXCEPTION_STACK=PREV_EXCEPTION_STACK->next;     	\
		       if(PREV_EXCEPTION_STACK->next!=NULL) {		\
			       EXCEPTION_STACK->exception=the_ex;      	\
			       longjmp(PREV_EXCEPTION_STACK->jmp,1); 	\
		       } else assert(UNCAUGHT_EXCEPTION);		\
	          } while(0)
#define EXCEPTION 	   		((OB)EXCEPTION_STACK->exception)

/* 
 * To protect some statements A against exception, use
 * PROTECT_BEGIN
 *	A;
 * PRTOECT_WHEN
 *	if(EXCEPTION==....
 * PRTOECT_END
 * If PROTECT_END is ever executed because the exception handler does
 * neither end the program, nor raise another exceptioexception or ESCAPE 
 * (see above), the exception is automatically propagated back to the 
 * next PROTECT by a RAISE(EXCEPTION).
 */
#define PROTECT_BEGIN	   { struct EXCEPT_ELEMENT p_n; 		     \
			     if(setjmp(p_n.jmp)==0) {		  	     \
				p_n.next=EXCEPTION_STACK;		     \
				p_n.mark=0;				     \
				EXCEPTION_STACK= &p_n;
             
#define PROTECT_WHEN	     POP_EXCEPTION1; } else {
#define PROTECT_END	     }   					     \
			   }

#endif

#endif
