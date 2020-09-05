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
 * Some useful macros for debugging
 *
 * Version 1.0 (released for 1.0.9) by Claudio Fleiner (fleiner@icsi.berkeley.edu)
 */
#ifdef __GNUC__
void debug(const char *file,long line,const char *c,...) __attribute__((format(printf,3,4)));
#else
void debug(const char *file,long line,const char *c,...);
#endif
#ifdef DEBUG
#define INIT_DEBUG debug(__FILE__,__LINE__,"")
#define DEBUG0(a)	debug(__FILE__,__LINE__,a)
#define DEBUG1(a,b)	debug(__FILE__,__LINE__,a,b)
#define DEBUG2(a,b,c)	debug(__FILE__,__LINE__,a,b,c)
#define DEBUG3(a,b,c,d)	debug(__FILE__,__LINE__,a,b,c,d)
#define DEBUG4(a,b,c,d,e)	debug(__FILE__,__LINE__,a,b,c,d,e)
#define DEBUG5(a,b,c,d,e,f)	debug(__FILE__,__LINE__,a,b,c,d,e,f)
#define DEBUG6(a,b,c,d,e,f,g)	debug(__FILE__,__LINE__,a,b,c,d,e,f,g)
#define UNREACHABLE	do { DEBUG0("unreachable code");abort(); } while(0)
#else
#define INIT_DEBUG 
#define DEBUG0(a)	
#define DEBUG1(a,b)	
#define DEBUG2(a,b,c)
#define DEBUG3(a,b,c,d)
#define DEBUG4(a,b,c,d,e)
#define DEBUG5(a,b,c,d,e,f)
#define DEBUG6(a,b,c,d,e,f,g)
#define UNREACHABLE
#endif

#ifdef REGISTER_THREADS
#define REG(arg1,arg2) reg_thread(__FILE__,__LINE__,arg1,arg2)
#define UNREG unreg_thread(__FILE__,__LINE__)
void reg_thread(char *,int,void (*)(),void *);
void unreg_thread(char *,int);
#else
#define REG(arg1,arg2)
#define UNREG
#endif
