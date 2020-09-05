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
 * Interface to trace functions
 *
 * Version 1.0 (released for 1.0.9) by Claudio Fleiner (fleiner@icsi.berkeley.edu)
 */
#ifndef _TRACE_H_
#define _TRACE_H_

/*
 * To get any trace, you need to define PSATHER_TRACE
 * This will get you a minimal trace. To get more information,
 * you can define 
 * LOCK_TRACE: to get information about which thread got
 *             which lock
 * ATTR_TRACE: to get information about who wanted to read
 *             which attributes
 */

void use_trace(int);
#ifdef PSATHER_TRACE

void trace(const char *,...);
#define TRACE(a)		trace(a)
#define TRACE1(a,b)		trace(a,b)
#define TRACE2(a,b,c)		trace(a,b,c)
#define TRACE3(a,b,c,d)		trace(a,b,c,d)
#define TRACE4(a,b,c,d,e)	trace(a,b,c,d,e)
#define TRACE5(a,b,c,d,e,f)	trace(a,b,c,d,e,f)
#define TRACE6(a,b,c,d,e,f,g)	trace(a,b,c,d,e,f,g)

#else

#ifdef ATTR_TRACE
#undef ATTR_TRACE
#endif
#ifdef LOCK_TRACE
#undef LOCK_TRACE
#endif

#define TRACE(a)	
#define TRACE1(a,b)	
#define TRACE2(a,b,c)	
#define TRACE3(a,b,c,d)	
#define TRACE4(a,b,c,d,e)	
#define TRACE5(a,b,c,d,e,f)	
#define TRACE6(a,b,c,d,e,f,g)	

#endif

#ifdef ATTR_TRACE
#define ATTR_TR(a)		TRACE(a)
#define ATTR_TR1(a,b)		TRACE1(a,b)
#define ATTR_TR2(a,b,c)		TRACE2(a,b,c)
#define ATTR_TR3(a,b,c,d)	TRACE3(a,b,c,d)
#define ATTR_TR4(a,b,c,d,e)	TRACE4(a,b,c,d,e)
#define ATTR_TR5(a,b,c,d,e,f)	TRACE5(a,b,c,d,e,f)
#define ATTR_TR6(a,b,c,d,e,f,g)	TRACE6(a,b,c,d,e,f,g)
#else
#define ATTR_TR(a)	
#define ATTR_TR1(a,b)	
#define ATTR_TR2(a,b,c)	
#define ATTR_TR3(a,b,c,d)	
#define ATTR_TR4(a,b,c,d,e)	
#define ATTR_TR5(a,b,c,d,e,f)	
#define ATTR_TR6(a,b,c,d,e,f,g)	
#endif

#ifdef LOCK_TRACE
#define LOCK_TR(a)		TRACE(a)
#define LOCK_TR1(a,b)		TRACE1(a,b)
#define LOCK_TR2(a,b,c)		TRACE2(a,b,c)
#define LOCK_TR3(a,b,c,d)	TRACE3(a,b,c,d)
#define LOCK_TR4(a,b,c,d,e)	TRACE4(a,b,c,d,e)
#define LOCK_TR5(a,b,c,d,e,f)	TRACE5(a,b,c,d,e,f)
#define LOCK_TR6(a,b,c,d,e,f,g)	TRACE6(a,b,c,d,e,f,g)
#else
#define LOCK_TR(a)	
#define LOCK_TR1(a,b)	
#define LOCK_TR2(a,b,c)	
#define LOCK_TR3(a,b,c,d)	
#define LOCK_TR4(a,b,c,d,e)	
#define LOCK_TR5(a,b,c,d,e,f)	
#define LOCK_TR6(a,b,c,d,e,f,g)	
#endif

#endif
