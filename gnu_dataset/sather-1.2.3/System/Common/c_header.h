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

/* This file is included at the head of the compiler-generated header file. */

#include <stdio.h>
#include <math.h>
#include <string.h>
#if !defined(__CYGWIN32__)
# include <sys/utsname.h> /* needed for detecting RISC/os 5.01 */
#endif
#if defined(V_RISCOS) && defined(R_5_0) /* yoshida@agusa.nuie.nagoya-u.ac.jp */
# ifndef _SETJMP_H
#  define _SETJMP_H
#  include <setjmp.h>
# endif
#else
# include <setjmp.h>
#endif
#include <setjmp.h>
#include <signal.h>
#include <limits.h>
#include <sys/types.h>
#include <sys/time.h>
#include <stdlib.h>
#include <time.h>
#include <float.h>

#if !defined(__NeXT__) && !defined(__CYGWIN32__)
# include <unistd.h>
#endif

#if !defined(__NeXT__) && !defined(__EMX__) && !defined(__NetBSD__) && !defined(__FreeBSD__) && !defined(__CYGWIN32__)
#  include <values.h>
#endif

/*  -- Sept. 99 by John Newman
#ifdef __CYGWIN32__        /* From robert@crclund.abb.se * /
# include <io.h>
# define lseek _lseek
# define read _read
# define open _open
# define close _close
# define O_RDONLY _O_RDONLY
#endif
*/

/* Determine the machine type: */
# if defined(__i386__) && defined(__sun__)
     #define SUNOS5
# endif
# if defined(sun) && defined(sparc)
#   define SPARC
    /* Test for SunOS 5.x */
#     include <errno.h>
#     ifdef ECHRNG
#       define SUNOS5
#     else
#       define SUNOS4
#     endif
# endif
# if defined(NeXT) && defined(mc68000)
#   define M68K
#   define NEXT
# endif
# if defined(vax)
#    define VAX
#    ifdef ultrix
#       define ULTRIX
#    else
#       define BSD
#    endif
# endif

#if defined(SPARC) && defined(SUNOS5) && defined(__SUNPRO_C)
# include <sunmath.h>             
#endif

/*
** ZALLOC(x) is a garbage collected malloc that returns zeroed memory.
** This is delegated to the collector appropriate for the target
** platform.  The _LEAF and _BIG forms denote regions that will be
** free of pointers and that will be very large.  (The latter is needed
** by the Boehm collector to do with blacklisting.)  
*/

#ifdef SIVA
#  define ZALLOC(x) 		((void*)SI_ALLOC(x))
#  define ZALLOC_LEAF(x) 	((void*)SI_ALLOC_LEAF(x))
#  define ZALLOC_BIG(x) 	((void*)SI_ALLOC(x))
#  define ZALLOC_LEAF_BIG(x)	((void*)SI_ALLOC_LEAF(x))
#  define ZFREE(x)		SI_dealloc(x)
#else
#  define ZALLOC(x)		GC_malloc(x)
#  define ZALLOC_LEAF(x)	GC_malloc_atomic(x)
#  define ZALLOC_BIG(x)		GC_malloc_ignore_off_page(x)
#  define ZALLOC_LEAF_BIG(x)	GC_malloc_atomic_ignore_off_page(x)
#  define ZFREE(x)		GC_free(x)
#endif

/* OBALLOC(x) returns a zeroed memory region of size and type x.
 * The compiler emits typedefs such that "x_struct" is the name
 * of the struct type that *x refers to.
*/
#define OBALLOC(x) ((x) ZALLOC(sizeof(struct x##_struct)))

/* For bootstrapping */
#define OB_ALLOC(x) OBALLOC(x)

/* These may be defined as appropriate to the target system */

typedef char BOOL;
#define FALSE 0
#define TRUE 1

#if defined(V_RISCOS) && defined(R_5_0) /* yoshida@agusa.nuie.nagoya-u.ac.jp */
# ifndef _MAXFLOAT
#  define _MAXFLOAT
#  define MAXFLOAT          ((float)3.40282346638528860e+38)
# endif
# define M_LOG2E             1.4426950408889634074
#endif

#ifdef ALPHA
typedef int INT;
#define SUINT_MAX UINT_MAX
#define SINT_MAX INT_MAX
#define SINT_MIN INT_MIN
#else
typedef int INT;
#define SUINT_MAX UINT_MAX
#define SINT_MAX INT_MAX
#define SINT_MIN INT_MIN
#endif

/* Layouts for the Sather classes which are built-in */

typedef struct {
    short tag;
#ifdef DESTROY_CHK
    char destroyed;
#endif
#ifdef DETERMINISTIC
    unsigned int id;
#endif
  } OB_HEADER;

typedef char CHAR;
typedef float FLT;
typedef double FLTD;
typedef double FLTX;  /* This is a hack */
typedef double FLTDX; /* This is a hack */
typedef void* EXT_OB;
#ifndef PSATHER
typedef void *GATE;
typedef void *MUTEX;
typedef long THREAD_ID;
typedef struct THREAD_ID_boxed_struct { OB_HEADER header; THREAD_ID immutable_part; } *THREAD_ID_boxed;
#define THR_HASH(x) x
#define thr_print_id(x,y)
#define LOCK_HEADER_STRUCT
#define LOCK_HEADER_STRUCT_REFS
#define THREAD_ID_zero (long)0
#define THREAD_ID_IS_EQ(a,b) ((a)==(b))
#define THREAD_ID_IS_VOID(a)  ((a)==0)
#endif



/* Little boxes made of ticky-tacky */

typedef struct BOOL_boxed_struct { OB_HEADER header; BOOL immutable_part; } *BOOL_boxed;
typedef struct CHAR_boxed_struct { OB_HEADER header; CHAR immutable_part; } *CHAR_boxed;
typedef struct INT_boxed_struct { OB_HEADER header; INT immutable_part; } *INT_boxed;
typedef struct FLT_boxed_struct { OB_HEADER header; FLT immutable_part; } *FLT_boxed;
typedef struct FLTD_boxed_struct { OB_HEADER header; FLTD immutable_part; } *FLTD_boxed;
typedef struct FLTX_boxed_struct { OB_HEADER header; FLTX immutable_part; } *FLTX_boxed;
typedef struct FLTDX_boxed_struct { OB_HEADER header; FLTDX immutable_part; } *FLTDX_boxed;
typedef struct EXT_OB_boxed_struct { OB_HEADER header; EXT_OB immutable_part; } *EXT_OB_boxed;

typedef struct STR_struct {
    OB_HEADER header;
    INT asize;
    CHAR arr_part[1];
} *STR;

/* bound iter stuff: a header for an iter frame, the actual iter is yet 
   not known  */
typedef struct iter_frame_stub_struct {
 INT state;
}*iter_frame_stub;

typedef struct OB_struct {
    OB_HEADER header;
} *OB;

#define INT_zero (INT)0
#define BOOL_zero (BOOL)0
#define CHAR_zero (CHAR)0
#define FLTD_zero (FLTD)0
#define FLT_zero (FLT)0
#define FLTX_zero (FLTX)0
#define FLTDX_zero (FLTDX)0
#define EXT_OB_zero (EXT_OB)0

#define STD_IS_EQ(a,b) ((a)==(b))
#define INT_IS_EQ(a,b) STD_IS_EQ(a,b)
#define BOOL_IS_EQ(a,b) STD_IS_EQ(a,b)
#define CHAR_IS_EQ(a,b) STD_IS_EQ(a,b)
#define FLTD_IS_EQ(a,b) STD_IS_EQ(a,b)
#define FLT_IS_EQ(a,b) STD_IS_EQ(a,b)
#define FLTX_IS_EQ(a,b) STD_IS_EQ(a,b)
#define FLTDX_IS_EQ(a,b) STD_IS_EQ(a,b)
#define EXT_OB_IS_EQ(a,b) STD_IS_EQ(a,b)

#define INT_IS_VOID(a) ((a)==0)
#define BOOL_IS_VOID(a) ((a)==0)
#define CHAR_IS_VOID(a) ((a)==0)
#define FLTD_IS_VOID(a) ((a)==0.0)
#define FLT_IS_VOID(a) ((a)==0.0)
#define FLTX_IS_VOID(a) ((a)==0.0)
#define FLTDX_IS_VOID(a) ((a)==0.0)
#define EXT_OB_IS_VOID(a) ((a)==0)

#undef ferror
#undef feof

/* include floating-point support */
#include "floatmath.h"

/* include exception handling support */
/* pSather has its own exception handling */
#ifndef PSATHER
#include "exception.h"
#endif

STR gen_SYS_str_for_tp(INT);
BOOL gen_SYS_ob_eq(OB,OB);

#define IS_ITER 0

/* include stuff for interfacing other languages */
#include "fortran.h"
#include "c.h"

/* 
 * The definition of multiple tables (mostly generated by the compiler)
 * Those tables are used for
 * - GC
 * - Debugging
 * - to move data between clusters (pSather)
 */

/* source files, used for nice error messages and debugging */
extern char *source_files[];
extern int sather_type_offset;
struct sather_attribute {
	char *sather_name;
	char *c_name;
	int type;
	long offset;
	long source;
};

extern struct sather_type_description {
	int *refs; /* list of offsets for reference objects, terminated with -1 */
		   /* void if object has no references at all. This information */
		   /* is obviously redundant, but speeds up pSather and GC	*/
		   /* References in embedded value type objects have to be      */
		   /* mentioned too!						*/
	char *sather_name;
	char *c_name;
	long source; /* source file and line where it is defined */
	int size;
	int boxed;  /* only immutable types: offset of 'immutable_part' when boxed	*/
	int attrs; /* No of attributes						*/
	unsigned is_immutable:1;
	unsigned is_ref:1;
	unsigned is_abstract:1;
	unsigned is_bound:1;
	unsigned is_ext:1;
	unsigned is_aref:1; /* last attr describs array				*/
	struct sather_attribute attr[1];
} **sather_types;

extern struct sather_function_definition {
	void (*cfunc)();  			/* pointer to C function 	*/
	long source;				/* sather file			*/
	char *sather_name;
	char *c_name;
	int args; 				/* number of args, including self */
	int locals; 				/* number of locals */
	struct sather_attribute attr[1]; 	/* first args, then locals */
} *sather_functions[];
	
struct _func_frame {
	long func;
	struct _func_frame *prev;
	void **args;  /* args[0]=self, first args, then locals */
};

extern char *sather_prog_name;

