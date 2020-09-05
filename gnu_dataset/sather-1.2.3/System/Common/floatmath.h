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

/* Have we already been here? */
#ifndef FLOATMATH
#define FLOATMATH 1
#include <float.h>

/* XPG-defined constants not supplied by MS Visual C++ include files */
#if defined(__CYGWIN32__)
# define M_E        2.7182818284590452354
# define M_LOG2E    1.4426950408889634074
# define M_LOG10E   0.43429448190325182765
# define M_LN2      0.69314718055994530942
# define M_LN10     2.30258509299404568402
# define M_PI       3.14159265358979323846
# define M_PI_2     1.57079632679489661923
# define M_PI_4     0.78539816339744830962
# define M_1_PI     0.31830988618379067154
# define M_2_PI     0.63661977236758134308
# define M_2_SQRTPI 1.12837916709551257390
# define M_SQRT2    1.41421356237309504880
# define M_SQRT1_2  0.70710678118654752440
#endif

#ifdef SUNOS5
double signaling_nan(int);
double infinity();
#endif

/*
* This file is included by Sather/System/header.h
* at the head of each compiler-generated header file.
*
* If the system supports single-precision library calls, these are used.
* Otherwise the floats are converted to double and back afterwards.
*/

#if defined(ASSIGNFLOAT)
/* WITH single-precision library */

#if defined(SPARC) && defined (SUNOS4)

/* Fix a problem in the <math.h> in SunOS 4 on the Sparc.
 * This otherwise causes annoying warning messages during compiling.
 * If you have problems with this redefinition, delete it and ignore
 * the warning messages.
 *
 * This is how the original definition should look like:
 * #define	ASSIGNFLOAT(x, y) { union { double _d; float _f } _kluge; \
 * 			_kluge._d = (y); x = _kluge._f; }
 */

#undef ASSIGNFLOAT
#define ASSIGNFLOAT(x, y) { union { double _d; float _f; } _kluge; \
			_kluge._d = (y); x = _kluge._f; }

#endif /* defined(SPARC) && defined (SUNOS4) */

#define _FM_F(name) \
	static FLT r_##name() \
	{ float f; ASSIGNFLOAT(f, r_##name##_()); return f; }

#define _FM_F_I(name) \
	static FLT r_##name(INT i) \
	{ float f; ASSIGNFLOAT(f, r_##name##_(&i)); return f; }

#define _FM_F_F(name) \
	static FLT r_##name(FLT f)\
	{ ASSIGNFLOAT(f, r_##name##_(&f)); return f; }

#define _FM_F_FF(name) \
	static FLT r_##name(FLT f, FLT g) \
	{ ASSIGNFLOAT(f, r_##name##_(&f, &g)); return f; }

#define _FM_F_FI(name) \
	static FLT r_##name(FLT f, INT i) \
	{ ASSIGNFLOAT(f, r_##name##_(&f, &i)); return f; }

#define _FM_F_IF(name)	\
	static FLT r_##name(INT i,FLT f)\
	{ ASSIGNFLOAT(f, r_##name##_(&i, &f)); return f; }

#define _FM_B_F(name) \
	static BOOL ir_##name(FLT f) \
	{ return ir_##name##_(&f); }

#define _FM_I_F(name) \
	static INT ir_##name(FLT f) \
	{ return ir_##name##_(&f); }

#define _FM_V_FP(name) \
	static void r_##name(FLT f, FLT g[]) \
	{ r_##name##_(&f, g, g+1); }

#else
/* WITHOUT single-precision library */

#define _FM_F(name)    static FLT r_##name() { return name(); }
#define _FM_F_I(name)  static FLT r_##name(INT i) { return name(i); }
#define _FM_F_F(name)  static FLT r_##name(FLT f) { return name(f); }
#define _FM_F_FF(name) static FLT r_##name(FLT f,FLT g) { return name(f, g); }
#define _FM_F_FI(name) static FLT r_##name(FLT f,INT i) { return name(f, i); }
#define _FM_F_IF(name) static FLT r_##name(INT i,FLT f) { return name(i, f); }
#define _FM_B_F(name)  static BOOL ir_##name(FLT f) { return ir_##name(f); }
#define _FM_I_F(name)  static INT ir_##name(FLT f) { return ir_##name(f); }
#define _FM_V_FP(name) static void r_##name(FLT f, FLT g[]) \
			{ double a, b; sincos(f, &a, &b); g[0]=a; g[1]=b; }

#endif

/* Nothing should go behind this #endif */
#endif
