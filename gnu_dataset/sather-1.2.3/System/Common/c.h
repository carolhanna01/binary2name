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

#ifndef _C_H_ 
#define _C_H_

#include <stddef.h>  /* needed for ptrdiff_t */

typedef char             C_CHAR;
typedef unsigned char    C_UNSIGNED_CHAR;
typedef signed char      C_SIGNED_CHAR;
typedef short            C_SHORT;
typedef int              C_INT;
typedef long             C_LONG;
typedef unsigned short   C_UNSIGNED_SHORT;
typedef unsigned int     C_UNSIGNED_INT;
typedef unsigned long    C_UNSIGNED_LONG;
typedef float            C_FLOAT;
typedef double           C_DOUBLE;
typedef long double      C_LONG_DOUBLE;

typedef void*            C_PTR;
typedef char*            C_CHAR_PTR;
typedef unsigned char*   C_UNSIGNED_CHAR_PTR;
typedef signed char*     C_SIGNED_CHAR_PTR;
typedef short*           C_SHORT_PTR;
typedef int*             C_INT_PTR;
typedef long*            C_LONG_PTR;
typedef unsigned short*  C_UNSIGNED_SHORT_PTR;
typedef unsigned int*    C_UNSIGNED_INT_PTR;
typedef unsigned long*   C_UNSIGNED_LONG_PTR;
typedef float*           C_FLOAT_PTR;
typedef double*          C_DOUBLE_PTR;
typedef long double*     C_LONG_DOUBLE_PTR; 

typedef size_t           C_SIZE_T;
typedef ptrdiff_t        C_PTRDIFF_T;

typedef struct C_CHAR_boxed_struct { OB_HEADER header; C_CHAR immutable_part; } *C_CHAR_boxed;
typedef struct C_UNSIGNED_CHAR_boxed_struct { OB_HEADER header; C_UNSIGNED_CHAR immutable_part; } *C_UNSIGNED_CHAR_boxed;
typedef struct C_SIGNED_CHAR_boxed_struct { OB_HEADER header; C_SIGNED_CHAR immutable_part; } *C_SIGNED_CHAR_boxed;
typedef struct C_SHORT_boxed_struct { OB_HEADER header; C_SHORT immutable_part; } *C_SHORT_boxed;
typedef struct C_INT_boxed_struct { OB_HEADER header; C_INT immutable_part; } *C_INT_boxed;
typedef struct C_LONG_boxed_struct { OB_HEADER header; C_LONG immutable_part; } *C_LONG_boxed;
typedef struct C_UNSIGNED_SHORT_boxed_struct { OB_HEADER header; C_UNSIGNED_SHORT immutable_part; } *C_UNSIGNED_SHORT_boxed;
typedef struct C_UNSIGNED_INT_boxed_struct { OB_HEADER header; C_UNSIGNED_INT immutable_part; } *C_UNSIGNED_INT_boxed;
typedef struct C_UNSIGNED_LONG_boxed_struct { OB_HEADER header; C_UNSIGNED_LONG immutable_part; } *C_UNSIGNED_LONG_boxed;
typedef struct C_FLOAT_boxed_struct { OB_HEADER header; C_FLOAT immutable_part; } *C_FLOAT_boxed;
typedef struct C_DOUBLE_boxed_struct { OB_HEADER header; C_DOUBLE immutable_part; } *C_DOUBLE_boxed;
typedef struct C_LONG_DOUBLE_boxed_struct { OB_HEADER header; C_LONG_DOUBLE immutable_part; } *C_LONG_DOUBLE_boxed;
typedef struct C_SIZE_T_boxed_struct { OB_HEADER header; C_SIZE_T immutable_part; } *C_SIZE_T_boxed;
typedef struct C_PTRDIFF_T_boxed_struct { OB_HEADER header; C_PTRDIFF_T immutable_part; } *C_PTRDIFF_T_boxed;


typedef struct C_PTR_boxed_struct { OB_HEADER header; C_PTR immutable_part; } *C_PTR_boxed;
typedef struct C_CHAR_PTR_boxed_struct { OB_HEADER header; C_CHAR_PTR immutable_part; } *C_CHAR_PTR_boxed;
typedef struct C_UNSIGNED_CHAR_PTR_boxed_struct { OB_HEADER header; C_UNSIGNED_CHAR_PTR immutable_part; } *C_UNSIGNED_CHAR_PTR_boxed;
typedef struct C_SIGNED_CHAR_PTR_boxed_struct { OB_HEADER header; C_SIGNED_CHAR_PTR immutable_part; } *C_SIGNED_CHAR_PTR_boxed;
typedef struct C_SHORT_PTR_boxed_struct { OB_HEADER header; C_SHORT_PTR immutable_part; } *C_SHORT_PTR_boxed;
typedef struct C_INT_PTR_boxed_struct { OB_HEADER header; C_INT_PTR immutable_part; } *C_INT_PTR_boxed;
typedef struct C_LONG_PTR_boxed_struct { OB_HEADER header; C_LONG_PTR immutable_part; } *C_LONG_PTR_boxed;
typedef struct C_UNSIGNED_SHORT_PTR_boxed_struct { OB_HEADER header; C_UNSIGNED_SHORT_PTR immutable_part; } *C_UNSIGNED_SHORT_PTR_boxed;
typedef struct C_UNSIGNED_INT_PTR_boxed_struct { OB_HEADER header; C_UNSIGNED_INT_PTR immutable_part; } *C_UNSIGNED_INT_PTR_boxed;
typedef struct C_UNSIGNED_LONG_PTR_boxed_struct { OB_HEADER header; C_UNSIGNED_LONG_PTR immutable_part; } *C_UNSIGNED_LONG_PTR_boxed;
typedef struct C_FLOAT_PTR_boxed_struct { OB_HEADER header; C_FLOAT_PTR immutable_part; } *C_FLOAT_PTR_boxed;
typedef struct C_DOUBLE_PTR_boxed_struct { OB_HEADER header; C_DOUBLE_PTR immutable_part; } *C_DOUBLE_PTR_boxed;
typedef struct C_LONG_DOUBLE_PTR_boxed_struct { OB_HEADER header; C_LONG_DOUBLE_PTR immutable_part; } *C_LONG_DOUBLE_PTR_boxed;

#define CHARC_CHAR(x)      ((C_CHAR)(x))
#define C_CHARCHAR(x)      ((CHAR)(x))
#define C_CHARPLUS(a,b)     a+b
#define C_CHARMINUS(a,b)    a-b
#define C_CHARTIMES(a,b)    a*b
#define C_CHARDIV(a,b)       a/b
#define C_CHARIS_LT(a,b)    (a<b)
#define C_CHARIS_LEQ(a,b)   (a<=b)
#define C_CHARIS_GT(a,b)    (a>b)
#define C_CHARIS_GEQ(a,b)   (a>=b)

#define CHARC_UNSIGNED_CHAR(x)      ((C_UNSIGNED_CHAR)(x))
#define C_UNSIGNED_CHARCHAR(x)      ((CHAR)(x))
#define C_UNSIGNED_CHARPLUS(a,b)     a+b
#define C_UNSIGNED_CHARMINUS(a,b)    a-b
#define C_UNSIGNED_CHARTIMES(a,b)    a*b
#define C_UNSIGNED_CHARDIV(a,b)       a/b
#define C_UNSIGNED_CHARIS_LT(a,b)    (a<b)
#define C_UNSIGNED_CHARIS_LEQ(a,b)   (a<=b)
#define C_UNSIGNED_CHARIS_GT(a,b)    (a>b)
#define C_UNSIGNED_CHARIS_GEQ(a,b)   (a>=b)

#define CHARC_SIGNED_CHAR(x)      ((C_SIGNED_CHAR)(x))
#define C_SIGNED_CHARCHAR(x)      ((CHAR)(x))
#define C_SIGNED_CHARPLUS(a,b)     a+b
#define C_SIGNED_CHARMINUS(a,b)    a-b
#define C_SIGNED_CHARTIMES(a,b)    a*b
#define C_SIGNED_CHARDIV(a,b)       a/b
#define C_SIGNED_CHARIS_LT(a,b)    (a<b)
#define C_SIGNED_CHARIS_LEQ(a,b)   (a<=b)
#define C_SIGNED_CHARIS_GT(a,b)    (a>b)
#define C_SIGNED_CHARIS_GEQ(a,b)   (a>=b)

#define INTC_SHORT(x)         ((C_SHORT)(x))
#define C_SHORTINT(x)         ((INT)(x))
#define C_SHORTPLUS(a,b)      a+b
#define C_SHORTMINUS(a,b)     a-b
#define C_SHORTTIMES(a,b)     a*b
#define C_SHORTDIV(a,b)       a/b
#define C_SHORTIS_LT(a,b)    (a<b)
#define C_SHORTIS_LEQ(a,b)   (a<=b)
#define C_SHORTIS_GT(a,b)    (a>b)
#define C_SHORTIS_GEQ(a,b)   (a>=b)

#define INTC_INT(x)         ((C_INT)(x))
#define C_INTINT(x)         ((INT)(x))
#define C_INTPLUS(a,b)      a+b
#define C_INTMINUS(a,b)     a-b
#define C_INTTIMES(a,b)     a*b
#define C_INTDIV(a,b)       a/b
#define C_INTIS_LT(a,b)    (a<b)
#define C_INTIS_LEQ(a,b)   (a<=b)
#define C_INTIS_GT(a,b)    (a>b)
#define C_INTIS_GEQ(a,b)   (a>=b)

#define INTC_LONG(x)         ((C_LONG)(x))
#define C_LONGINT(x)         ((INT)(x))
#define C_LONGPLUS(a,b)      a+b
#define C_LONGMINUS(a,b)     a-b
#define C_LONGTIMES(a,b)     a*b
#define C_LONGDIV(a,b)       a/b
#define C_LONGIS_LT(a,b)    (a<b)
#define C_LONGIS_LEQ(a,b)   (a<=b)
#define C_LONGIS_GT(a,b)    (a>b)
#define C_LONGIS_GEQ(a,b)   (a>=b)

#define INTC_UNSIGNED_SHORT(x)         ((C_UNSIGNED_SHORT)(x))
#define C_UNSIGNED_SHORTINT(x)         ((INT)(x))
#define C_UNSIGNED_SHORTPLUS(a,b)      a+b
#define C_UNSIGNED_SHORTMINUS(a,b)     a-b
#define C_UNSIGNED_SHORTTIMES(a,b)     a*b
#define C_UNSIGNED_SHORTDIV(a,b)       a/b
#define C_UNSIGNED_SHORTIS_LT(a,b)    (a<b)
#define C_UNSIGNED_SHORTIS_LEQ(a,b)   (a<=b)
#define C_UNSIGNED_SHORTIS_GT(a,b)    (a>b)
#define C_UNSIGNED_SHORTIS_GEQ(a,b)   (a>=b)

#define INTC_UNSIGNED_INT(x)         ((C_UNSIGNED_INT)(x))
#define C_UNSIGNED_INTINT(x)         ((INT)(x))
#define C_UNSIGNED_INTPLUS(a,b)      a+b
#define C_UNSIGNED_INTMINUS(a,b)     a-b
#define C_UNSIGNED_INTTIMES(a,b)     a*b
#define C_UNSIGNED_INTDIV(a,b)       a/b
#define C_UNSIGNED_INTIS_LT(a,b)    (a<b)
#define C_UNSIGNED_INTIS_LEQ(a,b)   (a<=b)
#define C_UNSIGNED_INTIS_GT(a,b)    (a>b)
#define C_UNSIGNED_INTIS_GEQ(a,b)   (a>=b)

#define INTC_UNSIGNED_LONG(x)         ((C_UNSIGNED_LONG)(x))
#define C_UNSIGNED_LONGINT(x)         ((INT)(x))
#define C_UNSIGNED_LONGPLUS(a,b)      a+b
#define C_UNSIGNED_LONGMINUS(a,b)     a-b
#define C_UNSIGNED_LONGTIMES(a,b)     a*b
#define C_UNSIGNED_LONGDIV(a,b)       a/b
#define C_UNSIGNED_LONGIS_LT(a,b)    (a<b)
#define C_UNSIGNED_LONGIS_LEQ(a,b)   (a<=b)
#define C_UNSIGNED_LONGIS_GT(a,b)    (a>b)
#define C_UNSIGNED_LONGIS_GEQ(a,b)   (a>=b)

#define FLTC_FLOAT(x)            ((C_FLOAT)(x))
#define C_FLOATFLT(x)            ((FLT)(x))
#define C_FLOATPLUS(a,b)         a+b
#define C_FLOATMINUS(a,b)        a-b
#define C_FLOATTIMES(a,b)        a*b
#define C_FLOATDIV(a,b)          a/b
#define C_FLOATIS_LT(a,b)       (a<b)
#define C_FLOATIS_LEQ(a,b)      (a<=b)
#define C_FLOATIS_GT(a,b)       (a>b)
#define C_FLOATIS_GEQ(a,b)      (a>=b)

#define FLTDC_DOUBLE(x)         ((C_DOUBLE)(x))
#define C_DOUBLEFLTD(x)         ((FLTD)(x))
#define C_DOUBLEPLUS(a,b)       a+b
#define C_DOUBLEMINUS(a,b)      a-b
#define C_DOUBLETIMES(a,b)      a*b
#define C_DOUBLEDIV(a,b)        a/b
#define C_DOUBLEIS_LT(a,b)      (a<b)
#define C_DOUBLEIS_LEQ(a,b)     (a<=b)
#define C_DOUBLEIS_GT(a,b)      (a>b)
#define C_DOUBLEIS_GEQ(a,b)     (a>=b)

#define FLTDXC_LONG_DOUBLE(x)         ((C_LONG_DOUBLE)(x))
#define C_LONG_DOUBLEFLTD(x)         ((FLTDX)(x))
#define C_LONG_DOUBLEPLUS(a,b)       a+b
#define C_LONG_DOUBLEMINUS(a,b)      a-b
#define C_LONG_DOUBLETIMES(a,b)      a*b
#define C_LONG_DOUBLEDIV(a,b)        a/b
#define C_LONG_DOUBLEIS_LT(a,b)      (a<b)
#define C_LONG_DOUBLEIS_LEQ(a,b)     (a<=b)
#define C_LONG_DOUBLEIS_GT(a,b)      (a>b)
#define C_LONG_DOUBLEIS_GEQ(a,b)     (a>=b)


#define C_CHAR_IS_EQ(a,b)             STD_IS_EQ(a,b)
#define C_UNSIGNED_CHAR_IS_EQ(a,b)    STD_IS_EQ(a,b)
#define C_SIGNED_CHAR_IS_EQ(a,b)      STD_IS_EQ(a,b)
#define C_SHORT_IS_EQ(a,b)            STD_IS_EQ(a,b)
#define C_INT_IS_EQ(a,b)              STD_IS_EQ(a,b)
#define C_LONG_IS_EQ(a,b)             STD_IS_EQ(a,b)
#define C_UNSIGNED_SHORT_IS_EQ(a,b)   STD_IS_EQ(a,b)
#define C_UNSIGNED_INT_IS_EQ(a,b)     STD_IS_EQ(a,b)
#define C_UNSIGNED_LONG_IS_EQ(a,b)    STD_IS_EQ(a,b)
#define C_FLOAT_IS_EQ(a,b)            STD_IS_EQ(a,b)
#define C_DOUBLE_IS_EQ(a,b)           STD_IS_EQ(a,b)
#define C_LONG_DOUBLE_IS_EQ(a,b)      STD_IS_EQ(a,b)


#define C_CHAR_zero                   (C_CHAR)0
#define C_UNSIGNED_CHAR_zero          (C_UNSIGNED_CHAR)0
#define C_SIGNED_CHAR_zero            (C_SIGNED_CHAR)0
#define C_SHORT_zero                  (C_SHORT)0
#define C_INT_zero                    (C_INT)0
#define C_LONG_zero                   (C_LONG)0
#define C_UNSIGNED_SHORT_zero         (C_UNSIGNED_SHORT)0
#define C_UNSIGNED_INT_zero           (C_UNSIGNED_INT)0
#define C_UNSIGNED_LONG_zero          (C_UNSIGNED_LONG)0
#define C_FLOAT_zero                  (C_FLOAT)0.0
#define C_DOUBLE_zero                 (C_DOUBLE)0.0
#define C_LONG_DOUBLE_zero            (C_LONG_DOUBLE)0.0
#define C_SIZE_T_zero                 (C_SIZE_T)0


#define C_CHAR_IS_VOID(a)             ((a)==0)
#define C_UNSIGNED_CHAR_IS_VOID(a)    ((a)==0)
#define C_SIGNED_CHAR_IS_VOID(a)      ((a)==0)
#define C_SHORT_IS_VOID(a)            ((a)==0)
#define C_INT_IS_VOID(a)              ((a)==0)
#define C_LONG_IS_VOID(a)             ((a)==0)
#define C_UNSIGNED_SHORT_IS_VOID(a)   ((a)==0)
#define C_UNSIGNED_INT_IS_VOID(a)     ((a)==0)
#define C_UNSIGEND_LONG_IS_VOID(a)    ((a)==0)
#define C_FLOAT_IS_VOID(a)            ((a)==0.0)
#define C_DOUBLE_IS_VOID(a)           ((a)==0.0)
#define C_LONG_DOUBLE_IS_VOID(a)      ((a)==0.0)


#define C_PTR_IS_EQ(a,b)                  (a==b)
#define C_CHAR_PTR_IS_EQ(a,b)             (a==b)
#define C_UNSIGNED_CHAR_PTR_IS_EQ(a,b)    (a==b)
#define C_SIGNED_CHAR_PTR_IS_EQ(a,b)      (a==b)
#define C_SHORT_PTR_IS_EQ(a,b)            (a==b)
#define C_INT_PTR_IS_EQ(a,b)              (a==b)
#define C_LONG_PTR_IS_EQ(a,b)             (a==b)
#define C_UNSIGNED_SHORT_PTR_IS_EQ(a,b)   (a==b)
#define C_UNSIGNED_INT_PTR_IS_EQ(a,b)     (a==b)
#define C_UNSIGNED_LONG_PTR_IS_EQ(a,b)    (a==b)
#define C_FLOAT_PTR_IS_EQ(a,b)            (a==b)
#define C_DOUBLE_PTR_IS_EQ(a,b)           (a==b)
#define C_LONG_DOUBLE_PTR_IS_EQ(a,b)      (a==b)


#define C_PTR_zero                        NULL           
#define C_CHAR_PTR_zero                   NULL
#define C_UNSIGNED_CHAR_PTR_zero          NULL
#define C_SIGNED_CHAR_PTR_zero            NULL
#define C_SHORT_PTR_zero                  NULL
#define C_INT_PTR_zero                    NULL
#define C_LONG_PTR_zero                   NULL
#define C_UNSIGNED_SHORT_PTR_zero         NULL
#define C_UNSIGNED_INT_PTR_zero           NULL
#define C_UNSIGNED_LONG_PTR_zero          NULL
#define C_FLOAT_PTR_zero                  NULL
#define C_DOUBLE_PTR_zero                 NULL
#define C_LONG_DOUBLE_PTR_zero            NULL

#define C_PTR_IS_VOID(a)              ((a)==NULL)
#define C_CHAR_PTR_IS_VOID(a)         ((a)==NULL)
#define C_UNSIGNED_CHAR_PTR_IS_VOID(a)(a)==NULL)
#define C_SIGNED_CHAR_PTR_IS_VOID(a)  ((a)==NULL)
#define C_SHORT_PTR_IS_VOID(a)        ((a)==NULL)
#define C_INT_PTR_IS_VOID(a)          ((a)==NULL)
#define C_LONG_PTR_IS_VOID(a)         ((a)==NULL)
#define C_UNSIGNED_SHORT_PTR_IS_VOID(a)((a)==NULL)
#define C_UNSIGNED_INT_PTR_IS_VOID(a) ((a)==NULL)
#define C_UNSIGEND_LONG_PTR_IS_VOID(a)((a)==NULL)
#define C_FLOAT_PTR_IS_VOID(a)        ((a)==NULL)
#define C_DOUBLE_PTR_IS_VOID(a)       ((a)==NULL)
#define C_LONG_DOUBLE_PTR_IS_VOID(a)  ((a)==NULL)

#endif





























