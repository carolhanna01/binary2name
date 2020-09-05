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

#ifndef _FORTRAN_H_
#define _FORTRAN_H_

typedef long int    F_INTEGER;
typedef long int    F_LOGICAL;
typedef float       F_REAL;
typedef double      F_DOUBLE;
typedef char        F_CHARACTER;  /*immutable FORTRAN CHARACTER*1 type*/

typedef long int   F_LENGTH;  /* length of FOTRTAN CHARACTER types */

typedef struct {
  F_REAL re, im;
} F_COMPLEX_struct;
typedef F_COMPLEX_struct F_COMPLEX;

static F_COMPLEX F_COMPLEX_zero;

typedef struct {
  F_DOUBLE re, im;
} F_DOUBLE_COMPLEX_struct;
typedef F_DOUBLE_COMPLEX_struct F_DOUBLE_COMPLEX;
static F_DOUBLE_COMPLEX F_DOUBLE_COMPLEX_zero;

typedef struct {
  F_CHARACTER *address;  /*Fortran string address*/
  F_LENGTH size;
} F_STRING_struct;
typedef F_STRING_struct *F_STRING;

typedef struct F_HANDLER_struct {
  OB_HEADER header;
  void (*funcptr)(void *);
} *F_HANDLER;

/* A pretty dumn definition for F_ROUT */
/* It is just fine to be passed to Fortran as is */
typedef void *F_ROUT;  

/* FORTRAN ARRAYS */
typedef F_INTEGER *F_ARRAY_F_INTEGER;
typedef F_INTEGER *F_ARRAY2_F_INTEGER;
typedef F_INTEGER *F_ARRAY3_F_INTEGER;

typedef F_LOGICAL *F_ARRAY_F_LOGICAL;
typedef F_LOGICAL *F_ARRAY2_F_LOGICAL;
typedef F_LOGICAL *F_ARRAY3_F_LOGICAL;

typedef F_REAL *F_ARRAY_F_REAL;
typedef F_REAL *F_ARRAY2_F_REAL;
typedef F_REAL *F_ARRAY3_F_REAL;

typedef F_DOUBLE *F_ARRAY_F_DOUBLE;
typedef F_DOUBLE *F_ARRAY2_F_DOUBLE;
typedef F_DOUBLE *F_ARRAY3_F_DOUBLE;

typedef F_CHARACTER *F_ARRAY_F_CHARACTER;
typedef F_CHARACTER *F_ARRAY2_F_CHARACTER;
typedef F_CHARACTER *F_ARRAY3_F_CHARACTER;

typedef F_COMPLEX *F_ARRAY_F_COMPLEX;
typedef F_COMPLEX *F_ARRAY2_F_COMPLEX;
typedef F_COMPLEX *F_ARRAY3_F_COMPLEX;

typedef F_DOUBLE_COMPLEX *F_ARRAY_F_DOUBLE_COMPLEX;
typedef F_DOUBLE_COMPLEX *F_ARRAY2_F_DOUBLE_COMPLEX;
typedef F_DOUBLE_COMPLEX *F_ARRAY3_F_DOUBLE_COMPLEX;

typedef struct F_INTEGER_boxed_struct { OB_HEADER header; F_INTEGER immutable_part; } *F_INTEGER_boxed;
typedef struct F_LOGICAL_boxed_struct { OB_HEADER header; F_LOGICAL immutable_part; } *F_LOGICAL_boxed;
typedef struct F_REAL_boxed_struct { OB_HEADER header; F_REAL immutable_part; } *F_REAL_boxed;
typedef struct F_DOUBLE_boxed_struct { OB_HEADER header; F_DOUBLE immutable_part; } *F_DOUBLE_boxed;
typedef struct F_CHARACTER_boxed_struct { OB_HEADER header; F_CHARACTER immutable_part; } *F_CHARACTER_boxed;
typedef struct F_COMPLEX_boxed_struct { OB_HEADER header; F_COMPLEX immutable_part; } *F_COMPLEX_boxed;
typedef struct F_DOUBLE_COMPLEX_boxed_struct { OB_HEADER header; F_DOUBLE_COMPLEX immutable_part; } *F_DOUBLE_COMPLEX_boxed;


/* A maximum string length allowed to be return by fortran calls */
/* This will go when we have F_CARACTER_N types */
#define F_CHARACTER_RETURN_SIZE  32

#define INTF_INTEGER(x)         ((F_INTEGER)(x))
#define F_INTEGERINT(x)         ((INT)(x))
#define F_INTEGERPLUS(a,b)      a+b
#define F_INTEGERMINUS(a,b)     a-b
#define F_INTEGERTIMES(a,b)     a*b
#define F_INTEGERDIV(a,b)       INTDIV(a,b)
#define F_INTEGERIS_LT(a,b)    (a<b)
#define F_INTEGERIS_LEQ(a,b)   (a<=b)
#define F_INTEGERIS_GT(a,b)    (a>b)
#define F_INTEGERIS_GEQ(a,b)   (a>=b)


#define FLTF_REAL(x)            ((F_REAL)(x))
#define F_REALFLT(x)            ((FLT)(x))
#define F_REALPLUS(a,b)         a+b
#define F_REALMINUS(a,b)        a-b
#define F_REALTIMES(a,b)        a*b
#define F_REALDIV(a,b)          a/b
#define F_REALIS_LT(a,b)       (a<b)
#define F_REALIS_LEQ(a,b)      (a<=b)
#define F_REALIS_GT(a,b)       (a>b)
#define F_REALIS_GEQ(a,b)      (a>=b)


#define FLTDF_DOUBLE(x)         ((F_DOUBLE)(x))
#define F_DOUBLEFLTD(x)         ((FLTD)(x))
#define F_DOUBLEPLUS(a,b)       a+b
#define F_DOUBLEMINUS(a,b)      a-b
#define F_DOUBLETIMES(a,b)      a*b
#define F_DOUBLEDIV(a,b)        a/b
#define F_DOUBLEIS_LT(a,b)      (a<b)
#define F_DOUBLEIS_LEQ(a,b)     (a<=b)
#define F_DOUBLEIS_GT(a,b)      (a>b)
#define F_DOUBLEIS_GEQ(a,b)     (a>=b)


#define BOOLF_LOGICAL(x)        ((F_LOGICAL)(x))
#define F_LOGICALBOOL(x)        ((BOOL)(x))
#define F_LOGICALNOT(x)         !x
#define F_LOGICALOR(a,b)        (a||b)
#define F_LOGICALAND(a,b)       (a&&b)
#define F_LOGICALIS_RQ(a,b)     (a==b)


#define CHARF_CHARACTER(x)      ((F_CHARACTER)(x))
#define F_CHARACTERCHAR(x)      ((CHAR)(x))
#define F_CHARACTERIS_LT(a,b)    (a<b)
#define F_CHARACTERIS_LEQ(a,b)   (a<=b)
#define F_CHARACTERIS_GT(a,b)    (a>b)
#define F_CHARACTERIS_GEQ(a,b)   (a>=b)

#define F_STRING_COPY_TO(dest,src,size) memcpy(dest,src,size)


#define F_INTEGER_IS_EQ(a,b)    STD_IS_EQ(a,b)
#define F_REAL_IS_EQ(a,b)       STD_IS_EQ(a,b)
#define F_CHARACTER_IS_EQ(a,b)  STD_IS_EQ(a,b)
#define F_LOGICAL_IS_EQ(a,b)    STD_IS_EQ(a,b)
#define F_DOUBLE_IS_EQ(a,b)     STD_IS_EQ(a,b)
#define F_COMPLEX_IS_EQ(a,b)    (STD_IS_EQ(a.re,b.re)&&STD_IS_EQ(a.im,b.im))
#define F_DOUBLE_COMPLEX_IS_EQ(a,b) (STD_IS_EQ(a.re,b.re)&&STD_IS_EQ(a.im,b.im))
#define F_HANDLERINVOKE_HANDLER(h) (*(h->funcptr))(h)

#define F_INTEGER_zero          (F_INTEGER)0
#define F_REAL_zero             (F_REAL)0.0
#define F_LOGICAL_zero          (F_LOGICAL)0
#define F_DOUBLE_zero           (F_LOGICAL)0.0
#define F_CHARACTER_zero        (F_CHARACTER)CHAR_zero
#define F_STRING_zero    (F_STRING)NULL
#define F_HANDLER_zero          (F_HANDLER)NULL
#define F_ROUT_zero             (F_ROUT)NULL

#define F_INTEGER_IS_VOID(a)    ((a)==0)
#define F_REAL_IS_VOID(a)       ((a)==0.0)
#define F_LOGICAL_IS_VOID(a)    ((a)==0)
#define F_DOUBLE_IS_VOID(a)     ((a)==0.0)
#define F_CHARACTER_IS_VOID(a)  ((a)==F_CHARACTER_zero)
#define F_STRING_is_void(a) ((a)==NULL)
/* These two are wrong */
#define F_COMPLEX_IS_VOID(a)    ((a)==NULL)
#define F_DOUBLE_COMPLEX_IS_VOID(a) ((a)==NULL)

#define F_HANDLER_IS_VOID(a)    ((a)==NULL)
#define F_ROUT_IS_VOID(a)       ((a)==NULL)

#endif




























