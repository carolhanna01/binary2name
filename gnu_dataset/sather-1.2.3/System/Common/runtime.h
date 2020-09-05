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

/* Routines which are provided in runtime.c */

void rt_aset(void *buf, INT bit, BOOL val, INT maxbits);
BOOL rt_aget(void *buf, INT bit, INT maxbits);
FLT rt_flt_aset(FLT f,INT bit,BOOL val);
BOOL rt_flt_aget(FLT f, INT bit);
FLTD rt_fltd_aset(FLTD f, INT bit, BOOL val);
BOOL rt_fltd_aget(FLTD f, INT bit);
void rt_flt_get_rep(FLT f,BOOL* sign,INT* exp,INT* mantissa);
void rt_fltd_get_rep(FLTD f,BOOL* sign,INT* exp,INT* mlo,INT* mhi);
int rt_str_file_open(char *nm);
int rt_str_file_size(int fd);
void rt_str_file_in_str(int fd, char *s, int st, int sz);
void rt_str_file_in_fstr(int fd, char *s, int st, int sz, int bst);
void rt_str_file_close(int fd);
char **rt_create_astr (int size, char *s);

/* These are no longer generated, but left here for bootstrapping purposes. */
void *rt_alloc(size_t size, INT tag);
void *rt_arr_alloc(size_t size1, INT tag, size_t size2, INT n);
void *rt_alloc_atomic(size_t size, INT tag);
void *rt_arr_alloc_atomic(size_t size1, INT tag, size_t size2, INT n);

void rt_segfault_handler();
int rt_fatal(char *file, int line, char *msg);
int rt_fatal_2(char *file, int line, char *msg, char *str);
int rt_fatal2(char *msg);
int rt_fatal2_2(char *msg, char *str);
void rt_start(int clusters, int argc, char *argv[]);
void rt_stop();
/*
int rt_file_open(CHAR *nm);
int rt_file_size(INT fd);
void rt_file_in_str(INT fd, CHAR *s, INT st, INT sz);
void rt_file_in_fstr(INT fd, CHAR *s, INT st, INT sz, INT bst);
void rt_file_close(INT fd);
*/

#ifdef DEBUG
#define S_DEBUG
#endif

#ifdef STATS
# ifndef RUNTIME
   extern int rt_dispatches;
# endif
# define COUNT_DISPATCH rt_dispatches++
#endif

/* It's okay to replicate arguments in macros here - the compiler
 * always makes temporary locals for anything that might be a macro.
*/

#ifdef PRINT_BACKTRACE
# ifdef DEBUG
   int rt_fatal_po(char *,int,char *,struct _func_frame *);
   int rt_fatal_p2_o(char *,int,char *,char *,struct _func_frame *);
#  define FATAL(msg)     rt_fatal_po(__FILE__,__LINE__,msg,&FF)
#  define FATAL2(msg,str) rt_fatal_2_po(__FILE__,__LINE__,msg,str,&FF)
# else
   int rt_fatal_po(char *,struct _func_frame *);
   int rt_fatal_p2_o(char *,char *,struct _func_frame *);
#  define FATAL(msg)     rt_fatal_po(msg,&FF)
#  define FATAL2(msg,str) rt_fatal_2_po(msg,str,&FF)
# endif
#else
# ifdef DEBUG
#  define FATAL(msg)     rt_fatal(__FILE__,__LINE__,msg)
#  define FATAL2(msg,str) rt_fatal_2(__FILE__,__LINE__,msg,str)
# else
#  define FATAL(msg)     rt_fatal2(msg)
#  define FATAL2(msg,str) rt_fatal2_2(msg,str)
# endif
#endif

#define SAMIN(x,y)       (((x)<(y))?(x):(y))

#define CHKERR(x,msg,y) (((x)?FATAL(msg):0),(y))
#define CHKOK(x,msg,y)      (((!(x))?FATAL(msg):0),(y))

#define DESTROYED(x)   ((OB)x)->header.destroyed
#define DESTROY(x)     ((OB)x)->header.destroyed=1
#define VOID(x)        (x)==NULL

#ifdef BOUNDS_CHK
# define CHK_BOUNDS(v,low,high,expr) CHKERR(((v)<(low)||(v)>(high)),"Out of bounds",expr)
#else
# define CHK_BOUNDS(v,low,high,expr) (expr)
#endif

#ifdef DESTROY_CHK
# if defined(VOID_CHK) && !defined(NULL_SEGFAULTS)
#  define CHK(x,y) CHKERR(VOID(x),"Access to void",CHKERR(DESTROYED(x),"Access to destroyed object",(y)))
# else
#  define CHK(x,y) CHKERR(DESTROYED(x),"Access to destroyed object",(y))
# endif
#else
# if defined(VOID_CHK) && !defined(NULL_SEGFAULTS)
#  define CHK(x,y) CHKERR(VOID(x),"Access to void",(y))
# else
#  define CHK(x,y) y
# endif
#endif

/* Macros for FLT and FLTD */

/* We could check for division by zero here, but IEEE says to
 * return NaN, and we don't handle any other IEEE traps so to
 * be consistent just ignore it here.
*/
#define FLTDIV(x,y)    (x/y)
#define FLTDDIV(x,y)   (x/y)

#ifndef ARITH_CHK 
# define FLTINT(x)      (INT)x
# define FLTDINT(x)     (INT)x
#else
# define FLTINT(x)      CHKERR(((INT)x)!=x,"Illegal FLT->INT conversion - implicit loss of precision. Use explicit .ceiling.int or .floor.int ",(INT)x)
# define FLTDINT(x)     CHKERR(((INT)x)!=x,"Illegal FLTD->INT conversion - implicit loss of precision. Use explicit .ceiling.int or .floor.int ",(INT)x)
#endif

#define FLTASET(f,b,v)  CHK_BOUNDS(b,0,31,rt_fltaset(f,b,v))
#define FLTAGET(f,b)    CHK_BOUNDS(b,0,31,rt_fltaget(f,b))
#define FLTDASET(f,b,v) CHK_BOUNDS(b,0,63,rt_fltdaset(f,b,v))
#define FLTDAGET(f,b)   CHK_BOUNDS(b,0,63,rt_fltdaget(f,b))

#define FLTSTORE(f,s)  (sprintf((s)->arr_part,"%g",f),strlen((s)->arr_part))
#define FLTSTOREPREC(f,p,s) (sprintf((s)->arr_part,"%.*g",p,f),strlen((s)->arr_part))
#define FLTDSTORE(f,s)  (sprintf((s)->arr_part,"%g",f),strlen((s)->arr_part))
#define FLTDSTOREPREC(f,p,s) (sprintf((s)->arr_part,"%.*lg",p,f),strlen((s)->arr_part))

#if !defined(FLT_MAX) || !defined(MINFLOAT)
# define FLTMAXNORMAL     r_max_normal()
# define FLTMINNORMAL     r_min_normal()
# define FLTMAXSUBNORMAL  r_max_subnormal()
# define FLTMINSUBNORMAL  r_min_subnormal()
# define FLTDMAXNORMAL    max_normal()
# define FLTDMINNORMAL    min_normal()
# define FLTDMAXSUBNORMAL max_subnormal()
# define FLTDMINSUBNORMAL min_subnormal()
#else
# define FLTMAXNORMAL     FLT_MAX
# define FLTMINORMAL      FLT_MIN
# define FLTMAXSUBNORMAL  MAXFLOAT /* Is this right? */
# define FLTMINSUBNORMAL  MINFLOAT
# define FLTDMAXNORMAL    DBL_MAX
# define FLTDMINORMAL     DBL_MIN
# define FLTDMAXSUBNORMAL MAXDOUBLE /* Is this right? */
# define FLTDMINSUBNORMAL MINDOUBLE
#endif

/* 4.1 AIX native C compiler does not allow to return qualified const types.
   Oddly enough, 3.2 AIX compiler does not mind */
/* This is also true for ULTRIX */
#if (defined(_AIX41) || defined(ultrix)) && !defined(__GNUC__) || (defined(__alpha) && !defined(__GNUC__))
# define RETURNED_CONST
#else
# define RETURNED_CONST const
#endif
 
#ifdef __linux__
# define FLTDEXP10(f)    pow(10.0,f)
                         /* Change 990218, by Nobbi: pow10 not available */
#define iszero(f) ((f)==0.0)
/* static int ilogb(double f) { int i; frexp(f,&i); return i-1; } */
#define isnormal(f) ((int)1)  /* these two are hacks esc*/
#define issubnormal(f) ((int)0)

#elif defined(_AIX) || defined(SUNOS5) || defined(__NeXT__) || defined(__sgi) || defined(ALPHA) || defined(__hpux) || defined(__FreeBSD__) || defined(__alpha) || defined(__CYGWIN32__)
# define FLTDEXP10(f)    pow(10.0,f)
#else
# define FLTDEXP10(f)    exp10(f)
#endif


/* This is from stoehr@informatik.tu-muenchen.de */
#if defined(__hpux) || defined(__linux__) || defined(__NEXT__)
# define cbrt(v) pow(v,(1.0/3.0))
double scalbn(double, int);
#endif

#if defined(SUNOS5) || defined(__hpux) || defined(__FreeBSD__) || defined(__sgi) || defined(__linux__) || defined(__alpha)
double signaling_nan(int sig);
double infinity();
#endif

/* Put other machines which define "aint" here (round to zero).  I
 * don't know why this function isn't defined on most platforms.  AIX
 * calls it "trunc".  "ceil" and "floor" _should_ be defined
 * everywhere, we hope.  Some platforms appear to support the float
 * version truncf (or ftrunc), but don't document it (bastards), so we
 * won't rely on it.  
*/
#if defined(SUNOS4)
# define FLTDTRUNCATE(x) aint(x)
# define FLTTRUNCATE(x) ((FLT)aint((FLTD)x))
#elif defined(_AIX) || defined(__sgi)
# define FLTDTRUNCATE(x) trunc(x)
# define FLTTRUNCATE(x) ((FLT)trunc((FLTD)x))
#else
# define FLTDTRUNCATE(x) ( ((x)<0.0)?ceil(x):floor(x) )
# define FLTTRUNCATE(x) (FLT)FLTDTRUNCATE((FLTD)x)
#endif

#if defined(__hpux) || defined(SCO)
/* This is just a quick hack - real rounding does something better
 * for rounding - somebody send me a correct version. 
 * Should be doing IEEE round to even.
*/
# define FLTDROUND(x)    (fabs(x-floor(x))<0.5)?floor(x):(floor(x)+1)
#else
# define FLTDROUND(x)    rint(x)
#endif
#define FLTROUND(x)      (FLT)(FLTDROUND((FLTD)(x)))

/* IN */
#define INGETSTRSIZED(s,sz) CHK_BOUNDS(sz-1,0,s->loc,fgets((s)->arr_part,sz,stdin))

/* INT */

/* Some of these rely on common subexpression elimination in the C 
 * compiler to allow them to conveniently be macros.  But then, this
 * is the '90s.
 * INTDIV and INTMOD may not be right for all machines/compilers.
 * Someone should take the time to scope out the right set of #ifdefs
 * for this.
*/

#ifndef ARITH_CHK

# define INTPLUS(x,y)    x+y
# define INTMINUS(x,y)   x-y
# define INTTIMES(x,y)   x*y
# define INTDIV(x,y)     (x<0&&x!=(x/y)*y)?(x/y)-1:(x/y)
# define INTMOD(x,y)     ((x%y)<0)?(x%y)+y:(x%y)
/* The following version was introduced by David, 
 * but yields different results when x<0. This breaks
 * the INTI class 
 * # define INTDIV(x,y)     (x>=0||x%y?x/y:y>0?x/y-1:x/y+1)
 * # define INTMOD(x,y)     (x%y>=0?x%y:y<0?x%y-y:x%y+y)
 */
# define INTUPLUS(x,y)   ((unsigned)x)+((unsigned)y)
# define INTUMINUS(x,y)  ((unsigned)x)-((unsigned)y)
# define INTUTIMES(x,y)  ((unsigned)x)*((unsigned)y)
# define INTUDIV(x,y)    ((unsigned)x)/((unsigned)y)
# define INTUMOD(x,y)    ((unsigned)x)%((unsigned)y)
# define INTFLT(x)       (FLT)x
# define INTFLTD(x)      (FLTD)x
# define INTRSHIFT(x,s)  (x<0)?x>>s:(~((~x)>>s))

#else

/* This is a VERY SLOW way of doing arithmetic checking, because
 * it doesn't take advantage of the overflow bits available on almost
 * any platform, but which aren't accessible from C.  It would be a
 * Good Thing to have special cases to exploit, for example, gcc's
 * "asm" extension.  If you feel arithmetic checking is too slow,
 * then please contribute a special case for your platform!
*/

#define INTPLUS(x,y)    CHKOK(((y>=0&&x<=(SINT_MAX-y))||(y<0&&x>=((signed)(((unsigned)SINT_MIN)-y)))),"Integer overflow on plus",x+y)
#define INTMINUS(x,y)   CHKOK(((y>=0&&x>=(SINT_MIN+y))||(y<0&&x<=((signed)(((unsigned)SINT_MAX)+y)))),"Integer overflow on minus",x-y)
#define INTTIMES(x,y)   CHKOK(((x==0)||(y==0)||(x>0&&y>0&&y<=SINT_MAX/x)||(x>0&&y<0&&y>=SINT_MIN/x)||(x<0&&y>0&&x>=SINT_MIN/y)||(x<0&&y<0&&x!=SINT_MIN&&y!=SINT_MIN&&-x<=SINT_MAX/(-y))),"Integer overflow on times",x*y)
#define INTDIV(x,y)     CHKOK(y!=0,"Division by zero",((x<0&&x!=(x/y)*y)?(x/y)-1:(x/y)))
#define INTMOD(x,y)     CHKOK(y!=0,"Mod by zero",((x%y)<0)?(x%y)+y:(x%y))
/* See comments about INTDIV above
 * #define INTDIV(x,y)     CHKOK(y!=0,"Division by zero",(x>=0||x%y?x/y:y>0?x/y-1:x/y+1))
 * #define INTMOD(x,y)     CHKOK(y!=0,"Mod by zero",(x%y>=0?x%y:y<0?x%y-y:x%y+y))
 */
#define INTUPLUS(x,y)   CHKOK(x<=(SUINT_MAX-y),"Integer overflow on unsigned plus",((unsigned)x)+((unsigned)y))
#define INTUMINUS(x,y)  CHKOK(y<=x,"Integer overflow on unsigned minus",((unsigned)x)-((unsigned)y))
#define INTUTIMES(x,y)  CHKOK((x==0)||(y<=SUINT_MAX/x),"Integer overflow on unsigned times",((unsigned)x)*((unsigned)y))
#define INTUDIV(x,y)    CHKOK(y!=0,"Integer unsigned division by zero",((unsigned)x)/((unsigned)y))
#define INTUMOD(x,y)    CHKOK(y!=0,"Integer unsigned mod by zero",((unsigned)x)%((unsigned)y))
#define INTFLT(x)       CHKOK(((INT)((FLT)x))==x,"Integer would overflow conversion to FLT",(FLT)x)
#define INTFLTD(x)      CHKOK(((INT)((FLTD)x))==x,"Integer would overflow conversion to FLTD",(FLTD)x)
#define INTRSHIFT(x,s)  (x<0)?x>>s:(~((~x)>>s))

#endif

/* SYS */

/* (JN) Next two macros are new for building the native compiler */

#define SYSFLTREP(x,y) {	\
  union {int i; float f;} cast;	\
  cast.f = x;			\
  *y = cast.i;			\
};

#define SYSFLTDREP(x,y,z) {		\
  union {				\
    struct {int msb; int lsb;} i;	\
    double f;				\
  } cast;				\
  cast.f = x;				\
  *y = cast.i.msb;			\
  *z = cast.i.lsb;			\
};


#ifndef DESTROY_CHK 

# define SYSDESTROY(x)   ZFREE(x)
# define ATTR(x,y)       (x)->y
# define ATTRs(x,y,n)    ((x)==NULL?(n):((x)->y))
# define SATTR(x,y,z)    (x)->y=z
# define TAG(x)          ((OB)x)->header.tag
# define ASIZE(x)        (x)->asize
# define VASIZE(x)       (x ## _asize) /* for value types, x must be the type */
# define ARR(x,y)        CHK_BOUNDS(y,0,ASIZE(x)-1,(x)->arr_part[y])
# define VARR(T,x,y)     CHK_BOUNDS(y,0,VASIZE(T)-1,(x).arr_part[y])
# define SARR(x,y,z)     CHK_BOUNDS(y,0,ASIZE(x)-1,(x)->arr_part[y]=z)
# define VSARR(T,x,y,z)  CHK_BOUNDS(y,0,VASIZE(T)-1,(x).arr_part[y]=z)

#else

# ifdef NULL_SEGFAULTS
/* if accesses of NULL pointers are guaranteed to segfault, we can
 * avoid generating code in-line to check for this.
*/
#  define SYSDESTROY(x)   CHKERR(DESTROYED(x),"Tried to destroy already destroyed object",DESTROY(x))
#  define ATTR(x,y)       CHKERR(DESTROYED(x),"Attr access of destroyed object",x->y)
#  define ATTRs(x,y,n)      ((x)==NULL?(n):((x)->y))
#  define SATTR(x,y,z)    CHKERR(DESTROYED(x),"Attr write access of destroyed object",x->y=z)
#  define TAG(x)          CHKERR(DESTROYED(x),"Tag access of destroyed object",((OB)x)->header.tag)
#  define ASIZE(x)        CHKERR(DESTROYED(x),"Asize access of destroyed object",(x)->asize)
#  define VASIZE(x)       (x ## _asize) /* for value types, x must be the type */
#  define ARR(x,y)        CHKERR(DESTROYED(x),"Array access of destroyed object",CHK_BOUNDS(y,0,ASIZE(x)-1,(x)->arr_part[y]))
#  define VARR(T,x,y)     CHK_BOUNDS(y,0,VASIZE(T)-1,(x).arr_part[y])
#  define SARR(x,y,z)     CHKERR(DESTROYED(x),"Array write access of destroyed object",CHK_BOUNDS(y,0,ASIZE(x)-1,(x)->arr_part[y]=z))
#  define VSARR(T,x,y,z)  CHK_BOUNDS(y,0,VASIZE(T)-1,(x).arr_part[y]=z)

# else
#ifdef __GNUC__
/* If this is GNU C, use the hash directives, which provide arg names */
#  define SYSDESTROY(x)   CHKERR(VOID(x),"Tried to destroy void",CHKERR(DESTROYED(x),"Tried to destroy already destroyed object",DESTROY(x)))
#  define ATTR(x,y)       CHKERR(VOID(x),"Attr access of void " #x "." #y,CHKERR(DESTROYED(x),"Attr access of destroyed object",((x)->y)))
#  define ATTRs(x,y,n)    ((x)==NULL?(n):((x)->y))
#  define SATTR(x,y,z)    CHKERR(VOID(x),"Attr write access of void " #x "." #y "=" #z,CHKERR(DESTROYED(x),"Attr write access of destroyed object",x->y=z))
#  define TAG(x)          CHKERR(VOID(x),"Tag access of void " #x,CHKERR(DESTROYED(x),"Tag access of destroyed object",((OB)x)->header.tag))
#  define ASIZE(x)        CHKERR(VOID(x),"Asize access of void " #x,CHKERR(DESTROYED(x),"Asize access of destroyed object",(x)->asize))
#  define VASIZE(x)       (x ## _asize) /* for val types, x must be the type */
#  define ARR(x,y)        CHKERR(VOID(x),"Array access of void " #x "[" #y "]",CHKERR(DESTROYED(x),"Array access of destroyed object",CHK_BOUNDS(y,0,ASIZE(x)-1,(x)->arr_part[y])))
#  define VARR(T,x,y)     CHK_BOUNDS(y,0,VASIZE(T)-1,(x).arr_part[y])
#  define SARR(x,y,z)     CHKERR(VOID(x),"Array write access of void " #x "[" #y "]=" #z,CHKERR(DESTROYED(x),"Array write access of destroyed object",CHK_BOUNDS(y,0,ASIZE(x)-1,(x)->arr_part[y]=z)))
#  define VSARR(T,x,y,z)  CHK_BOUNDS(y,0,VASIZE(T)-1,(x).arr_part[y]=z)
#else
/* If not GNU */
#  define SYSDESTROY(x)   CHKERR(VOID(x),"Tried to destroy void",CHKERR(DESTROYED(x),"Tried to destroy already destroyed object",DESTROY(x)))
#  define ATTR(x,y)       CHKERR(VOID(x),"Attr access of void",CHKERR(DESTROYED(x),"Attr access of destroyed object",((x)->y)))
#  define ATTRs(x,y,n)    ((x)==NULL?(n):((x)->y))
#  define SATTR(x,y,z)    CHKERR(VOID(x),"Attr write access of void",CHKERR(DESTROYED(x),"Attr write access of destroyed object",x->y=z))
#  define TAG(x)          CHKERR(VOID(x),"Tag access of void",CHKERR(DESTROYED(x),"Tag access of destroyed object",((OB)x)->header.tag))
#  define ASIZE(x)        CHKERR(VOID(x),"Asize access of void",CHKERR(DESTROYED(x),"Asize access of destroyed object",(x)->asize))
#  define VASIZE(x)       (x ## _asize) /* for value types, x must be the type */
#  define ARR(x,y)        CHKERR(VOID(x),"Array access of void",CHKERR(DESTROYED(x),"Array access of destroyed object",CHK_BOUNDS(y,0,ASIZE(x)-1,(x)->arr_part[y])))
#  define VARR(T,x,y)     CHK_BOUNDS(y,0,VASIZE(T)-1,(x).arr_part[y])
#  define SARR(x,y,z)     CHKERR(VOID(x),"Array write access of void",CHKERR(DESTROYED(x),"Array write access of destroyed object",CHK_BOUNDS(y,0,ASIZE(x)-1,(x)->arr_part[y]=z)))
#  define VSARR(T,x,y,z)  CHK_BOUNDS(y,0,VASIZE(T)-1,(x).arr_part[y]=z)
#endif

# endif
#endif

/*
 * define some macros used in pSather, which are also used in Sather
 * (for compatibility reasons)
 */
#ifndef PSATHER
#define READTAG(t,obj)	((t)=TAG(obj))
#endif


#ifdef DETERMINISTIC
extern INT ob_count;
# define SYSID(x)        CHK(x,CHKOK(((OB)x)->header.tag>=0,"Called SYS::id on value type",((OB)x)->header.id))
#else
/* Chop off bits from right side.  The exect number depends on whether the
 * allocator aligns things strongly.  If this isn't true, then
 * a smaller number of bits (like 2) might be more appropriate.
*/
# define SYSID(x)        CHK(x,CHKOK(((OB)x)->header.tag>=0,"Called SYS::id on value type",(INT)(((unsigned long)x)>>3)))
#endif

#define SYSTP(x)        TAG(x)
#define SYSSTRFORTP(x)  gen_SYS_str_for_tp(x)
#define SYSOBEQ(x,y)    (x==y)||(x!=NULL)&&(y!=NULL)&&(((OB)x)->header.tag==((OB)y)->header.tag)&&gen_SYS_ob_eq((OB)x,(OB)y)
#define SYSEXTOBFOR(x)  (void*)x 

/* CHAR */

#define CHARAGET(c,b)   CHK_BOUNDS(b,0,7,(CHAR)((c&(1<<b))!=0))
#define CHARASET(c,b,v) CHK_BOUNDS(b,0,7,(CHAR)((c&(~(1<<b)))|(v<<b)))

/* FSTR */

#define FSTRASET(f,i,c) CHK(f,CHK_BOUNDS(i,0,f->loc-1,(f)->arr_part[i]=c))
#define FSTRAGET(f,i)   CHK(f,CHK_BOUNDS(i,0,f->loc-1,(f)->arr_part[i]))
#define FSTRACOPY(f,s)  CHK(s,CHK(f,CHK_BOUNDS(ASIZE(s),0,ASIZE(f),(memcpy((f)->arr_part,(s)->arr_part,ASIZE(s))))))
#define FSTRACOPYF(f,f2) CHK(f2,CHK(f,CHK_BOUNDS(f2->loc,0,ASIZE(f),(memcpy((f)->arr_part,(f2)->arr_part,f2->loc)))))
#define FSTRACOPYN(f,s,n) CHK(s,CHK(f,CHK_BOUNDS(n,0,ASIZE(f),CHK_BOUNDS(n,0,ASIZE(s),(memcpy((f)->arr_part,(s)->arr_part,n))))))
#define FSTRACOPYNF(f,f2,n) CHK(f2,CHK(f,CHK_BOUNDS(n,0,ASIZE(f),CHK_BOUNDS(n,0,f2->loc,(memcpy((f)->arr_part,(f2)->arr_part,n))))))
#define FSTRACOPYIS(f,i,s) CHK(s,CHK(f,CHK_BOUNDS(i,0,ASIZE(f)-1,memcpy((f)->arr_part+i,(s)->arr_part,SAMIN(ASIZE(f)-i,ASIZE(s))))))
#define FSTRACOPYIF(f,i,f2) CHK(f2,CHK(f,CHK_BOUNDS(i,0,ASIZE(f)-1,memcpy((f)->arr_part+i,(f2)->arr_part,SAMIN(ASIZE(f)-i,f2->loc)))))

/* STR */

#define STRAGET(s,i)    CHK(s,CHK_BOUNDS(i,0,ASIZE(s)-1,(s)->arr_part[i]))
#define STRACOPYNF(s,f,n) CHK(s,CHK(f,CHK_BOUNDS(n,0,ASIZE(s),CHK_BOUNDS(n,0,f->loc,memcpy((s)->arr_part,(f)->arr_part,n)))))
#define STRACOPYN(s,s2,n) CHK(s,CHK(s2,CHK_BOUNDS(n,0,ASIZE(s),CHK_BOUNDS(n,0,ASIZE(s2),memcpy((s)->arr_part,(s2)->arr_part,n)))))
#define STRACOPYIS(s,i,s2) CHK(s2,CHK(s,CHKOK((ASIZE(s2)==0)||((i>=0)&&(i<ASIZE(s))),"Bad precondition for STR::acopy",memcpy((s)->arr_part+i,(s2)->arr_part,SAMIN(ASIZE(s)-i,ASIZE(s2))))))
#define STRISEQHELPER(s,s2,len) (memcmp((s)->arr_part,(s2)->arr_part,len)==0)

/* AREF */
#define AREFACOPY(s,f)   CHK(s,CHK(f,memcpy((s)->arr_part,(f)->arr_part,sizeof((s)->arr_part[0])*SAMIN(ASIZE(s),ASIZE(f)))))
#define AREFACOPYB(s,b,f) CHK(s,CHK(f,CHK_BOUNDS(b,0,ASIZE(s),memcpy((s)->arr_part+b,(f)->arr_part,sizeof((s)->arr_part[0])*SAMIN(ASIZE(s)-b,ASIZE(f))))))
#define AREFACOPYBN(s,b,n,f) CHK(s,CHK(f,CHK_BOUNDS(b,0,ASIZE(s),CHK_BOUNDS(b+n,0,ASIZE(s),CHK_BOUNDS(n,0,ASIZE(f),memcpy((s)->arr_part+b,(f)->arr_part,sizeof((s)->arr_part[0])*n))))))
#define AREFACOPYBNS(s,b,n,sr,f) CHK(s,CHK(f,CHK_BOUNDS(b,0,ASIZE(s),CHK_BOUNDS(b+n,0,ASIZE(s),CHK_BOUNDS(sr,0,ASIZE(f),CHK_BOUNDS(n+sr,0,ASIZE(f),memcpy((s)->arr_part+b,(f)->arr_part+sr,sizeof((s)->arr_part[0])*n)))))))
#define AREFACLEAR(s)    CHK(s,memset((s)->arr_part,0,sizeof((s)->arr_part[0])*ASIZE(s)))

/* AVAL */
#define AVALACOPY(T,s,f)   CHK(s,CHK(f,memcpy(s.arr_part,f.arr_part,sizeof(s.arr_part[0])*VASIZE(T))))
#define AVALACOPYB(T,s,b,f) CHK(s,CHK(f,CHK_BOUNDS(b,0,VASIZE(T),memcpy(s.arr_part+b,f.arr_part,sizeof(s.arr_part[0])*VASIZE(T)-b))))
#define AVALACOPYBN(T,s,b,n,f) CHK(s,CHK(f,CHK_BOUNDS(b,0,VASIZE(T),CHK_BOUNDS(b+n,0,VASIZE(T),CHK_BOUNDS(n,0,VASIZE(T),,memcpy(s.arr_part+b,f.arr_part,sizeof(s.arr_part[0])*n))))))
#define AVALACOPYBNS(T,s,b,n,sr,f) CHK(s,CHK(f,CHK_BOUNDS(b,0,VASIZE(T),CHK_BOUNDS(b+n,0,VASIZE(T),CHK_BOUNDS(sr,0,VASIZE(T),CHK_BOUNDS(n+sr,0,VASIZE(T),memcpy(s.arr_part+b,f.arr_part+sr,sizeof(s.arr_part[0])*n)))))))
