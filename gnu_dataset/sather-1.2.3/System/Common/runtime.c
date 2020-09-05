/*------------------------->  ANSI C - sourcefile  <-------------------------*/
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

#include <stdlib.h>
#include <string.h>
#include <sys/types.h>
#include <sys/file.h>
#include <fcntl.h>
#include <stdio.h>
#include <memory.h>
#define RUNTIME
#include "sather.h"
#include "tags.h"

#ifdef _POSIX_SOURCE
#include <signal.h>
#endif

/* We define a dummy function frame here, just to make the compiler
   happy and it helps the debugger too. (it is used in some of
   the macros*/
struct _func_frame FF;

/* Set (or clear) a bit.  It's a fatal error for bit to be less than
 * zero or greater than or equal to maxbits.  This implementation
 * is probably endian dependent.  If anyone knows a more portable
 * way to do this, let me know.
*/

void rt_aset(void *buf, INT bit, BOOL val, INT maxbits)
{
    INT i,j,mask,*p;
    if (bit<0||bit>=maxbits) FATAL("aset index out of range");
    i = bit/(sizeof(INT)*8);
    j = bit%(sizeof(INT)*8);
    mask = ~(1<<j);
    p = ((INT*) buf)+i;
    *p = ((*p)&mask)|(val<<j);
}

BOOL rt_aget(void *buf, INT bit, INT maxbits)
{
    INT i,j,*p;
    if (bit<0||bit>=maxbits) FATAL("aget index out of range");
    i = bit/(sizeof(INT)*8);
    j = bit%(sizeof(INT)*8);
    p = ((INT*) buf)+i;
    return (BOOL) (((*p)&(1<<j))!=0);
}

FLT rt_flt_aset(FLT f,INT bit,BOOL val)
{
  rt_aset(&f,bit,val,32); 
  return f;
}

BOOL rt_flt_aget(FLT f, INT bit) 
{ 
  return rt_aget(&f, bit, 32);
}

FLTD rt_fltd_aset(FLTD f, INT bit, BOOL val)
{
  rt_aset(&f, bit, val, 64);
  return f;
}
 
BOOL rt_fltd_aget(FLTD f, INT bit)
{
 return rt_aget(&f, bit, 64);
}

/* Get the internal representation from floating point numbers */
/* Conforming to IEEE 754. Endian independant: thanks to Erik Schnetter */
void rt_flt_get_rep(FLT f,BOOL* neg,INT* ex,INT* m)
{
  int i=1;
  if (*(char*)&i) {		/* little endian */
    union {
      FLT f;
      struct {
	/* This piece of code is from the file "ieeefp.h" which is part
	   of the GNU C Library, (C) Free Software Foundation, Inc. */
	unsigned int mantissa:23;
	unsigned int exponent:8;
	unsigned int negative:1;
      } fields;
    } x;
    x.f = f;
    *neg = x.fields.negative;
    *ex = x.fields.exponent;
    *m = x.fields.mantissa;
  } else {			/* big endian */
    union {
      FLT f;
      struct {
	/* This piece of code is from the file "ieeefp.h" which is part
	   of the GNU C Library, (C) Free Software Foundation, Inc. */
	unsigned int negative:1;
	unsigned int exponent:8;
	unsigned int mantissa:23;
      } fields;
    } x;
    x.f = f;
    *neg = x.fields.negative;
    *ex = x.fields.exponent;
    *m = x.fields.mantissa;
  }
}

void rt_fltd_get_rep(FLTD f,BOOL* neg,INT* ex,INT* m1, INT* m0)
{
  int i=1;
  if (*(char*)&i) {		/* little endian */
    union {
      FLTD f;
      struct {
	/* This piece of code is from the file "ieee754.h" which is part
	   of the GNU C Library, (C) Free Software Foundation, Inc. */
	unsigned int mantissa1:32;
	unsigned int mantissa0:20;
	unsigned int exponent:11;
	unsigned int negative:1;
      } fields;
    } x;
    x.f = f;
    *neg = x.fields.negative;
    *ex = x.fields.exponent;
    *m0 = x.fields.mantissa0;
    *m1 = x.fields.mantissa1;
  } else {			/* big endian */
    union {
      FLTD f;
      struct {
	/* This piece of code is from the file "ieee754.h" which is part
	   of the GNU C Library, (C) Free Software Foundation, Inc. */
	unsigned int negative:1;
	unsigned int exponent:11;
	unsigned int mantissa0:20;
	unsigned int mantissa1:32;
      } fields;
    } x;
    x.f = f;
    *neg = x.fields.negative;
    *ex = x.fields.exponent;
    *m0 = x.fields.mantissa0;
    *m1 = x.fields.mantissa1;
  }
}

/* Open a file for reading. */
int rt_str_file_open(char *nm){return open(nm,O_RDONLY,0);}

/* Return the size of the file with descriptor `fd'. */
int rt_str_file_size(int fd){return (int)lseek(fd,0L,2);}

/* Fill in `s' with the characters of `fd' starting at `st' and
going for `sz' chars. */
void rt_str_file_in_str(int fd, char *s, int st, int sz)
{lseek(fd,st,0); read(fd,s,sz);}

/* Fill in `s' with the characters of `fd' starting at `st' and
going for `sz' chars, start at an offset of `bst' in `s'. */
void rt_str_file_in_fstr(int fd, char *s, int st, int sz, int bst)
{lseek(fd,st,0); read(fd,s+bst,sz);}

/* Close the file described by descriptor `fd'. */
void rt_str_file_close(int fd){close(fd);}

/* Split concatinate strings separated by '\0' into array of string in C. */
char **
rt_create_astr (int size, char *s)
{
  char **ptr;
  int i;

  ptr = (char **) malloc (sizeof (char *) * size);
  for (i = 0; (i < size) && (*s != 0); i++)
    {
      ptr[i] = s;
      s = (char *) (s + strlen(s) + 1);
    }
  return  ptr;
}

/* For exception handling, a global storing the state information
 * needed to longjmp back to the dynamically most recent protect
 * statement.  Also needed is a global for returning the exception
 * value.
*/

jmp_buf last_protect;
void *exception;

#ifdef DETERMINISTIC
INT ob_count;
#endif

/* These four allocation routines are now macros, but left here
   for bootstrapping purposes. */
void *rt_alloc(size_t size, INT tag) {
    void *res;
    /* printf("sbi_alloc %d %d\n",size,tag); */
    res=(void *) ZALLOC(size);
    if (res==NULL) FATAL("Unable to allocate more memory");
    ((OB)res)->header.tag=tag;
#ifdef DESTROY_CHK
    ((OB)res)->header.destroyed=0;  /* Just allocated - may have to do this without GC */
#endif

#ifdef DETERMINISTIC
    ((OB)res)->header.id=ob_count++;
#endif
    return res;
}

void *rt_arr_alloc(size_t size1, INT tag, size_t size2, INT n) {
    void *res;
    /* printf("sbi_arr_alloc %d %d %d %d\n",size1,tag,size2,n); */
    res=(void *) ZALLOC_BIG(size1+(n-1)*size2);
    if (res==NULL) FATAL("Unable to allocate more memory");
    ((OB)res)->header.tag=tag;
#ifdef DESTROY_CHK
    ((OB)res)->header.destroyed=0;  /* Just allocated - may have to do this without GC */
#endif
#ifdef DETERMINISTIC
    ((OB)res)->header.id=ob_count++;
#endif
    return res;
}

void *rt_alloc_atomic(size_t size, INT tag) {
     void *res;
     /* printf("sbi_alloc_atomic %d %d\n",size,tag); */
     res=(void *) ZALLOC_LEAF(size);
     /* we must clear it manually */
     if (res==NULL) FATAL("Unable to allocate more memory");
     memset(res,0,size);
     ((OB)res)->header.tag=tag;
#ifdef DESTROY_CHK
    ((OB)res)->header.destroyed=0;  /* Just allocated - may have to do this without GC */
#endif
#ifdef DETERMINISTIC
     ((OB)res)->header.id=ob_count++;
#endif
     return res;
}
 
void *rt_arr_alloc_atomic(size_t size1, INT tag, size_t size2, INT n) {
     void *res;
     /* printf("sbi_arr_alloc %d %d %d %d\n",size1,tag,size2,n); */
     size_t size = size1+(n-1)*size2;
     /* Allocate one more byte than necessary, to ensure that
     *  strings are always null terminated.
     */
     res=(void *) ZALLOC_LEAF_BIG(size+1);
     if (res==NULL) FATAL("Unable to allocate more memory");
     memset(res,0,size);
     ((OB)res)->header.tag=tag;
#ifdef DESTROY_CHK
    ((OB)res)->header.destroyed=0;  /* Just allocated - may have to do this without GC */
#endif
#ifdef DETERMINISTIC
     ((OB)res)->header.id=ob_count++;
#endif
     return res;
}
 

/* This catches void references */

void rt_segfault_handler()
{
  FATAL("Attribute or array access of void");
}

char *sather_prog_name;
#ifdef PSATHER
volatile static int gdb_already_started=0;
void remote_gdb(vnn_t from)
{
	char com[300];
	if(gdb_already_started) return;
	gdb_already_started=1;
	if(getenv("START_GDB")==NULL) return;
	fprintf(stderr,"Starting gdb for cluster %d ...\n",HERE);
	fflush(stderr);
	if(getenv("START_GDB_COMMAND")!=NULL) sprintf(com,"%s %s %d",getenv("START_GDB_COMMAND"),sather_prog_name,getpid());
	else sprintf(com,"xterm -fn 7x13 -T \"gdb for cluster %d\" -e gdb %s %d&",HERE,sather_prog_name,getpid());
	printf("system(%s)\n",com);
	system(com);
}
#endif

void start_gdb(int stop)
{
	short c;
#ifdef PSATHER
	int i;
	
	extern void break_psather(void *);
	void volatile (*f)(void *)=(void *)break_psather;

	if(getenv("START_GDB")==NULL) return;
	/*
	for(i=0;i<CLUSTERS;i++)
		if(i!=HERE) am_request_0(i,remote_gdb);
		*/
	remote_gdb(HERE);
	/* we don't stop the cluster in pSather, but we call the same function
	   in an endless loop (the function should not be inlined, so that
	   we can use a breakpoint in gdb to stop this thread) */
	while(1) {(*f)(&f);}
	
#else

	if(getenv("START_GDB")==NULL) return;
	fprintf(stderr,"Please type 'g' if you want to start gdb,\nany other key will abort this program\n");
	fflush(stderr);
	c=getchar();
	if(c=='g') {
		char com[200];
		fprintf(stderr,"Starting gdb ...\n");
		fflush(stderr);
		sprintf(com,"gdb %s %d",sather_prog_name,getpid());
		/*
		if(!stop) { if(fork()==0) { system(com);exit(0); } }
		else system(com);
		*/
		if(fork()==0) { system(com);exit(0); }
		if(stop) while(1) sleep(1);
	}

#endif
}

/* This isn't declared void so we won't get warnings when we
 * "use" it's result in-line during arithmetic and bounds checks
*/
int rt_fatal(char *file, int line, char *msg) {
  fprintf(stderr,"%s:%d: Runtime error - %s\n",file,line,msg);
  fflush(stderr);
  start_gdb(1);
#ifdef PSATHER
  PSATHER_ABORT;
#endif
  abort();
}
int rt_fatal_2(char *file, int line, char *msg,char *str) {
  fprintf(stderr,"%s:%d: Runtime error - %s, %s\n",file,line,msg,str);
  fflush(stderr);
  start_gdb(1);
#ifdef PSATHER
  PSATHER_ABORT;
#endif
  abort();
}
/* rt_fatal2 is used when -debug wasn't given so there's no
 * symbolic info to print
*/
int rt_fatal2(char *msg) {
  fprintf(stderr,"Runtime error - %s\n",msg);
  fprintf(stderr,"(Use \"-debug\" to compile symbolic info)\n");
  fflush(stderr);
  start_gdb(1);
#ifdef PSATHER
  PSATHER_ABORT;
#endif
  abort();
}
int rt_fatal2_2(char *msg,char *str) {
  fprintf(stderr,"Runtime error - %s, %s\n",msg,str);
  fprintf(stderr,"(Use \"-debug\" to compile symbolic info)\n");
  fflush(stderr);
  start_gdb(1);
#ifdef PSATHER
  PSATHER_ABORT;
#endif
  abort();
}

/* Statistics collection */

#ifdef STATS
int rt_dispatches=0;
time_t time_at_start;
#endif

void gdb_signal_handler(int sig) 
{
	static int in_handler=0;
#ifdef PSATHER
	fprintf(stderr,"CLUSTER %d: ",HERE);
#endif
	fprintf(stderr,"signal %d caught ... \n",sig);
	fflush(stderr);
	if(getenv("START_GDB")==NULL) abort();
	if(!in_handler) {
		in_handler=1;
		if(sig==3) start_gdb(0);
		else start_gdb(1);
	} else if(sig!=3) while(1);
	if(sig!=3) abort();
}

void rt_start(int clusters, int argc, char *argv[]) {

#ifdef PSATHER
	signal(SIGQUIT,(void*)remote_gdb);
#else
	signal(SIGQUIT,gdb_signal_handler);
#endif
	signal(SIGILL,gdb_signal_handler);
	signal(SIGBUS,gdb_signal_handler);
	signal(SIGSEGV,gdb_signal_handler);

#if defined(__linux__) 	
	/* esc: Ignore the SIGFPE signal (arithmetic error).*/
	signal(SIGFPE,SIG_IGN);
#else
	signal(SIGFPE,gdb_signal_handler);
#endif
	/* These two do not always exist*/

#ifdef SIGEMT
	signal(SIGEMT,gdb_signal_handler);
#endif

#ifdef SIGSYS
	signal(SIGSYS,gdb_signal_handler);
#endif

#ifdef STATS
	time_at_start=time(NULL);
#endif

#ifdef SIVA
	SI_init(clusters,argc,argv);
#endif

#ifdef GC_ENABLE_INCREMENTAL
	GC_enable_incremental();
#endif
}

void rt_stop() {
#ifdef STATS
    fprintf(stderr,"\nRuntime statistics:\n\n");
    fprintf(stderr,"    Dispatched calls:      %d\n",rt_dispatches);
# ifndef SIVA
    fprintf(stderr,"    Number of collections: %d\n",GC_gc_no);
    GC_gcollect();
    fprintf(stderr,"    Live heap at end:      %d Kb\n",
	                                   GC_get_heap_size()/1024);
# endif
    fprintf(stderr,"    Execution wall time:   %d secs\n\n",
	                                   time(NULL)-time_at_start);
#endif
}

/* This is from stoehr@informatik.tu-muenchen.de */
#if defined(__hpux) || defined(__linux__) || defined(__NEXT__)
double scalbn(double v, int n)
{
    int i,nn;
    double res;

    res = 1.0;
    nn = abs(n);
    for (i=0; i<nn; i++)
        res += res;
    if (n<0) return(v / res);
    return(v*res);
}
#endif
#if defined (hpux)
char *getwd(char *a)
{
    return(getcwd(a,1023));
}
#endif


/*#if defined(SUNOS5) || defined(__hpux) || defined(__sgi) || defined(__linux__) defined(__NetBSD__) || defined(__FreeBSD__) || defined(__OpenBSD__) */

/* If IEEE FP support is present, please add the needed checks below for
   "better" FP behavior */
#if ~defined(SUNOS4)
double signaling_nan(int sig) { return 0.0/0.0; }
double infinity() { return 1.0/0.0; }
#endif


/* Open a file for reading. */
int rt_file_open(char *nm){return open(nm,O_RDONLY,0);}

/* Return the size of the file with descriptor `fd'. */
int rt_file_size(int fd){return (int)lseek(fd,0L,2);}

/* Fill in `s' with the characters of `fd' starting at `st' and
going for `sz' chars. */
void rt_file_in_str(int fd, char *s, int st, int sz)
{lseek(fd,st,0); read(fd,s,sz);}

/* Fill in `s' with the characters of `fd' starting at `st' and
going for `sz' chars, start at an offset of `bst' in `s'. */
void rt_file_in_fstr(int fd, char *s, int st, int sz, int bst)
{lseek(fd,st,0); read(fd,s+bst,sz);}

/* Close the file described by descriptor `fd'. */
void rt_file_close(int fd){close(fd);}

/* for RISCos5.01 BSD43, added by yoshida@agusa.nuie.nagoya-u.ac.jp*/
#if defined(V_RISCOS) && defined(R_5_0)
# ifdef feof
#  undef feof
# endif
# ifdef ferror
#  undef ferror
# endif

int feof(FILE *p) { return (((p)->_flag&_IOEOF)!=0); }
int ferror(FILE *p) { return (((p)->_flag&_IOERR)!=0); }
#endif
