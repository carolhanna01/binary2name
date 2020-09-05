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

#include <sys/lwp.h>

/*
** Walk over all roots; ie. all stacks, global space, etc. that might
** nonredundantly point into the zoned storage.  The code should call
** `void mark_from_region(caddr_t start, size_t len)' for each region.
*/

static int procfd;

static caddr_t stacktop, bdata;

void SI_mark_roots() {
   int nummaps, retval, i;
   prmap_t *mappings;
   void mark_from_region(caddr_t start, size_t len);
   void GC_push_regs();
   static jmp_buf env;

#ifdef _REENTRANT
   sigset_t set, old_mask;
   prstatus_t status, *lwpstats;
   lwpid_t my_lwpid;
   int other_lwps_still_running;

   /* Turn off preemption. */
   sigfillset(&set);
   sigprocmask(SIG_SETMASK, &set, &old_mask);

   my_lwpid = _lwp_self();

   /*
   ** Stop all other lwps.  Because lwps may be forked or die out
   ** while doing this, we need to loop until we're sure that we
   ** are the only lwp running.
   */
   other_lwps_still_running = 1;
   while (other_lwps_still_running) {
      retval = ioctl(procfd, PIOCSTATUS, &status);
      if (retval < 0) { perror("Couldn't PIOCSTATUS"); exit(1); }

      /* 
      ** The number of lwps might not be right here, so we fake
      ** it by making room for some extra lwps (100).  This is
      ** a hack, but there doesn't seem to be any way around it.
      */
      lwpstats = malloc(sizeof(prstatus_t)*(status.pr_nlwp+100));
      retval = ioctl(procfd, PIOCLSTATUS, lwpstats);
      if (retval < 0) { perror("Couldn't PIOCLSTATUS"); exit(1); }

      other_lwps_still_running = 0;
      /* Try to suspend the other lwps. */
      for (i=1; i<status.pr_nlwp+1; i++) {
	 if (lwpstats[i].pr_who == my_lwpid) continue;
	 if (!(lwpstats[i].pr_flags & PR_STOPPED)) {
	    if (_lwp_suspend(lwpstats[i].pr_who) < 0) {
	       /* Might fail if lwp exited already */
	    }
	    other_lwps_still_running = 1;
	 }
      }

      if (other_lwps_still_running) free(lwpstats);
   }

   /*
   ** At this point the registers of all the other lwps are
   ** on the heap in the lwpstats status structures.
   */
#endif

   GC_push_regs();      /* Force my registers onto stack. */
   (void) setjmp(env);  /* Just additional paranoia.   */

/* The `solaris-specific' code below doesn't make it through a testplatform
   yet, so it is left commented out. */
#ifdef _REENTRANT
/* #if 1 */
   retval = ioctl(procfd, PIOCNMAP, &nummaps);
   if (retval < 0) { perror("Couldn't PIOCNMAP"); exit(1); }

   /* The +2 is critical; it avoids a Solaris bug in PIOCMAP */
   mappings=(prmap_t*)malloc(sizeof(struct prmap)*(nummaps+2));
   retval = ioctl(procfd, PIOCMAP, mappings);
   if (retval < 0) { perror("Couldn't PIOCMAP"); exit(1); }

   for (i=0; i<nummaps+2; i++) {
      caddr_t start = mappings[i].pr_vaddr;
      u_long len = mappings[i].pr_size;
      u_long f = mappings[i].pr_mflags;
      if (start==NULL) break;
      if (start!=NULL && (f&MA_WRITE) && !(f&MA_SHARED)
	    && ((start<heap) || (start>=heap+total_mapping_size)))
	 mark_from_region(start, len);
   }
   free(mappings);

#else /* Serial */
  {
  /*
  ** Highly solaris-specific stuff.  Assume data segment starts below
  ** _edata, the initialized segment has no roots, and that the heap
  ** is contiguous and continues to the value of sbrk(0).
  */
  extern int _etext, _end, _edata;
  extern int zdebug;
  caddr_t stackbottom = (caddr_t)((word)&stackbottom);
  caddr_t heaptop = sbrk(0);
  /*
  if (zdebug) 
     fprintf(stderr, "\netext %08x  bdata %08x  edata %08x  end %08x  sbrk %08x\nstackbottom %08x  stacktop %08x\n",
	&_etext, bdata, &_edata, &_end, heaptop, stackbottom, stacktop);
  */

  /* Heap and static area */
  mark_from_region(bdata, heaptop - bdata);
  mark_from_region(stackbottom, stacktop - stackbottom);
  }
#endif

#ifdef _REENTRANT
   /* Turn other lwps back on. */
   for (i=1; i<status.pr_nlwp+1; i++) {
      if (lwpstats[i].pr_who == my_lwpid) continue;
      retval = _lwp_continue(lwpstats[i].pr_who);
      if (retval < 0) { perror("_lwp_continue failed"); exit(1); }
   }

   free(lwpstats);

   /* Turn preemption back on. */
   sigprocmask(SIG_SETMASK, &old_mask, NULL);
#endif

}

#define SVR4

/*
** These settings are for the ICSI machine `icsib18', which has 4
** hypersparc modules.
*/

/*
** Find index of first set bit (least significant first), with the least
** significant bit indexed zero.  Results undefined if all bits zero.
**
** Note that Sun also supplies ffs() to do this functionality,
** but disassembly indicates their implementation is naive and poor.
** Perhaps they were worried about bit-length compatability?
*/

unsigned char bytelookuptable[256];
 
union u1 {
   unsigned char bytes[4];
   unsigned int word;
};
 
unsigned int first_bit(word arg) {
   union u1 u;
   unsigned char b, b2, b3, b4;
   u.word = arg;
   b = u.bytes[3];
   if (b) return bytelookuptable[b];
   b2 = u.bytes[2];
   if (b2) return bytelookuptable[b2]+8;
   b3 = u.bytes[1];
   if (b3) return bytelookuptable[b3]+16;
   b4 = u.bytes[0];
   return bytelookuptable[b4]+24;
}

unsigned int naiveffb(unsigned int x) {
   unsigned int c=0;
   unsigned int m=1;
   while (m) {
      if (m&x) return c;
      c++;
      m<<=1;
   }
   abort();
}
 
void init_other() {
   int i, retval, nummaps;
   prmap_t *mappings;
   char buf[80];
   extern int _edata;
   void SI_fatal(char *s);

   /* First position in table is never used */
   for (i=1; i<256; i++)
      bytelookuptable[i] = naiveffb(i);

   sprintf(buf, "/proc/%lu", getpid());
   procfd = open(buf, O_RDONLY);
   if (procfd < 0) { perror("Couldn't open proc file"); exit(1); }

#ifndef _REENTRANT
   /* Find out where the beginning of the data segment is, and avoid
      making these expensive calls ever again.  This only works for the
      serial case, because we don't know where the stacks are when there
      are other threads.  */
   retval = ioctl(procfd, PIOCNMAP, &nummaps);
   if (retval < 0) { perror("Couldn't PIOCNMAP"); exit(1); }
 
   /* The +2 is critical; it avoids a Solaris bug in PIOCMAP */
   mappings=(prmap_t*)malloc(sizeof(struct prmap)*(nummaps+2));
   retval = ioctl(procfd, PIOCMAP, mappings);
   if (retval < 0) { perror("Couldn't PIOCMAP"); exit(1); }
 
   for (i=0; i<nummaps+2; i++) {
      caddr_t start = mappings[i].pr_vaddr;
      u_long len = mappings[i].pr_size;
      u_long f = mappings[i].pr_mflags;
      if (start==NULL) break;
      if (start!=NULL && (f&MA_WRITE) && !(f&MA_SHARED)
            && (start<((caddr_t)(&_edata))) 
	    && (start+len>=((caddr_t)(&_edata)))) {
	 bdata = start;
	 break;
      }
   }
   free(mappings);
   if (bdata==NULL) SI_fatal("Couldn't find start of data segment");
#endif

   stacktop = (caddr_t) ROUND_TO_PAGE((word)&buf);
}

/*
** Stolen and slightly modified from the Boehm collector, to which I am in
** eternal debt.
** 
**  - Dave
*/
#define SPARC

/* 
 * Copyright 1988, 1989 Hans-J. Boehm, Alan J. Demers
 * Copyright (c) 1991-1994 by Xerox Corporation.  All rights reserved.
 *
 * THIS MATERIAL IS PROVIDED AS IS, WITH ABSOLUTELY NO WARRANTY EXPRESSED
 * OR IMPLIED.  ANY USE IS AT YOUR OWN RISK.
 *
 * Permission is hereby granted to use or copy this program
 * for any purpose,  provided the above notices are retained on all copies.
 * Permission to modify the code and to distribute modified code is granted,
 * provided the above notices are retained, and a notice that the code was
 * modified is included with the above copyright notice.
 */
/* Boehm, December 12, 1994 5:03 pm PST */

/* Routine to mark from registers that are preserved by the C compiler. */
/* This must be ported to every new architecture.  There is a generic   */
/* version at the end, that is likely, but not guaranteed to work       */
/* on your architecture.  Run the test_setjmp program to see whether    */
/* there is any chance it will work.                                    */

void GC_push_regs()
{
#       ifdef SPARC
	  {
	      word GC_save_regs_in_stack();
	      
	      /* generic code will not work */
	      (void)GC_save_regs_in_stack();
	  }
#       endif
}

/* On register window machines, we need a way to force registers into 	*/
/* the stack.	Return sp.						*/
# ifdef SPARC
    asm("	.seg 	\"text\"");
#   ifdef SVR4
      asm("	.globl	GC_save_regs_in_stack");
      asm("GC_save_regs_in_stack:");
      asm("	.type GC_save_regs_in_stack,#function");
#   else
      asm("	.globl	_GC_save_regs_in_stack");
      asm("_GC_save_regs_in_stack:");
#   endif
    asm("	ta	0x3   ! ST_FLUSH_WINDOWS");
    asm("	mov	%sp,%o0");
    asm("	retl");
    asm("	nop");
#   ifdef SVR4
      asm("	.GC_save_regs_in_stack_end:");
      asm("	.size GC_save_regs_in_stack,.GC_save_regs_in_stack_end-GC_save_regs_in_stack");
#   endif
#   ifdef LINT
	word GC_save_regs_in_stack() { return(0 /* sp really */);}
#   endif
# endif


/* GC_clear_stack_inner(arg, limit) clears stack area up to limit and	*/
/* returns arg.  Stack clearing is crucial on SPARC, so we supply	*/
/* an assembly version that's more careful.  Assumes limit is hotter	*/
/* than sp, and limit is 8 byte aligned.				*/
#if defined(ASM_CLEAR_CODE) && !defined(THREADS)
#ifndef SPARC
	--> fix it
#endif
# ifdef SUNOS4
    asm(".globl _GC_clear_stack_inner");
    asm("_GC_clear_stack_inner:");
# else
    asm(".globl GC_clear_stack_inner");
    asm("GC_clear_stack_inner:");
    asm(".type GC_save_regs_in_stack,#function");
# endif
  asm("mov %sp,%o2");		/* Save sp	*/
  asm("add %sp,-8,%o3");	/* p = sp-8	*/
  asm("clr %g1");		/* [g0,g1] = 0	*/
  asm("add %o1,-0x60,%sp");	/* Move sp out of the way,	*/
  				/* so that traps still work.	*/
  				/* Includes some extra words	*/
  				/* so we can be sloppy below.	*/
  asm("loop:");
  asm("std %g0,[%o3]");		/* *(long long *)p = 0	*/
  asm("cmp %o3,%o1");
  asm("bgu loop	");		/* if (p > limit) goto loop	*/
    asm("add %o3,-8,%o3");	/* p -= 8 (delay slot) */
  asm("retl");
    asm("mov %o2,%sp");		/* Restore sp., delay slot	*/
  /* First argument = %o0 = return value */
#   ifdef SVR4
      asm("	.GC_clear_stack_inner_end:");
      asm("	.size GC_clear_stack_inner,.GC_clear_stack_inner_end-GC_clear_stack_inner");
#   endif
  
# ifdef LINT
    /*ARGSUSED*/
    ptr_t GC_clear_stack_inner(arg, limit)
    ptr_t arg; word limit;
    { return(arg); }
# endif
#endif  

