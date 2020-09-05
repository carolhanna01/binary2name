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

/*
** Header file for the Siva platform which uses the Boehm collector,
** "serial_boehm".  Unlike the other platforms, this header doesn't
** have to specify all the constants such as "SI_MIN_ALIGNMENT" because
** this platform doesn't use the general Siva memory manager code.
*/

#ifdef SI_IMPL
# define EXTERNAL
#else
# define EXTERNAL extern
#endif

#include <stdio.h>

/*
** To emulate "where()", each object is allocated 8 bytes more
** than asked for.  This is in addition to any object header added
** by the compiler.  PADDING_FOR_HEADER needs to be a multiple of
** the alignment guaranteed by C for object allocation.
*/
#define PADDING_FOR_HEADER 8

/*
** Initialization; must be called once before any other routines.
** "SI_init" initializes Brahma, so "BR_init" should not be called
** elsewhere.  The arguments are passed to "BR_init".
*/
EXTERNAL void SI_init(int clusters, int argc, char *argv[]);

/*
** Shut system down.  "SI_exit" shuts down Brahma, so "BR_exit"
** should not be called elsewhere.
*/
EXTERNAL void SI_exit();

/*
** Specify a function to be called once on each cluster every time
** a collection occurs there.  If "SI_call_on_collect" is never
** called, no action will be taken.
*/
EXTERNAL void SI_call_on_collect(void (*f)());

/*
** Specify a function to be called on cluster zero if the system
** runs out of memory.  Unlike "malloc", there is no reason to
** test the return result of Siva allocation calls.
*/
EXTERNAL void SI_call_on_failure(void (*f)());

/*
** Allocate storage.  Objects allocated with this call
** will be scanned using "SI_scan_object" (see below).
*/
EXTERNAL caddr_t SI_alloc(size_t size);

/* Allocate storage which must be scanned conservatively. */
EXTERNAL caddr_t SI_alloc_untyped(size_t size);

/* Allocate some storage that will never be scanned for pointers. */
EXTERNAL caddr_t SI_alloc_leaf(size_t size);

/* Deallocate.  This is only a hint. */
EXTERNAL void SI_dealloc(void *x);

/* Perform garbage collection now.  This is only a hint. */
EXTERNAL void SI_collect_now();


/*
** Macro versions of these calls to be used when size is known to be a
** compile-time constant:
*/

#define SI_ALLOC_CONST(s) SI_alloc(s)
#define SI_ALLOC_UNTYPED_CONST(s) SI_alloc_untyped(s)
#define SI_ALLOC_LEAF_CONST(s) SI_alloc_leaf(s)

/* Definition of ZONE type (may have system-specific fields) */
typedef struct SI_zone_t_struct {
   unsigned int capacity;
   int divisions;  /* -1 for a software zone */
   struct SI_zone_t_struct *parent;
   struct SI_zone_t_struct *children;
   struct SI_zone_t_struct *next_sibling;
} *SI_zone_t;

/*
** Global on cluster zero that defines "ZONE::global".  This should
** be initialized in SI_platform_specific().
*/
EXTERNAL SI_zone_t SI_global;
     
/*
** The zone currently associated with the calling thread.
** This is different than BR_HERE() because it returns a zone
** rather than an integer cluster id.  SI_SET_HERE() sets this
** value.  When "main" begins, the zone is automatically set to be
** the value of SI_global.
** 
** Similarly, over exceptions (longjmp()) it must
** be restored explicitly.
*/

EXTERNAL SI_zone_t SI_here;
#define SI_HERE() SI_here

/* EXTERNAL void SI_SET_HERE(SI_zone_t); */
#define SI_SET_HERE(x) SI_here=(x)

/*
** The zone of the argument object.  May be a far pointer.  In this
** platform, the zone of an object is stored in the doubleword preceeding
** the true allocated object.
*/
/* EXTERNAL SI_zone_t SI_WHERE(void *); */
#define SI_WHERE(x) (*((SI_zone_t*)(((caddr_t)x)-PADDING_FOR_HEADER)))

/* Create a software zone which is a child of the current zone. */
EXTERNAL SI_zone_t SI_create();

/* Nonzero if zone y is within (a descendent of) zone x. */
EXTERNAL int SI_within(SI_zone_t x, SI_zone_t y);

/* Return number of divisions of the enclosing hardware zone. */
EXTERNAL unsigned int SI_divisions(SI_zone_t);

/* Return division `d' of the enclosing hardware zone. */
EXTERNAL SI_zone_t SI_division(SI_zone_t x, unsigned int d);

/* Return capacity of the enclosing hardware zone. */
EXTERNAL unsigned int SI_capacity(SI_zone_t);

/* Return the enclosing hardware zone of x in which y resides. */
EXTERNAL SI_zone_t SI_division_of(SI_zone_t x, void *y);

/* Return human-readable description of the Siva platform. */
EXTERNAL const char *SI_ASCII_PLATFORM;
