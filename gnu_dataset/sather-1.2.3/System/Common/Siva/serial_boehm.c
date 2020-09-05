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

#define SI_IMPL
#include "siva.h"

static void (*SI_collect_hook)();
static void (*SI_failure_hook)();

static SI_fatal(char *s) {
   fprintf(stderr,"Siva fatal error: %s.\n",s);
   exit(1);
};

/*
** Initialization; must be called once before any other routines.
*/
void SI_init(int clusters, int argc, char *argv[]) {
   BR_init(clusters, argc, argv);
   GC_register_displacement(PADDING_FOR_HEADER);
   SI_collect_hook = NULL;
   SI_failure_hook = NULL;
   SI_global = (SI_zone_t) GC_malloc(sizeof(struct SI_zone_t_struct));
   SI_global->capacity = BR_PROCESSORS();
   SI_global->divisions = 0;       /* A hardware leaf zone */
   SI_global->parent = NULL;
   SI_global->children = NULL;
   SI_global->next_sibling = NULL;
   SI_SET_HERE(SI_global);
}

/*
** Shut system down.
*/
void SI_exit() {
   BR_exit();
}

/*
** Specify a function to be called once on each cluster every time
** a collection occurs there.  If "SI_call_on_collect" is never
** called, no action will be taken.
*/
void SI_call_on_collect(void (*f)()) {
   SI_collect_hook = f;
}

/*
** Specify a function to be called on cluster zero if the system
** runs out of memory.  Unlike "malloc", there is no reason to
** test the return result of Siva allocation calls.
*/
void SI_call_on_failure(void (*f)()) {
   SI_failure_hook = f;
}

/*
** Allocate storage.  Objects allocated with this call
** will be scanned using "SI_scan_object" (see below).
** On this null platform, of course, this doesn't ever
** really happen.
*/
caddr_t SI_alloc(size_t size) {
   caddr_t res = (caddr_t) GC_malloc(PADDING_FOR_HEADER + size);
   if (res==NULL) {
      if (SI_failure_hook!=NULL) SI_failure_hook();
      /* Shouldn't get here, but if we do... */
      SI_fatal("Siva null platform ran out of memory");
   }
   /* Store current zone into padded object header */
   *((SI_zone_t *) res) = SI_HERE();
   return res + PADDING_FOR_HEADER;
}

/* Allocate storage which must be scanned conservatively. */
caddr_t SI_alloc_untyped(size_t size) {
   return SI_alloc(size);
}

/* Allocate some storage that will never be scanned for pointers. */
caddr_t SI_alloc_leaf(size_t size) {
   caddr_t res = (caddr_t) GC_malloc_atomic(PADDING_FOR_HEADER + size);
   if (res==NULL) {
      if (SI_failure_hook!=NULL) SI_failure_hook();
      /* Shouldn't get here, but if we do... */
      SI_fatal("Siva null platform ran out of memory");
   }
   /* Store current zone into padded object header */
   *((SI_zone_t *) res) = SI_HERE();
   return res + PADDING_FOR_HEADER;
}

/* Deallocate.  This is only a hint. */
void SI_dealloc(void *x) {
   GC_free(((caddr_t)x) - PADDING_FOR_HEADER);
}

/* Perform garbage collection now.  This is only a hint. */
void SI_collect_now() {
   GC_gcollect();
}

/* Create a software zone which is a child of the current zone. */
SI_zone_t SI_create() {
   SI_zone_t res;
   res = (SI_zone_t) GC_malloc(sizeof(struct SI_zone_t_struct));
   res->capacity = SI_here->capacity;
   res->divisions = -1;     /* A software zone */
   res->parent = SI_here;
   res->children = NULL;
   res->next_sibling = SI_here->children;
   SI_here->children = res;
   return res;
}

/* Nonzero if zone y is within (a descendent of) zone x. */
int SI_within(SI_zone_t x, SI_zone_t y) {
   SI_zone_t p = y;
   while (1) {
      if (p == x) return 1;
      p = p->parent;
      if (p == NULL) return 0;
   }
}

/* Return number of divisions of the enclosing hardware zone. */
unsigned int SI_divisions(SI_zone_t x) {
   SI_zone_t p = x;
   while (1) {
      /* The top zone will always be hardware with divisions > 0 */
      if (p->divisions >= 0) return p->divisions;
      p = p->parent;
   }
}

/* Return division `d' of the enclosing hardware zone. */
SI_zone_t SI_division(SI_zone_t x, unsigned int d) {
   SI_fatal("SI_division called on serial_boehm platform");
}

/* Return capacity of the enclosing hardware zone. */
unsigned int SI_capacity(SI_zone_t x) {
   return x->capacity;
}

/* Return the enclosing hardware zone of x in which y resides. */
SI_zone_t SI_division_of(SI_zone_t x, void *y) {
   SI_fatal("SI_division_of called on serial_boehm platform");
}

/* Return human-readable description of the Siva platform. */
const char *SI_ASCII_PLATFORM = "serial boehm collector with zone emulation";
