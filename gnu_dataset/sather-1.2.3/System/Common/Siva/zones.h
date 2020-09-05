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

#ifdef SI_IMPL
# define EXTERNAL
#else
# define EXTERNAL extern
#endif

/*
** The system specific header file is expected to be included before this
** is reached.
*/

#include <stdio.h>
#include <unistd.h>
#include <stdlib.h>
#include <sys/stat.h>
#include <fcntl.h>
#include <sys/types.h>
#include <sys/mman.h>
#include <assert.h>
#include <sys/signal.h>
#include <sys/fault.h>
#include <sys/syscall.h>
#include <sys/procfs.h>


/*
** This file defines important system constants.  Many of these are
** system-specific.  Some aren't known until runtime.  Constants which are
** known at compile-time or can be derived at compile time are kept in
** macros.  Constants which wait for runtime are computed at
** initialization and saved in globals.
** 
** One important run-time constant is the threshold of memory that the
** system is expected to start thrashing at.  This can be set by the user
** with the environment variable SATHER_MEMORY.  If this variable
** is not set it is estimated as 3/4 of the size physical memory as
** reported by sysconf().
*/

/* #define PARANOID */
/* #define _REENTRANT */

#define MIN_WORDS_PER_OBJECT	(SI_MIN_ALIGNMENT/SI_BYTES_PER_WORD)
#define WORDS_PER_BLOCK		(SI_BYTES_PER_BLOCK/SI_BYTES_PER_WORD)
#define WORDS_PER_PAGE		(SI_BYTES_PER_PAGE/SI_BYTES_PER_WORD)
#define BLOCKS_PER_PAGE		(SI_BYTES_PER_PAGE/SI_BYTES_PER_BLOCK)

#define MAX_OBJECTS_PER_BLOCK	(WORDS_PER_BLOCK/MIN_WORDS_PER_OBJECT)
#define BITS_PER_WORD		(SI_BYTES_PER_WORD*8)
#define MIN_BITS_PER_OBJECT	(SI_MIN_ALIGNMENT*8)
#define BITS_PER_BLOCK		(SI_BYTES_PER_BLOCK*8)

#define MAX_OBJECTS_PER_REGION	BITS_PER_WORD
#define WORDS_PER_REGION	(MAX_OBJECTS_PER_REGION*MIN_WORDS_PER_OBJECT)
#define BYTES_PER_REGION	(WORDS_PER_REGION*SI_BYTES_PER_WORD)
#define BLOCKS_PER_REGION	(WORDS_PER_REGION/WORDS_PER_BLOCK)

#define REGIONS_PER_PAGE	(SI_BYTES_PER_PAGE/BYTES_PER_REGION)

/*
** These are some macros to derive size constants at compile time.  Their
** expansion is very hairy and not very fast.  I'd love to hear of a better
** way to implement these as macros.  
*/
#define FIRST_1(x)		(((x)<=0x1)?(x):(1+((x)>>1)))
#define FIRST_2(x)		(((x)<=0x3)?FIRST_1(x):(2+FIRST_1((x)>>2)))
#define FIRST_4(x)		(((x)<=0xf)?FIRST_2(x):(4+FIRST_2((x)>>4)))
#define FIRST_8(x)		(((x)<=0xff)?FIRST_4(x):(8+FIRST_4((x)>>8)))
#define FIRST_16(x)		(((x)<=0xffff)?FIRST_8(x):(16+FIRST_8((x)>>16)))
#define FIRST_32(x)		(((x)<=0xffffffff)?FIRST_16(x):(32+FIRST_16(((x)>>16)>>16)))
#define BITS_FOR(x)		FIRST_32(((word)x)-1)
#define IS_POWER_OF_2(x)	(((x)&((x)-1))==0)

#define BITS_FOR_PAGE		BITS_FOR(SI_BYTES_PER_PAGE)
#define PAGE_UPPER_MASK		((~0)<<BITS_FOR_PAGE)
#define PAGE_LOWER_MASK		(~PAGE_UPPER_MASK)
#define ROUND_TO_PAGE(x)	((((word)x)+(SI_BYTES_PER_PAGE-1))&PAGE_UPPER_MASK)

#define BITS_FOR_BLOCK		BITS_FOR(SI_BYTES_PER_BLOCK)
#define BLOCK_UPPER_MASK	((~0)<<BITS_FOR_BLOCK)
#define BLOCK_LOWER_MASK	(~BLOCK_UPPER_MASK)
#define ROUND_TO_BLOCK(x)	((((word)x)+(SI_BYTES_PER_BLOCK-1))&BLOCK_UPPER_MASK)

#define BITS_FOR_REGION		BITS_FOR(BYTES_PER_REGION)
#define REGION_UPPER_MASK	((~0)<<BITS_FOR_REGION)
#define REGION_LOWER_MASK	(~REGION_UPPER_MASK)
#define ROUND_TO_REGION(x)	((((word)x)+(BYTES_PER_REGION-1))&REGION_UPPER_MASK)

#define BITS_FOR_MIN_ALIGN	BITS_FOR(SI_MIN_ALIGNMENT)
#define MIN_ALIGN_UPPER_MASK	((~0)<<BITS_FOR_MIN_ALIGN)
#define MIN_ALIGN_LOWER_MASK	(~MIN_ALIGN_UPPER_MASK)
#define ROUND_TO_MIN_ALIGN(x)	((((word)x)+(SI_MIN_ALIGNMENT-1))&MIN_ALIGN_UPPER_MASK)

#define FINALIZATION_STACK_SIZE ROUND_TO_PAGE(SI_MAX_FINALIZATION*SI_BYTES_PER_WORD)
#define MARK_STACK_SIZE         ROUND_TO_PAGE(SI_MAX_MARK*SI_BYTES_PER_WORD)
#define MAXIMUM_PAGES		(SI_MAXIMUM_HEAP/SI_BYTES_PER_PAGE)
#define MAXIMUM_REGIONS		(SI_MAXIMUM_HEAP/BYTES_PER_REGION)

typedef struct splay_node_struct {
   size_t size;
   caddr_t mem;
   struct splay_node_struct *left, *right;
} *splay_node;

#define SPLAY_HEAP_SIZE         ROUND_TO_PAGE(SI_MAX_SPLAY*sizeof(struct splay_node_struct))

typedef struct allocator_struct {
   caddr_t bins[MAX_OBJECTS_PER_BLOCK];
   splay_node free_blocks;
   struct SI_zone_t_struct *zone;
   splay_node splay_freelist;
} *allocator_t;

typedef struct SI_zone_t_struct {
   BR_SPINLOCK_DEC(spin);		/* used for locking */
   struct allocator_struct allocator;
   struct allocator_struct leaf_allocator;
   unsigned int home;			/* page descriptor allocation start */
   unsigned int capacity;
   unsigned int cluster;
   int divisions;                       /* -1 for a software zone */
   struct SI_zone_t_struct *parent;
   struct SI_zone_t_struct *children;
   struct SI_zone_t_struct *next_sibling;
} *SI_zone_t;

/* 
** Page descriptors are a single word which encodes a pointer to
** an allocator, as well as a bit indicating if that allocator
** is a leaf allocator; in other words, whether this page only
** has leaf objects.  This encoding speeds up the marking loop.
*/

#define ALLOCATOR_OF(x) ((allocator_t)((x)&(~1)))
#define PAGE_WITH_POINTERS(x) (((word)(x))&1)
#define PAGE_DESCRIPTOR(x) (((word)(x))|(((x)!=NULL)&&((&((x)->zone->allocator))==(x))))
typedef word page_desc;

typedef struct region_desc_struct {
   word parcel_bits;	/* Set for each parcel of memory  */
   word start_bits;	/* Set for each allocated object  */
   word mark_bits;	/* Used during garbage collection */
} *region_desc;

typedef struct mark_entry_struct {
   caddr_t ob;
   size_t len;
} mark_entry;
 
#define REGION_DESC_TABLE_SIZE  ROUND_TO_PAGE(MAXIMUM_REGIONS*sizeof(struct region_desc_struct))
#define PAGE_DESC_TABLE_SIZE    ROUND_TO_PAGE(MAXIMUM_PAGES*sizeof(page_desc))

EXTERNAL size_t total_mapping_size;
EXTERNAL caddr_t heap;
EXTERNAL struct region_desc_struct *region_desc_table;

/*
** Here are macros for the most common operations such as object
** allocation.  The intention is that the compiler will do constant
** propagation for objects of size known at compile time, eliminating most
** of the complexity seen here.
*/

/* These macros set, clear and test specific bits in word y. */
#define SET_BIT(x,y)   ((y)|(1<<(x)))
#define CLEAR_BIT(x,y) ((y)&(~(1<<(x))))
#define TEST_BIT(x,y)  ((y)&(1<<(x)))

/* Make sure an free header points to something reasonable */
#define INHEAP(x) (((word)(x))>=((word)heap)&&((word)(x))<((word)heap)+SI_MAXIMUM_HEAP)
#ifdef PARANOID
# define PARANOID_ONLY(x) x
# define CHK_IN_HEAP(x,y) (INHEAP(x)?(y):(abort(),0))
#else
# define PARANOID_ONLY(x)
# define CHK_IN_HEAP(x,y) y
#endif

/* Test the descriptor bit for object at x, assuming already locked. */
#define DESC_BIT(x,y) 			\
   CHK_IN_HEAP(x,(TEST_BIT((((word)(x))&REGION_LOWER_MASK)>>BITS_FOR_MIN_ALIGN,(region_desc_table+((((word)(x))-((word)heap))>>BITS_FOR_REGION))->y##_bits)))

/* Set the descriptor bit for object at x, assuming already locked. */
#define SET_DESC_BIT(x,y) {		\
   region_desc reg=region_desc_table    \
	    +((((word)(x))-((word)heap))>>BITS_FOR_REGION);\
   size_t offset=(((word)(x))&REGION_LOWER_MASK)>>BITS_FOR_MIN_ALIGN;\
   reg->y##_bits=CHK_IN_HEAP(x,SET_BIT(offset,reg->y##_bits));\
}

/* Clear the descriptor bit for object at x, assuming already locked. */
#define CLEAR_DESC_BIT(x,y) {		\
   region_desc reg=region_desc_table    \
	    +((((word)(x))-((word)heap))>>BITS_FOR_REGION);\
   size_t offset=(((word)(x))&REGION_LOWER_MASK)>>BITS_FOR_MIN_ALIGN;\
   reg->y##_bits=CHK_IN_HEAP(x,CLEAR_BIT(offset,reg->y##_bits));\
}

/****************************************************************************/
/* The actual Siva interface follows.  Everything above was implementation. */
/****************************************************************************/

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



/* Deallocate.  This is only a hint. */
EXTERNAL void SI_dealloc(void *x);

/* Perform garbage collection now.  This is only a hint. */
EXTERNAL void SI_collect_now();


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
** The zone of the argument object.  May be a far pointer.
*/
EXTERNAL SI_zone_t SI_WHERE(void *);

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




/* Some macros to help with collecting statistics with no impact
   when they are turned off. */

caddr_t allocate(allocator_t a, size_t size);

#ifdef SI_STATISTICS
  size_t parcel_size(caddr_t x);
  EXTERNAL size_t requested, total_allocated;
  EXTERNAL size_t statstab[];
# define SI_STAT(x) x
# define STATS_REGISTER(x) if ((x)<K) statstab[x]++; else statstab[K-1]+=((x)/K)
#else
# define SI_STAT(x)
# define STATS_REGISTER(x)
#endif


/* This macro is only valid in gcc, due to the use of `statement expr's. */
/* Some of the weirdness here is because there's no way to shadow labels. */
/* Also the handling of ## and __LINE__ is exceedingly weird, requiring
   this macro indirection.  Don't blame me, blame ANSI. */
#define poof(x,y) x ## y
#define SI_ALLOC2(a,s,l) ({		\
 caddr_t mem; unsigned bi;		\
 size_t rs = ROUND_TO_MIN_ALIGN(s);	\
 SI_zone_t z = SI_here; 		\
 allocator_t za = &(z->a);		\
 SI_STAT(requested+=(s);)		\
 STATS_REGISTER(s);			\
 if (rs>SI_BYTES_PER_BLOCK) goto poof(alloc,l); \
 bi = (rs>>BITS_FOR_MIN_ALIGN)-1;	\
 BR_SPINLOCK_LOCK(z->spin);		\
 mem = za->bins[bi];			\
 if (mem==NULL) {			\
  BR_SPINLOCK_UNLOCK(z->spin); 		\
  poof(alloc,l): mem = allocate(za,rs);	\
  goto poof(done,l); }			\
 /* SET_DESC_BIT(mem,start); GOO */	\
 za->bins[bi] = *(void **)mem;		\
 BR_SPINLOCK_UNLOCK(z->spin);		\
 SI_ZERO_OUT(mem,rs);			\
 poof(done,l): 				\
 SI_STAT(total_allocated+=parcel_size(mem);)\
 mem; })

#define SI_ALLOC(s) SI_ALLOC2(allocator,(s),__LINE__)
#define SI_ALLOC_LEAF(s) SI_ALLOC2(leaf_allocator,(s),__LINE__)

