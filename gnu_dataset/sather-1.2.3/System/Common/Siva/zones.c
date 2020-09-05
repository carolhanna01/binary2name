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

/*********************************************************************/
/*                     GLOBAL STUFF
**
** These are globals which apply to all threads (most never change after
** initialization).
*/

/*
** These aren't constants because they can depend on the SATHER_MEMORY
** environment variable or the currently consumed physical memory.  They
** do remain fixed after initialization.
*/
static size_t physical_bytes_per_node;
static size_t physical_pages_per_node;
static size_t nodes_per_system;

/* file descriptor used for obtaining system statistics. */
static int pidfd;

/*
** These three tables are allocated from virtual zero memory, so they
** consume no physical resources but do not need to be resized.
** 
** Pages are divided into regions, regions are divided into cache blocks,
** and objects are carved out of those, bouncing up to higher levels when
** the size is too big.  There is at most one zone ever using a page, and
** hence at most one zone ever uses a block.
** 
** The finalization stack holds all the objects waiting to be finalized.
*/
static page_desc *page_desc_table;
static caddr_t finalization_stack;
static mark_entry *mark_stack;
static splay_node splay_heap;

/* This variable is set if the DEBUG_SIVA environment variable is defined. */
int zdebug;

/*
** The global (default) zone is in static memory (all other zones are
** allocated just like other objects).
*/
static struct SI_zone_t_struct SI_global_storage;

/* The amount of memory currently allocated, ie. granted to any zones */
static size_t zoned_heap;

/* Pointer above the splay heap, where to allocate more */
static splay_node splay_top;

/* Above global variables are protected by the cluster_spin spinlock. */
static BR_SPINLOCK_DEC(cluster_spin);

/* We avoid multiple collections by threads that reach the collect() */
/* routine at the same time. */
static int in_collection;
static BR_SPINLOCK_DEC(collect_spin);

/*
** The allocated heap size at which garbage collection will occur.
** This changes, but only under unusual circumstances not requiring a
** spinlock.
*/
static size_t gc_limit, gc_limit_was;

/* Prototypes for internal routines. */
static caddr_t allocate_blocks(allocator_t a, size_t size, size_t alignment);
static void allocate_pages(allocator_t a, size_t size);
void mark_from_region(caddr_t start, size_t len);
static void set_page_descriptors(allocator_t a, caddr_t from, size_t size);
static void debug_regions(caddr_t x, caddr_t y);
#ifndef SI_STATISTICS
static 
#endif
       size_t parcel_size(caddr_t x);
static void add_free_parcel(allocator_t a, caddr_t x, size_t size);
static splay_node splay_splay(size_t i, splay_node t);
static splay_node splay_insert(splay_node new, splay_node t);
static splay_node splay_delete_root(splay_node t);
static splay_node splay_next_bigger(splay_node t);
static splay_node new_splay(allocator_t a);
static void recycle_splay(allocator_t a, splay_node s);
static void collect();
void SI_mark_roots();
static void clear_free_lists(SI_zone_t z);
static void acquire_spinlocks(SI_zone_t z);
static void release_spinlocks(SI_zone_t z);
static void verify_tree(splay_node t);
static double secs(timestruc_t x);

#ifdef SI_STATISTICS
size_t requested=0, total_allocated=0;
static size_t last_free=SI_MINIMUM_HEAP, last_allocated=0;
size_t statstab[K];
#endif

/*******************************************************************/
/*                      INITIALIZATION CODE
**
** The system specific specific file foo.c is expected to have been
** included by the time we get here.
*/

static size_t env_var_to_number(const char *var) {
   size_t number;
   char suffix;
   char *env_var = getenv(var);
   if (env_var==NULL) return 0;
   else {
      switch (sscanf(env_var,"%d%c",&number,&suffix)) {
	 case 1: break;
	 case 2: 
	    switch (suffix) {
	       case 'k': case 'K':
		  number = number * K; break;
	       case 'm': case 'M': 
	          number = number * M; break;
	       default: 
		  fprintf(stderr, "Malformed %s suffix.\n", var); exit(1);
	    }; break;
	 default:
	    fprintf(stderr, "Malformed %s env var.\n", var); exit(1);
      }
   }
   return number;
}

static void (*SI_collect_hook)();
static void (*SI_failure_hook)();

void SI_fatal(char *s) {
   fprintf(stderr,"Siva fatal error: %s.\n",s);
   exit(1);
}

/*
** Initialization; must be called once before any other routines.
** Set up system tables and virtual space for the allocator 
*/
void SI_init(int clusters, int argc, char *argv[]) {
   
   int zero_filedes; /* fd used for building the virtual map */
   char buf[80];

   init_other();

   BR_init(clusters, argc, argv); /* Initialize Brahma */
   SI_collect_hook = NULL;
   SI_failure_hook = NULL;

   zdebug = (getenv("DEBUG_SIVA")!=NULL);

   physical_bytes_per_node = env_var_to_number("SIVA_MEMORY");
   if (physical_bytes_per_node == 0) {
      physical_pages_per_node = sysconf(_SC_PHYS_PAGES) * 3/4;
      physical_bytes_per_node = physical_pages_per_node * SI_BYTES_PER_PAGE;
   }
   else
      physical_pages_per_node = physical_bytes_per_node / SI_BYTES_PER_PAGE;

   nodes_per_system = env_var_to_number("SIVA_NODES");
   if (nodes_per_system == 0)
      nodes_per_system = SI_MAX_NODES;

   /*
   ** Open descriptor to use to refer to the page and region descriptor
   ** tables with.
   */
   zero_filedes = open("/dev/zero", O_RDWR);
   if (zero_filedes == -1) { 
      perror("Unable to open /dev/zero for virtual map1");
      exit(1);
      }

   total_mapping_size = SI_MAXIMUM_HEAP + PAGE_DESC_TABLE_SIZE
					+ REGION_DESC_TABLE_SIZE 
					+ FINALIZATION_STACK_SIZE
					+ MARK_STACK_SIZE
					+ SPLAY_HEAP_SIZE;

   heap = (void *) mmap(NULL, 
			  total_mapping_size,
			  PROT_READ | PROT_WRITE,
			  MAP_PRIVATE | MAP_NORESERVE,
			  zero_filedes,
			  (off_t) 0);
   if (heap == MAP_FAILED) {
      perror("mmap of /dev/zero for page descriptor");
      exit(1);
      }
   page_desc_table = (void *) (heap+SI_MAXIMUM_HEAP);
   region_desc_table = (void *) (((caddr_t)page_desc_table)+PAGE_DESC_TABLE_SIZE);
   finalization_stack = (void *) (((caddr_t)region_desc_table)+REGION_DESC_TABLE_SIZE);
   mark_stack = (void *) (((caddr_t)finalization_stack)+FINALIZATION_STACK_SIZE);
   splay_heap = (void *) (((caddr_t)mark_stack)+MARK_STACK_SIZE);

   /* Don't need the descriptor after establishing mapping */
   close(zero_filedes);

   /* Establish file descriptor to use for obtaining system specifics */
   sprintf(buf, "/proc/%lu", getpid());
   pidfd = open(buf, O_RDONLY);
   if (pidfd < 0) { perror("Couldn't open proc file"); exit(1); }

   BR_SPINLOCK_INIT(cluster_spin);
   BR_SPINLOCK_INIT(collect_spin);

   zoned_heap = 0;
   gc_limit = SI_MINIMUM_HEAP;

   splay_top = splay_heap;

   /* Allocate the global (default) zone */
   SI_global = &SI_global_storage;
   BR_SPINLOCK_INIT(SI_global->spin);
   SI_global->allocator.free_blocks = NULL;
   SI_global->allocator.zone = SI_global;
   SI_global->leaf_allocator.free_blocks = NULL;
   SI_global->leaf_allocator.zone = SI_global;
   /* Global zone is special, always at middle of heap */
   SI_global->home = MAXIMUM_PAGES/2;
   SI_global->capacity = BR_PROCESSORS();
   SI_global->cluster = 0;
   SI_global->divisions = 0;       
   SI_global->parent = NULL;
   SI_global->children = NULL;
   SI_global->next_sibling = NULL;

   SI_here = SI_global;

#ifdef SI_STATISTICS
   { int i; for (i=0; i<K; i++) statstab[i]=0; }
#endif

   if (zdebug) 
      fprintf(stderr, "Siva platform \"%s\" initialized.\n", SI_ASCII_PLATFORM);
}


/***********************************************************************/
/*                          BIN ALLOCATION
**
** This is the general allocator.  There are also macro forms which should
** be used when the size may be a compile-time constant.  They punt to
** allocate() for the uncommon cases.
** 
** allocate() returns a pointer to storage at least as large as the
** argument.  Minimum alignment is given by SI_MIN_ALIGNMENT and is
** platform specific.
** 
** The allocator services requests for different object sizes using
** different allocation "services".  There are four levels of service, for
** objects smaller than a cache block, fitting in the zone, fitting within
** existing alloted heap and those requiring garbage collect.
** When one level can't service a request because it is too big or there
** is no memory at that level, it is bounced up to the next higher level.
** 
** Sizes smaller than a cache block are never allocated crossing cache
** block boundaries.  Sizes larger than a cache block are aligned to a
** cache block.  Sizes larger than a page are aligned to a page.
*/

#ifdef PARANOID
static int in_allocate = 0;
#endif

static parcel_lookup[4] = { 0xffffffff, 0x55555555, 0x99999999, 0x11111111 };

/*
** Allocate storage.  Objects allocated with this call will be scanned
** using "SI_scan_object" (see below).  This routine services allocation
** requests smaller than a cache block, and passes the request to the
** block allocator "allocate_blocks" if not successful or the request is
** too big.  This routine obtains the zone spinlock when needed and before
** bumping requests out to the next level.
*/
caddr_t allocate(allocator_t a, size_t size) {
   caddr_t mem, last, llast;
   unsigned int i, bin_idx;
   size_t rounded_size = ROUND_TO_MIN_ALIGN(size);		
   SI_zone_t z = a->zone;

#ifdef PARANOID
   if (in_allocate) { fprintf(stderr,"Recursive allocate!\n"); abort(); }
   in_allocate = 1;
#endif

   if (rounded_size <= SI_BYTES_PER_BLOCK) {
      /* Small enough to be in a bin */
      bin_idx = (rounded_size >> BITS_FOR_MIN_ALIGN) - 1;
      BR_SPINLOCK_LOCK(z->spin);	
      mem = a->bins[bin_idx];	
      if (mem == NULL) {		
	 /* Nothing waiting in the bin, so get a region and carve it up */
	 mem=allocate_blocks(a, BYTES_PER_REGION, BYTES_PER_REGION);

/* Use precomputed table for the common case of 32 byte cache lines
   and 8 bytes minimum alignment. */
#ifdef SI_USE_PARCEL_LOOKUP
/*
**          region_desc_table[((mem-heap)>>BITS_FOR_REGION)].parcel_bits = 
** 	    parcel_lookup[bin_idx];
** GOO
*/
         { 
	 region_desc reg = &(region_desc_table[((mem-heap)>>BITS_FOR_REGION)]);
	 word lookup = parcel_lookup[bin_idx];
         reg->start_bits = lookup;
         reg->parcel_bits = lookup;
	 }
#endif

         /* If block is a multiple of the size, then no leftovers */
	 if (IS_POWER_OF_2(rounded_size)) {
	    /* Thread the new bins together into a linked list */
	    caddr_t term;
	    last = NULL;
	    term = mem+BYTES_PER_REGION;
	    for (; mem<term; mem=mem+rounded_size) {
	       *((caddr_t *)mem) = last;
	       last = mem;
#ifndef SI_USE_PARCEL_LOOKUP
	       SET_DESC_BIT(mem,parcel);
	       SET_DESC_BIT(mem,start); /* GOO */
#endif
	    }
	    a->bins[bin_idx] = last;
	    mem = last;
	 }

	 /* If not a multiple, there are leftovers.  We know the */
	 /* leftovers are in a different bin.                    */
	 else {
	    last = NULL;
	    llast = a->bins[MAX_OBJECTS_PER_BLOCK-bin_idx];
	    for (i=0; i<BLOCKS_PER_REGION; i++) {
	       caddr_t lmem = mem+rounded_size;
	       *((caddr_t *)mem) = last;
	       *((caddr_t *)lmem) = llast;
	       last = mem;
	       llast = lmem;
#ifndef SI_USE_PARCEL_LOOKUP
	       SET_DESC_BIT(mem, parcel);
	       SET_DESC_BIT(lmem, parcel);
	       SET_DESC_BIT(mem, start);  /* GOO */
	       SET_DESC_BIT(lmem, start); /* GOO */
#endif
	       mem += SI_BYTES_PER_BLOCK;
	    }
	    a->bins[bin_idx] = last;
	    mem = last;
	    a->bins[MAX_OBJECTS_PER_BLOCK-bin_idx] = llast;
	 }
      }
#ifdef PARANOID
      if (DESC_BIT(mem,start)) {
	 fprintf(stderr,"Start bit was already set!\n");
	 abort();
      }
      if (parcel_size(mem)!=rounded_size) {
	 fprintf(stderr,"Size should have been %d, was %d!\n",
			parcel_size(mem),rounded_size);
	 abort();
      }
#endif
      /* GOO */
      /* SET_DESC_BIT(mem,start); */
      a->bins[bin_idx] = *(void **)mem;  /* Unlink object from bin */
      BR_SPINLOCK_UNLOCK(z->spin);	
      SI_ZERO_OUT(mem, rounded_size);		
      PARANOID_ONLY(in_allocate = 0;)
      /* if (zdebug) fprintf(stderr,"allocate(%d) = %x\n",size,(unsigned int)mem); */
      return mem;
   } else {
      size_t block_size=ROUND_TO_BLOCK(size);
      size_t difference;
      /* bigger than a block, so bump up to block allocator */
      BR_SPINLOCK_LOCK(z->spin);	
      mem=allocate_blocks(a, block_size, SI_BYTES_PER_BLOCK);
      SET_DESC_BIT(mem,start);
      difference=block_size-rounded_size;
      if (difference!=0)
	 add_free_parcel(a,mem+rounded_size,difference);
      BR_SPINLOCK_UNLOCK(z->spin);	
      PARANOID_ONLY(in_allocate = 0;)
      SI_ZERO_OUT(mem, rounded_size);		
      /* if (zdebug)
	      fprintf(stderr,"allocate(%d) = %x\n",size,(unsigned int)mem); */
      return mem;
   }
}


/***********************************************************************/
/*                          BLOCK ALLOCATION
**
** Allocate region of memory, a multiple of the block size, with the given
** alignment.  This is used for allocations larger than the block size.
** It is also used to allocate regions that are carved up to fill bins for
** small objects.  The size is assumed to be a multiple of the block
** size.  Alignment is assumed to be a power of two and multiple of the
** block size.  If the allocation cannot be serviced here it is passed up
** to "allocate_pages".  The zone spinlock is assumed to be obtained before
** getting to allocate_blocks.
*/

static caddr_t allocate_blocks(allocator_t a, size_t size, size_t alignment) {
   size_t tsize, before_size, after_size;
   splay_node lastx=NULL, was;
   caddr_t points, al;

   /* Try to get memory region from the splay tree */
   a->free_blocks = splay_splay(size, a->free_blocks);
   while(1) {
      if (a->free_blocks!=NULL) {
	 tsize = a->free_blocks->size;
#ifdef PARANOID
	 points = a->free_blocks->mem;
	 if (tsize!=parcel_size(points)) {
	    printf("sizes didn't match: tsize:%d parcel_size:%d heap:%08lx\n", tsize, parcel_size(points), (word)heap);
	    abort();
	 }
	 if (ROUND_TO_BLOCK(((size_t)points))!=((size_t)points)) {
	    printf("node not block aligned\n");
	    abort();
	 }
#endif
      }
      else tsize = 0;
      /* printf("Want size %d, got size %d\n",size,tsize); */
      if (tsize >= size) {
         /* check if possible to extract aligned of the right size */
	 points = a->free_blocks->mem;
	 al = (caddr_t) ((((word)points)-1)&(~(alignment-1)))+alignment;
#if 0
	 if (size<SI_BYTES_PER_PAGE) {
	    /* Do not to straddle page boundaries when taking out a chunk */
	    caddr_t ral = (caddr_t) ROUND_TO_PAGE(al);
	    if (al+size > ral) al = ral;
	 }
#endif
	 if (al+size <= points + tsize) {
	    /* It will fit, so break up as necessary and return useful part. */
	    /* The rest is reclaimed for future allocations. */
	    before_size = al - points;
	    after_size = points+tsize-(al+size);
	    /* printf("Request for size %x, aligned %x:\n\tcell %08x, size %x, aligned %08x, before %x, after %x\n",size,alignment,(unsigned int)a->free_blocks,tsize,al,before_size,after_size); */
	    was = a->free_blocks;
	    a->free_blocks = splay_delete_root(was);
	    recycle_splay(a, was);
	    if (before_size > 0) {
	       PARANOID_ONLY(assert(before_size>=32);)
	       add_free_parcel(a, points, before_size);
	       SET_DESC_BIT(al,parcel);
	       }
	    if (after_size > 0) {
	       PARANOID_ONLY(assert(after_size>=32);)
	       add_free_parcel(a, al+size, after_size);
	    }
	    PARANOID_ONLY(verify_tree(a->free_blocks);)
	    return al;
	 }
      }
      /* Couldn't fit there.  Try next node. */
      lastx = a->free_blocks;
      a->free_blocks = splay_next_bigger(a->free_blocks);
      if (a->free_blocks==NULL || a->free_blocks==lastx) {
	 /* Nothing big enough was found in the zone.  Get more pages. */
         allocate_pages(a, ROUND_TO_PAGE(size));
	 a->free_blocks = splay_splay(size, a->free_blocks);
      }
   }
}

/***********************************************************************/
/*                         EXPAND A ZONE
**
** Allocate region of memory.  This is used when block allocation fails.
** There is at most one zone governing each page.  The size is assumed to
** be a multiple of the page size.   If the request can't be serviced
** here, it is passed up to the service which will garbage collect and/or
** resize the heap.  This is assumed to be called after the zone spinlock
** is obtained.  Instead of returning a pointer to memory, this routine
** adds the granted pages to the splay tree.
*/

static void allocate_pages(allocator_t a, size_t size) {
   unsigned int need, i, j;
   caddr_t mem;
   splay_node sp;
   size_t home = a->zone->home;
   int not_found;

   /* get cluster spinlock */
   BR_SPINLOCK_LOCK(cluster_spin);
   if (zoned_heap+size > gc_limit) {
      /* Whoops, that means it's time for a garbage collection... */
      /* Have to unlock zone before collecting to avoid deadlock */
      BR_SPINLOCK_UNLOCK(cluster_spin);
      BR_SPINLOCK_UNLOCK(a->zone->spin);

      /* If another collection is in progress, just fall through. */
      /* In other words, extra page allocation happens to avoid deadlock. */
      BR_SPINLOCK_LOCK(collect_spin)
      if (!in_collection) {
	 in_collection = 1;
	 BR_SPINLOCK_UNLOCK(collect_spin);
	 collect();
	 in_collection = 0;
      } else
	 BR_SPINLOCK_UNLOCK(collect_spin);

      BR_SPINLOCK_LOCK(a->zone->spin);
      BR_SPINLOCK_LOCK(cluster_spin);
   }
   zoned_heap += size;
   /* Look for right number of consecutive pages, starting at the */
   /* position in the heap used as home base for this zone.       */
   need = size / SI_BYTES_PER_PAGE;

   not_found = 1;

   /* Pointer-bearing heap looks forward, leaves look backwards.    */
   /* The goal is to distribute pages in both directions, to help   */
   /* give this quadratic algorithm a forgivingly small             */
   /* constant factor...  better to use a less naive approach.      */

   if (a== &(a->zone->allocator)) {
      for (i=home; i<=MAXIMUM_PAGES-need; i++) {
	 if (page_desc_table[i]==NULL) {
	    for (j=i+1; j<i+need; j++) 
	       if (page_desc_table[j]!=NULL) break;
	    if (j==i+need) {
	       not_found = 0;
	       break;
	    } else i=j;
	 }
      }
      if (not_found) {
	 /* Didn't find it after home, try before. */
	 for (i=home-1; i>=need; i--) {
	    if (page_desc_table[i]==NULL) {
	       for (j=i-1; j>i-need; j--) 
		  if (page_desc_table[j]!=NULL) break;
	       if (j==i-need) {
		  i=j+1;
		  break;
	       }
	       else i=j;
	    }
	 }
	 if (i<need) {
	    BR_SPINLOCK_UNLOCK(cluster_spin);
	    if (SI_failure_hook!=NULL) SI_failure_hook();
	    /* Shouldn't get here, but if we do... */
	    SI_fatal("SI_MAXIMUM_HEAP exceeded");
	 }
      }
   } else {
      for (i=home; i>=need; i--) {
	 if (page_desc_table[i]==NULL) {
	    for (j=i-1; j>i-need; j--) 
	       if (page_desc_table[j]!=NULL) break;
	    if (j==i-need) {
	       not_found = 0;
	       i=j+1;
	       break;
	    }
	    else i=j;
	 }
      }
      if (not_found) {
	 /* Didn't find it before home, try after. */
	 for (i=home+1; i<=MAXIMUM_PAGES-need; i++) {
	    if (page_desc_table[i]==NULL) {
	       for (j=i+1; j<i+need; j++) 
		  if (page_desc_table[j]!=NULL) break;
	       if (j==i+need) {
		  not_found = 0;
		  break;
	       } else i=j;
	    }
	 }
	 if (i>MAXIMUM_PAGES-need) {
	    BR_SPINLOCK_UNLOCK(cluster_spin);
	    if (SI_failure_hook!=NULL) SI_failure_hook();
	    /* Shouldn't get here, but if we do... */
	    SI_fatal("SI_MAXIMUM_HEAP exceeded");
	 }
      }
   }

   /* Ok, now we've found enough contiguous pages to satisfy the request. */
   /* Add the pages to the splay tree. */

   mem = heap + SI_BYTES_PER_PAGE * i;
   set_page_descriptors(a,mem,size);
   BR_SPINLOCK_UNLOCK(cluster_spin);

   sp = new_splay(a);
   sp->size = size;
   sp->mem = mem;
   a->free_blocks = splay_insert(sp, a->free_blocks);
   SET_DESC_BIT(mem, parcel);
   if (mem+size<heap+SI_MAXIMUM_HEAP) SET_DESC_BIT(mem+size, parcel);
   PARANOID_ONLY(verify_tree(a->free_blocks);)
   PARANOID_ONLY(assert(page_desc_table[(mem-heap)/SI_BYTES_PER_PAGE]!=NULL);)
   return;
}


/*
** Set the zones in the page descriptors corresponding to a range of
** memory.  Assumes cluster spinlock has been obtained.
*/

static void set_page_descriptors(allocator_t a, caddr_t from, size_t size) {
   size_t sfrom = (size_t) from;
   size_t sto = (size_t) from+size-1;
   size_t sheap = (size_t) heap;
   size_t i;
   page_desc aa = PAGE_DESCRIPTOR(a);

   PARANOID_ONLY(assert(ROUND_TO_PAGE(sfrom)==sfrom);)
   PARANOID_ONLY(assert(ROUND_TO_PAGE(size)==size);)

   sfrom -= sheap;
   sfrom /= SI_BYTES_PER_PAGE;

   sto -= sheap;
   sto /= SI_BYTES_PER_PAGE;

   for (i=sfrom;i<=sto;i++)
      page_desc_table[i] = aa;
}


/***********************************************************************/
/*                         PARCEL MANAGEMENT
**
** Compute the size of a parcel starting at `x'.  Assumes no clashing
** accesses to parcel bits as it does this, but does not explicitly lock
** anything against this.
*/

/* FOO */
/* static */
size_t parcel_size(caddr_t x) {
   region_desc reg,after_last_reg;
   word parcel_bits;
   off_t count, offset;

   reg = region_desc_table + ((x-heap) >> BITS_FOR_REGION);
   offset = (((word)(x))&REGION_LOWER_MASK)>>BITS_FOR_MIN_ALIGN;

   /* Clear any bits before and including the parcel we want */
   /* The wierdness here is to keep from shifting more than wordsize-1 */
   parcel_bits = reg->parcel_bits & (((~((word)0))<<offset)<<1);

   /* Optimization for the common case where no searching is necessary */
   /* Loop is unrolled once */
   if (parcel_bits!=0)
      return (SI_FIRST_BIT(parcel_bits)-offset)*SI_MIN_ALIGNMENT;

   after_last_reg = region_desc_table + (SI_MAXIMUM_HEAP>>BITS_FOR_REGION);
   reg++;
   count = BYTES_PER_REGION - offset * SI_MIN_ALIGNMENT;
   parcel_bits = reg->parcel_bits;
   while (1) {
      if (reg == after_last_reg) { /* last parcel in heap */
	 return heap+SI_MAXIMUM_HEAP-x;
      }
      if (parcel_bits != 0)      /* found following parcel */
	 return count + SI_FIRST_BIT(parcel_bits)*SI_MIN_ALIGNMENT;
      reg++;
      count += BYTES_PER_REGION;
      parcel_bits = reg->parcel_bits;
   }
}

/* Add a free block to the appropriate list depending on its size. */
/* Alignments may have to be fixed up too.				*/
static void add_free_parcel(allocator_t a, caddr_t x, size_t size) {
   /* signed! */ off_t rounded_size, aligned_size, before_size, after_size;
   caddr_t aligned_start, aligned_end;
   unsigned int bin_idx;
   splay_node sp;

   PARANOID_ONLY(assert((size & MIN_ALIGN_LOWER_MASK)==0);)
   aligned_start = (caddr_t) ROUND_TO_BLOCK(x);
   aligned_end = (caddr_t) (((word)(x+size)) & BLOCK_UPPER_MASK);
   aligned_size = aligned_end - aligned_start;
   if (aligned_size>0) {
      sp = new_splay(a);
      sp->size=aligned_size;
      sp->mem=aligned_start;
      a->free_blocks = splay_insert(sp, a->free_blocks);
      SET_DESC_BIT(aligned_start,parcel);
      SI_STAT(if (in_collection) STATS_REGISTER(aligned_size);)
   }
   before_size = aligned_start - x;
   if (before_size>size) before_size = size;
   if (before_size>0) {
      PARANOID_ONLY(assert((before_size & MIN_ALIGN_LOWER_MASK)==0);)
      PARANOID_ONLY(assert(before_size<=32&&before_size>=8);)
      rounded_size = ROUND_TO_MIN_ALIGN(before_size);
      bin_idx = (rounded_size >> BITS_FOR_MIN_ALIGN) - 1;
      *((caddr_t *)x)=a->bins[bin_idx];
      a->bins[bin_idx]=x;
      SET_DESC_BIT(x,parcel);
      /* GOO */
      SET_DESC_BIT(x,start);
      SI_STAT(if (in_collection) STATS_REGISTER(before_size);)
   }
   after_size = (((size_t)x)+size)-((size_t)aligned_end);
   if (after_size>0 && after_size<=size) {
      PARANOID_ONLY(assert((after_size & MIN_ALIGN_LOWER_MASK)==0);)
      PARANOID_ONLY(assert(after_size<=32&&after_size>=8);)
      rounded_size = ROUND_TO_MIN_ALIGN(after_size);
      bin_idx = (rounded_size >> BITS_FOR_MIN_ALIGN) - 1;
      *((caddr_t *)aligned_end)=a->bins[bin_idx];
      a->bins[bin_idx]=aligned_end;
      SET_DESC_BIT(aligned_end,parcel);
      /* GOO */
      SET_DESC_BIT(aligned_end,start);
      SI_STAT(if (in_collection) STATS_REGISTER(after_size);)
   }
#ifdef PARANOID
   if (before_size>0 && parcel_size(x)!=before_size) {
      fprintf(stderr,"aligned before_size not same as parcel: %d %ld\n",
 			parcel_size(x), before_size);
      fprintf(stderr,"x:%08x size:%d aligned_start:%08x aligned_end:%08x before_size:%ld aligned_size:%ld after_size:%ld\n",(size_t)x,size,(size_t)aligned_start,(size_t)aligned_end,before_size,aligned_size,after_size);
      abort();
   }
   assert(before_size<=0 || parcel_size(x)==before_size);
#endif
}

PARANOID_ONLY(int in_SI_dealloc = 0;)
/* Deallocate a region of memory starting at `x'. */
void SI_dealloc(void *x) {
   size_t size;
   allocator_t a;

#ifdef PARANOID
   if (in_SI_dealloc) {
      fprintf(stderr,"recursive free!\n");
      abort();
   }
   in_SI_dealloc = 1;
#endif
   a=ALLOCATOR_OF(page_desc_table[(((caddr_t)x)-heap)/SI_BYTES_PER_PAGE]);
   BR_SPINLOCK_LOCK(a->zone->spin);	
   size=parcel_size(x);
   add_free_parcel(a,x,size);
   if (size>SI_BYTES_PER_BLOCK) /* GOO - was always */
      CLEAR_DESC_BIT(x,start);
   BR_SPINLOCK_UNLOCK(a->zone->spin);
#ifdef PARANOID
   in_SI_dealloc = 0;
   verify_tree(a->free_blocks);
#endif

   /* if (zdebug) fprintf(stderr,"SI_dealloc(%x)\n",(unsigned int)x); */
}


/********************************************************************/
/*                       GARBAGE COLLECTION
**
** Collect memory.  This walks the descriptor bits, reconstructing the
** free_block information and coalescing adjacent blocks.  All spinlocks
** should be given up before calling this, to avoid deadlock.
*/

#ifdef SI_STATISTICS
static int in_collection;
#endif
static prusage_t usage_at_last_collect, usage_before, usage_after;
static void collect() {
   size_t i, j;
   size_t freed = 0, leaf_heap = 0;
   size_t returned = 0;
   caddr_t x, y, prev;
   allocator_t a;
   page_desc aa;
   int retval;
   double elapsed_time, page_time, gc_time, gc_page_time, ratio;

   if (zdebug) fprintf(stderr, "\nGC: ");

   /*
   ** Get timing statistics before and after the collection.  We want
   ** page fault overhead to equal garbage collect overhead.
   */
   usage_at_last_collect = usage_after;
   retval = ioctl(pidfd, PIOCUSAGE, &usage_before);
   if (retval < 0) { perror("Couldn't PIOCLUSAGE"); exit(1); }

   /* 
   ** Obtain all spinlocks, in order to get the zone structures in
   ** a known state before collecting.  The cluster_spin spinlock
   ** is only obtained while a zone spinlock is obtained, so we
   ** don't have to worry about it - it suffices to have all the
   ** zone spinlocks.
   */
   acquire_spinlocks(SI_global);

#ifdef SI_STATISTICS
   in_collection = 1;

   if (zdebug) {
      int sum;
      fprintf(stderr,"Allocations ---\nOb size\tCount\tPercent by mass\n");
      sum=0; for (i=0;i<K;i++) sum+=i*statstab[i];
      sum/=100;
      for (i=0;i<K;i++) {
	 size_t p = i*statstab[i]/sum;
	 if (p>0) fprintf(stderr, "%d\t%d\t%d\n", i, statstab[i], p); 
	 statstab[i]=0;
      }
   }
#endif

   if (zdebug) fprintf(stderr, "marking... ");

   SI_mark_roots();

   if (zdebug) fprintf(stderr, "sweeping... ");

   /* First clear out all zones and splay free lists. */
   splay_top = splay_heap;
   clear_free_lists(SI_global);

   /* Now merge adjacent free spots and reconstruct free lists. */
   /* REPLACE THIS WITH EFFICIENT VERSION */
   /* ALSO, ZONE OBJECTS THEMSELVES SHOULD BE COLLECTABLE */

   i = 0;
   while (i<MAXIMUM_PAGES) {
      /* One contiguous set of pages belonging to same allocator at a time */
      j = i+1;
      aa = page_desc_table[i];
      if (aa!=NULL) {
	 while (j<MAXIMUM_PAGES && page_desc_table[j]==aa) j++;
	 a = ALLOCATOR_OF(aa);
	 x = heap + SI_BYTES_PER_PAGE * i;
	 y = heap + SI_BYTES_PER_PAGE * j;
	 if (!PAGE_WITH_POINTERS(aa)) leaf_heap += y-x;
	 prev = NULL;
	 while (x<y) {
	    /* One parcel at a time */
	    PARANOID_ONLY(assert(DESC_BIT(x,parcel));)
	    if (DESC_BIT(x,start)) { /* If there was an object in this parcel */
	       if (DESC_BIT(x,mark)) { /* ... and it was marked */
		  CLEAR_DESC_BIT(x,mark);
		  if (prev!=NULL) {
		     add_free_parcel(a, prev, x - prev);
		     freed += x - prev;
		     prev = NULL;
		  }
	       } else {                /* ... and it was not marked */
		  CLEAR_DESC_BIT(x,start);
		  if (prev==NULL) prev = x;
		  else CLEAR_DESC_BIT(x,parcel);
	       }
	    } else { /* No object was in this parcel */
	       if (prev==NULL) prev = x;
	       else CLEAR_DESC_BIT(x,parcel);
	    }
	    x += parcel_size(x);
	 }
	 PARANOID_ONLY(assert(x==y);)
	 PARANOID_ONLY(assert((((size_t)x)&PAGE_UPPER_MASK)==((size_t)x));)
	 if (prev!=NULL) {
	    /* If pages at end are empty, return them to freedom */
	    if (x-prev >= SI_BYTES_PER_PAGE) {
	       caddr_t w = (caddr_t) ROUND_TO_PAGE(prev);
	       size_t p = (w-heap)/SI_BYTES_PER_PAGE;
	       PARANOID_ONLY(assert(x-w > 0);)
	       set_page_descriptors(NULL, w, x-w);
	       returned += (x-w);
	       PARANOID_ONLY(assert(p < MAXIMUM_PAGES);)
	       if (p==0 || page_desc_table[p-1]!=NULL) {
		  SET_DESC_BIT(w,parcel);
	       } else
		  CLEAR_DESC_BIT(w,parcel);
	       if (j<MAXIMUM_PAGES && page_desc_table[j]==NULL)
		  CLEAR_DESC_BIT(x,parcel);
	       /* This would be a good place to return the memory
		  to the OS too, if that was desired. */
	       if (w!=prev) {
		  add_free_parcel(a, prev, w - prev);
		  freed += w - prev;
	       }
	    } else {
	       add_free_parcel(a, prev, x - prev);
	       freed += x - prev;
	    }
	 }
      }
      i=j;
   }

   if (zdebug)
      fprintf(stderr, "\n%dKB/%dKB = %d%% live   %dKB = %d%% returned   %d%% leaf heap\n",
			(zoned_heap-returned-freed)/K, zoned_heap/K,
			(zoned_heap-returned-freed)/(zoned_heap/100),
			returned/K, returned/(zoned_heap/100),
			leaf_heap/(zoned_heap/100));

   zoned_heap -= returned;
   PARANOID_ONLY(assert(zoned_heap < SI_MAXIMUM_HEAP);)

   retval = ioctl(pidfd, PIOCUSAGE, &usage_after);
   if (retval < 0) { perror("Couldn't PIOCLUSAGE"); exit(1); }

   elapsed_time = secs(usage_after.pr_rtime) 
		     - secs(usage_at_last_collect.pr_rtime);
   page_time = secs(usage_after.pr_dftime)  /* includes GC paging */
		     + secs(usage_after.pr_tftime)
		     + secs(usage_after.pr_kftime)
		     - secs(usage_at_last_collect.pr_dftime)
		     - secs(usage_at_last_collect.pr_tftime)
		     - secs(usage_at_last_collect.pr_kftime);

   /* Sometimes page time is less than zero (inexplicably). */
   if (page_time < 0.0) page_time = -page_time;

   gc_page_time = secs(usage_after.pr_dftime) 
		     + secs(usage_after.pr_tftime)
		     + secs(usage_after.pr_kftime)
		     - secs(usage_before.pr_tftime)
		     - secs(usage_before.pr_kftime)
		     - secs(usage_before.pr_dftime);
   gc_time = secs(usage_after.pr_rtime) - secs(usage_before.pr_rtime)
		     - gc_page_time;

   if (page_time==0.0 || gc_limit < physical_bytes_per_node/2)
      /* If it looks like there won't be any problems, go ahead and take
	 a big chunk. */
      if ((gc_limit*1.5)<physical_bytes_per_node) ratio = 1.5;
      else if ((gc_limit*1.1)<physical_bytes_per_node) ratio = 1.1;
      else ratio = 1.05;
   else {
      /* If there is any evidence we might be near the edge of the
	 thrashing threshold, then play very conservatively. */
      ratio = gc_time / (page_time * 10.0);  /* Paging likely underestimated */
      if (ratio > 1.05) ratio = 1.05;
   }
   gc_limit_was = gc_limit;
   gc_limit = (size_t) (gc_limit * ratio);
   if (gc_limit < zoned_heap * 0.95) gc_limit = (size_t) (zoned_heap * 0.95);
   if (gc_limit < SI_MINIMUM_HEAP) gc_limit = SI_MINIMUM_HEAP;
   else if (gc_limit > SI_MAXIMUM_HEAP) gc_limit = SI_MAXIMUM_HEAP;
   
   if (zdebug) fprintf(stderr, "CPU overhead: %.3g/%.3g = %d%%   Paging: %.3g = %d%%   GC paging: %.3g (secs)\nHeap limit set to %dKB (%+d%%)\n", 
       gc_time, elapsed_time, (int)(gc_time*100/elapsed_time),
       page_time, (int)(page_time*100/elapsed_time), gc_page_time,
       gc_limit/K, gc_limit/(gc_limit_was/100)-100);

#ifdef SI_STATISTICS
   if (zdebug) {
      int i, sum;
      fprintf(stderr, "Total allocated: %dMB  Fragmented: %d%% internal, %d%% external\n",
	 total_allocated/M, 
	 (total_allocated-requested)/(total_allocated/100),
	 (last_free-(total_allocated-last_allocated))/(last_free/100));
      last_allocated = total_allocated;
      last_free=freed;
      if (gc_limit>zoned_heap) last_free+=(gc_limit-zoned_heap);
      if (zdebug) {
	 fprintf(stderr,"Free ---\nOb size\tCount\tPercent by mass\n");
	 sum=0; for (i=0;i<K;i++) sum+=i*statstab[i];
	 sum/=100;
	 for (i=0;i<K;i++) {
	    size_t p = i*statstab[i]/sum;
	    if (p>0) fprintf(stderr, "%d\t%d\t%d\n", i, statstab[i], p); 
	    statstab[i]=0;
	 }
      }
   }

   in_collection = 0;
#endif

   /* All done, release the spinlocks again. */
   release_spinlocks(SI_global);
   PARANOID_ONLY(verify_tree(SI_global->allocator.free_blocks);)
   PARANOID_ONLY(verify_tree(SI_global->leaf_allocator.free_blocks);)

   if (SI_collect_hook!=NULL) SI_collect_hook();

}

/* Clear out free lists of zones rooted at z. */
static void clear_free_lists(SI_zone_t z) {
   unsigned int i;
   if (z==NULL) return;
   for (i=0; i<MAX_OBJECTS_PER_BLOCK; i++) {
      z->allocator.bins[i] = NULL;
      z->leaf_allocator.bins[i] = NULL;
   }
   z->allocator.free_blocks = NULL;
   z->leaf_allocator.free_blocks = NULL;
   z->allocator.splay_freelist = NULL;
   z->leaf_allocator.splay_freelist = NULL;
   clear_free_lists(z->children);
   clear_free_lists(z->next_sibling);
}

static void acquire_spinlocks(SI_zone_t z) {
   if (z==NULL) return;
   BR_SPINLOCK_LOCK(z->spin);
   acquire_spinlocks(z->children);
   acquire_spinlocks(z->next_sibling);
}

static void release_spinlocks(SI_zone_t z) {
   if (z==NULL) return;
   BR_SPINLOCK_UNLOCK(z->spin);
   release_spinlocks(z->children);
   release_spinlocks(z->next_sibling);
}

/* Convert timestruc_t format to seconds */
static double secs(timestruc_t x) {
   return ((double)x.tv_sec) + ((double)x.tv_nsec)/1.0e9;
}

/*
** Look for roots in a region of memory.  Assumes pointers are aligned and
** that `start' is pointer aligned.  This is the critical inner loop of
** the mark phase, so it is coded to help the C compiler as much as 
** possible.
*/

void mark_from_region(caddr_t beginning, size_t len) {

   /* Hoist everything imaginable */
   caddr_t lheap = heap;
   region_desc lregion_desc_table = region_desc_table;
   page_desc *lpage_desc_table = page_desc_table;
   caddr_t above_heap = lheap + SI_MAXIMUM_HEAP;
   page_desc a;
   caddr_t ob;
   region_desc reg;
   size_t offset;
   word mb, mask;
   caddr_t x;
   mark_entry *tos = mark_stack;

   /* printf("Region begin: %08lx, length %lu\n", (word)beginning, (word)len); */

   while (1) {
      caddr_t above_region = beginning + len;
      for (x=beginning; x<above_region; x+=sizeof(caddr_t)) {
	 ob = *((caddr_t *)x);
	 if ((ob >= lheap) 
	       && ((((word)ob)&MIN_ALIGN_LOWER_MASK)==0)
	       && (ob < above_heap)) {
	    reg = lregion_desc_table + (((word)(ob-lheap))>>BITS_FOR_REGION);
	    offset = (((word)ob)&REGION_LOWER_MASK)>>BITS_FOR_MIN_ALIGN;
	    mask = (1<<offset);
	    mb = reg->mark_bits;
	    if (((reg->start_bits)&(~(mb))) & mask) {
	       a = lpage_desc_table[(ob-lheap)>>BITS_FOR_PAGE];
	       PARANOID_ONLY(assert(a!=NULL);)
	       reg->mark_bits = mb | mask;
	       if (PAGE_WITH_POINTERS(a)) {
		  /* printf("Calling begin: %08lx, length %lu\n",(u_long)ob,(u_long)parcel_size(ob)); */
		  tos->ob = ob;
		  tos->len = parcel_size(ob);
		  tos++;
	       }
	    }
	 }
      }
      tos--;
      if (tos < mark_stack) return;
      beginning = tos->ob;
      len = tos->len;
   }
}



/********************************************************************/
/*                   MISC SIVA FUNCTIONALITY
*/

#if 0
/* Allocate storage which is typed and may have pointers. */
caddr_t SI_alloc(size_t size) {
   /* For now, everything is being scanned conservatively. */
   return allocate(&(SI_here->allocator), size);
}
 
/* Allocate storage which must be scanned conservatively. */
caddr_t SI_alloc_untyped(size_t size) {
   /* For now, everything is being scanned conservatively. */
   return allocate(&(SI_here->allocator), size);
}
 
/* Allocate some storage that will never be scanned for pointers. */
caddr_t SI_alloc_leaf(size_t size) {
   /* For now, everything is being scanned conservatively. */
   return allocate(&(SI_here->leaf_allocator), size);
}
#endif

/*
** Shut system down.
*/
void SI_exit() {
   close(pidfd);
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

/* Perform garbage collection now.  This is only a hint. */
void SI_collect_now() {
   collect();
}

/* Create a software zone which is a child of the current zone. */
SI_zone_t SI_create() {
   SI_zone_t res;
   res = (SI_zone_t) SI_ALLOC(sizeof(struct SI_zone_t_struct));
   BR_SPINLOCK_INIT(res->spin);
   res->allocator.zone = res;
   res->leaf_allocator.zone = res;
   res->capacity = SI_here->capacity;
   res->cluster = SI_here->cluster; /* For now */
   res->divisions = -1; 	/* A software zone */
   res->home = ((unsigned)lrand48())%MAXIMUM_PAGES;
   res->parent = SI_here;
   res->children = NULL;
   BR_SPINLOCK_LOCK(SI_here->spin);
   res->next_sibling = SI_here->children;
   SI_here->children = res;
   BR_SPINLOCK_UNLOCK(SI_here->spin);
   PARANOID_ONLY((void) SI_WHERE(res);)
   return res;
}

SI_zone_t SI_WHERE(void *x) {
   PARANOID_ONLY(assert(((caddr_t)x)>=heap && ((caddr_t)x)<(heap+SI_MAXIMUM_HEAP)));
   PARANOID_ONLY(assert(page_desc_table[(((caddr_t)x)-heap)>>BITS_FOR_PAGE] != NULL);)
   return ALLOCATOR_OF(page_desc_table[(((caddr_t)x)-heap)>>BITS_FOR_PAGE])->zone;
}

/* Nonzero if zone y is within (a descendent of) zone x. */
int SI_within(SI_zone_t y, SI_zone_t x) {
   SI_zone_t p = y;
   if (x == y) return 0;   /* Zones don't contain themselves */
   while (1) {
      p = p->parent;
      if (p == NULL) return 0;
      if (p == x) return 1;
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
   SI_zone_t p = x;
   while (1) {
      /* The top zone will always be hardware with divisions > 0 */
      if (p->divisions >= 0) {
	 if (d<0 || d>=p->divisions) 
	    SI_fatal("ZONE::division argument out of range");
	 SI_fatal("division call not implemented");
	 }
      p = p->parent;
   }
}

/* Return capacity of the enclosing hardware zone. */
unsigned int SI_capacity(SI_zone_t x) {
   return x->capacity;
}

/* Return the enclosing hardware zone of x in which y resides. */
SI_zone_t SI_division_of(SI_zone_t x, void *y) {
   SI_fatal("division_of call not implemented");
   abort();
}

/*****************************************************************/
/*                           SPLAY CODE
**
** This code has been adapted from D. Sleator's splay code to use the
** splay algorithms for manipulating blocks of memory.  The splay nodes
** are actually headers of larger contiguous blocks of memory of at least
** cache block size.  The field sorted on is the size of the block.  The
** code has been adapted to allow multiple instances of the size key (it's
** a bag, not a set) and to make it easy to repeatedly query for the next
** larger value (to support best+aligned fit.)
** 
** The original code is available at
** "ftp://ftp.cs.cmu.edu/user/sleator/".
*/

static splay_node splay_splay(size_t i, splay_node t) {
/* Simple top down splay, not requiring i to be in the tree t.  */
/* What it does is described above.  Lots of extra locals are   */
/* used to help the compiler hoist unchanging expressions.      */
    struct splay_node_struct N;
    splay_node l, r, y;
    size_t t_size;
    splay_node t_left, t_right;
    if (t == NULL) return t;
    N.left = N.right = NULL;
    l = r = &N;

    while (1) {
	t_size = t->size;
	if (i < t_size) {  
	    t_left = t->left;
	    if (t_left == NULL) break;
	    if (i < t_left->size) {
		y = t_left;                           /* rotate right */
		t_left = y->right;
		t->left = t_left;
		y->right = t;
		t = y;
		t_left = t->left;
		if (t_left == NULL) break;
	    }
	    r->left = t;                               /* link right */
	    r = t;
	    t = t_left;
	} else if (i > t_size) {
	    t_right = t->right;
	    if (t_right == NULL) break;
	    if (i > t_right->size) {
		y = t_right;                          /* rotate left */
		t_right = y->left;
		t->right = t_right;
		y->left = t;
		t = y;
		t_right = t->right;
		if (t_right == NULL) break;
	    }
	    l->right = t;                              /* link left */
	    l = t;
	    t = t_right;
	}
	else break;
    }
    l->right = t->left;                                /* assemble */
    r->left = t->right;
    t->left = N.right;
    t->right = N.left;
    return t;
}

static splay_node splay_insert(splay_node new, splay_node t) {
/* Insert new into the tree t				*/
/* Return a pointer to the resulting tree.              */
    size_t i = new->size;

    if (t == NULL) {
	new->left = new->right = NULL;
	return new;
    }
    t = splay_splay(i,t);
    if (i <= t->size) {
	new->left = t->left;
	new->right = t;
	t->left = NULL;
	return new;
    } else {
	new->right = t->right;
	new->left = t;
	t->right = NULL;
	return new;
    }
}

static splay_node splay_delete_root(splay_node t) {
/* Delete the root of the tree, moving the next highest */
/* value into place at the root.			*/
   splay_node x;
   if (t->right == NULL)
      return t->left;
   /* x = splay_splay(t->size-1, t->right); */
   x = splay_splay(t->size, t->right);
   x->left = t->left;
   return x;
}

static splay_node splay_next_bigger(splay_node t) {
/* splay to root the next higher node. */
   splay_node x;
   if (t == NULL) return t;
   if (t->right == NULL)
      return t;
   /* x = splay_splay(t->size-1, t->right); */
   x = splay_splay(t->size, t->right);
   t->right = NULL;
   x->left = t;
   return x;
}

/***********************************************************************/
/*                         SPLAY NODE MANAGEMENT
**
** Allocation and deallocation of splay nodes.
** These routines assume the zone has already been locked but not the
** cluster.
*/

static splay_node new_splay(allocator_t a) {
   size_t i;
   splay_node f;

   if (a->splay_freelist == NULL) {
      /* There weren't any splay nodes, so get a page's worth */
      BR_SPINLOCK_LOCK(cluster_spin);
      a->splay_freelist = splay_top;
      for (i=0; i<(SI_BYTES_PER_PAGE/sizeof(struct splay_node_struct))-1; i++) {
	 splay_top->left=(splay_top+1);
	 splay_top++;
      }
      splay_top->left = NULL;
      splay_top++;
      BR_SPINLOCK_UNLOCK(cluster_spin);
   }
   f = a->splay_freelist;
   a->splay_freelist = f->left;
   return f;
}

static void recycle_splay(allocator_t a, splay_node s) {
   s->left = a->splay_freelist;
   a->splay_freelist = s;
}


/************************************************************************/
/*                        DEBUGGING STUFF
*/

#ifdef PARANOID
/* Some atomic-powered debugging stuff. */

static void verify_tree(splay_node t) {
   size_t ps;
   caddr_t p;
   if (t==NULL) return;
   p = t->mem;
   if (DESC_BIT(p,start)) {
      fprintf(stderr,"Descriptor bit was on\n");
      abort();
   }
   ps = parcel_size(p);
   if (ps != t->size || ps < 32) {
      fprintf(stderr,"t->size %d, parcel size %d\n",t->size,ps);
      abort();
   }
   if (ROUND_TO_BLOCK(((size_t)p))!=((size_t)p)) {
      printf("node not block aligned\n");
      abort();
   }

#if 0
   verify_tree(t->left);
   verify_tree(t->right);
#endif
}

word C_heap_checksum;
caddr_t top_of_heap_at_freeze;
extern edata;
#define BOTTOM_OF_C_HEAP ((caddr_t)0x42000) /* Could change from run to run */
#define BOTTOM_OF_C_HEAP_WAS ((~(sysconf(_SC_PAGESIZE)-1))&((word)(&edata)))
#define TOP_OF_C_HEAP ((caddr_t)(((((word)sbrk(0))-1)&(~(sysconf(_SC_PAGESIZE)-1)))+sysconf(_SC_PAGESIZE)))

word compute_checksum() {
   word *ptr, *sb;
   word sum = 0;

   sb = (word*) TOP_OF_C_HEAP;
   for (ptr=(word*)BOTTOM_OF_C_HEAP; ptr < sb; ptr++) 
      if (ptr!=(&C_heap_checksum)) {
	 sum ^= *ptr;
	 sum = (sum << 17) ^ (sum >> 15);
      }
   return sum;
}

void start_C_heap_check() {
   top_of_heap_at_freeze = TOP_OF_C_HEAP;

/*
**    printf("Protecting [%x, %x)\n",(unsigned int)BOTTOM_OF_C_HEAP,(unsigned int)top_of_heap_at_freeze);
**    fflush(stdout);
*/

   C_heap_checksum = compute_checksum();
   if (mprotect(BOTTOM_OF_C_HEAP, top_of_heap_at_freeze - BOTTOM_OF_C_HEAP,
		PROT_READ)<0) {
      perror("couldn't mprotect A"); exit(1);
   }
}

void end_C_heap_check() {
   if (mprotect(BOTTOM_OF_C_HEAP, top_of_heap_at_freeze - BOTTOM_OF_C_HEAP,
		PROT_READ|PROT_WRITE)<0) {
      perror("couldn't mprotect A"); exit(1);
   }

   if (compute_checksum() != C_heap_checksum) abort();
}

#endif /* PARANOID */

/*
** Debugging function: prints the regions descriptors for a range [x, y]
** of memory in an intelligible form.
*/

void debug_word(word x);

void debug_regions(caddr_t x, caddr_t y) {
   unsigned long xmem, ymem, i;
   xmem = ((unsigned long) x) & REGION_UPPER_MASK;
   ymem = ((unsigned long) y) | REGION_LOWER_MASK;
   xmem -= (unsigned long) heap;
   ymem -= (unsigned long) heap;
   xmem /= BYTES_PER_REGION;
   ymem /= BYTES_PER_REGION;
   for (i=xmem; i<=ymem; i++) {
      printf("\nRegion for %08lx - %08lx:",
		((unsigned long) heap) + BYTES_PER_REGION*i,
		((unsigned long) heap) + BYTES_PER_REGION*(i+1) - 1);
      printf("\nParcel bits: "); debug_word(region_desc_table[i].parcel_bits);
      printf("\nStart bits:  "); debug_word(region_desc_table[i].start_bits);
      printf("\nMark bits:   "); debug_word(region_desc_table[i].mark_bits);
      putchar('\n');
   }
}

void debug_word(word x) {
   int i;
   for (i=BITS_PER_WORD-1;i>=0;i--) {
      if (TEST_BIT(i,x)) putchar('1');
      else putchar('0');
   }
}

