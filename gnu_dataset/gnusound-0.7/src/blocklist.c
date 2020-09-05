/*
 * GNUsound - a sound editor for GNOME.
 * Copyright (C) 2002-2004  Pascal Haakmat <a.haakmat@chello.nl>
 *
 * This program is free software; you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation; either version 2 of the License, or
 * (at your option) any later version.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with this program; if not, write to the Free Software
 * Foundation, Inc., 675 Mass Ave, Cambridge, MA 02139, USA.
 *
 */

/**
 * @file
 * A blocklist is a component of a track, used to manage the list of
 * blocks that make up a track. Basically a blocklist is a fancy,
 * specialized version of a linked list. The problem with an ordinary
 * linked list is that it becomes very slow to find blocks near the end
 * of the linked list as the number of blocks increases. Since a block
 * only knows its size, and does not know what position it is in
 * relative to the other blocks, when searching for a block at a
 * specific position, it is necessary to do a linear search of all
 * blocks preceding the required block to find it.
 *
 * To speed up this process, the blocklist maintains a small cache of
 * recently referenced blocks (the bcache) and their positions in the
 * track. In other words, it maintains a partial index. When the block
 * for a specific position is requested, the blocklist first looks in
 * the cache to see if the block is in there. If it is, then that
 * block is returned. Otherwise, the block that is closest to the
 * requested position is returned. From there the required block is
 * located using a linear search and added to the cache. The cache is
 * updated after every insert or delete operation. This admittedly
 * crude mechanism is surprisingly effective. The key, of course, is
 * to tune the number of elements in the bcache so as to remain as
 * small as possible, while still getting decent hit rates
 * (MAX_BCACHE_ENTRIES). Since audio application generally have
 * straightforward access patterns this is feasible.
 *
 * Another obvious optimization would be to maintain a list of lists,
 * with each list containing some maximum number of samples.
 * 
 */

#include <config.h>
#include <assert.h>
#include <glib.h>
#include "blocklist.h"
#include "mem.h"
#include "snd.h"
#include "block.h"
#include "sample.h"

#define USE_NEW_INSERT     1
#define USE_BCACHE         1
#define USE_EXTRA_CHECKS   0

AFframecount
blocklist_blocks_count(GList *l) {
    AFframecount fc = 0;
    for(; l; l = l->next) 
        fc += ((block *)l->data)->count;
    return fc;    
}

#if USE_EXTRA_CHECKS
#define VERIFY_COUNT(blocklist) \
  if((blocklist)->count != blocklist_blocks_count((blocklist)->l)) { \
    FAIL("bl->count(%ld) != blocklist_count(%ld)!\n", \
            (blocklist)->count, blocklist_blocks_count((blocklist)->l)); \
    blocklist_dump((blocklist)); \
    *(char *)NULL = 0; \
  } 
#define VERIFY_TAIL(blocklist) \
  if(((blocklist)->tail && \
      (blocklist)->tail->next != NULL) || \
     g_list_last((blocklist)->l) != (blocklist)->tail) { \
    FAIL("bl->tail(%p) != g_list_last(%p)!\n", \
         (blocklist)->tail, g_list_last((blocklist)->l)); \
    blocklist_dump((blocklist)); \
    abort(); \
  }     
#else
#define VERIFY_COUNT(blocklist) 
#define VERIFY_TAIL(blocklist) 
#endif

AFframecount
blocklist_count(struct blocklist *bl) {
    VERIFY_COUNT(bl);
    return bl->count;
}

/*
 * Updates the cache after an insert operation of count frames
 * at offset. Determines whether the position of the cache
 * elements has moved forward or has been left unchanged.
 */

void
blocklist_bcache_insert_adjust(struct blocklist *bl,
                               AFframecount offset,
                               AFframecount count) {
    int i;
    struct bcache *bc;
    for(i = 0; i < MAX_BCACHE_ENTRIES; i++) {
        bc = &bl->bc[i];

        if(!bc->is_valid)
            continue;

        if(bc->offset < offset)
            continue;

        if(bc->offset < offset &&
           bc->offset + ((block *)bc->block)->count > offset) {
            continue;
        }

        if(bc->offset >= offset &&
           bc->offset + ((block *)bc->block)->count <= offset + count) {
            bc->offset += count;
            continue;
        }

        if(bc->offset >= offset &&
           bc->offset < offset + count &&
           bc->offset + ((block *)bc->block)->count > offset + count) {
            bc->offset += count;
            continue;
        }

        if(bc->offset > offset) {
            bc->offset += count;
            continue;
        }

        DEBUG("all cases missed?\n");
    }
}

/*
 * Updates the cache after a delete operation of count frames at
 * offset. Examines all cache elements and determines whether
 * the position of cache elements has moved forward, has become
 * invalid, or left unchanged.
 */

void
blocklist_bcache_delete_adjust(struct blocklist *bl,
                               AFframecount offset,
                               AFframecount count) {
    int i;
    struct bcache *bc;
    for(i = 0; i < MAX_BCACHE_ENTRIES; i++) {
        bc = &bl->bc[i];

        if(!bc->is_valid)
            continue;

        /* Cache element is before deletion, nothing happens. */

        if(bc->offset < offset)
            continue;

        if(bc->offset < offset &&
           bc->offset + ((block *)bc->block)->count <= offset + count)
            continue;

        if(bc->offset >= offset && 
           bc->offset + ((block *)bc->block)->count <= offset + count) { 
            bc->is_valid = 0;
            continue;
        }

        if(bc->offset >= offset && 
           bc->offset + ((block *)bc->block)->count > offset + count) {
            bc->is_valid = 0;
            continue;
        }

        if(bc->offset > offset + count) {
            bc->offset -= count;
            continue;
        }

        DEBUG("missed all cases?\n");
    }
}

/*
 * Locates a block in the bcache that contains, or is close to, the
 * position given by offset.
 *
 * On success, 0 is returned, the b parameter receives the block that
 * was found and the offset parameter is adjusted to become a
 * relative offset into that block.
 *
 * Otherwise a non-zero error code is returned. There are two cases:
 * if the cache was empty, then the b parameter receives
 * NULL. Otherwise the b parameter receives the block closest to the
 * requested offset and the offset receives the absolute offset
 * of that block.
 */

int
blocklist_bcache_find(struct blocklist *bl,
                      AFframecount *offset,
                      GList **b) {
    int i;
    struct bcache *bc, *bc_nearest = NULL;
    AFframecount d = 0, org_off = *offset;

    for(i = 0; i < MAX_BCACHE_ENTRIES; i++) {
        bc = &bl->bc[i];
        if(!bc->is_valid) 
            continue;
        if(!bc_nearest || ABS(org_off - bc->offset) < ABS(d)) {
            d = org_off - bc->offset;
            bc_nearest = bc;
        }
        if(*offset >= bc->offset &&
           *offset < bc->offset + ((block *)(bc->block->data))->count) {
            *offset -= bc->offset;
            bc->hits++;
            //                DEBUG("offset: %ld, bc->offset: %ld, bc->block->count: %ld, returning %d\n",
            //                      *offset, bc->offset, ((block *)(bc->block->data))->count, i);
            *b = bc->block;
            return 0;
        }
    }

    /* Cache was empty. */

    if(!bc_nearest) {
        *b = NULL;
        return 1;
    }

    *b = bc_nearest->block;
    *offset = bc_nearest->offset;
    return 1;
}

/*
 * Adds a block l at position offset to the cache.  If the cache
 * has room, then it is added at the first available position.
 * Otherwise, the cache element that was least frequently used is
 * discarded to make room.
 */

void
blocklist_bcache_fill(struct blocklist *bl,
                      AFframecount offset,
                      GList *l) {
    int i, lfu = 0, lowest_hits = -1;
    struct bcache *bc;
    if(offset == blocklist_count(bl)) {
        DEBUG("offset: %ld\n", offset);
        blocklist_dump(bl);
        abort();
    }
    for(i = 0; i < MAX_BCACHE_ENTRIES; i++) {
        bc = &bl->bc[i];
        if(!bc->is_valid) {
            bc->offset = offset;
            bc->hits = 0;
            bc->block = l;
            bc->is_valid = 1;
            return;
        }
        if(bc->offset == offset)
            return;
        if(lowest_hits == -1 || bc->hits <= lowest_hits) {
            lowest_hits = bc->hits;
            lfu = i;
        }
    }
    
    /* No empty, throw away LFU. */

    bl->bc[lfu].offset = offset;
    bl->bc[lfu].hits = 0;
    bl->bc[lfu].block = l;
    bl->bc[lfu].is_valid = 1;
}

/**
 * Finds the block containing *offset, given an existing block
 * with a known position.
 * 
 * @param bl The blocklist.
 * @param offset On entry, contains the request offset,
 * on exit, depending on whether the block was actually found,
 * receives either the relative offset into the found block, or the
 * distance to the end of the last block in the list.
 * @param b On entry, points to a block to start the search from. On
 * exit, recieves the block that contains offset (if found),
 * or the last block in the list otherwise.
 * @param block_offset The absolute offset of the block b.
 *
 * @return On success, 0 is returned and the b parameter receives the
 * found block, and offset is adjusted to become a relative
 * offset into that block. Otherwise, a non-zero error code is
 * returned, b receives the last block in the list, and offset
 * receives the distance of the _end_ of that block to the requested
 * position.
 */

int
blocklist_find_block_from(struct blocklist *bl,
                          AFframecount *offset,
                          GList **b,
                          AFframecount block_offset) {
    GList *l;
    AFframecount org_off = *offset;

    //    DEBUG("offset: %ld, block_offset: %ld\n",
    //          *offset, block_offset);
    //    blocklist_dump(bl);

    if(*offset - block_offset >= 0) {

        /* Wanted offset is after block. */

        *offset -= block_offset;
        for(l = *b; l; l = l->next) {
            *b = l;
            if(*offset < ((block *)l->data)->count) {
                blocklist_bcache_fill(bl, block_offset, l);
                //    DEBUG("returning 0, offset: %ld, *b: %p\n",
                //      *offset, *b);
                return 0;
            }
            *offset -= ((block *)l->data)->count;
            block_offset += ((block *)l->data)->count;
        }
        //        DEBUG("returning 1, offset: %ld, *b: %p\n",
        //             *offset, *b);
        return 1;

    } else {

        /* Wanted offset is before block. */

        for(; *b; *b = (*b)->prev) {
            if(*offset >= block_offset) {
                *offset -= block_offset;
                blocklist_bcache_fill(bl, block_offset, *b);
                //                DEBUG("returning 0, offset: %ld, *b: %p\n",
                //       *offset, *b);
                return 0;
            }
            if(!(*b)->prev)
                break;
            block_offset -= ((block *)((*b)->prev->data))->count;
        }
    }
        
    /* The frame offset is invalid. */

    FAIL("invalid offset: %ld\n", org_off);
    blocklist_dump(bl);
    abort();
    return 1;
}

/**
 * Finds a block given an offset. 
 *
 * @param bl The blocklist.
 * @param offset The offset to locate.
 * @param b Receives the block requested.
 * @return On success, 0 is returned, b receives the requested block,
 * and offset receives the relative offset in that block.
 * 
 * Otherwise, 1 is returned, b receives the last block in this
 * blocklist, and offset receives the distance between
 * the end of that block and the requested offset.
 */

int
blocklist_find_block(struct blocklist *bl,
                     AFframecount *offset,
                     GList **b) {
    AFframecount from_offset = 0;
    int err;
#ifdef USE_BCACHE
    AFframecount requested_offset = *offset;
#endif
    *b = bl->l;
#ifdef USE_BCACHE
    err = blocklist_bcache_find(bl, offset, b);
    
    if(!err) 
        return 0;

    if(err && *b) 
        from_offset = *offset;
    else
        *b = bl->l;

    *offset = requested_offset;
#endif
    return blocklist_find_block_from(bl, offset, b, from_offset);
}

/**
 * Discards any trailing NULL blocks.
 */

AFframecount
blocklist_trim_right(struct blocklist *bl) {
    GList *l;
    AFframecount deleted = 0, offset = blocklist_count(bl);

    VERIFY_TAIL(bl);

    l = bl->tail;

    while(l && ((block *)l->data)->type == CACHE_NULL) {
        
        bl->tail = l->prev;

        if(l->prev) 
            l->prev->next = NULL;
        else
            bl->l = NULL;

        l->prev = NULL;

        deleted += ((block *)l->data)->count;

        DEBUG("trimming %ld frames\n", ((block *)l->data)->count);
        
        block_unref((block *)l->data);

        g_list_free(l);

        l = bl->tail;

    }

    blocklist_bcache_delete_adjust(bl, offset - deleted, deleted);
    bl->count -= deleted;

    VERIFY_TAIL(bl);

    return deleted;
}

/**
 * Removes count frames at offset and returns a list of blocks
 * containing the deleted blocks. Any trailing NULL blocks which
 * remain after the delete operation are discarded.
 *
 * @param bl The blocklist.
 * @param deleted Contains the deleted blocks on success.
 * @param offset At which sample to start deleting.
 * @param count How many samples to delete.
 * @return 0 for success, non-zero error code otherwise.
 */

int
blocklist_delete(struct blocklist *bl,
                 GList **deleted,
                 AFframecount offset,
                 AFframecount count) {
    int err;
    block *blk_split;
    GList *l, *l_next, *del_blks = NULL;
    AFframecount count_old = count;
    AFframecount deleted_count = 0, requested_offset = offset;
    gpointer data;

    /*
     * The general idea:
     *
     * 1. Find the block that the offset resides in. This translates
     *    offset into a local offset relative to the block.
     *
     * 2. If the deletion count extends beyond block[idx], and does
     *    not start at offset 0 in block[lpos], then block[lpos] is split
     *    at the offset and we proceed to the next block.
     *
     * 3. As long as the deletion count exceeds or equals the number
     *    of frames in block[idx], we subtract the number of frames in
     *    the block from the deletion count, and move the block from the
     *    active list to the deleted list.
     *
     * 4. At this point if the deletion count is still non-zero, it is
     *    guaranteed to be smaller than one block. So if there is still a
     *    block left, we shrink it.
     *
     * Returns NULL (not enough memory to rearrange blocks or offset out
     * of range), or the start of the deleted block list. This function
     * may delete less frames than were requested if we run out of memory 
     * or count exceeds the track sample count. FIXME: maybe better
     * to promise either full success or full failure.
     */

    /* Step 1. */

    err = blocklist_find_block(bl, &offset, &l);

    if(err) 
        return err;

    /* Step 2. */

    if(offset && 
       offset + count > ((block *)l->data)->count) {
        count -= ((block *)l->data)->count - offset;
        //        DEBUG("splitting block...\n");
        blk_split = block_split(((block *)l->data), offset);
        
        if(!blk_split) {
            FAIL("cannot split block for deletion buffer\n");
            return 1;
        }

        del_blks = g_list_append(del_blks, blk_split);
        deleted_count += blk_split->count;
        offset = 0;
        l = l->next;
    }

    /* Step 3. */

    while(l && count >= ((block *)l->data)->count) {
        deleted_count += ((block *)l->data)->count;
        count -= ((block *)l->data)->count;
        del_blks = g_list_append(del_blks, l->data); /* FIXME: slow */

        /* Did we run into the last block? */

        if(l->next) {
            data = l->next->data;
        } else {
            data = NULL;
            bl->tail = l->prev;
        }

        //        data = l->next ? l->next->data : NULL;
        l_next = l->next;
        bl->l = g_list_remove_link(bl->l, l); /* FIXME: slow (?) */
        g_list_free(l);
        l = l_next;
    }

    /*
     * At this point either:
     * 1. The demand has been satisfied.
     * 2. The supply is exhausted.
     * 3. The remainder of the demand is smaller than 1 block.
     * Test for the first 2 conditions to see if we can exit.
     */
    
    if(count == 0 || (!l && count)) {
        if(!l && count)
            DEBUG("demand exceeds supply: demand: %ld, supply: %ld\n",
                  count_old, (count_old - count));
        DEBUG("bl->count: %ld, deleted_count: %ld\n",
              bl->count, deleted_count);
        bl->count -= deleted_count;
        blocklist_bcache_delete_adjust(bl, 
                                       requested_offset,
                                       deleted_count);
        VERIFY_COUNT(bl);
        VERIFY_TAIL(bl);
        *deleted = del_blks;
        return 0;
    }

    /*
     * Step 4. To satisfy the remainder of the demand we need 
     * to split the block we're at.
     */

    blk_split = block_clone((block *)l->data,
                            offset,
                            count);

    if(!blk_split) {
        FAIL("cannot split final block for deletion buffer\n");
        bl->count -= deleted_count;
        blocklist_bcache_delete_adjust(bl, 
                                       requested_offset,
                                       deleted_count);
        if(blocklist_insert_blocks(bl,
                                   del_blks,
                                   requested_offset)) 
            FAIL("cannot recover, %ld samples have been lost\n",
                 blocklist_blocks_count(del_blks));
        blocklist_blocks_destroy(del_blks);
        VERIFY_COUNT(bl);
        VERIFY_TAIL(bl);
        return 1;
    }

    del_blks = g_list_append(del_blks, blk_split);
    deleted_count += blk_split->count;
    
    block_move((block *)l->data, offset, 
               offset + count, 
               ((block *)l->data)->count - (offset + count));
    block_resize((block *)l->data,
                 offset + (((block *)l->data)->count - 
                           (offset + count)));

#if USE_EXTRA_CHECKS
    if(deleted_count != blocklist_blocks_count(del_blks)) {
        FAIL("calculated deleted sample count: %ld, actual: %ld\n", 
             deleted_count, blocklist_blocks_count(del_blks));
        abort();
    }
#endif

    bl->count -= deleted_count;
    blocklist_bcache_delete_adjust(bl, 
                                   requested_offset,
                                   deleted_count);
    VERIFY_COUNT(bl);
    VERIFY_TAIL(bl);
    *deleted = del_blks;
    return 0;
}

void
list_print(GList *l) {
    for(; l; l = l->next) 
        INFO("%p%s", 
             l, l->next ? " -> " : "");
    INFO("\n");
}

/**
 * Inserts the given block list into the target track at the specified
 * offset. The given blocks are not actually copied, instead their
 * reference count is increased and they are added to the target block
 * list. The source block list should be destroyed by the caller.
 *
 * @param bl The blocklist.
 * @param source The list of blocks to insert.
 * @param offset The offset in the blocklist to insert the blocks at.
 * Should be less than or equal to the number of samples in this
 * block list.
 * @return 0 for success.
 */

#if USE_NEW_INSERT
int
blocklist_insert_blocks(struct blocklist *bl,
                        const GList *source,
                        AFframecount offset) {
    block *blk_split = NULL;
    AFframecount inserted_count = 0, requested_offset = offset;
    const GList *src_ptr = NULL;
    GList *l = NULL, *dst_ptr = NULL;
    int err;

    if(offset > blocklist_count(bl)) {
        FAIL("offset(%ld) > blocklist_count(%ld)\n",
             offset, blocklist_count(bl));
        abort();
    }

    /* Blocklist is empty, no need to insert. */

    if(!bl->l) {
        bl->l = g_list_copy((GList *)source);
        if(!bl->l) {
            FAIL("could not copy list\n");
            return 1;
        }
        for(src_ptr = bl->l; src_ptr; src_ptr = src_ptr->next) {
            block_addref((block *)src_ptr->data);
            if(!src_ptr->next)
                bl->tail = src_ptr;
        }
        blocklist_blocks_set(bl, bl->l);
        VERIFY_TAIL(bl);
        return 0;
    }

    /* Otherwise find the block to insert into/behind. */

    err = blocklist_find_block(bl, &offset, &dst_ptr);
    
    if(err && !dst_ptr) {
        FAIL("didn't find offset %ld but also did not get last block\n", 
             requested_offset);
        blocklist_dump(bl);
        abort();
    }

    if(err && offset) {
        FAIL("didn't find offset %ld and offset extends %ld beyond "
             "end of last block\n", requested_offset, offset);
        blocklist_dump(bl);
        abort();
    }

    /* Didn't find the block, so append after last block. */

    if(err)
        offset = ((block *)dst_ptr->data)->count;

    /* Did find block: does it need to be split? */

    if(!err && offset && offset < ((block *)dst_ptr->data)->count) {

        /*
         * Yes, we need to split the block:
         *
         * +---------+ +---------------+ +---------+     
         * | block 1 |-| dst_ptr->data |-| block 3 |
         * +---------+ +---------------+ +---------+
         *                 |
         *                 +- splitting point
         *
         * +---------+ +---------------+ +---------+     
         * | block 1 |-| dst_ptr->data |-| block 3 |
         * +---------+ +---------------+ +---------+
         *                 +-----------+
         *                 | blk_split |
         *                 +-----------+
         */

        blk_split = block_split((block *)dst_ptr->data, offset);
        if(!blk_split) {
            FAIL("insertion at %ld failed: could not split block at %ld\n", 
                 requested_offset, offset);
            return 1;
        }

        /*
         * And reinsert the splitted block into the list:
         *
         * +---------+ +---------------+ +-----------+ +---------+
         * | block 1 |-| dst_ptr->data |-| blk_split |-| block 3 |
         * +---------+ +---------------+ +-----------+ +---------+
         */

        l = g_list_append(NULL, blk_split);

        if(dst_ptr->next) 
            dst_ptr->next->prev = l;
        else
            bl->tail = l;

        l->prev = dst_ptr;
        l->next = dst_ptr->next;
        dst_ptr->next = l;
    }

    /*
     * At this point all that remains is to copy each link from
     * src_ptr to dst_ptr. 
     */

    for(src_ptr = source; src_ptr; src_ptr = src_ptr->next) {
        block_addref((block *)src_ptr->data);

        /* Create a single element list to insert. */

        l = g_list_append(NULL, (block *)src_ptr->data);

        inserted_count += ((block *)src_ptr->data)->count;

        /* Either insert before or insert after. */

        if(offset == 0) {

            /* Insert the single element list immediately before dst_ptr. */

            l->prev = dst_ptr->prev;
            l->next = dst_ptr;
            if(l->prev) 
                l->prev->next = l;
            dst_ptr->prev = l;

            /* If dst_ptr was the start of the list, then now the
               single element is the new start of list. */
            
            if(dst_ptr == bl->l)
                bl->l = l;

        } else {

            /* Insert the single element list immediately after
               dst_ptr, and adjust dst_ptr to point at this
               element. */

            l->next = dst_ptr->next;
            if(dst_ptr->next)
                dst_ptr->next->prev = l;
            l->prev = dst_ptr;
            dst_ptr->next = l;
            dst_ptr = dst_ptr->next;

            if(!l->next)
                bl->tail = l;

        }
    }

    bl->count += inserted_count;
    blocklist_bcache_insert_adjust(bl, 
                                   requested_offset,
                                   inserted_count);
    VERIFY_COUNT(bl);
    VERIFY_TAIL(bl);

    return 0;
}

#else /* USE_NEW_INSERT */

/*
 * Old insert, relies on block position which is not provided by the
 * new blocklist_find_block() function. Slow but works. 
 */

GList *
blocklist_find_block_old(struct blocklist *bl,
                         AFframecount *offset,
                         int *lpos) {
    GList *l;
    if(lpos)
        *lpos = 0;
    for(l = bl->l; l; l = l->next) {
        if(((block *)l->data)->count > *offset) 
            return l;
        if(lpos)
            (*lpos)++;
        *offset -= ((block *)l->data)->count;
    }
    return NULL;
}

int
blocklist_insert_blocks(struct blocklist *bl,
                        const GList *source,
                        AFframecount offset) {
    block *blk_split = NULL;
    AFframecount inserted_count = 0, requested_offset = offset;
    GList *l1 = NULL, *l2;
    int lpos;
    
    if(offset > blocklist_count(bl)) {
        FAIL("offset(%ld) > blocklist_count(%ld)\n",
             offset, blocklist_count(bl));
        abort();
    }

    if(offset == blocklist_count(bl)) {

        /* Append, go to last block. */

        lpos = g_list_length(bl->l);
        offset = 0;

    } else {

        /* Otherwise find the block to insert into. */

        l1 = blocklist_find_block_old(bl, &offset, &lpos);
        if(offset) {
            blk_split = block_split(((block *)l1->data), offset);
            if(!blk_split) {
                FAIL("insertion at %ld failed: could not split block %d\n",
                     offset, lpos);
                return 1;
            }
        }
    }

    for(l2 = source; l2; l2 = l2->next, lpos++) {
        block_addref((block *)l2->data);
        inserted_count += ((block *)l2->data)->count;
        bl->l = g_list_insert(bl->l, l2->data,
                              !blk_split ? lpos : lpos + 1);
    }

    if(blk_split)
        bl->l = g_list_insert(bl->l, blk_split, lpos + 1);

    bl->count += inserted_count;
    blocklist_bcache_insert_adjust(bl,
                                   requested_offset,
                                   inserted_count);
    VERIFY_COUNT(bl);

    return 0;
}
#endif /* USE_NEW_INSERT */

/**
 * @internal
 * Tries to append the given buffer to the given block.
 * @return 0 on success, non-zero otherwise.
 */

int
blocklist_append_buffer_to_block(struct blocklist *bl,
                                 block *b,
                                 const void *sample_bits,
                                 AFframecount offset,
                                 AFframecount count) {
    int r;

    if(!block_resize(b, b->count + count)) 
        return 1;

    if((r = block_put_samples(b, sample_bits, b->count - count, count))) {
        DEBUG("fill failed, shrinking block again\n");
        block_resize(b, b->count - count);
        return r;
    }

    bl->count += count;
    blocklist_bcache_insert_adjust(bl, offset, count);

    return 0;    
}

/**
 * Inserts a buffer with sample data into the blocklist.
 * @param bl The blocklist.
 * @param sample_bits The sample data to insert.
 * @param sample_type The type of the sample data in sample_bits.
 * @param offset At which offset in the blocklist to insert into.
 * @param count The number of samples in sample_bits.
 * @return 0 on success.
 */

int
blocklist_insert_buffer(struct blocklist *bl,
                        enum sample_type sample_type,
                        const void *sample_bits,
                        AFframecount offset,
                        AFframecount count) {
    int r;
    GList *l = NULL, *target_block;
    block *b;
    AFframecount blkoff = offset;

    assert(sample_bits != NULL);
    assert(count > 0);
    assert(offset >= 0);

    /*
     * First we try to append the buffer to an existing block if
     * possible, to keep the number of blocks low and the average
     * number of samples per block high.
     */

    r = blocklist_find_block(bl, &blkoff, &target_block);

    if((r || blkoff == 0) && target_block) {

        /* OK, can append to either last block or target_block->prev. */

        if(!r) 
            target_block = target_block->prev; 

        if(target_block) {

            /* Check to see if block is allowed to hold more data. */

            if(((block *)target_block->data)->count + count <=
               MAX_BLOCK_SIZE) {
                if(!blocklist_append_buffer_to_block(bl,
                                                     target_block->data, 
                                                     sample_bits,
                                                     offset,
                                                     count)) {
                    return 0;
                }
            }
        }
    }
    
    /*
     * Couldn't append, so just create a new block, fill it, and insert it. 
     */
    
    b = block_new(CACHE_REAL, sample_type, count);
    if(!b) {
        FAIL("could not create new block for put, maybe out of memory\n");
        return 1;
    }

    block_put_samples(b, sample_bits, 0, count);

    l = g_list_append(l, b);
    r = blocklist_insert_blocks(bl, l, offset);
    g_list_free(l);
    block_unref(b);
    return r;
}

/*
void
blocklist_compact(struct blocklist *bl) {
    block *blk1 = NULL, *blk2 = NULL;
    GList *l = bl->l, *l2, *l3 = NULL;
    AFframecount original_count = blocklist_blocks_count(l);

    for(l2 = l; l2->next; l2 = l2->next) {
        if(((block *)(l2->data))->ref > 1) {
            blk1 = NULL;
            continue;
        }
        if(!blk1) {
            blk1 = (block *)(l2->data);
            continue;
        }
        blk2 = (block *)l2->data;
        if(blk1->count + blk2->count >= DEF_BLOCK_SIZE) {
            blk1 = NULL;
            continue;
        }

        blk1 = block_join(blk1, blk2);
        if(!blk1)
            continue;
        l3 = l2->prev;
        l = g_list_remove_link(l, l2);
        block_unref(blk2);
        if(l2->data != blk2) {
            FAIL("bug: l2->data: %p != blk2: %p\n", l2->data, blk2);
            abort();
        }
        l2->data = NULL;
        g_list_free_1(l2);
        l2 = l3;
    }
    
    DEBUG("original frame count: %ld, new frame count after join: %ld\n", 
          original_count, blocklist_blocks_count(l));

}
*/

/**
 * Clones the block list. Returns NULL on failure.
 * @param bl The blocklist to clone.
 * @return Copy of the blocklist or NULL on error.
 */

struct blocklist *
blocklist_clone(struct blocklist *bl) {
    GList *l1 = NULL, *l2 = NULL, *l3;
    block *b;
    int fail = 0;

    for(l1 = bl->l; l1; l1 = l1->next) {
        b = block_clone(((block *)l1->data),
                        0,
                        ((block *)l1->data)->count);

        if(!b) {
            fail = 1;
            break;
        }

        l3 = g_list_append(l2, b);

        if(!l3) {
            fail = 1;
            break;
        }

        l2 = l3;
    }

    if(fail) {
        FAIL("failed to clone block list\n");
        blocklist_blocks_destroy(l2);
        l2 = NULL;
        return NULL;
    }

    return blocklist_new(l2);
}

/**
 * @internal
 */

void
blocklist_bcache_dump(struct blocklist *bl) {
    int i;
    DEBUG("bcache:\n");
    for(i = 0; i < MAX_BCACHE_ENTRIES; i++) {
        if(bl->bc[i].is_valid)
            DEBUG("[%03d] offset: %ld, hits: %d\n",
                  i,
                  bl->bc[i].offset,
                  bl->bc[i].hits);
    }
}

/**
 * @internal
 */

void
blocklist_dump(struct blocklist *bl) {
    GList *l;
    int i = 0;
    AFframecount c = 0;

    DEBUG("blocks:\n");
    for(l = bl->l; l; l = l->next) {
        c += ((block *)l->data)->count;
        INFO("%4d: %10ld [%10ld] %p=%p (%s) {pl: %p, ph: %p, rms: %p, s: %p}\n", 
             i++, c, ((block *)l->data)->count, l, l->data,
             sample_get_description(((block *)l->data)->sample_type),
             ((block *)l->data)->peak_lows->data,
             ((block *)l->data)->peak_highs->data,
             ((block *)l->data)->rms->data,
             ((block *)l->data)->samples->data);
        /*        cache_dump(((block *)l->data)->samples);
                  INFO("     gr              ");
        cache_dump(((block *)l->data)->peak_lows);
        cache_dump(((block *)l->data)->peak_highs);
        */
    }
    DEBUG("tail: %p\n", bl->tail);
    blocklist_bcache_dump(bl);
}

/**
 * @internal
 */

void
blocklist_block_release(gpointer data, 
                        gpointer user_data) {
    block_unref((block *) data);
}

/**
 * @internal
 */

void
blocklist_blocks_destroy(GList *l) {
    g_list_foreach(l, blocklist_block_release, NULL);
    g_list_free(l);
    l = NULL;
}
    
void
blocklist_destroy(struct blocklist *bl) {
    blocklist_blocks_destroy(bl->l);
    bl->l = NULL;
    //    DEBUG("freeing blocklist: %p\n", bl);
    mem_free(bl);
    bl = NULL;
}

void
blocklist_blocks_set(struct blocklist *bl, 
                     GList *l) {
    bl->l = l;
    bl->count = blocklist_blocks_count(l);
    bl->tail = g_list_last(l);
}

/**
 * Creates a new blocklist.
 * @param l List of blocks to initialize the blocklist with.
 * @return New blocklist or NULL on error.
 */

struct blocklist *
blocklist_new(GList *l) {
    int i;
    struct blocklist *bl = mem_alloc(sizeof(struct blocklist));
    if(!bl) {
        FAIL("not enough memory for blocklist struct (%"CONVSPEC_SIZE_T" bytes)\n",
             sizeof(struct blocklist));
        return NULL;
    }
    for(i = 0; i < MAX_BCACHE_ENTRIES; i++) 
        bl->bc[i].is_valid = 0;
    bl->count = 0;
    blocklist_blocks_set(bl, l);
    //    DEBUG("allocated blocklist: %p\n", bl);
    return bl;
}

