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

#include <config.h>
#include <assert.h>
#include <math.h>
#include <audiofile.h>
#include "mem.h"
#include "cache.h"
#include "block.h"
#include "snd.h"

static int calculate_rms;

/**
 * @file
 * The basic building blocks for a track. Contains the 
 * sample data as well as peak and RMS caches.
 */

/**
 * Updates the peak cache for the block.
 * @param block The block.
 * @param offset The start point of update.
 * @param count The number of frames to update.
 */

void
block_update_peaks(block *block,
                   AFframecount offset,
                   AFframecount count) {
    AFframecount peak_offset = floor(offset / PEAK_HRES) * sizeof(peak_unit_t);
    
    assert(offset + count <= block->count);

    switch(block->type) {
    case CACHE_REAL:
        sample_scale_samples(block->peak_lows->data + peak_offset,
                             block->peak_highs->data + peak_offset,
                             block->sample_type,
                             block->samples->data + (offset * sample_get_width(block->sample_type)),
                             count, //block->count - offset,
                             PEAK_HRES);
        break;
    case CACHE_NULL:
        return;
    }
}

/**
 * Updates the RMS cache for the block.
 * @param block The block.
 * @param offset The start point of update.
 * @param count The number of frames to update.
 */

void
block_update_rms(block *block,
                 AFframecount offset,
                 AFframecount count) {
    AFframecount rms_offset = floor(offset / RMS_HRES) * sizeof(peak_unit_t);
    
    assert(offset + count <= block->count);

    block->flags |= BLOCK_HAS_RMS;

    switch(block->type) {
    case CACHE_REAL:
        sample_calc_rms(block->rms->data + rms_offset,
                        block->sample_type,
                        block->samples->data + (offset * sample_get_width(block->sample_type)),
                        count, //block->count,
                        RMS_HRES);
        break;
    case CACHE_NULL:
        return;
    }
}

/**
 * Whether the RMS (power) of sample data should be calculated.
 * @param state True to enable RMS calculation or false otherwise.
 */

void
block_set_calculate_rms(int state) {
    calculate_rms = state;
}

/**
 * Returns pointers into the RMS cache buffer.
 * @param block The block.
 * @param lows Receives a pointer into the minima.
 * @param highs Receives a pointer into the maxima.
 * @param offset At which position in the block to start.
 * @param count The number of frames to retrieve.
 * @return Number of frames available in the lows and highs.
 */

AFframecount 
block_peek_rms(block *block,
               const rms_unit_t **rms,
               AFframecount offset,
               AFframecount count) {
    
    AFframecount avail = MIN(count, block->count - offset);
    AFframecount rms_offset = floor(offset / RMS_HRES) * sizeof(rms_unit_t);

    if(!(block->flags & BLOCK_HAS_RMS)) 
        block_update_rms(block, 0, block->count);

    assert(offset >= 0);
    assert(count > 0);
    assert(offset < block->count);

    switch(block->type) {
    case CACHE_REAL:
        *rms = block->rms->data + rms_offset;
        break;
    case CACHE_NULL:
        *rms = NULL;
        break;
    }
    
    return avail;
}

/**
 * Returns pointers into the peak cache buffers.
 * @param block The block.
 * @param lows Receives a pointer into the minima.
 * @param highs Receives a pointer into the maxima.
 * @param offset At which position in the block to start.
 * @param count The number of frames to retrieve.
 * @return Number of frames available in the lows and highs.
 */

AFframecount 
block_peek_peaks(block *block,
                 const peak_unit_t **lows,
                 const peak_unit_t **highs,
                 AFframecount offset,
                 AFframecount count) {

    AFframecount avail = MIN(count, block->count - offset);
    AFframecount peak_offset = floor(offset / PEAK_HRES) * sizeof(peak_unit_t);

    assert(offset >= 0);
    assert(count > 0);
    assert(offset < block->count);

    switch(block->type) {
    case CACHE_REAL:
        *lows = block->peak_lows->data + peak_offset;
        *highs = block->peak_highs->data + peak_offset;
        break;
    case CACHE_NULL:
        *lows = NULL;
        *highs = NULL;
        break;
    }
    
    return avail;
}

/**
 * Retrieve data from the peak cache.
 * @param block The block.
 * @param lows The low points.
 * @param highs The high points.
 * @param offset At which position in the block to start.
 * @param count The number of frames to retrieve.
 * @return Number of frames retrieved.
 */

AFframecount 
block_get_peaks(block *block,
                peak_unit_t *lows,
                peak_unit_t *highs,
                AFframecount offset,
                AFframecount count) {

    AFframecount avail = MIN(count, block->count - offset);
    AFframecount peak_offset = floor(offset / PEAK_HRES) * sizeof(peak_unit_t);
    AFframecount peak_count = ceil(avail / PEAK_HRES) * sizeof(peak_unit_t);

    assert(offset >= 0);
    assert(count > 0);
    assert(offset < block->count);

    switch(block->type) {
    case CACHE_REAL:
        memcpy(lows,
               block->peak_lows->data + peak_offset,
               peak_count);
        memcpy(highs,
               block->peak_highs->data + peak_offset,
               peak_count);
        break;
    case CACHE_NULL:
        memset(lows, '\0', peak_count);
        memset(highs, '\0', peak_count);
        break;
    }
    
    return avail;
}

/**
 * Fills the block with sample data from the given buffer.
 * @param block The block to fill.
 * @param buf The buffer to fill the block with.
 * @param offset The offset in the block to start filling at.
 * @param count The number of samples in the buffer.
 * @return 0 on success, non-zero error code otherwise.
 */

int
block_put_samples(block *block,
                  const void *buf,
                  AFframecount offset,
                  AFframecount count) {
    
    assert(buf != NULL);
    assert(offset + count <= block->count);

    switch(block->type) {
    case CACHE_NULL:
        return 1;
    case CACHE_REAL:
        memcpy(block->samples->data + 
               (offset * sample_get_width(block->sample_type)),
               buf,
               count * sample_get_width(block->sample_type));
    }
    
    block_update_peaks(block, offset, count);
    if(calculate_rms)
        block_update_rms(block, offset, count);
    return 0;
}

/**
 * Returns a pointer into the sample buffer.
 * @param block The block to retrieve from.
 * @param buf Receives a pointer into the buffer or NULL for NULL blocks.
 * @param offset The offset in the block to return.
 * @param count The number of samples to retrieve.
 * @return Number of readable samples at returned pointer.
 */

AFframecount
block_peek_samples(block *block,
                   const void **buf,
                   AFframecount offset,
                   AFframecount count) {
    AFframecount avail = MIN(count, block->count - offset);
    AFframecount off = offset * sample_get_width(block->sample_type);
    AFframecount ct = avail * sample_get_width(block->sample_type);

    assert(buf != NULL);
    assert(off >= 0);
    assert(ct >= 0);

    switch(block->type) {
    case CACHE_REAL:
        *buf = block->samples->data + off;
        break;
    case CACHE_NULL:
        *buf = NULL;
        break;
    }

    return avail;
}

/**
 * Retrieves samples from the block.
 * @param block The block to retrieve from.
 * @param buf The buffer to receive the sample data.
 * @param offset The offset in the block to retrieve from.
 * @param count The number of samples to retrieve.
 */

AFframecount
block_get_samples(block *block,
                  void *buf,
                  AFframecount offset,
                  AFframecount count) {

    AFframecount avail = MIN(count, block->count - offset);
    AFframecount off = offset * sample_get_width(block->sample_type);
    AFframecount ct = avail * sample_get_width(block->sample_type);

    assert(buf != NULL);
    assert(off >= 0);
    assert(ct >= 0);

    switch(block->type) {
    case CACHE_REAL:
        memcpy(buf, block->samples->data + off, ct);
        break;
    case CACHE_NULL:
        memset(buf, '\0', ct);
        break;
    }

    return avail;
}

/**
 * Resizes a block. It is guaranteed to succeed when the new size
 * is smaller than the old size.
 * @param block The block to resize.
 * @param count The new block size.
 * @return Pointer to block or NULL on error.
 */

block *
block_resize(block *block,
             AFframecount count) {
    int err;
    AFframecount frames_before_resize = block->count;

    assert(count > 0);

    /* Resize RMS cache. */

    err = cache_resize(block->rms,
                       ceil(count / RMS_HRES) * sizeof(rms_unit_t));
    if(err) {
        FAIL("could not resize avg cache.\n");
        return NULL;
    }

    /* Resize low peaks cache. */

    err = cache_resize(block->peak_lows, 
                       ceil(count / PEAK_HRES) * sizeof(peak_unit_t));
    if(err) {
        FAIL("could not resize low peak cache.\n");
        goto recover_rms;
    }

    /* Resize high peaks cache. */

    err = cache_resize(block->peak_highs, 
                       ceil(count / PEAK_HRES) * sizeof(peak_unit_t));
    if(err) {
        FAIL("could not resize high peak cache\n");
        goto recover_peaks_high;
    }

    /* Resize sample cache. */

    err = cache_resize(block->samples, 
                       count * sample_get_width(block->sample_type));
    if(err) {
        FAIL("could not resize sample cache\n");
        goto recover_peaks_low;
    }

    block->count = count;
    return block;

 recover_peaks_low:
    err = cache_resize(block->peak_lows, 
                       ceil(frames_before_resize / 
                            PEAK_HRES) * sizeof(peak_unit_t));
    if(err) 
        FAIL("could not recover low peak cache\n");
    
 recover_peaks_high:
    err = cache_resize(block->peak_highs, 
                       ceil(frames_before_resize / 
                            PEAK_HRES) * sizeof(peak_unit_t));
    if(err) {
        FAIL("could not recover high peak cache\n");
        block_update_peaks(block, 0, block->count);
    }
    
 recover_rms:
    err = cache_resize(block->rms,
                       ceil(frames_before_resize / RMS_HRES) *
                       sizeof(rms_unit_t));
    if(err) {
        FAIL("could not recover rms cache.\n");
        block_update_peaks(block, 0, block->count);
        if(calculate_rms)
            block_update_rms(block, 0, block->count);
    }
    
    return NULL;
}

/**
 * Moves data inside a block. 
 * @param block The block.
 * @param new_offset The new position of the data.
 * @param old_offset The old position of the data.
 * @param count The length of the data.
 */

void
block_move(block *block,
           AFframecount new_offset,
           AFframecount old_offset,
           AFframecount count) {

    assert(old_offset + count <= block->count);
    assert(new_offset >= 0);

    cache_move(block->samples, 
               new_offset * sample_get_width(block->sample_type),
               old_offset * sample_get_width(block->sample_type),
               count * sample_get_width(block->sample_type));

    /*
     * For the peak caches, moving data is a little tricky.  Each peak
     * element describes PEAK_HRES samples, and to move the peak
     * elements so that they correspond precisely with the moved
     * sample data is only possible when the offsets and the count
     * fall precisely on multiples of PEAK_HRES. In practice, that's
     * usually not the case, so we would need to make some estimation,
     * and keep track of the accumulating error so we can adjust for
     * it. The question is whether all that would be faster then
     * simply recalculating the peaks, especially since we have highly
     * optimized peak calculation routines. It's a given that it would
     * never yield better accuracy. So we just recalculate the peaks,
     * if it turns out this does in fact yield lower performance at
     * some point we can get back to this.
     */

    block_update_peaks(block, 0, block->count);

    /*
     * It's pretty much the same story for the RMS cache, with the
     * exception that our RMS calculation routines are not optimized
     * as well as the peak calculation routines, so it might be more
     * worthwile to do partial updates.
     */
     
    if(calculate_rms)
        block_update_rms(block, 0, block->count);
}

/**
 * Splits a block at the specified offset, and returns the latter 
 * part.
 * @param block The block to split.
 * @param offset At which point to split the block.
 * @return A new block containing the data after the split point or NULL
 * on error.
 */

block *
block_split(block *block,
            AFframecount offset) {
    struct _block *blk_split;

    assert(offset > 0 && offset < block->count);

    blk_split = block_clone(block, offset, block->count - offset);
    
    if(!blk_split) {
        FAIL("could not clone block (%ld-%ld)\n", 
             offset, block->count - offset);
        return NULL;
    }

    block_resize(block, offset);

    return blk_split;
}

static void
block_destroy(block *block) {
    //    DEBUG("destroying block, %ld frames\n", block->count);
    cache_destroy(block->samples);
    cache_destroy(block->peak_lows);
    cache_destroy(block->peak_highs);
    cache_destroy(block->rms);
    free(block);
    block = NULL;
}

void
block_unref(block *block) {
    block->ref--;
    if(block->ref == 0)
        block_destroy(block);
}

void
block_addref(block *block) {
    block->ref++;
}


/**
 * Creates a block.
 * @param type The kind of block to create (see cache_type).
 * @param sample_type The type of samples in the block.
 * @param count The number of frames in the block.
 * @param fc The frame cache holding the sample data.
 * @param gc_low The minima for the frame cache.
 * @param gc_high The maxima for the frame cache.
 * @return The block or NULL on error (in which case the caches are
 * destroyed).
 */

block *
block_construct(cache_type type,
                enum sample_type sample_type,
                AFframecount count,
                cache *samples,
                cache *lows,
                cache *highs,
                cache *avgs) {
    block *b = NULL;

    assert(count != 0);

    if(!samples || !lows || !highs || !avgs) 
        goto fail;

    b = mem_calloc(1, sizeof(block));
    if(!b) 
        goto fail;

    b->type = type;
    b->ref = 1;
    b->sample_type = sample_type;
    b->count = count;
    b->samples = samples;
    b->peak_lows = lows;
    b->peak_highs = highs;
    b->rms = avgs;
    b->flags = calculate_rms ? BLOCK_HAS_RMS : 0;

    /*    if(b->type == CACHE_NULL) 
        DEBUG("created %ld frame NULL block.\n", count);
    */

    return b;

 fail:
    if(avgs)
        cache_destroy(avgs);
    if(lows)
        cache_destroy(lows);
    if(highs)
        cache_destroy(highs);
    if(samples)
        cache_destroy(samples);
    if(b) {
        mem_free(b);
        b = NULL;
    }
    return NULL;
}


/**
 * (Partially) clones a block. 
 * @param block The block.
 * @param offset From what offset to clone the block.
 * @param count How many frames to clone.
 * @return Cloned block or NULL on error.
 */

block *
block_clone(const block *block,
            AFframecount offset,
            AFframecount count) {
    size_t foff, fc, goff, gc, aoff, ac;
    struct _block *blk_split;

    foff = offset * sample_get_width(block->sample_type);
    fc = count * sample_get_width(block->sample_type);
    goff = (size_t) (offset / PEAK_HRES) * sizeof(peak_unit_t);
    gc = ceil(count / PEAK_HRES) * sizeof(peak_unit_t);
    aoff = (size_t) (offset / RMS_HRES) * sizeof(rms_unit_t);
    ac = ceil(count / RMS_HRES) * sizeof(rms_unit_t);
    
    blk_split = block_construct(block->type,
                                block->sample_type,
                                count,
                                cache_clone(block->samples, foff, fc),
                                cache_clone(block->peak_lows, goff, gc),
                                cache_clone(block->peak_highs, goff, gc),
                                cache_clone(block->rms, aoff, ac));
    
    if(!blk_split) {
        FAIL("cannot create new block for clone (%ld frames)\n", count);
        return NULL;
    }

    block_update_peaks(blk_split, 0, blk_split->count);
    if(calculate_rms)
        block_update_rms(blk_split, 0, blk_split->count);
    
    return blk_split;
}

/**
 * Creates a new block.
 * @param type The block type.
 * @param sample_type The sample type.
 * @param count The size of the block.
 */

block *
block_new(cache_type type,
          enum sample_type sample_type,
          AFframecount count) {
    return block_construct(type,
                           sample_type,
                           count,
                           cache_new(type, (count * 
                                            sample_get_width(sample_type))),
                           cache_new(CACHE_REAL, 
                                     ceil(count / PEAK_HRES) * 
                                     sizeof(peak_unit_t)),
                           cache_new(CACHE_REAL, 
                                     ceil(count / PEAK_HRES) * 
                                     sizeof(peak_unit_t)),
                           cache_new(CACHE_REAL, 
                                     ceil(count / RMS_HRES) * 
                                     sizeof(rms_unit_t)));
    
}
