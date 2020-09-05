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
#include <stdlib.h>
#include <stdarg.h>
#include <string.h>
#include <glib.h>
#include <math.h>
#include <errno.h>
#include "mem.h"
#include "snd.h"
#include "track.h"
#include "blocklist.h"
#include "cache.h"

/**
 * Returns the number of samples on this track.
 * 
 * @param tr the track.
 * @return number of samples on track.
 */

AFframecount
track_count(track *tr) {
    AFframecount count;
    
    rwlock_rlock(&tr->rwl);
    count = blocklist_count(tr->bl);
    rwlock_runlock(&tr->rwl);

    return count;
}

/**
 * Inserts silence of the specified duration at the specified
 * offset. This block of silence takes up very little memory.
 * 
 * @param tr The track.
 * @param offset The offset at which to insert the silence.
 * @param count The number of frames to insert.
 * @return 0 for success.
 */

int
track_insert_silence(track *tr,
                     AFframecount offset,
                     AFframecount count) {
    block *b;
    GList *l = NULL;

    if(count == 0)
        return 0;

    rwlock_wlock(&tr->rwl);

    /* Pad. */

    if(offset > track_count(tr)) {
        count += offset - track_count(tr);
        offset = track_count(tr);
    }

    //    DEBUG("creating NULL block, %ld frames\n", count);
    b = block_new(CACHE_NULL, tr->sample_type, count);
    if(!b) {
        FAIL("could not create %ld frame NULL block\n", count);
        rwlock_wunlock(&tr->rwl);
        return 1;
    }
    //    DEBUG("inserting NULL block\n");
    l = g_list_append(l, b);
    blocklist_insert_blocks(tr->bl, l, offset);
    block_unref(b);
    l->data = NULL;
    g_list_free(l);

    rwlock_wunlock(&tr->rwl);

    return 0;
}


/**
 * Inserts the given block list into the target track at the specified
 * offset. The given blocks are not actually copied, instead their
 * reference count is increased and they are added to the target block
 * list. The source block list should be destroyed by the caller.
 *
 * If the given offset equals the count for this track, then insert
 * becomes append and the source block list is appended. If the
 * given offset exceeds the count for this track, then a NULL block
 * is inserted to span the difference, followed by an append. 
 *
 * @param tr The track.
 * @param source The list of source blocks to insert.
 * @param offset The offset to insert the list at.
 * @return 0 on success, non-zero error code otherwise.
 */

int
track_insert(track *tr,
             GList *source,
             AFframecount offset) {
    int r;
    
    rwlock_wlock(&tr->rwl);

    /* Append with silence to the required length first if
       necessary. */

    if(offset > track_count(tr)) {
        if(track_insert_silence(tr, 
                                track_count(tr), 
                                offset - track_count(tr))) {
            FAIL("could not pad track up to required length %ld\n", offset);
            rwlock_wunlock(&tr->rwl);
            return 1;
        }
    }

    r = blocklist_insert_blocks(tr->bl, source, offset);

    rwlock_wunlock(&tr->rwl);
    return r;
}

/**
 * Removes count samples at offset and returns a list of
 * blocks containing the deleted frames. The block list may contain
 * fewer samples than were requested if the offset + count exceeds the
 * track length.
 * @param tr The track.
 * @param deleted Receives the deleted block list on success. Can be NULL
 * in which case the deleted blocks are immediately destroyed.
 * @param offset Where to start deleting.
 * @param count How many samples to delete.
 * @return 0 success, non-zero error code otherwise.
 */

int
track_delete(track *tr,
             GList **deleted,
             AFframecount offset,
             AFframecount count) {
    int r;
    GList *tmp;
    rwlock_wlock(&tr->rwl);
    r = blocklist_delete(tr->bl, &tmp, offset, count);
    blocklist_trim_right(tr->bl);
    rwlock_wunlock(&tr->rwl);

    if(deleted)
        *deleted = tmp;
    else
        blocklist_blocks_destroy(tmp);

    return r;
}

/**
 * Calculates the absolute average from the samples.
 */

AFframecount
track_get_rms_from_samples(track *tr, 
                           rms_unit_t *rms,
                           AFframecount offset,
                           AFframecount count,
                           float hres) {
    int err;
    GList *bptr;
    AFframecount got = 0, dst_off = 0;
    const void *buf;

    rwlock_rlock(&tr->rwl);

    err = blocklist_find_block(tr->bl, &offset, &bptr);
    
    /* The offset is out of bounds. */

    if(err) {
        rwlock_runlock(&tr->rwl);
        return -1; 
    }
    
    while(bptr && count) {
        got = block_peek_samples((block *)bptr->data,
                                 &buf,
                                 offset,
                                 count);

        if(buf == NULL) {
            memset(&rms[(int)floor(dst_off / hres)], '\0', 
                   ceil(got / hres) * sizeof(rms_unit_t));
        } else {
            sample_calc_rms(&rms[(int)floor(dst_off / hres)],
                            tr->sample_type,
                            buf,
                            got,
                            hres);
        }
        count -= got;
        offset = 0;
        bptr = bptr->next;
        dst_off += got;
    }

    rwlock_runlock(&tr->rwl);
    return dst_off;
}

/**
 * Calculates the RMS from the RMS cache.  Yes this function looks
 * terribly like track_get_rms_from_samples and they could be merged
 * fairly easily but it works now and there's more important stuff to
 * do. But it would be a nice cleanup.
 */

AFframecount
track_get_rms_from_cache(track *tr, 
                         rms_unit_t *rms,
                         AFframecount offset,
                         AFframecount count,
                         float hres) {
    int err;
    GList *bptr;
    AFframecount got = 0, dst_off = 0;
    const rms_unit_t *src_rms;

    rwlock_rlock(&tr->rwl);

    err = blocklist_find_block(tr->bl, &offset, &bptr);
    
    /* The offset is out of bounds. */

    if(err) {
        rwlock_runlock(&tr->rwl);
        return -1; 
    }
    
    while(bptr && count) {
        got = block_peek_rms((block *)bptr->data,
                             &src_rms,
                             offset,
                             count);
        if(src_rms == NULL) {
            memset(&rms[(int)floor(dst_off / hres)], '\0', 
                   (AFframecount)ceil(got / hres) * sizeof(rms_unit_t));
        } else {
            sample_calc_rms(&rms[(int)floor(dst_off / hres)],
                            SAMPLE_TYPE_INT_8,
                            src_rms,
                            (AFframecount)ceil(got / RMS_HRES),
                            hres / RMS_HRES);
        }
        count -= got;
        offset = 0;
        bptr = bptr->next;
        dst_off += got;
    }

    rwlock_runlock(&tr->rwl);
    return dst_off;
}

/**
 * Retrieves the root-mean-square values for the requested sample
 * region into the buffer given by rms. The buffer must be large
 * enough to hold at least ceil(count / hres) * sizeof(rms_unit_t)
 * elements. This function only returns meaningful data when
 * the view.wave.draw.rms.enable preference is true.
 *
 * @param tr The track.
 * @param rms The buffer that receives the RMS values.
 * @param offset The offset in the track from which to return data
 * @param count The (non-zero) number of frames to copy.
 * @param hres The required scaling factor (e.g. 0.5, 1, 2, 4, ...).
 * @return Number of frames actually copied on success, negative error
 * code otherwise.
 */

AFframecount
track_get_rms(track *tr, 
              rms_unit_t *rms,
              AFframecount offset,
              AFframecount count,
              float hres) {

    rwlock_rlock(&tr->rwl);
    
    if(hres < tr->graph_hres)         
        count = track_get_rms_from_samples(tr, rms, offset, count, hres);
    else
        count = track_get_rms_from_cache(tr, rms, offset, count, hres);
    
    rwlock_runlock(&tr->rwl);
    
    return count;
}

/**
 * Gets peaks from the cache, then uses those to scale further.
 */

AFframecount
track_get_peaks_from_cache(track *tr, 
                           peak_unit_t *lows,
                           peak_unit_t *highs,
                           AFframecount offset,
                           AFframecount count,
                           float hres) {
    int err, may_need_blending = 0;
    GList *bptr;
    AFframecount got = 0, dst_off = 0, blend_idx = 0;
    const peak_unit_t *src_lows, *src_highs;
    peak_unit_t l = 0, h = 0;
    
    rwlock_rlock(&tr->rwl);

    err = blocklist_find_block(tr->bl, &offset, &bptr);
    
    /* The offset is out of bounds. */

    if(err) {
        rwlock_runlock(&tr->rwl);
        return -1; 
    }
    
    //    if(offset % (int)hres)
    //        DEBUG("offset %% hres: %ld, offset: %ld\n",
    //              offset % (int)hres, offset);

    while(bptr && count) {
        got = block_peek_peaks((block *)bptr->data,
                               &src_lows,
                               &src_highs,
                               offset,
                               count);
        if(src_lows == NULL) {
            memset(&lows[(int)floor(dst_off / hres)], 
                   '\0', ceil(got / hres) * sizeof(peak_unit_t));
            memset(&highs[(int)floor(dst_off / hres)],
                   '\0', ceil(got / hres) * sizeof(peak_unit_t));
        } else { 
            /* FIXME: need to blend peak units */
            sample_scale_peaks(&lows[(int)floor(dst_off / hres)],
                               &highs[(int)floor(dst_off / hres)],
                               src_lows,
                               src_highs,
                               got,
                               hres);
        }

        /* 
         * Assume we have three blocks:
         * block 1 (length 200) -- block 2 (length 128) -- block 3 (length 400)
         * Each peak cache element describes 128 samples or less.
         * So block 1 has 2 peak cache elements, block 2 has 1 and block 3 
         * has 4.
         * The total length is 200 + 128 + 400 = 728. This can be described
         * by 728 / 128 = 6 peak elements. But the blocks total 7 peak 
         * elements!
         * So we need to "blend" peak units:
         *
         * [block 1]      [block 2]      [block 3]
         * s1 s2          s3             s4 s5 s6 s7          source peak units
         *
         *  |  \_____ ____/\______ ______/   |  |  |
         *  v        v            v          v  v  v
         *   
         * d1       d2           d3         d4 d5 d6          dest. peak units
         *          
         * But, d2 = blend(s2, s3) is the same as d2 = blend(d2, s3),
         * if we copy s2 to d2 before looking at s3. 
         * 
         * In general, {i=p..q} d = blend(s_i, s_i+1) is equivalent to 
         * {j=p..q} d = blend(d, s_j).
         */
        /*
        if(may_need_blending) {
            if(blend_idx == (int)floor(dst_off / hres)) {
                lows[blend_idx] = MIN(lows[blend_idx], l);
                highs[blend_idx] = MAX(highs[blend_idx], h);
            }
            
        }
        */

        count -= got;
        offset = 0;
        bptr = bptr->next;
        dst_off += got;

        blend_idx = (int)floor(dst_off / hres);
        l = lows[blend_idx];
        h = highs[blend_idx];
        may_need_blending = 1;

    }
    
    rwlock_runlock(&tr->rwl);
    return dst_off;
}

/**
 * Calculates peaks from the sample data.
 */

AFframecount
track_get_peaks_from_samples(track *tr, 
                             peak_unit_t *lows,
                             peak_unit_t *highs,
                             AFframecount offset,
                             AFframecount count,
                             float hres) {
    int err;
    GList *bptr;
    AFframecount got = 0, dst_off = 0;
    const void *buf;

    rwlock_rlock(&tr->rwl);

    err = blocklist_find_block(tr->bl, &offset, &bptr);
    
    /* The offset is out of bounds. */

    if(err) {
        rwlock_runlock(&tr->rwl);
        return -1; 
    }
    
    while(bptr && count) {
        got = block_peek_samples((block *)bptr->data,
                                 &buf,
                                 offset,
                                 count);
        if(buf == NULL) {
            memset(&lows[(int)floor(dst_off / hres)], 
                   '\0', (got / hres) * sizeof(peak_unit_t));
            memset(&highs[(int)floor(dst_off / hres)],
                   '\0', (got / hres) * sizeof(peak_unit_t));
        } else {
            /* FIXME: need to blend peak units */
            sample_scale_samples(&lows[(int)floor(dst_off / hres)],
                                 &highs[(int)floor(dst_off / hres)],
                                 tr->sample_type,
                                 buf,
                                 got,
                                 hres);
        }
        count -= got;
        offset = 0;
        bptr = bptr->next;
        dst_off += got;
    }

    rwlock_runlock(&tr->rwl);
    return dst_off;
}

/**
 * Retrieves the peaks of the requested sample region, scaled
 * according to a scaling factor, into the buffers given by lows and
 * highs. The buffers must be large enough to hold at least 
 * ceil(count / hres) elements.
 *
 * @param tr The track.
 * @param lows The buffer that receives the minima.
 * @param highs The buffer that receives the maxima.
 * @param offset The offset in the track from which to return data
 * @param count The (non-zero) number of frames to copy.
 * @param hres The required scaling factor (e.g. 0.5, 1, 2, 4, ...).
 * @return Number of frames actually copied on success, negative error
 * code otherwise.
 */

AFframecount
track_get_peaks(track *tr, 
                peak_unit_t *lows,
                peak_unit_t *highs,
                AFframecount offset,
                AFframecount count,
                float hres) {

    rwlock_rlock(&tr->rwl);

    if(hres < tr->graph_hres) {
        
        count = track_get_peaks_from_samples(tr,
                                             lows,
                                             highs,
                                             offset,
                                             count, 
                                             hres);
        
    } else {
        
        count = track_get_peaks_from_cache(tr,
                                           lows,
                                           highs,
                                           offset,
                                           count,
                                           hres);

    }
    
    rwlock_runlock(&tr->rwl);
    
    return count;
}


/**
 * Copies the requested samples from the track into the supplied
 * buffer.
 * 
 * @param tr The track.
 * @param buf The buffer to copy the data into.
 * @param offset The start of the data to copy.
 * @param count The number of samples to copy.
 * @return Number of samples actually copied on success, negative
 * error code otherwise.
 */

AFframecount
track_get_samples(track *tr,
                  void *buf,
                  AFframecount offset, 
                  AFframecount count) {
    int err;
    GList *bptr;
    AFframecount buf_off, got = 0;

    rwlock_rlock(&tr->rwl);

    err = blocklist_find_block(tr->bl, &offset, &bptr);
    
    /* The offset is out of bounds. */

    if(err) {
        rwlock_runlock(&tr->rwl);
        return -1; 
    }
    
    buf_off = 0;
    while(bptr && count) {
        got = block_get_samples((block *)bptr->data,
                                buf + (buf_off * 
                                       sample_get_width(tr->sample_type)),
                                offset,
                                count);
        count -= got;
        buf_off += got;
        offset = 0;
        bptr = bptr->next;
    }

    rwlock_runlock(&tr->rwl);
    return buf_off;
}

/**
 * Copies the requested samples from the track into the supplied
 * buffer, after converting them to the requested type.
 * 
 * @param tr The track.
 * @param type The sample type to convert to.
 * @param buf The buffer to copy the data into.
 * @param offset The start of the data to copy.
 * @param count The number of samples to copy.
 * @return Number of samples actually copied on success, negative
 * error code otherwise.
 */

AFframecount
track_get_samples_as(track *tr,
                     enum sample_type type,
                     void *buf,
                     AFframecount offset, 
                     AFframecount count) {
    AFframecount r;
    void *tmp;

    if(type == tr->sample_type)
        return track_get_samples(tr, buf, offset, count);

    tmp = mem_alloc(sample_get_width(tr->sample_type) * count);

    if(!tmp)
        return -1;

    r = track_get_samples(tr, tmp, offset, count);

    if(r < 0) {
        mem_free(tmp);
        return r;
    }

    sample_convert(tr->sample_type, type, tmp, buf, r);
    mem_free(tmp);

    return r;
}

/**
 * Inserts samples into the track.
 * @param tr The track.
 * @param buf The sample buffer.
 * @param offset The offset to insert at.
 * @param count The number of samples in buf.
 * @return 0 on success or non-zero error code otherwise.
 */

int
track_insert_samples(track *tr,
                     const void *buf,
                     AFframecount offset,
                     AFframecount count) {
    int r;

    if(count == 0)
        return 0;

    rwlock_wlock(&tr->rwl);
    
    /* Pad with silence to the required length first if
       necessary. */

    if(offset > track_count(tr)) {
        r = track_insert_silence(tr, 
                                 MIN(track_count(tr), offset), 
                                 (MAX(track_count(tr), offset) -
                                  MIN(track_count(tr), offset)));
        if(r) {
            rwlock_wunlock(&tr->rwl);
            return r;
        }
    }

    /* 
     * Try to determine whether the buffer is all zero,
     * in which case we insert a NULL block.
     */

    if(sample_buffer_is_silence(tr->sample_type, buf, count)) {
        DEBUG("all zero block found of size %ld\n", count);
        r = track_insert_silence(tr, offset, count);
        rwlock_wunlock(&tr->rwl);
        return r;
    }
    
    r = blocklist_insert_buffer(tr->bl, tr->sample_type, buf, offset, count);

    rwlock_wunlock(&tr->rwl);
    return r;
}

/**
 * Inserts samples into the track, converting them to the
 * required type if necessary.
 * @param tr The track.
 * @param type The type of the samples in buf.
 * @param buf The sample buffer.
 * @param offset The offset to insert at.
 * @param count The number of samples in buf.
 * @return 0 on success or non-zero error code otherwise.
 */

int
track_insert_samples_from(track *tr,
                          enum sample_type type,
                          const void *buf,
                          AFframecount offset,
                          AFframecount count) {
    int r;
    void *tmp;

    if(count == 0)
        return 0;

    if(tr->sample_type == type)
        return track_insert_samples(tr, buf, offset, count);
    
    tmp = mem_alloc(sample_get_width(tr->sample_type) * count);

    if(!tmp)
        return 1;

    sample_convert(type, tr->sample_type, buf, tmp, count);
    r = track_insert_samples(tr, tmp, offset, count);
    mem_free(tmp);

    return r;
}

/**
 * Replaces a region with a sample buffer.
 * @param tr The track.
 * @param buf The sample buffer.
 * @param offset The start of the region.
 * @param count The length of the region.
 * @return 0 on success, non-zero error code otherwise.
 */

int
track_replace_samples(track *tr,
                      const void *buf,
                      AFframecount offset,
                      AFframecount count) {
    int r;
    GList *del_blks;

    if(count == 0)
        return 0;

    rwlock_wlock(&tr->rwl);
    if(track_delete(tr, &del_blks, offset, count)) {
        rwlock_wunlock(&tr->rwl);
        return 1;
    }

    r = track_insert_samples(tr, buf, offset, count);
    
    if(r) {
        if(track_insert(tr, del_blks, offset)) 
            FAIL("unable to recover, data lost\n");
    }
    blocklist_blocks_destroy(del_blks);
    
    rwlock_wunlock(&tr->rwl);
    return r;
}

/**
 * Replaces a region with a sample buffer, converting the sample
 * buffer first if necessary.
 * @param tr The track.
 * @param type The type of the samples in buf.
 * @param buf The sample buffer.
 * @param offset The start of the region.
 * @param count The length of the region.
 * @return 0 on success, non-zero on error.
 */

int
track_replace_samples_from(track *tr,
                           enum sample_type type,
                           const void *buf,
                           AFframecount offset,
                           AFframecount count) {
    int r;
    void *tmp;

    if(type == tr->sample_type)
        return track_replace_samples(tr, buf, offset, count);

    tmp = mem_alloc(sample_get_width(tr->sample_type) * count);
    
    if(!tmp)
        return 1;

    sample_convert(type, tr->sample_type, buf, tmp, count);
    r = track_replace_samples(tr, tmp, offset, count);
    mem_free(tmp);

    return r;
}

/**
 * Clones the track, depending on the method, either just the track
 * properties or the track data as well.
 * 
 * @param tr the track.  
 * @param method if CLONE_DATA, then the track blocklist is cloned as well.
 * @return cloned track or NULL on error.
 */

track *
track_clone(track *tr,
            track_clone_method method) {
    track *tr_clone = track_new(tr->sample_type);

    if(!tr_clone) {
        FAIL("could not create new track for clone\n");
        return NULL;
    }

    if(method & CLONE_DATA) {
        rwlock_rlock(&tr->rwl);
        if(tr->bl) {
            if(tr_clone->bl) 
                blocklist_destroy(tr_clone->bl);
            tr_clone->bl = blocklist_clone(tr->bl);
            if(!tr_clone->bl) {
                FAIL("could not clone block list for track clone\n");
                track_destroy(tr_clone);
                rwlock_runlock(&tr->rwl);
                return NULL;
            }
        }
        rwlock_runlock(&tr->rwl);
    }
    return tr_clone;
}

/**
 * Destroys the track. 
 * 
 * @param tr the track.
 */

void
track_destroy(track *tr) {
    if(tr->bl) {
        blocklist_destroy(tr->bl);
        tr->bl = NULL;
    }

    rwlock_destroy(&tr->rwl);
    mem_free(tr);
}

/**
 * Creates a new track.
 * 
 * @param sample_type the sample type.
 * @return new track or NULL on error.
 */

track *
track_new(enum sample_type sample_type) {
    track *tr = mem_calloc(1, sizeof(track));    
    
    if(!tr)
        return NULL;

    tr->sample_type = sample_type;
    tr->graph_hres = PEAK_HRES;
    rwlock_init(&tr->rwl);

    tr->bl = blocklist_new(NULL);
    if(!tr->bl) {
        track_destroy(tr);
        return NULL;
    }

    return tr;
}



/********************************************************************/
/********************************************************************/
/********************************************************************/
/********************************************************************/
/********************************************************************/
/********************************************************************/

/********************************************************************/

/********************************************************************/


/********************************************************************/



/********************************************************************/




/********************************************************************/

#if 0

/*
 * The below code no longer has any use at this time but when
 * track_frames_insert() becomes more intelligent it might be handy
 * again. It might also become useful again at a time when some kind 
 * of load-on-demand is implemented (again: very early version of
 * GNUsound did that).
 */

void
track_frame_cache_fill(track *tr,
                       void *bits,
                       AFframecount frame_offset,
                       AFframecount frame_count) {
    GList *l = NULL;
    cache *c;
    size_t cache_el_sz, src_off, r;
    int err;

    err = blocklist_find_block(tr->bl, &frame_offset, &l);

    if(err) {
        FAIL("could not find block for frame_offset %ld\n", frame_offset);
        return;
    }

    cache_el_sz = tr->frame_width;
    src_off = 0;

    //    DEBUG("request %s: frame_offset: %ld, frame_count: %ld\n", 
    //          is_graph_cache ? "GRAPH" : "FRAME", frame_offset, frame_count);

    while(l && frame_count > 0) {
        c = ((block *)l->data)->frame_cache;
        r = cache_fill(c,
                       bits + src_off,
                       frame_offset * cache_el_sz,
                       frame_count * cache_el_sz);
        src_off += r;
        r /= cache_el_sz;
        frame_count -= r;
        frame_offset = 0;
        l = l->next;
    }
    
    if(!l && frame_count) {
        FAIL("cache fill failed, %ld bytes lost\n", frame_count);
        blocklist_dump(tr->bl);
        abort();
    }
}

void
track_graph_cache_fill(track *tr,
                       void *bits,
                       AFframecount frame_offset,
                       AFframecount frame_count) {
    GList *l = NULL;
    cache *c;
    AFframecount fc_req, fc_total;
    size_t cache_el_sz, src_off, r;
    float hres;
    int err;

    DEBUG("frame_offset: %ld, frame_count: %ld\n", 
          frame_offset, frame_count);
    err = blocklist_find_block(tr->bl, &frame_offset, &l);

    if(err) {
        FAIL("could not find block for frame_offset %ld\n", frame_offset);
        return;
    }

    cache_el_sz = sizeof(struct graph_bits_t);
    hres = tr->graph_hres;
    frame_offset /= hres;
    src_off = 0;
    fc_total = 0;

    //    DEBUG("request %s: frame_offset: %ld, frame_count: %ld\n", 
    //          is_graph_cache ? "GRAPH" : "FRAME", frame_offset, frame_count);

    while(l && frame_count > 0) {
        c = ((block *)l->data)->graph_cache;

        fc_req = MIN(((block *)l->data)->count, frame_count);

        r = cache_fill(c,
                       bits + src_off,
                       frame_offset * cache_el_sz,
                       ceil(fc_req / hres) * cache_el_sz);
        fc_total += fc_req;
        src_off = floor(fc_total / hres) * cache_el_sz;
        frame_count -= fc_req;
        frame_offset = 0;
        l = l->next;
    }
    
    if(!l && frame_count) {
        FAIL("cache fill failed, %ld bytes lost\n", frame_count);
        blocklist_dump(tr->bl);
        abort();
    }
}



/*
 * Almost identical to track_frame_cache_find() above. It is a bit more
 * complicated though because it has to account for the fact that a
 * block may contain less frames than a a single graph_bits_t
 * describes, in which case the graph caches for consecutive blocks
 * need to be "blended". 
 */

void
track_graph_cache_find(track *tr,
                       void *low_bits,
                       void *high_bits,
                       AFframecount *frame_offset,
                       AFframecount *frame_count) {
    AFframecount fo, frames_read, frames_left, frames_total_read;
    GList *l = NULL;
    cache *c_low, *c_high;
    size_t cache_el_sz, src_off, off, start_gap, ct;
    int fail_on_gap = 0, has_gap = 0, spill = 0, err = 0;
    float frame_divider;

    fo = *frame_offset;
    frames_left = *frame_count;

    /* Setup. */
    
    err = blocklist_find_block(tr->bl, &fo, &l);

    if(err)
        return;

    c_low = ((block *)l->data)->peak_lows;
    c_high = ((block *)l->data)->peak_highs;
    frame_divider = tr->graph_hres;
    cache_el_sz = sizeof(peak_unit_t);
    src_off = 0;

    off = floor(fo / frame_divider) * cache_el_sz;
    ct = ceil(MIN(((block *)l->data)->count, frames_left) / frame_divider) * cache_el_sz;
    cache_find(c_low,
               low_bits + src_off,
               &off,
               &ct);
    cache_find(c_high,
               high_bits + src_off,
               &off,
               &ct);
    
    /* Calculate how many frames (as opposed to peak cache elements)
       were actually returned. */

    frames_read = (ct / cache_el_sz) * frame_divider;
    if(((off + ct) / cache_el_sz) * frame_divider > ((block *)l->data)->count) 
        frames_read -= 
            ((((off + ct) / cache_el_sz) * frame_divider) - ((block *)l->data)->count);

    /* Case 1: Didn't find anything, no use looking further. */
    
    if(!ct)
        return;

    /* Case 2 & 4: */

    if(off == floor(fo / frame_divider) * cache_el_sz &&
       (frames_read == frames_left || 
        ((off / cache_el_sz) * frame_divider) + frames_read < ((block *)l->data)->count)) {
        *frame_offset += frames_read;
        *frame_count -= frames_read;
        return;
    }

    /* Case 5: No need to adjust frame_offset. */

    if((floor(off / cache_el_sz) * frame_divider) + frames_read == 
       (floor(fo / frame_divider) * cache_el_sz) + frames_left) {
        *frame_count -= frames_read;
        return;
    }

    /* Case 7: Found something, but it does not start at the requested
       beginning and it does not end at the end of the cache buffer
       and it does not end at the requested end -> useless. */
    
    if(off > floor(fo / frame_divider) * cache_el_sz &&
       ((off / cache_el_sz) * frame_divider) + frames_read < ((block *)l->data)->count &&
       ((off / cache_el_sz) * frame_divider) + frames_read < 
       (floor(fo / frame_divider) * cache_el_sz) + frames_left) {
        return;
    }
    
    /* Now only cases 3 and 6 remain. */

    /* Intermezzo.

    The number of frames in a block is not always a neat multiple
    of the number of frames represented by a single graph_bits unit
    (pair of low/high peaks). The block code guarantees that the
    graph cache is always "too big", so the maximum error that we
    can get because of this (in the extreme case where a block
    contains just 1 frame) is PEAK_HRES - 1 frames. When we
    need to stitch multiple blocks together this rapidly adds up.
    So we need a compensation mechanism for that. We do this by
    keeping track of how many frames each block contains/describes,
    then advancing the index into the graph cache by
    sizeof(graph_bits_t) at each multiple of 128 (i.e. the
    graph_hres). 

    At this point we know we need to get another block and stitch
    things together. In case we did not read a nice PEAK_HRES
    multiple, then we know we need to recalculate the final peak
    element. The spill variable tracks this. */

    start_gap = off - (floor(fo / frame_divider) * cache_el_sz);
    spill = frames_read % (int)frame_divider;
    src_off = start_gap + (ct - (ceil(spill / frame_divider) * cache_el_sz));
    frames_total_read = frames_read;
    frames_left -= frames_read;

    l = l->next;

    //    DEBUG("frames_read: %ld, frames_left: %ld, spill: %d, new ct: %d, src_off: %d\n",
    //          frames_read, frames_left, spill, ct, src_off);

    /* Case 6: fail when subsequent caches cannot satisfy the request
       up to requested end (i.e. throw away all our work). */

    if(start_gap)
        fail_on_gap = 1;

    while(l && frames_left > 0) {

        /* FIXME: in case we haven't read PEAK_HRES frames yet
           we fudge the offset to overwrite the previous peak element,
           thus destroying some peak data. We should instead combine
           the last peak element from the previous buffer with the
           first peak element from the current buffer and calculate a
           new high/low. */

        has_gap = 1;
        c_low = ((block *)l->data)->peak_lows;
        c_high = ((block *)l->data)->peak_highs;
        off = 0;
        ct = ceil(MIN(((block *)l->data)->count, frames_left) / frame_divider) * cache_el_sz;
        cache_find(c_low,
                   low_bits + src_off,
                   &off,
                   &ct);
        cache_find(c_high,
                   high_bits + src_off,
                   &off,
                   &ct);

        if(off)
            break;

        if(!ct)
            break;

        frames_read = (ct / cache_el_sz) * frame_divider;
        if(frames_read > MIN(((block *)l->data)->count, frames_left)) 
            frames_read = MIN(((block *)l->data)->count, frames_left);

        /* Ends at some X, but not at end of cache and not at end of request. */

        if(frames_read < ((block *)l->data)->count &&
           frames_read < frames_left) 
            if(fail_on_gap)
                break;

        spill = frames_read % (int)frame_divider;
        frames_total_read += frames_read;
        frames_left -= frames_read;
        src_off = floor(frames_total_read / frame_divider) * cache_el_sz;

        //        DEBUG("frames_read: %ld, frames_left: %ld, spill: %d, new ct: %d, src_off: %d\n",
        //              frames_read, frames_left, spill, ct, src_off);

        l = l->next;
        has_gap = 0;
    }

    //    DEBUG("fell out of loop: off: %d, ct: %d, l: %p, frames_read: %ld, frames_left: %ld, spill: %d, src_off: %d, frames_total_read: %ld\n",
    //          off, ct, l, frames_read, frames_left, spill, src_off, frames_total_read);

    if(fail_on_gap && has_gap)
        return;

    /* Case 3: adjust frame_offset. */
    
    if(!start_gap)
        *frame_offset += frames_total_read;

    *frame_count -= frames_total_read;
}

/*
 * Copies the requested samples from the track into the supplied
 * buffer. The frame_count parameter must be non-zero.
 * 
 * @param tr the track.
 * @param frame_bits the buffer to copy the data into.
 * @param frame_offset the start of the data to copy.
 * @param frame_count the (non-zero) number of frames to copy.
 * @return 0 on error, the actual number of frames copied otherwise.
 * FIXME: return code doesn't distinguish between errors or 
 * simply "no data".
 */

AFframecount
track_frames_get(track *tr,
                 frame_bits_t frame_bits,
                 AFframecount frame_offset,
                 AFframecount frame_count) {
    AFframecount tr_ct, frame_count_old = frame_count;

    rwlock_rlock(&tr->rwl);

    /* Clamp frame count. */

    tr_ct = track_count(tr);
    if(frame_offset + frame_count > tr_ct) {
        frame_count = tr_ct - frame_offset;
        frame_count_old = frame_count;
        DEBUG("adjusted frame_count: %ld\n", frame_count);
        if(frame_count <= 0) {
            rwlock_runlock(&tr->rwl);
            return 0;
        }
    }

    track_frame_cache_find(tr,
                           frame_bits,
                           &frame_offset,
                           &frame_count);
    
    DEBUG("after frame_cache_find: frame_offset: %ld, frame_count: %ld\n",
          frame_offset, frame_count);
    rwlock_runlock(&tr->rwl);

    if(!frame_count) 
        return frame_count_old;

    return 0;
}

/*
 * Retrieves a scaled representation of the requested frames into the 
 * buffer given by gb. The buffer must be large enough to hold at least 
 * frame_count * sizeof(_graph_bits) elements. 
 *
 * @param tr the track
 * @param gb the buffer that receives the scaled representation.
 * @param frame_offset the offset in the track from which to return data
 * @param frame_count the (non-zero) number of frames to copy.
 * @param hres the required resolution (e.g. 0.5, 1, 2, 4, ...)
 * @return 0 on error, number of frames actually copied otherwise.
 */

/* AFframecount */
/* track_graph_get(track *tr,  */
/*                 _graph_bits gb, */
/*                 AFframecount frame_offset, */
/*                 AFframecount frame_count, */
/*                 float hres) { */
/*     AFframecount r = 0, frame_count_old = frame_count; */
/*     size_t gbc_sz;  */
/*     _graph_bits gbc = NULL; */

/*     if(frame_count <= 0) { */
/*         FAIL("frame_count <= 0\n"); */
/*         abort(); */
/*     } */

/*     rwlock_rlock(&tr->rwl); */

/*     if(hres >= tr->graph_hres) { */

/*         /\* Need memory to scale up from cache. *\/ */

/*         gbc_sz = (size_t) ceil(frame_count / hres) *  */
/*             sizeof(_graph_bits_t) * (hres / tr->graph_hres); */
/*         gbc = mem_calloc(1, gbc_sz); */
/*         if(!gbc) { */
/*             FAIL("could not get memory for temporary graph cache buffer\n"); */
/*             rwlock_runlock(&tr->rwl); */
/*             return 0; */
/*         } */

/*         track_graph_cache_find(tr, */
/*                                gbc, */
/*                                &frame_offset, */
/*                                &frame_count); */
/*     } */

/*     /\* We now either have something fit for immediate consumption by */
/*        the caller (i.e. a graph at the requested hres), or something */
/*        fit for insertion into the cache (i.e. a graph at the cache */
/*        hres). *\/ */

/*     if(hres >= tr->graph_hres) { */

/*         /\* Scale the graph from the cache hres (probably 128) to  */
/*            the requested hres. *\/ */

/*         snd_scale(gb, */
/*                   gbc, */
/*                   ((frame_count_old - frame_count) + r) / tr->graph_hres, */
/*                   tr->frame_width, */
/*                   hres / tr->graph_hres, */
/*                   0, */
/*                   0, */
/*                   1); */
/*         mem_free(gbc); */
/*     } */

/*     rwlock_runlock(&tr->rwl); */

/*     return (frame_count_old - frame_count) + r; */
/* } */






/*
 * Retrieves the requested frames from the frame buffer, scales them
 * according to the given scale factor, and delivers the result in the
 * supplied buffer. There is no explicit error condition for this
 * function. A return value of 0 may either mean there are no more
 * frames or that some memory could not be allocated. 
 */

/*
AFframecount
track_graph_read(track *tr, 
                 _graph_bits graph_bits,
                 AFframecount frame_offset,
                 AFframecount frame_count,
                 float hres) {
    frame_bits_t fb;
    AFframecount r;
    size_t fb_sz = tr->frame_width * frame_count;

    fb = mem_alloc(fb_sz);
    if(!fb) 
        return 0;

    r = track_frame_buffer_get(tr, 
                               fb, 
                               frame_offset, 
                               frame_count);
    
    if(r <= 0) {
        mem_free(fb);
        return 0;
    }
    
    snd_frames_buffer_to_graph_buffer(graph_bits,
                                      fb,
                                      r,
                                      tr->frame_width,
                                      hres);
    mem_free(fb);
    return r;
}
*/

/*
 * Retrieves the requested frames from the frame buffer, scales them
 * according to the given scale factor, and delivers the result in the
 * supplied buffer. There is no explicit error condition for this
 * function. A return value of 0 may either mean there are no more
 * frames or that some memory could not be allocated. 
 */

AFframecount
track_graph_read(track *tr, 
                 peak_unit_t *graph_bits_low,
                 peak_unit_t *graph_bits_high,
                 AFframecount frame_offset,
                 AFframecount frame_count,
                 float hres) {
    frame_bits_t fb;
    AFframecount r;
    size_t fb_sz = tr->frame_width * frame_count;
    fb = mem_alloc(fb_sz);
    if(!fb) 
        return 0;

    r = track_frames_get(tr, 
                         fb, 
                         frame_offset, 
                         frame_count);
    
    if(r <= 0) {
        mem_free(fb);
        return 0;
    }
    
    snd_samples_to_peaks(graph_bits_low,
                         graph_bits_high,
                         fb,
                         r,
                         tr->frame_width,
                         hres);
    mem_free(fb);
    return r;
}

/*
 * Retrieves a scaled representation of the requested frames into the 
 * buffer given by gb. The buffer must be large enough to hold at least 
 * frame_count * sizeof(_graph_bits) elements. 
 *
 * @param tr the track
 * @param gb the buffer that receives the scaled representation.
 * @param frame_offset the offset in the track from which to return data
 * @param frame_count the (non-zero) number of frames to copy.
 * @param hres the required resolution (e.g. 0.5, 1, 2, 4, ...)
 * @return 0 on error, number of frames actually copied otherwise.
 */

AFframecount
track_graph_get(track *tr, 
                peak_unit_t *gb_lows,
                peak_unit_t *gb_highs,
                AFframecount frame_offset,
                AFframecount frame_count,
                float hres) {
    AFframecount r = 0, frame_offset_old = frame_offset, frame_count_old = frame_count;
    size_t gbc_sz, soff = 0; 
    float q = hres;
    peak_unit_t *gbp_lows = NULL, *gbp_highs = NULL, *gbc_lows = NULL, *gbc_highs = NULL;

    if(frame_count <= 0) {
        FAIL("frame_count <= 0\n");
        abort();
    }

    rwlock_rlock(&tr->rwl);

    if(hres >= tr->graph_hres) {

        /* Need memory to scale up from cache. */

        gbc_sz = (size_t) ceil(frame_count / hres) * 
            sizeof(peak_unit_t) * (hres / tr->graph_hres) * 2;
        gbc_lows = mem_calloc(1, gbc_sz);
        if(!gbc_lows) {
            FAIL("could not get memory for temporary graph cache buffer\n");
            rwlock_runlock(&tr->rwl);
            return 0;
        }
        gbc_highs = (peak_unit_t *)(((int8_t *)gbc_lows) + (gbc_sz / 2));
        /*
        DEBUG("frame_offset: %ld, frame_count: %ld, gbc_lows: %p, gbc_high: %p\n",
              frame_offset, frame_count, gbc_lows, gbc_highs);
        */
        track_graph_cache_find(tr,
                               gbc_lows,
                               gbc_highs,
                               &frame_offset,
                               &frame_count);
        /*
        DEBUG("%s: graph cache returned frame_offset: %ld, frame_count: %ld\n", 
                      (frame_count == frame_count_old ? "MISS" : 
                       (frame_count == 0 ? "HIT" : "PARTIAL")), frame_offset, frame_count);
        */
        

    }

    if(frame_count) {

        /* Get it either at the requested scaling factor or at a
           scaling factor suitable for the cache. */

        if(hres >= tr->graph_hres) {
            gbp_lows = gbc_lows;
            gbp_highs = gbc_highs;
            q = tr->graph_hres;
        } else {
            gbp_lows = gb_lows;
            gbp_highs = gb_highs;
            q = hres;
        }
        
        soff = (frame_offset / q) - (frame_offset_old / q);
        
        r = track_graph_read(tr,
                             &gbp_lows[soff],
                             &gbp_highs[soff],
                             frame_offset,
                             frame_count, 
                             q);
        
    }

    /* We now either have something fit for immediate consumption by
       the caller (i.e. a graph at the requested hres), or something
       at the cache hres. */

    if(hres >= tr->graph_hres) {

        /* Scale the graph from the cache hres to the requested
           hres. */

        snd_scale(gb_lows,
                  gb_highs,
                  gbc_lows,
                  gbc_highs,
                  ((frame_count_old - frame_count) + r) / tr->graph_hres,
                  tr->frame_width,
                  hres / tr->graph_hres,
                  0,
                  0,
                  1);

        mem_free(gbc_lows);
    }

    rwlock_runlock(&tr->rwl);

    return (frame_count_old - frame_count) + r;
}

/*
 * Copies data from the frame caches in the blocks for this track into
 * the given buffer. The close relative for this function is
 * track_graph_cache_find(), it uses the same algorithm but knows how
 * to "blend" graph caches (i.e. how to combine two blocks' peak data
 * properly).
 *
 * @param tr the track.
 * @param bits the buffer to copy into.
 * @param frame_offset the offset to find from. On return, receives
 * the offset at which copying started.
 * @param frame_count the number of frames to find. On return,
 * receives the number of frames copied.
 */

void
track_frame_cache_find(track *tr,
                       void *bits,
                       AFframecount *frame_offset,
                       AFframecount *frame_count) {
    AFframecount fo, fc;
    GList *l = NULL;
    cache *c;
    size_t cache_el_sz, src_off, off, start_gap, ct, total_ct;
    int fail_on_gap = 0, has_gap = 0, err;

    /*
     * In the setup phase we find the block that the requested offset
     * resides in. Then we ask the frame cache for that block to return as
     * much data as possible.
     * 
     * Then we evaluate the cache reply. We need to return a contiguous
     * block that either starts at the requested start and extends to
     * some X, or, a contiguous block that starts at some X and
     * extends to the requested end. This is because cache misses need
     * to be replenished in a single (contiguous) read. So there are
     * seven cases:
     *
     * 1. Nothing found.
     * 2. Found something from the requested start up to requested end.
     * 3. Found something from the requested start up to the end of the cache.
     * 4. Found something from the requested start up to some other X.
     * 5. Found something from some X up to the requested end.
     * 6. Found something from some X up to the end of the cache.
     * 7. Found something from some X up to some Y.
     *
     * In case 1 we simply fail and return nothing.
     *
     * In cases 2, 4 and 5 we have found some data, and also know that
     * this is the last block that we need to look at, so we can
     * return.
     *
     * In case 3 we need to look at the next block(s) and evaluate it
     * similarly to this block until there is no more data to be
     * retrieved (or until we encounter a gap).
     *
     * In case 6 we also need to look at the next block(s), but with
     * the added constraint that any subsequently provided data MUST
     * ultimately end at the requested end, without any intermediate
     * gaps. IOW, in case 6 we "fail_on_gap".
     *
     * Finally in case 7 we have a gap and return nothing.
     *
     * On entry the frame_offset and frame_count parameters specify how
     * many frames are to be retrieved from what offset. On return,
     * frame_offset and frame_count indicate to what extent the request
     * was satisfied. That is the frame_count variable returns the number
     * of bytes that remain to be read and the frame_offset variable
     * returns the position that they should be read at.
     */

    fo = *frame_offset;
    fc = *frame_count;

    /* Setup. */

    err = blocklist_find_block(tr->bl, &fo, &l);

    if(err)
        return;

    c = ((block *)l->data)->frame_cache;
    cache_el_sz = tr->frame_width;
    src_off = 0;

    off = fo * cache_el_sz;
    ct = fc * cache_el_sz;
    cache_find(c,
               bits + src_off,
               &off,
               &ct);
    
    /* Case 1: Didn't find anything, no use looking further. */
    
    if(!ct)
        return;

    /* Case 2 & 4: */

    if(off == fo * cache_el_sz &&
       (ct == fc * cache_el_sz ||
        off + ct < ((block *)l->data)->count * cache_el_sz)) {
        *frame_offset += ct / cache_el_sz;
        *frame_count -= ct / cache_el_sz;
        return;
    }

    /* Case 5: No need to adjust frame_offset. */

    if(off + ct == (fo + fc) * cache_el_sz) {
        *frame_count -= ct / cache_el_sz;
        return;
    }

    /* Case 7: Found something, but it does not start at the requested
       beginning and it does not end at the end of the cache buffer
       and it does not end at the requested end -> useless. */
    
    if(off > fo * cache_el_sz &&
       off + ct < ((block *)l->data)->count * cache_el_sz &&
       (off + ct < (fo + fc) * cache_el_sz)) {
        return;
    }

    /* Now only cases 3 and 6 remain. */

    start_gap = off - (fo * cache_el_sz);
    src_off += ct + start_gap;
    total_ct = ct;
    ct = (fc * cache_el_sz) - src_off;
    off = 0;
    l = l->next;

    /* Case 6: fail when returned data does not end at requested end. */

    if(start_gap)
        fail_on_gap = 1;

    while(l && ct > 0) {
        has_gap = 1;
        c = ((block *)l->data)->frame_cache;
        cache_find(c,
                   bits + src_off,
                   &off,
                   &ct);

        if(!ct)
            break;

        if(off)
            break;

        /* Ends at some X, but not at end of cache and not at end of request. */

        if((off + ct < ((block *)l->data)->count * cache_el_sz) &&
           ((fc * cache_el_sz) - (src_off + ct)) > 0)
            if(fail_on_gap)
                break;

        src_off += ct;
        total_ct += ct;
        ct = (fc * cache_el_sz) - src_off;
        off = 0;
        l = l->next;
        has_gap = 0;
    }

    if(fail_on_gap && has_gap)
        return;

    /* Case 3: adjust frame_offset. */

    if(!start_gap)
        *frame_offset += total_ct / cache_el_sz;

    *frame_count -= total_ct / cache_el_sz;
}


/**
 * Inserts a buffer of sample data into the track.  The offset
 * can extend beyond the actual length of the track. In this case the
 * track is padded with virtual zero data up to the required length.
 *
 * @param tr the track.
 * @param sample_bits the sample data, of type tr->sample_type.
 * @param offset the offset to insert at.
 * @param count the number of samples in the buffer.
 * @return 0 on success, 1 if there was not enough memory.
 */

int
track_frames_insert(track *tr,
                    void *sample_bits,
                    AFframecount offset,
                    AFframecount count) {
    int r;
    rwlock_wlock(&tr->rwl);

    /* Append with silence to the required length first if
       necessary. */

    if(offset > track_count(tr)) 
        track_insert_silence(tr, 
                             MIN(track_count(tr), offset), 
                             MAX(track_count(tr), offset) -
                             MIN(track_count(tr), offset));

    r = blocklist_insert_buffer(tr->bl,
                                tr->sample_type,
                                sample_bits,
                                offset,
                                count);
    rwlock_wunlock(&tr->rwl);
    return r;
}

/*
 * Same as track_frames_get but converts the data to 32 bits
 * signed integer before returning. 
 */

AFframecount
track_int32_frames_get(track *tr,
                       int32_t *int32_frame_bits,
                       AFframecount frame_offset,
                       AFframecount frame_count) {
    AFframecount r;
    frame_bits_t fb;

    if(!(frame_count > 0 && frame_offset >= 0))
        return 0;

    if(tr->sample_type == SAMPLE_TYPE_INT_32) {
        fb = int32_frame_bits;
    } else {
        fb = mem_alloc(frame_count * sample_get_width(tr->sample_type));
        if(!fb)
            return 0;
    }

    r = track_get_samples(tr, fb, frame_offset, frame_count);

    if(tr->sample_type == SAMPLE_TYPE_INT_32) 
        return r;

    sample_convert(tr->sample_type,
                   SAMPLE_TYPE_INT_32,
                   fb,
                   int32_frame_bits,
                   r);
    mem_free(fb);
    return r;
}

/*
 * Same as track_frames_replace but expects sample data to 
 * be 32 bit unsigned format.
 */

int
track_int32_frames_replace(track *tr,
                           const int32_t *int32_frame_bits,
                           AFframecount frame_offset,
                           AFframecount frame_count) {
    GList *del_blks;
    int r;

    /* FIXME: memcopy replace would be more efficient sometimes than
       delete/put combo. */
    
    rwlock_wlock(&tr->rwl);
    if(track_delete(tr,
                    &del_blks,
                    frame_offset,
                    frame_count)) {
        rwlock_wunlock(&tr->rwl);
        return 1;
    }
    r = track_int32_frames_insert(tr,
                                  int32_frame_bits,
                                  frame_offset,
                                  frame_count);
    
    if(r) {
        if(track_insert(tr,
                        del_blks,
                        frame_offset)) 
            FAIL("unable to recover\n");
    } else 
        blocklist_blocks_destroy(del_blks);
    
    rwlock_wunlock(&tr->rwl);
    return r;
}

/*
 * Same as track_frames_insert but for 32 bit unsigned integers.
 */

int
track_int32_frames_insert(track *tr,
                          int32_t *int32_frame_bits,
                          AFframecount frame_offset,
                          AFframecount frame_count) {
    int r;
    frame_bits_t fb;

    if(!(frame_count > 0 && frame_offset >= 0))
        return 1;

    if(tr->sample_type == SAMPLE_TYPE_INT_32) 
        return track_frames_insert(tr, 
                                   int32_frame_bits, 
                                   frame_offset,
                                   frame_count);

    fb = mem_alloc(frame_count * sample_get_width(tr->sample_type));
    if(!fb)
        return 1;

    sample_convert(SAMPLE_TYPE_INT_32,
                   tr->sample_type,
                   int32_frame_bits,
                   fb,
                   frame_count);
    
    r = track_frames_insert(tr, fb, frame_offset, frame_count);
    mem_free(fb);
    return r;
}


/*
 * Same as track_frames_get but converts the data to floats
 * before returning. 
 */

AFframecount
track_float_frames_get(track *tr,
                       float *float_frame_bits,
                       AFframecount frame_offset,
                       AFframecount frame_count) {
    AFframecount r;
    frame_bits_t fb;

    if(!(frame_count > 0 && frame_offset >= 0))
        return 0;

    if(tr->sample_type == SAMPLE_TYPE_FLOAT_32) {
        fb = float_frame_bits;
    } else {
        fb = mem_alloc(frame_count * sample_get_width(tr->sample_type));
        if(!fb)
            return 0;
    }

    r = track_get_samples(tr, fb, frame_offset, frame_count);

    sample_convert(tr->sample_type,
                   SAMPLE_TYPE_FLOAT_32,
                   fb,
                   float_frame_bits,
                   r);

    if(tr->sample_type == SAMPLE_TYPE_FLOAT_32) 
        return r;

    mem_free(fb);
    return r;
}

/*
 * Same as track_frames_replace but expects sample data to 
 * be floating point.
 */

/*
int
track_float_frames_replace(track *tr,
                           float *float_frame_bits,
                           AFframecount frame_offset,
                           AFframecount frame_count) {
    GList *del_blks;
    int r;

  
    rwlock_wlock(&tr->rwl);
    del_blks = track_delete(tr,
                            frame_offset,
                            frame_count);
    if(!del_blks) {
        rwlock_wunlock(&tr->rwl);
        return 1;
    }
    r = track_float_frames_insert(tr,
                                  float_frame_bits,
                                  frame_offset,
                                  frame_count);

    if(r) 
        track_insert(tr,
                     del_blks,
                     frame_offset);
    else 
        blocklist_blocks_destroy(del_blks);
    
    rwlock_wunlock(&tr->rwl);
    return r;
}

*/

int
track_float_frames_replace(track *tr,
                           float *float_frame_bits,
                           AFframecount frame_offset,
                           AFframecount frame_count) {
    GList *del_blks = NULL;
    int r;

    /* First insert, then delete on success. */

    rwlock_wlock(&tr->rwl);
    r = track_float_frames_insert(tr,
                                  float_frame_bits,
                                  frame_offset,
                                  frame_count);

    if(r) {
        FAIL("could not insert, frame_offset: %ld, frame_count: %ld\n",
             frame_offset, frame_count);
        rwlock_wunlock(&tr->rwl);
        return r;
    }

    /* There is nothing to delete at the end of the track. */

    if(frame_offset + frame_count == track_count(tr)) {
        rwlock_wunlock(&tr->rwl);
        return 0;
    }

    if(track_delete(tr,
                    &del_blks,
                    frame_offset + frame_count,
                    frame_count)) {
        FAIL("delete failed\n");
        
        /* Delete previously inserted stuff if delete fails. */

        if(track_delete(tr,
                        &del_blks,
                        frame_offset,
                        frame_count))
            FAIL("unable to recover\n");
        blocklist_blocks_destroy(del_blks);
        rwlock_wunlock(&tr->rwl);
        return 1;
    }
    blocklist_blocks_destroy(del_blks);
    rwlock_wunlock(&tr->rwl);
    return 0;
}

/*
 * Same as track_frames_insert but for floats.
 */

int
track_float_frames_insert(track *tr,
                          float *float_frame_bits,
                          AFframecount frame_offset,
                          AFframecount frame_count) {
    int r;
    frame_bits_t fb;

    if(!(frame_count > 0 && frame_offset >= 0)) 
        return 1;

    if(tr->sample_type == SAMPLE_TYPE_FLOAT_32) {
        fb = float_frame_bits;
    } else {
        fb = mem_alloc(frame_count * sample_get_width(tr->sample_type));
        if(!fb) 
            return 1;
    }

    if(tr->sample_type == SAMPLE_TYPE_FLOAT_32) 
        return track_frames_insert(tr, 
                                   float_frame_bits, 
                                   frame_offset,
                                   frame_count);

    fb = mem_alloc(frame_count * sample_get_width(tr->sample_type));
    if(!fb)
        return 1;

    sample_convert(SAMPLE_TYPE_FLOAT_32,
                   tr->sample_type,
                   float_frame_bits,
                   fb,
                   frame_count);
    
    r = track_frames_insert(tr, fb, frame_offset, frame_count);
    mem_free(fb);
    return r;
}


/*
 * Almost identical to track_frame_cache_find() above. It is a bit more
 * complicated though because it has to account for the fact that a
 * block may contain less frames than a a single graph_bits_t
 * describes, in which case the graph caches for consecutive blocks
 * need to be "blended". 
 */

void
track_graph_cache_find(track *tr,
                       void *low_bits,
                       void *high_bits,
                       AFframecount *frame_offset,
                       AFframecount *frame_count) {
    AFframecount fo, frames_read, frames_left, frames_total_read;
    GList *l = NULL;
    cache *c_low, *c_high;
    size_t cache_el_sz, src_off, off, start_gap, ct;
    int fail_on_gap = 0, has_gap = 0, spill = 0, err = 0;
    float frame_divider;

    fo = *frame_offset;
    frames_left = *frame_count;

    /* Setup. */
    
    err = blocklist_find_block(tr->bl, &fo, &l);

    if(!l)
        return;

    c_low = ((block *)l->data)->peak_lows;
    c_high = ((block *)l->data)->peak_highs;
    frame_divider = tr->graph_hres;
    cache_el_sz = sizeof(graph_bits_unit_t);
    src_off = 0;

    off = floor(fo / frame_divider) * cache_el_sz;
    ct = ceil(MIN(((block *)l->data)->count, frames_left) / frame_divider) * cache_el_sz;
    cache_find(c_low,
               low_bits + src_off,
               &off,
               &ct);
    cache_find(c_high,
               high_bits + src_off,
               &off,
               &ct);
    
    /* Calculate how many frames (as opposed to peak cache elements)
       were actually returned. */

    frames_read = (ct / cache_el_sz) * frame_divider;
    if(((off + ct) / cache_el_sz) * frame_divider > ((block *)l->data)->count) 
        frames_read -= 
            ((((off + ct) / cache_el_sz) * frame_divider) - ((block *)l->data)->count);

    /* Case 1: Didn't find anything, no use looking further. */
    
    if(!ct)
        return;

    /* Case 2 & 4: */

    if(off == floor(fo / frame_divider) * cache_el_sz &&
       (frames_read == frames_left || 
        ((off / cache_el_sz) * frame_divider) + frames_read < ((block *)l->data)->count)) {
        *frame_offset += frames_read;
        *frame_count -= frames_read;
        return;
    }

    /* Case 5: No need to adjust frame_offset. */

    if((floor(off / cache_el_sz) * frame_divider) + frames_read == 
       (floor(fo / frame_divider) * cache_el_sz) + frames_left) {
        *frame_count -= frames_read;
        return;
    }

    /* Case 7: Found something, but it does not start at the requested
       beginning and it does not end at the end of the cache buffer
       and it does not end at the requested end -> useless. */
    
    if(off > floor(fo / frame_divider) * cache_el_sz &&
       ((off / cache_el_sz) * frame_divider) + frames_read < ((block *)l->data)->count &&
       ((off / cache_el_sz) * frame_divider) + frames_read < 
       (floor(fo / frame_divider) * cache_el_sz) + frames_left) {
        return;
    }
    
    /* Now only cases 3 and 6 remain. */

    /* Intermezzo.

    The number of frames in a block is not always a neat multiple
    of the number of frames represented by a single graph_bits unit
    (pair of low/high peaks). The block code guarantees that the
    graph cache is always "too big", so the maximum error that we
    can get because of this (in the extreme case where a block
    contains just 1 frame) is GRAPH_BITS_HRES - 1 frames. When we
    need to stitch multiple blocks together this rapidly adds up.
    So we need a compensation mechanism for that. We do this by
    keeping track of how many frames each block contains/describes,
    then advancing the index into the graph cache by
    sizeof(graph_bits_t) at each multiple of 128 (i.e. the
    graph_hres). 

    At this point we know we need to get another block and stitch
    things together. In case we did not read a nice 128 multiple, then
    we know we need to recalculate the final peak element. The 
    spill variable tracks this. */

    start_gap = off - (floor(fo / frame_divider) * cache_el_sz);
    spill = frames_read % (int)frame_divider;
    src_off = start_gap + (ct - (ceil(spill / frame_divider) * cache_el_sz));
    frames_total_read = frames_read;
    frames_left -= frames_read;

    l = l->next;

    //    DEBUG("frames_read: %ld, frames_left: %ld, spill: %d, new ct: %d, src_off: %d\n",
    //          frames_read, frames_left, spill, ct, src_off);

    /* Case 6: fail when subsequent caches cannot satisfy the request
       up to requested end (i.e. throw away all our work). */

    if(start_gap)
        fail_on_gap = 1;

    while(l && frames_left > 0) {

        /* FIXME: in case we haven't read 128 frames yet we fudge the
           offset to overwrite the previous peak element, thus
           destroying some peak data. We should instead combine the
           last peak element from the previous buffer with the first
           peak element from the current buffer and calculate a new
           high/low. */

        has_gap = 1;
        c_low = ((block *)l->data)->peak_lows;
        c_high = ((block *)l->data)->peak_highs;
        off = 0;
        ct = ceil(MIN(((block *)l->data)->count, frames_left) / frame_divider) * cache_el_sz;
        cache_find(c_low,
                   low_bits + src_off,
                   &off,
                   &ct);
        cache_find(c_high,
                   high_bits + src_off,
                   &off,
                   &ct);

        if(off)
            break;

        if(!ct)
            break;

        frames_read = (ct / cache_el_sz) * frame_divider;
        if(frames_read > MIN(((block *)l->data)->count, frames_left)) 
            frames_read = MIN(((block *)l->data)->count, frames_left);

        /* Ends at some X, but not at end of cache and not at end of request. */

        if(frames_read < ((block *)l->data)->count &&
           frames_read < frames_left) 
            if(fail_on_gap)
                break;

        spill = frames_read % (int)frame_divider;
        frames_total_read += frames_read;
        frames_left -= frames_read;
        src_off = floor(frames_total_read / frame_divider) * cache_el_sz;

        //        DEBUG("frames_read: %ld, frames_left: %ld, spill: %d, new ct: %d, src_off: %d\n",
        //              frames_read, frames_left, spill, ct, src_off);

        l = l->next;
        has_gap = 0;
    }

    //    DEBUG("fell out of loop: off: %d, ct: %d, l: %p, frames_read: %ld, frames_left: %ld, spill: %d, src_off: %d, frames_total_read: %ld\n",
    //          off, ct, l, frames_read, frames_left, spill, src_off, frames_total_read);

    if(fail_on_gap && has_gap)
        return;

    /* Case 3: adjust frame_offset. */
    
    if(!start_gap)
        *frame_offset += frames_total_read;

    *frame_count -= frames_total_read;
}


/*
 * Retrieves the requested frames from the frame buffer, scales them
 * according to the given scale factor, and delivers the result in the
 * supplied buffer. There is no explicit error condition for this
 * function. A return value of 0 may either mean there are no more
 * frames or that some memory could not be allocated. 
 */

AFframecount
track_graph_read(track *tr, 
                 graph_bits_unit_t *graph_bits_low,
                 graph_bits_unit_t *graph_bits_high,
                 AFframecount frame_offset,
                 AFframecount frame_count,
                 float hres) {
    frame_bits_t fb;
    AFframecount r;
    size_t fb_sz = sample_get_width(tr->sample_type) * frame_count;

    fb = mem_alloc(fb_sz);
    if(!fb) 
        return 0;

    r = track_get_samples(tr, 
                          fb, 
                          frame_offset, 
                          frame_count);
    
    if(r <= 0) {
        mem_free(fb);
        fb = NULL;
        return 0;
    }
    
    sample_scale_samples(graph_bits_low,
                         graph_bits_high,
                         fb,
                         r,
                         tr->sample_type,
                         hres);
    mem_free(fb);
    fb = NULL;
    return r;
}

/*
 * Retrieves a scaled representation of the requested frames into the
 * buffers given by gb_lows and gb_highs. The buffers must be large
 * enough to hold at least frame_count * sizeof(_graph_bits) elements.
 *
 * @param tr the track
 * @param gb the buffer that receives the scaled representation.
 * @param frame_offset the offset in the track from which to return data
 * @param frame_count the (non-zero) number of frames to copy.
 * @param hres the required resolution (e.g. 0.5, 1, 2, 4, ...)
 * @return 0 on error, number of frames actually copied otherwise.
 */

AFframecount
track_get_peaks(track *tr, 
                peak_unit_t *gb_lows,
                peak_unit_t *gb_highs,
                AFframecount frame_offset,
                AFframecount frame_count,
                float hres) {
    AFframecount r = 0, frame_offset_old = frame_offset, frame_count_old = frame_count;
    size_t gbc_sz, soff = 0; 
    float q = hres;
    graph_bits_unit_t *gbp_lows = NULL, *gbp_highs = NULL, *gbc_lows = NULL, *gbc_highs = NULL;

    if(frame_count <= 0) {
        FAIL("frame_count <= 0\n");
        abort();
    }

    //    memset(gb_lows, '\0', floor(frame_count / hres));
    //    memset(gb_lows, '\0', floor(frame_count / hres));

    rwlock_rlock(&tr->rwl);

    if(hres >= tr->graph_hres) {

        /* Need memory to scale up from cache. */

        gbc_sz = (size_t) ceil(frame_count / hres) * 
            sizeof(graph_bits_unit_t) * (hres / tr->graph_hres) * 2;
        gbc_lows = mem_calloc(1, gbc_sz);
        if(!gbc_lows) {
            FAIL("could not get memory for temporary graph cache buffer\n");
            rwlock_runlock(&tr->rwl);
            return 0;
        }
        gbc_highs = (graph_bits_unit_t *)(((int8_t *)gbc_lows) + (gbc_sz / 2));
        //        DEBUG("frame_offset: %ld, frame_count: %ld, gbc_lows: %p, gbc_high: %p\n",
        //              frame_offset, frame_count, gbc_lows, gbc_highs);
        track_graph_cache_find(tr,
                               gbc_lows,
                               gbc_highs,
                               &frame_offset,
                               &frame_count);
        /*        DEBUG("%s: graph cache returned frame_offset: %ld, frame_count: %ld\n", 
                      (frame_count == frame_count_old ? "MISS" : 
               (frame_count == 0 ? "HIT" : "PARTIAL")), frame_offset, frame_count);
        */

    }

    if(frame_count) {

        /* Get it either at the requested scaling factor or at a
           scaling factor suitable for the cache. */

        if(hres >= tr->graph_hres) {
            gbp_lows = gbc_lows;
            gbp_highs = gbc_highs;
            q = tr->graph_hres;
        } else {
            gbp_lows = gb_lows;
            gbp_highs = gb_highs;
            q = hres;
        }
        
        soff = (frame_offset / q) - (frame_offset_old / q);
        
        r = track_graph_read(tr,
                             &gbp_lows[soff],
                             &gbp_highs[soff],
                             frame_offset,
                             frame_count, 
                             q);
        
    }

    /* We now either have something fit for immediate consumption by
       the caller (i.e. a graph at the requested hres), or something
       at the cache hres. */

    if(hres >= tr->graph_hres) {

        /* Scale the graph from the cache hres to the requested
           hres. */

        sample_scale_samples_or_peaks(gb_lows,
                                      gb_highs,
                                      gbc_lows,
                                      gbc_highs,
                                      ((frame_count_old - frame_count) + r) / tr->graph_hres,
                                      tr->sample_type,
                                      hres / tr->graph_hres,
                                      0,
                                      0,
                                      1);

        mem_free(gbc_lows);
    }

    rwlock_runlock(&tr->rwl);

    return (frame_count_old - frame_count) + r;
}

#endif

