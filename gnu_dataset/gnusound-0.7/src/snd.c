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
#include <errno.h>
#include <string.h>
#include <stdlib.h>
#include "pref.h"
#include "mem.h"
#include "sample.h"
#include "snd.h"

/**
 *@file
 * A snd object describes a piece of sound data. It has functions for
 * adding and removing tracks, and inserting and deleting samples.
 * 
 */

/** 
 * Creates a "shadow" of the given snd object. A shadow holds a copy
 * of a region of the specified snd object. The region can be extended
 * by using snd_shadow_extend(). This is useful for incrementally
 * building a region, e.g. to be pushed onto the history.
 * @param src The source snd to shadow.
 * @param map The tracks to shadow.
 * @param offset Start of shadow on src.
 * @param count Length of shadow on src.
 */

struct snd_shadow *
snd_shadow_new(snd *src,
               track_map_t map,
               AFframecount offset,
               AFframecount count) {
    struct snd_shadow *shadow = mem_alloc(sizeof(*shadow));

    if(!shadow)
        return NULL;

    shadow->dst = snd_copy(src, map, offset, count);

    if(!shadow->dst) {
        mem_free(shadow);
        return NULL;
    }

    snd_dump(shadow->dst);
    shadow->src = src;
    shadow->offset = shadow->pivot = offset;
    shadow->count = count;
    shadow->map = map;
    return shadow;
}

/**
 * Extends the shadow.
 * @param shadow The shadow to extend.
 * @param offset The start of the region to add to the shadow.
 * @param count The length of the region to add to the shadow.
 */

int
snd_shadow_extend(struct snd_shadow *shadow,
                  AFframecount offset,
                  AFframecount count) {
    int err;
    track_map_t target_map, source_map;
    snd *region;

    /* Already in shadow */

    if(offset > shadow->offset && 
       offset + count < shadow->offset + shadow->count)
        return 0;
    
    /* We can't work with gaps. */

    if(offset != shadow->offset + shadow->count && 
       offset + count != shadow->offset) {
        return 1;
    }

    /* If the region completely covers what we have already and more
       then translate it into two updates, one for the end and one for
       the beginning. */

    if(offset < shadow->offset && 
       offset + count > shadow->offset + shadow->count) {
        if(snd_shadow_extend(shadow, shadow->offset + shadow->count, 
                             ((offset + count) - 
                              (shadow->offset + shadow->count))))
            return 1;
        return snd_shadow_extend(shadow, offset, shadow->offset - offset);
    }
    
    /* If the region starts inside and extends beyond what we have,
       adjust offset & count so we just take the last part. */

    if(offset > shadow->offset && 
       offset < shadow->offset + shadow->count && 
       offset + count > shadow->offset + shadow->count) {
        count -= (offset + count) - (shadow->offset + shadow->count);
        offset = shadow->offset + shadow->count;
    }

    /* If the region starts before and extends over what we have,
       decrease the count. */

    if(offset < shadow->offset && 
       offset + count <= shadow->offset + shadow->count) 
        count = shadow->offset - offset;
    
    region = snd_copy(shadow->src, shadow->map, offset, count);

    if(!region) 
        return 1;
    
    source_map = snd_map_avail(region);
    target_map = shadow->map;
    err = snd_insert(shadow->dst, 
                     &target_map,
                     region, 
                     &source_map,
                     (offset < shadow->pivot ? 
                      0 : snd_frame_count(shadow->dst, target_map)));
    snd_destroy(region);

    if(err)
        return err;
    
    if(offset < shadow->offset)
        shadow->offset = offset;
    shadow->count += count;

    return 0;
}

/**
 * Destroys the shadow.
 * @param shadow The shadow.
 */

void
snd_shadow_destroy(struct snd_shadow *shadow) {
    if(shadow->dst)
        snd_destroy(shadow->dst);
    mem_free(shadow);
}


/**
 * Acquires the thread lock for reading for all tracks.
 * @param sr The sound object.
 */

void
snd_rlock_tracks(snd *sr) {
    int i;
    for(i = 0; i < sr->channels; i++)
        rwlock_rlock(&sr->tracks[i]->rwl);
}

void
snd_wlock_tracks(snd *sr) {
    int i;
    for(i = 0; i < sr->channels; i++)
        rwlock_wlock(&sr->tracks[i]->rwl);
}

void
snd_runlock_tracks(snd *sr) {
    int i;
    for(i = 0; i < sr->channels; i++)
        rwlock_runlock(&sr->tracks[i]->rwl);
}

void
snd_wunlock_tracks(snd *sr) {
    int i;
    for(i = 0; i < sr->channels; i++)
        rwlock_wunlock(&sr->tracks[i]->rwl);
}

/**
 * Moves a specified track to the specified position. The preceding
 * and subsequent tracks are moved down and up respectively, depending
 * on whether the destination is above or below the track to move.
 * If from and to are equal, nothing happens and the function returns
 * success.
 * @param sr The sound object.
 * @param from The track to move.
 * @param to Where to move the track.
 * @return 0 on success, non-zero otherwise. 
 */

int
snd_move_track(snd *sr,
               int from,
               int to) {
    int i;
    track *tmp;

    if(from < 0 || from >= sr->channels) {
        error_set(&sr->error, 
                  "Source track out of range");
        return 1;
    }

    if(to < 0 || to >= sr->channels) {
        error_set(&sr->error, 
                  "Destination track out of range");
        return 1;
    }    

    if(from == to)
        return 0;

    snd_wlock_tracks(sr);
    rwlock_wlock(&sr->rwl);
    tmp = sr->tracks[from];
    if(from > to) {
        for(i = from; i > to; i--)
            sr->tracks[i] = sr->tracks[i - 1];
    } else {
        for(i = from; i < to; i++)
            sr->tracks[i] = sr->tracks[i + 1];
    }
    sr->tracks[to] = tmp;
    rwlock_wunlock(&sr->rwl);
    snd_wunlock_tracks(sr);
    return 0;
}

/**
 * Tries to determine whether the sound object and underlying
 * data structures are consistent and not corrupted.
 * @return 0 if the sound object is certainly corrupted, non-zero
 * if it might be OK.
 */

int
snd_verify(snd *sr) {
    if(!sr) 
        return 0;

    if(sr->channels <= 0) {
        FAIL("sr->channels <= 0!\n");
        return 0;
    }

    if(sr->channels > pref_get_as_int("max_tracks")) {
        FAIL("sr->channels > max_tracks!\n");
        return 0;
    }

    if(sr->rate < 0) {
        FAIL("sr->rate < 0!\n");
        return 0;
    }

    if(sr->rate > 1000000) {
        FAIL("sr->rate > 1000000!\n");
        return 0;
    }
    
    return 1;
}

/**
 * Returns a map denoting all tracks with a non-zero sample count.
 */

track_map_t
snd_map_avail(snd *sr) {
    int i, map = 0;

    for(i = 0; i < sr->channels; i++)
        if(track_count(sr->tracks[i]))
            map |= (1 << i);

    return map;
}

/**
 * Copies the specified region and returns it as a new snd object.
 * Locks are not copied.
 * @param sr The snd to copy from.
 * @param channel_map The channels to copy.
 * @param frame_offset Where to start copying.
 * @param frame_count The number of frames to copy.
 * @return NULL on error and sets the error value for sr.
 */

snd *
snd_copy(snd *sr,
         track_map_t channel_map,
         AFframecount frame_offset,
         AFframecount frame_count) {
    snd *del_sr, *clone_sr;
    track_map_t del_map;

    if(error_thrown(ERROR(sr)))
        return NULL;
    
    rwlock_wlock(&sr->rwl);
    
    /* First delete the requested region from the sound object so that
       we can clone it. */
    
    del_sr = snd_delete(sr, channel_map, frame_offset, frame_count);
    if(error_thrown(&sr->error)) {
        error_cascade(&sr->error,
                      &sr->error,
                      "Could not delete %ld-%ld",
                      frame_offset, frame_offset + frame_count);
        rwlock_wunlock(&sr->rwl);
        return NULL;
    }

    del_map = snd_map_avail(del_sr);

    /* Clone the deleted region. */

    clone_sr = snd_clone(del_sr, (CLONE_STRUCTURE | CLONE_TRACK_DATA | 
                                  CLONE_TRACK_STRUCTURE));
    if(error_thrown(&del_sr->error)) {
        error_cascade(&sr->error, 
                      &del_sr->error,
                      "Could not copy %ld-%ld",
                      frame_offset, frame_offset + frame_count);
        if(snd_insert(sr, &channel_map, del_sr, &del_map, frame_offset)) 
            FAIL("recovery failed\n");
        snd_destroy(del_sr);
        rwlock_wunlock(&sr->rwl);
        return NULL;
    }

    /* Put the deleted region back into the sound object. */

    snd_insert(sr, &channel_map, del_sr, &del_map, frame_offset);
    if(error_thrown(&sr->error)) {
        error_cascade(&sr->error,
                      &sr->error,
                      "Could not re-insert frames, data lost");
        snd_destroy(clone_sr);
        snd_destroy(del_sr);
        rwlock_wunlock(&sr->rwl);
        return NULL;
    }

    snd_destroy(del_sr);

    rwlock_wunlock(&sr->rwl);

    /* Return the cloned region. */

    return clone_sr;
}

/**
 * Converts the given sound object to the specified type and sample rate.
 * @param sr The sound object.
 * @param sample_type The new sample type.
 * @param sample_rate The new sample rate.
 * Any errors that occur during conversion are stored in the error
 * object associated with this sound object.
 */

void
snd_convert(snd *sr,
            enum sample_type sample_type,
            float sample_rate) {
    int i, r;
    void *frame_buffer;
    snd *converted_sr;
    AFframecount frame_offset, frame_count, c;
    track *old_tracks[pref_get_as_int("max_tracks")];

    DEBUG("old rate: %f, old type: %d, new rate: %f, new type: %d\n",
          sr->rate, sr->sample_type, sample_rate, sample_type);
    rwlock_wlock(&sr->rwl);

    if(sample_type != sr->sample_type) {
        frame_buffer = mem_alloc(MAX_BLOCK_SIZE * sample_get_width(sr->sample_type));
        if(!frame_buffer) {
            error_set(&sr->error, 
                      "Unable to get buffer for conversion");
            rwlock_wunlock(&sr->rwl);
            return;
        }
        
        converted_sr = snd_clone(sr,
                                 CLONE_STRUCTURE |
                                 CLONE_TRACK_STRUCTURE);
        if(error_thrown(&sr->error)) {
            error_cascade(&sr->error, 
                          &sr->error,
                          "Unable to clone for conversion");
            mem_free(frame_buffer);
            rwlock_wunlock(&sr->rwl);
            return;
        }
        converted_sr->sample_type = sample_type;
        
        for(i = 0; i < sr->channels; i++) {
            //            DEBUG("converting track %d sample width from %d to %d\n",
            //                  i, converted_sr->tracks[i]->frame_width, sample_width);
            converted_sr->tracks[i]->sample_type = sample_type;
            frame_count = track_count(sr->tracks[i]);
            c = MIN(frame_count, MAX_BLOCK_SIZE);
            for(frame_offset = 0; frame_count; c = MIN(frame_count, 
                                                       MAX_BLOCK_SIZE)) {
                c = track_get_samples(sr->tracks[i],
                                      frame_buffer,
                                      frame_offset,
                                      c);
                r = track_insert_samples_from(converted_sr->tracks[i],
                                              sr->sample_type,
                                              frame_buffer,
                                              frame_offset,
                                              c);
                if(r) {
                    mem_free(frame_buffer);
                    snd_destroy(converted_sr);
                    error_set(&sr->error, 
                              "Unable to convert buffer");
                    rwlock_wunlock(&sr->rwl);
                    return;
                }
                
                frame_offset += c;
                frame_count -= c;
                if(c < 1) 
                    frame_count = 0;
            }
            old_tracks[i] = sr->tracks[i];
        }

        /* Steal the converted data from the converted_sr and put the 
           old data in the converted_sr so we can destroy it. */

        sr->sample_type = sample_type;
        for(i = 0; i < converted_sr->channels; i++) {
            sr->tracks[i] = converted_sr->tracks[i];
            converted_sr->tracks[i] = old_tracks[i];
        }
        
        snd_destroy(converted_sr);
        mem_free(frame_buffer);
    }
    
    if(sample_rate != sr->rate) 
        sr->rate = sample_rate;

    rwlock_wunlock(&sr->rwl);
 
    return;
}

void
snd_tracks_dump(snd *sr) {
    int i;
    //    DEBUG("'%s' claims to have %d tracks, dumping all %d:\n", 
    //          sr->name, sr->channels, prefs.invariants.max_tracks);
    
    for(i = 0; i < sr->channels; i++) {
        INFO("%2d: %p, bl->l: %p, frame_count: %ld\n", 
             i, sr->tracks[i], sr->tracks[i]->bl->l, track_count(sr->tracks[i]));    
        blocklist_dump(sr->tracks[i]->bl);
    }
}

/**
 * Deletes the tracks specified by channel_map from the specified snd and 
 * returns the deleted tracks. 
 * @param sr The sound object.
 * @param channel_map The tracks to delete.
 * @return Sound object with the deleted tracks.
 */

snd *
snd_delete_tracks(snd *sr,
                  track_map_t channel_map) {
    int i, j, num_avail = sr->channels;
    snd *del_sr;

    rwlock_wlock(&sr->rwl);

    /*
    for(i = 0; i < sr->channels; i++)
        if((1 << i) & channel_map) 
            num_deleted++;
    if(num_deleted == 0) {
        error_set(&sr->error, "Scared to resize to 0 tracks");
        rwlock_wunlock(&sr->rwl);
        return NULL;
    }
    */

    del_sr = snd_clone(sr, CLONE_STRUCTURE);
    
    if(error_thrown(&sr->error)) {
        error_cascade(&sr->error, 
                      &sr->error,
                      "Unable to clone to store deleted tracks");
        rwlock_wunlock(&sr->rwl);
        return NULL;
    }

    snd_set_name(del_sr, "deleted tracks");

    del_sr->channels = 0;
    for(i = 0; i < sr->channels; i++) {
        if((1 << i) & channel_map) {
            DEBUG("reparenting track %d\n", i);
            del_sr->tracks[del_sr->channels++] = sr->tracks[i];
            sr->tracks[i] = NULL;
        }
    }

    /* Compact tracks. */

    sr->channels = 0;

    for(i = 0; i < num_avail; i++) {
        if(sr->tracks[i]) {
            sr->channels++;
            continue;
        }

        for(j = i + 1; j < num_avail; j++) {
            if(sr->tracks[j]) {
                sr->tracks[i] = sr->tracks[j];
                sr->tracks[j] = NULL;
                sr->channels++;
                break;
            }
        }
    }

    snd_dump(sr);
    snd_dump(del_sr);

    rwlock_wunlock(&sr->rwl);
    return del_sr;
}

/**
 * Inserts the tracks specified by ins_sr into sr according to the
 * given channel_map. When the channel_map specifies more channels
 * than exist in the given ins_sr, then this function appends empty
 * tracks. 
 *
 * @param sr The snd object to insert into.
 * @param ins_sr The snd object to insert from. It may be NULL in
 * which case empty tracks will be inserted. Tracks that are inserted
 * will be removed from the snd object and any resulting gaps will be
 * closed. 
 * @param channel_map Before which channels in sr to insert the 
 * tracks.
 * @return On error, the error value for sr is set.
 * FIXME: doesn't return anything useful.
 */

void
snd_insert_tracks(snd *sr,
                  snd *ins_sr,
                  track_map_t channel_map) {
    int i, j = 0, k, l, max_tracks = pref_get_as_int("max_tracks");
    track *tr = NULL;
    snd backup_sr = *sr, backup_ins_sr;

    DEBUG("before:\n");
    snd_dump(sr);
    if(ins_sr)
        snd_dump(ins_sr);

    if(ins_sr)
        backup_ins_sr = *ins_sr;

    if(error_thrown(&sr->error))
        return;

    rwlock_wlock(&sr->rwl);

    /* Count number of tracks that is requested to be inserted. */

    for(i = 0; i < max_tracks; i++)
        if((1 << i) & channel_map)
            j++;

    if(sr->channels + j > max_tracks) {
        error_set(ERROR(sr),
                  "Cannot insert/append %d tracks because this would "
                  "exceed the maximum number of tracks", j);
        rwlock_wunlock(&sr->rwl);
        return;
    }

    j = 0;

    for(i = 0; i < max_tracks; i++) {
        if((1 << i) & channel_map) { 
            
            /* Use a track from ins_sr if one is available, otherwise
               create a new track. */
            
            if(ins_sr && j < ins_sr->channels) {

                /* FIXME: if the function fails after doing this once,
                   then the source object is left in a broken state. */

                tr = ins_sr->tracks[j];
                ins_sr->tracks[j] = NULL;
                j++;

            } else {

                tr = track_new(sr->sample_type);
                if(!tr) {

                    /* FIXME: error return but already inserted 
                       tracks remain in the sr, possibly inconsistent. */

                    error_set(&sr->error, 
                                  "Could not create new empty track");
                    *sr = backup_sr;
                    if(ins_sr)
                        *ins_sr = backup_ins_sr;
                    rwlock_wunlock(&sr->rwl);
                    return;
                }

            }

            l = MIN(i, sr->channels);

            /* Make space for new track by moving subsequent tracks down. */
            
            for(k = max_tracks - 1; k > l; k--) 
                sr->tracks[k] = sr->tracks[k-1];
            
            sr->channels++;
            sr->tracks[l] = tr;
        }
    }

    if(ins_sr) {

        /* Compact ins_sr and count remaining tracks. */
        /* FIXME: locks */

        for(i = 0; i < max_tracks - 1; i++) {
            for(j = i; ins_sr->tracks[i] == NULL && j < max_tracks - 1; j++)
                for(k = i; k < max_tracks - 1; k++)
                    ins_sr->tracks[k] = ins_sr->tracks[k+1];
        }
        
        j = 0;
        for(i = 0; i < max_tracks; i++)
            if(ins_sr->tracks[i] != NULL)
                j++;

        ins_sr->channels = j;
    }
    
    DEBUG("after:\n");
    snd_dump(sr);
    if(ins_sr)
        snd_dump(ins_sr);

    rwlock_wunlock(&sr->rwl);
}

/**
 * Returns a copy of the given sr. The method parameter specifies
 * whether to clone just the basic sound properties, or whether to
 * also clone the tracks, or whether to also clone the track data. 
 * Locks are never cloned.
 *
 * @param sr The snd to clone.
 * @param method The method.
 * @return On error, the error value for sr is set and NULL is returned.
 */

snd *
snd_clone(snd *sr,
          snd_clone_method method) {
    int t;
    snd *copy;

    if(error_thrown(&sr->error))
        return 0;

    rwlock_rlock(&sr->rwl);

    /* Create a new snd. */

    copy = snd_new(sr->channels, sr->rate, sr->sample_type);
    
    if(!copy || error_thrown(&copy->error)) {
        if(copy)
            error_cascade(&sr->error,
                          &copy->error,
                          "Unable to clone sound object");
        else
            error_set(&sr->error,
                      "Unable to clone sound object");
        snd_destroy(copy);
        rwlock_runlock(&sr->rwl);
        return NULL;
    }

    copy->sample_type = sr->sample_type;
    copy->channels = sr->channels;
    copy->rate = sr->rate;

    if(method & CLONE_TRACK_STRUCTURE) {

        /* Clone the tracks. */

        snd_wlock_tracks(sr);

        for(t = 0; t < sr->channels; t++) {

            track_destroy(copy->tracks[t]);
            copy->tracks[t] = track_clone(sr->tracks[t], method);

            if(copy->tracks[t] == NULL) {
                error_set(&sr->error, 
                          "Unable to clone track %d", t);
                snd_destroy(copy);
                copy = NULL;
                break;
            }

        }

        snd_wunlock_tracks(sr);

    }

    rwlock_runlock(&sr->rwl);

    return copy;
}

/**
 * Inserts audio data from one snd into another at the specified position. 
 * The sample data is shared between the two snd's. The source must be 
 * destroyed when no longer needed.
 * @param target The target snd to insert into.
 * @param target_map Value-return parameter. On entry, should contain
 * a mask which specifies which tracks on the target snd to insert
 * data into. On successfull return, describes which tracks on the
 * target actually had data inserted on them. Can be NULL in which
 * case MAP_ALL is assumed.
 * @param source The source snd to insert.
 * @param source_map Value-return parameter. On entry, specifies which
 * tracks in the source are eligible for insertion. On successfull return,
 * describes which tracks in the source were actually used. Can be NULL
 * in which case MAP_ALL is assumed.
 * @return 0 on success, non-zero error code otherwise.
 *
 * Meaning of track_map:
 * e.g. 1101  means: 
 *      |||`- put first mapped track in source onto track 0 in sr.
 *      ||`-- do not put anything on track 1 in sr.
 *      |`--- put second mapped track in source onto track 2 in sr.
 *      `---- put third mapped track in source onto track 3 in sr.
 * The number of bits looked at is always smaller than sr->channels.
 * A 'mapped track' is a track in source for which the corresponding
 * bit in source_map has been set.

 * More ASCII art:
 *
 * source: | source_map: | target: | target_map:         
 * --------+-------------+---------+-------------
 * track 1 | 0  /~~~~~~~~~>track 1 | 1
 * track 2 | 1~/         | track 2 | 0
 * track 3 | 1~~~~~~~~~~~~>track 3 | 1
 *         |             | track 4 | 1            
 *
 */

/* Used to store information needed for recovery. */

struct snd_insert_log {
    int target;
    int source;
    AFframecount count;
};

int
snd_insert(snd *target,
           track_map_t *target_map,
           snd *source,
           track_map_t *source_map,
           AFframecount offset) {
    gboolean recovery_failed;
    int err = 0, s = 0, t = 0, ops = 0;
    track_map_t target_map_in = 0, source_map_in = 0;
    track_map_t target_map_out = 0, source_map_out = 0;
    struct snd_insert_log log[MAX_TRACKS];

    if(error_thrown(&target->error))
        return 1;

    if(error_thrown(&source->error)) {
        error_cascade(ERROR(target), ERROR(source), "Error on source");
        return 1;
    }

    if(target == source) {
        error_set(ERROR(target), "Cannot insert snd on itself");
        return 1;
    }
    
    target_map_in = (target_map == NULL ? MAP_ALL : *target_map);
    source_map_in = (source_map == NULL ? MAP_ALL : *source_map);

    rwlock_rlock(&target->rwl);
    rwlock_rlock(&source->rwl);
    snd_wlock_tracks(target);
    snd_rlock_tracks(source);

    for(t = 0; t < target->channels; t++) {
        
        /* Skip target tracks until we find one to insert on. */

        if(!((1 << t) & target_map_in)) 
            continue;
        
        /* Find a source track to insert from. */

        for(; s < source->channels; s++) 
            if((1 << s) & source_map_in)
                break;

        /* Exit if all source tracks have been exhausted. */

        if(s >= source->channels) 
            break;    

        /* Insert data from the source into the target. */

        err = track_insert(target->tracks[t],
                           source->tracks[s]->bl->l,
                           offset);

        /* If there weren't any errors, log the operation (so we can
           recover it if future operations fail) and continue. */

        if(!err) {

            target_map_out |= (1 << t);
            source_map_out |= (1 << s);
            
            log[ops].target = t;
            log[ops].source = s;
            log[ops].count = track_count(source->tracks[s]);
            ops++;
            s++;

            continue;

        }
        
        /* If we get here then the track_insert() (just above) failed
           and we try to recover by deleting what we already
           inserted. */
        
        for(; ops >= 0; ops--) {

            if(track_delete(target->tracks[log[ops].target],
                            NULL,
                            offset,
                            log[ops].count)) {

                FAIL("unable to recover on track %d\n", log[ops].target);
                recovery_failed = TRUE;

            }

        }

        error_set(&target->error, "Unable to insert on track %d%s", t,
                  (recovery_failed ? " and recovery failed, data lost" : ""));

        break;

    }

    snd_runlock_tracks(source);
    snd_wunlock_tracks(target);
    rwlock_runlock(&source->rwl);
    rwlock_runlock(&target->rwl);

    if(target_map)
        *target_map = target_map_out;
    if(source_map)
        *source_map = source_map_out;

    return err;
}

#if 0
track_map_t
snd_insert(snd *target,
           track_map_t target_map,
           snd *source,
           track_map_t source_map,
           AFframecount offset) {
    int t, u, v, i_map = 0, err = 0, recov_err = 0;
    GList *del_blks;

    if(error_thrown(&sr->error))
        return 0;

    if(sr == ins_sr) {
        error_set(ERROR(sr), "Cannot insert snd on itself");
        return 0;
    }
        
    rwlock_rlock(&sr->rwl);
    snd_wlock_tracks(sr);
    snd_rlock_tracks(ins_sr);

    for(t = 0, u = 0; t < sr->channels && u < ins_sr->channels; t++) {
        if((1 << t) & map) {
            
            /* Find next available track in ins_sr. */

    while(u < ins_sr->channels &&
                  !track_count(ins_sr->tracks[u])) u++;
            
            /* If there are no more available tracks we're done. */

            if(u == ins_sr->channels)
                break;

            err = track_insert(sr->tracks[t],
                               ins_sr->tracks[u++]->bl->l,
                               offset);

            if(err) {
                
                /* Recovery. */
                
                FAIL("insert failed\n");
                for(v = 0, u = 0; v < t && u < ins_sr->channels; v++) {
                    if((1 << v) & map) {
                        while(u < ins_sr->channels &&
                              !track_count(ins_sr->tracks[u])) u++;
                        if(track_delete(sr->tracks[v],
                                        NULL,
                                        offset,
                                        track_count(ins_sr->tracks[u]))) {
                            FAIL("unable to recover on track %d\n", v);
                            recov_err = 1;
                        }
                    }
                }
                
                error_set(&sr->error, "Unable to insert on track %d%s", t,
                          (recov_err ? 
                           " and recovery failed, data lost" : ""));
                snd_runlock_tracks(ins_sr);
                snd_wunlock_tracks(sr);
                rwlock_runlock(&sr->rwl);
                return 0;
            }

            i_map |= (1 << t);
        }
    }

    snd_runlock_tracks(ins_sr);
    snd_wunlock_tracks(sr);

    rwlock_runlock(&sr->rwl);

    return i_map;
}
#endif

/**
 * Deletes frames from the given sr and returns them. This function
 * may delete fewer frames than requested if the offset +
 * count exceeds the length of the tracks.
 * @param sr The sound to delete from.
 * @param map Which tracks to delete from.
 * @param offset Where to start deleting.
 * @param count How much frames to delete.
 * @return NULL on error, the deleted frames otherwise.
 */

snd *
snd_delete(snd *sr,
           track_map_t map,
           AFframecount offset,
           AFframecount count) {
    int t, u, recov_err = 0;
    GList *del_blks;
    snd *del_sr;

    if(error_thrown(&sr->error))
        return NULL;

    rwlock_rlock(&sr->rwl);

    del_sr = snd_clone(sr, CLONE_STRUCTURE | CLONE_TRACK_STRUCTURE);

    if(error_thrown(&sr->error)) {
        error_cascade(&sr->error,
                      &sr->error,
                      "Unable to clone for deleted frames (%s)");
        snd_destroy(del_sr);
        rwlock_runlock(&sr->rwl);
        return NULL;
    }

    snd_set_name(del_sr, "deleted frames");

    snd_wlock_tracks(sr);

    for(t = 0; t < sr->channels; t++) {
        del_blks = NULL;
        if(((1 << t) & map) &&
           offset < track_count(sr->tracks[t])) {
            if(track_delete(sr->tracks[t],
                            &del_blks,
                            offset,
                            count)) {
                FAIL("error deleting on track %d\n", t);
                for(u = 0; u < t; u++) {
                    if((1 << u) & map) {
                        if(track_insert(sr->tracks[u],
                                        del_sr->tracks[u]->bl->l,
                                        offset)) {
                            FAIL("unable to recover on track %d\n", u);
                            recov_err = 1;
                        }
                    }
                }
                error_set(ERROR(sr),
                          "Unable to delete %ld samples on track %d%s", 
                          count, t, 
                          (recov_err ? " and recovery failed, data lost" :
                           ""));
                snd_destroy(del_sr);
                snd_wunlock_tracks(sr);
                rwlock_runlock(&sr->rwl);
                return NULL;
            }
        }
        blocklist_blocks_set(del_sr->tracks[t]->bl, del_blks);
    }
    
    snd_wunlock_tracks(sr);
    rwlock_runlock(&sr->rwl);

    return del_sr;
}

/**
 * Pads the given tracks up to the given length.
 * @param sr The sound object.
 * @param channel_map The tracks to pad.
 * @param frame_count The length to pad them to.
 * @return 0 on success, non-zero otherwise.
 */

int
snd_pad(snd *sr,
        track_map_t channel_map,
        AFframecount frame_count) {
    int t;
    GList *l = NULL;
    block *b[MAX_TRACKS] = { 0 };

    if(error_thrown(&sr->error))
        return 1;

    /* Create the NULL blocks needed to do padding. */
    
    for(t = 0; t < sr->channels; t++) {
        if((1 << t) & channel_map) {
            if(track_count(sr->tracks[t]) < frame_count) {
                b[t] = block_new(CACHE_NULL, 
                                 sr->tracks[t]->sample_type,
                                 frame_count - track_count(sr->tracks[t]));

                /* Handle error. */

                if(!b[t]) {
                    error_set(ERROR(sr),
                              "Cannot create NULL block of length %ld "
                              "for track %d", frame_count - 
                              track_count(sr->tracks[t]), t);
                    for(; t > -1; t--)
                        if(b[t])
                            block_unref(b[t]);
                    return 1;
                }
            }
        }
    }

    rwlock_rlock(&sr->rwl);

    for(t = 0; t < sr->channels; t++) {
        if((1 << t) & channel_map) {
            if(track_count(sr->tracks[t]) < frame_count) {
                DEBUG("padding track %d to frame_count: %ld\n", t, 
                      frame_count);
                l = g_list_append(l, b[t]);
                track_insert(sr->tracks[t], l, track_count(sr->tracks[t]));
                block_unref(b[t]);
                l->data = NULL;
                g_list_free(l);
                l = NULL;

            }
        }
    }

    rwlock_runlock(&sr->rwl);

    return 0;
}

/**
 * Fills a region with silence.
 * @param sr The sound object.
 * @param channel_map The tracks to erase.
 * @param offset The start of the region to erase.
 * @param count The length to erase.
 * @return Previous frames in region on success or NULL on error.
 */

snd *
snd_erase(snd *sr,
          track_map_t channel_map,
          AFframecount frame_offset,
          AFframecount frame_count) {
    int t;
    snd *del_sr;
    GList *l = NULL;
    block *b[MAX_TRACKS] = { 0 };

    if(error_thrown(&sr->error))
        return NULL;

    /* Create the NULL blocks needed to do padding. */
    
    for(t = 0; t < sr->channels; t++) {
        if((1 << t) & channel_map) {
            b[t] = block_new(CACHE_NULL, 
                             sr->tracks[t]->sample_type,
                             frame_count);
            
            /* Handle error. */
            
            if(!b[t]) {
                error_set(ERROR(sr),
                          "Cannot create NULL block of length %ld "
                          "for track %d", frame_count, t);
                for(; t > -1; t--)
                    if(b[t])
                        block_unref(b[t]);
                return NULL;
            }
        }
    }

    rwlock_rlock(&sr->rwl);
    snd_wlock_tracks(sr);

    if(snd_pad(sr, channel_map, frame_offset + frame_count)) {
        error_cascade(ERROR(sr),
                      ERROR(sr),
                      "Cannot pad to length %ld", frame_offset + frame_count);
        snd_wunlock_tracks(sr);
        rwlock_rlock(&sr->rwl);
        return NULL;
    }

    del_sr = snd_delete(sr, channel_map, frame_offset, frame_count);

    if(error_thrown(ERROR(sr))) {
        error_cascade(ERROR(sr),
                      ERROR(sr),
                      "Cannot delete %ld frames at %ld", 
                      frame_count, frame_offset);
        snd_wunlock_tracks(sr);
        rwlock_runlock(&sr->rwl);
        return NULL;
    }

    for(t = 0; t < sr->channels; t++) {
        if((1 << t) & channel_map) {
            l = g_list_append(l, b[t]);
            track_insert(sr->tracks[t], l, frame_offset);
            block_unref(b[t]);
            g_list_free(l);
            l = NULL;
        }
    }

    snd_wunlock_tracks(sr);
    rwlock_runlock(&sr->rwl);
    return del_sr;
}

int
snd_track_count(snd *sr) {
    return sr->channels;
}

/**
 * Returns the largest amount of frames in the tracks of the sound object.
 * @param sr The sound object.
 * @param map Which tracks to count.
 * @return Amount of frames in largest track.
 */

AFframecount 
snd_frame_count(snd *sr,
                track_map_t map) {
    int t;
    AFframecount s, m = 0;
    rwlock_rlock(&sr->rwl);
    for(t = 0; t < sr->channels; t++) {        
        if(map == MAP_ALL || (map & (1 << t))) {
            s = track_count(sr->tracks[t]);
            if(s > m) 
                m = s;
        }
    }
    rwlock_runlock(&sr->rwl);
    return m;
}

double 
snd_frames_to_time(const snd *sr, AFframecount c) {
    return ((double) c / sr->rate);
}

void
snd_info_dump(snd *sr) {
    snd_dump(sr);
}

void
snd_set_name(snd *sr,
             const char *name) {
    char *p;

    if(!name) {
        error_set(&sr->error, 
                  "name == NULL\n");
        return;
    }

    p = strdup(name);

    if(p == NULL) {
        error_set(&sr->error, 
                  RESULT_ERROR_NOMEM);
        return;
    }

    if(sr->namep)
        mem_free(sr->namep);

    sr->namep = p;
}

/**
 * Retrieves audio data into the fb_targets pointer array.
 *
 * @param sr The snd to get the audio from.
 * @param fb_targets A pointer array, one for each track, to store the
 * audio in. Each target must be big enough to hold at least frame_count 
 * frames.
 * @param channel_map Which channels to get; the others receive silence.
 * @param frame_offset Position to get samples from.
 * @param frame_count How many frames to get.
 * @return Largest number of frames actually retrieved.
 */

AFframecount
snd_getn(snd *sr,
         void * const fb_targets[],
         track_map_t channel_map,
         AFframecount frame_offset,
         AFframecount frame_count) {
    AFframecount high = 0, r;
    int i;

    if(error_thrown(&sr->error))
        return 0;

    rwlock_rlock(&sr->rwl);

    for(i = 0; i < sr->channels; i++) {
        memset(fb_targets[i], '\0', frame_count * 
               sample_get_width(sr->sample_type));
        if((1 << i) & channel_map) {
            r = track_get_samples(sr->tracks[i],
                                  fb_targets[i],
                                  frame_offset,
                                  frame_count);
            if(r > high)
                high = r;
        }
    }

    rwlock_runlock(&sr->rwl);
    
    return high;
}

/**
 * Retrieves interleaved audio data into the fb_i array.
 * Wrapper around snd_getn().
 * 
 * @param fb_n Buffers needed get non-interleaved data before
 * interleaving it.
 */

AFframecount
snd_geti(snd *sr,
         void *fb_i,
         void *fb_n[],
         track_map_t channel_map,
         AFframecount frame_offset,
         AFframecount frame_count) {
    AFframecount r;
    r = snd_getn(sr, fb_n, channel_map, frame_offset, frame_count);
    sample_interleave(sr->sample_type, fb_i, fb_n, sr->channels, 0, r);
    return r;
}

/**
 * Stores the data referenced by the fb_sources pointer array into the
 * sr. The insert_map bitmap maps channels from fb_sources to tracks
 * in the sr object. I.e. if insert_map is 1011, then the mapping
 * (source channel -> target track) is: (1 -> 1), (2 -> 2), (3 -> 4).
 * 
 * @param sr The sound object.
 * @param fb_sources Pointer array with source data.
 * @param channels Number of channels in fb_sources.
 * @param insert_map Bitmap specifying on which tracks in sr the data
 * should appear.
 * @param frame_offset Position to insert at.
 * @param frame_count Number of frames in fb_sources.
 * @return On success, 0 is returned. Otherwise a non-zero error code.
 */

int
snd_putn(snd *sr,
         const void * const fb_sources[],
         int channels,
         track_map_t insert_map,
         AFframecount frame_offset,
         AFframecount frame_count) {
    int i, t;
    
    if(error_thrown(&sr->error))
        return 1;
    
    rwlock_rlock(&sr->rwl);

    for(i = 0, t = 0; i < sr->channels && t < channels; i++) {
        if((1 << i) & insert_map) {
            if(track_insert_samples(sr->tracks[i],
                                    fb_sources[t++],
                                    frame_offset,
                                    frame_count)) {
                /* 
                 * FIXME: delete everything inserted up until this
                 * point and return 1. 
                 */
                FAIL("error trying to put %ld frames into track %d at %ld\n",
                     frame_count, i, frame_offset);
            }
        }
    }
    rwlock_runlock(&sr->rwl);

    return 0;
}

/**
 * Stores interleaved audio data into sr.
 * Wrapper around snd_putn().
 * 
 * @param fb_n Buffers needed to temporarily store non-interleaved data.
 */

int
snd_puti(snd *sr,
         const void *fb_i,
         void * const fb_n[],
         int channels,
         track_map_t insert_map,
         AFframecount frame_offset,
         AFframecount frame_count) {
    int i, t;
    for(i = 0, t = 0; i < sr->channels && t < channels; i++) 
        if((1 << i) & insert_map) 
            sample_deinterleave(sr->sample_type, fb_i, fb_n[t++], i, channels, frame_count);
    return snd_putn(sr, (const void **)fb_n, channels, insert_map, frame_offset, frame_count);
}

void
snd_dump(snd *sr) {
    int i;
    DEBUG("[ name: %s, tracks: %d, rate: %f, sample_type: %s, frame_count: %ld, tracks: ", sr->namep, sr->channels, sr->rate, sample_get_description(sr->sample_type), snd_frame_count(sr, MAP_ALL));

    for(i = 0; i < sr->channels; i++) {
        printf("[ %d, length: %ld ]", i + 1, track_count(sr->tracks[i]));
        if(i + 1 < sr->channels)
            printf(", ");
    }

    printf("]\n");
}

/**
 * Deletes this sound object and releases all resources associated
 * with it. It is harmless to supply a NULL argument.
 */

void 
snd_destroy(snd *sr) {
    int i;

    if(!sr) {
        DEBUG("sr == NULL\n");
        return;
    }

    error_free(&sr->error);

    //    DEBUG("destroying '%s'\n", sr->name);

    for(i = 0; i < MAX_TRACKS; i++) 
        if(sr->tracks[i])
            track_destroy(sr->tracks[i]);
    
    rwlock_destroy(&sr->rwl);
    if(sr->namep)
        mem_free(sr->namep);
    mem_free(sr);
}

snd *
snd_new(int tracks,
        float rate,
        enum sample_type sample_type) {
    int i;
    snd *sr = mem_calloc(sizeof(snd), 1);

    if(!sr)
        return NULL;

    rwlock_init(&sr->rwl);
    sr->channels = tracks;
    sr->rate = rate;
    sr->sample_type = sample_type;
    sr->time = 0;

    //    DEBUG("set rate %f, type: %d\n", sr->rate, sr->sample_type);

    for(i = 0; i < MAX_TRACKS; i++)
        sr->tracks[i] = NULL;

    /* Reserve & setup memory. */

    for(i = 0; i < sr->channels; i++) {
        sr->tracks[i] = track_new(sr->sample_type);
        if(sr->tracks[i] == NULL) {
            snd_destroy(sr);
            return NULL;
        }
    }

    error_init(&sr->error);

    return sr;
}

