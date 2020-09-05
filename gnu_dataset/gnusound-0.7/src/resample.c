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
 * Interface to resampling routines. This file may just be a
 * little bit messy.
 */

#include <config.h>
#include <math.h>
#include <audiofile.h>
#include <glib.h>
#include "mem.h"
#include "shell.h"
#include "resample.h"
#include "iterator.h"

#ifdef HAVE_SAMPLERATE
#include <samplerate.h>
#endif

#define RESAMPLE_INT_ZOH_DESCRIPTION "A very bad (but very fast) resampling algorithm. You are better off installing SRC: http://www.mega-nerd.com/SRC/";

const char *
resample_algo_name(int algo) {
#ifdef HAVE_SAMPLERATE
    if(src_get_name(algo) == NULL && src_get_name(algo-1))
        return "Integer ZOH";
    return src_get_name(algo);
#else
    if(algo == 0) 
        return "Integer ZOH";
    return NULL;
#endif
}

const char *
resample_algo_description(int algo) {
#ifdef HAVE_SAMPLERATE
    if(src_get_description(algo) == NULL && src_get_description(algo-1))
        return RESAMPLE_INT_ZOH_DESCRIPTION;
    return src_get_description(algo);
#else
    if(algo == 0) 
        return RESAMPLE_INT_ZOH_DESCRIPTION;
    return NULL;
#endif
}

AFframecount
#ifdef HAVE_SAMPLERATE
resample_int_zoh(shell *shl,
#else
resample_track(shell *shl,
#endif
               int track,
               AFframecount start_offset,
               AFframecount end_offset,
               AFframecount new_frame_count,
               int algo,
               enum resample_flags flags,
               struct filter_stats *stats) {
    int32_t *out_buffer;
    double j = 0, factor = (double)(end_offset - start_offset) / (double)new_frame_count,
        adjusted_factor = factor, slope_value;
    GList *del_blks;
    AFframecount out_buf_count = ceil((1.0f / (factor / 2)) * EFFECT_BUF_SIZE);
    AFframecount out_buf_count_iteration = 0, total_frames_written = 0;
    
    ITERATOR_INIT(start_offset, end_offset - start_offset);

    gui_alert("This resampling algorithm is broken.\n"
              "Please install SRC: http://www.mega-nerd.com/SRC\n"
              "Then recompile GNUsound.");
    ITERATOR_EXIT();
    return 0;

    DEBUG("algo: %d (zoh), start_offset: %ld, end_offset: %ld, new_frame_count: %ld, factor: %f\n", algo, start_offset, end_offset, new_frame_count, factor);
    out_buffer = mem_calloc(1, out_buf_count * sizeof(int32_t));
    if(!out_buffer) {
        FAIL("not enough memory for resample buffer (%ld bytes)\n",
             out_buf_count * sizeof(int32_t));
        ITERATOR_EXIT();
        return end_offset - start_offset;
    }
    
    ITERATOR(shl, shl->clip->sr->tracks[track], 
             for(out_buf_count_iteration = 0, j = 0; floor(j) < iter_read;
                 out_buf_count_iteration++, j += adjusted_factor) {
                 out_buffer[out_buf_count_iteration] = int32_frame_bits[(int)floor(j)];
                 if((flags & RESAMPLE_HONOR_ENVELOPES) &&
                    (shl->clip->markers->lists[track]->marker_types_enabled & MARKER_SLOPE)) {
                     slope_value = 
                         marker_list_slope_value(shl->clip->markers->lists[track],
                                                 (iter_frame_offset + 
                                                  out_buf_count_iteration),
                                                 MARKER_SLOPE); 
                     adjusted_factor = 
                         (slope_value < 0 ? 
                          factor - ((factor * -slope_value) / 2) :
                          factor + (factor * slope_value));
                     
                 }
             }
             track_delete(shl->clip->sr->tracks[track],
                          &del_blks,
                          iter_frame_offset,
                          iter_read);
             blocklist_blocks_destroy(del_blks);

             if((flags & RESAMPLE_GUARANTEE_NEW_FRAME_COUNT) && 
                total_frames_written + out_buf_count_iteration > new_frame_count) {
                 DEBUG("clamping, %ld frames lost\n", 
                       (total_frames_written + out_buf_count_iteration) - new_frame_count);
                 out_buf_count_iteration -= 
                     (total_frames_written + out_buf_count_iteration) - new_frame_count;
             }
             track_get_samples_as(shl->clip->sr->tracks[track],
                                  SAMPLE_TYPE_INT_32,
                                  out_buffer,
                                  iter_frame_offset,
                                  out_buf_count_iteration);
             iter_written = out_buf_count_iteration;
             total_frames_written += iter_written);
    
    if((flags & RESAMPLE_GUARANTEE_NEW_FRAME_COUNT) &&
       total_frames_written < new_frame_count) {
        DEBUG("padding, %ld frames inserted\n",
              new_frame_count - total_frames_written);
        track_insert_silence(shl->clip->sr->tracks[track],
                          iter_frame_offset,
                          new_frame_count - total_frames_written);
        total_frames_written += (new_frame_count - total_frames_written);
    }
    free(out_buffer);
    ITERATOR_EXIT();
    return total_frames_written;
}

#ifdef HAVE_SAMPLERATE
AFframecount
resample_track(shell *shl,
               int track,
               AFframecount start_offset,
               AFframecount end_offset,
               AFframecount new_frame_count,
               int algo,
               enum resample_flags flags,
               struct filter_stats *stats) {
    float *out_buffer;
    double factor = (double)(end_offset - start_offset) / (double)new_frame_count,
        adjusted_factor = factor, slope_value;
    AFframecount out_buf_count = ceil((1.0f / (factor / 2)) * EFFECT_BUF_SIZE);
    AFframecount out_buf_count_iteration = 0, in_buf_count_iteration = 0, total_frames_written = 0;
    struct marker *marker;
    int err;
    GList *del_blks;
    SRC_STATE *src_state;
    SRC_DATA src_data;
    ITERATORF_INIT(start_offset, end_offset - start_offset);

    if(src_get_name(algo) == NULL && src_get_name(algo-1)) {
        ITERATORF_EXIT();
        return resample_int_zoh(shl, 
                                track,
                                start_offset,
                                end_offset,
                                new_frame_count, 
                                algo,
                                flags,
                                stats);
    }

    DEBUG("algo: %d (%s), start_offset: %ld, end_offset: %ld, new_frame_count: %ld, factor: %f\n", algo, src_get_name(algo), start_offset, end_offset, new_frame_count, factor);

    src_state = src_new(algo, 1, &err);
    if(!src_state) {
        FAIL("cannot initialize sample rate converter (algorithm: %d): %s\n",
             algo, src_strerror(err));
        ITERATORF_EXIT();
        return end_offset - start_offset;
    }

    out_buffer = mem_calloc(1, out_buf_count * sizeof(float));
    if(!out_buffer) {
        FAIL("not enough memory for resample buffer (%ld bytes)\n",
             out_buf_count * sizeof(float));
        src_delete(src_state) ;
        ITERATORF_EXIT();
        return end_offset - start_offset;
    }
    src_data.end_of_input = 0;
    ITERATORF(shl, shl->clip->sr->tracks[track], 
              if(!(flags & RESAMPLE_HONOR_ENVELOPES) ||
                 !(shl->clip->markers->lists[track]->marker_types_enabled & MARKER_SLOPE)) {

                  /* Envelopes are disabled, that makes the entire
                     process very simple. */

                  DEBUG("envelope disabled or not honored\n");
                  src_data.end_of_input = iter_is_last_iteration;
                  src_data.input_frames = iter_read;
                  src_data.output_frames = out_buf_count;
                  src_data.data_in = float_frame_bits;
                  src_data.data_out = out_buffer;
                  src_data.src_ratio = 1 / adjusted_factor;
                  if((err = src_process(src_state, &src_data))) {
                      FAIL("samplerate returned error: %s\n",
                           src_strerror(err));
                      ITERATOR_ESCAPE();
                      break;
                  }
                  out_buf_count_iteration = src_data.output_frames_gen;
                  DEBUG("input_frames_used: %ld, output_frames_gen: %ld\n", 
                        src_data.input_frames_used, src_data.output_frames_gen);
              } else {

                  /* Envelopes are enabled, so we need to split up the
                     job along envelope slope change boundaries. We do
                     this by splitting it into a head, a body, and a
                     tail. */

                  in_buf_count_iteration = out_buf_count_iteration = 0;

                  /* Head. */

                  slope_value = marker_list_slope_value(shl->clip->markers->lists[track],
                                                        iter_frame_offset,
                                                        MARKER_SLOPE);
                  src_data.input_frames = 1;
                  src_data.output_frames = out_buf_count;
                  src_data.data_in = &float_frame_bits[in_buf_count_iteration];
                  src_data.data_out = &out_buffer[out_buf_count_iteration];
                  adjusted_factor = (slope_value < 0 ? 
                                     factor - ((factor * -slope_value) / 2) :
                                     factor + (factor * slope_value));
                  src_data.src_ratio = 1 / adjusted_factor;
                  if((err = src_process(src_state, &src_data))) {
                      FAIL("samplerate returned error: %s\n",
                           src_strerror(err));
                      ITERATOR_ESCAPE();
                      break;
                  }
                  out_buf_count_iteration += src_data.output_frames_gen;
                  //                  DEBUG("head: input_frames_used: %ld, output_frames_gen: %ld\n", 
                  //                  src_data.input_frames_used, src_data.output_frames_gen);
                  in_buf_count_iteration = 1;

                  /* Body. */
                  
                  for(marker = marker_list_next(shl->clip->markers->lists[track],
                                                iter_frame_offset,
                                                MARKER_SLOPE);
                      marker && marker->frame_offset < iter_frame_offset + iter_read;
                      marker = marker_list_next(shl->clip->markers->lists[track],
                                                iter_frame_offset + in_buf_count_iteration + 1,
                                                MARKER_SLOPE)) {
                      src_data.input_frames = marker->frame_offset - 
                          (iter_frame_offset + in_buf_count_iteration);
                      src_data.output_frames = out_buf_count - out_buf_count_iteration;
                      src_data.data_in = &float_frame_bits[in_buf_count_iteration];
                      src_data.data_out = &out_buffer[out_buf_count_iteration];
                      slope_value = marker->multiplier;
                      adjusted_factor = (slope_value < 0 ? 
                                         factor - ((factor * -slope_value) / 2) :
                                         factor + (factor * slope_value));
                      
                      src_data.src_ratio = 1 / adjusted_factor;
                      if((err = src_process(src_state, &src_data))) {
                          FAIL("samplerate returned error: %s\n",
                               src_strerror(err));
                          ITERATOR_ESCAPE();
                          break;
                      }
                      out_buf_count_iteration += src_data.output_frames_gen;
                      //                      DEBUG("body: input_frames_used: %ld, output_frames_gen: %ld\n", 
                      //                            src_data.input_frames_used, src_data.output_frames_gen);
                      in_buf_count_iteration = marker->frame_offset - iter_frame_offset;
                  }
                  
                  if(iter_escape) 
                      break;
                  
                  /* Tail. */
                  
                  slope_value = marker_list_slope_value(shl->clip->markers->lists[track],
                                                        iter_frame_offset + iter_read,
                                                        MARKER_SLOPE);
                  src_data.input_frames = iter_read - in_buf_count_iteration;
                  src_data.output_frames = out_buf_count - out_buf_count_iteration;
                  src_data.data_in = &float_frame_bits[in_buf_count_iteration];
                  src_data.data_out = &out_buffer[out_buf_count_iteration];
                  adjusted_factor = (slope_value < 0 ? 
                                     factor - ((factor * -slope_value) / 2) :
                                     factor + (factor * slope_value));
                  src_data.src_ratio = 1 / adjusted_factor;
                  src_data.end_of_input = iter_is_last_iteration;
                  if((err = src_process(src_state, &src_data))) {
                      FAIL("samplerate returned error: %s\n",
                           src_strerror(err));
                      ITERATOR_ESCAPE();
                      break;
                  }
                  out_buf_count_iteration += src_data.output_frames_gen;
                  //                  DEBUG("tail: input_frames_used: %ld, output_frames_gen: %ld\n", 
                  //                        src_data.input_frames_used, src_data.output_frames_gen);
                  in_buf_count_iteration = marker->frame_offset - iter_frame_offset;
              }
              track_delete(shl->clip->sr->tracks[track],
                           &del_blks,
                           iter_frame_offset,
                           iter_read);
              blocklist_blocks_destroy(del_blks);              
              if((flags & RESAMPLE_GUARANTEE_NEW_FRAME_COUNT) && 
                 total_frames_written + out_buf_count_iteration > new_frame_count) {
                  DEBUG("clamping, %ld frames lost\n", 
                        (total_frames_written + out_buf_count_iteration) - new_frame_count);
                  out_buf_count_iteration -= 
                      (total_frames_written + out_buf_count_iteration) - new_frame_count;
              }
              track_insert_samples_from(shl->clip->sr->tracks[track],
                                        SAMPLE_TYPE_FLOAT_32,
                                        out_buffer,
                                        iter_frame_offset,
                                        out_buf_count_iteration);
              iter_written = out_buf_count_iteration;
              total_frames_written += iter_written);
    
    if((flags & RESAMPLE_GUARANTEE_NEW_FRAME_COUNT) && 
       total_frames_written < new_frame_count) {
        DEBUG("padding, %ld frames inserted\n",
              new_frame_count - total_frames_written);
        track_insert_silence(shl->clip->sr->tracks[track],
                             iter_frame_offset,
                             new_frame_count - total_frames_written);
        total_frames_written += new_frame_count - total_frames_written;
    }
    src_delete(src_state);
    free(out_buffer);
    ITERATORF_EXIT();

    if(stats) {
        stats->produced = total_frames_written;
        stats->consumed = iter_frames_processed;
    }

    return total_frames_written;
}

#endif /* HAVE_SAMPLERATE */

