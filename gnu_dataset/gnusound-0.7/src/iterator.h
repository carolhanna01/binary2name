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
 * This file provides a few macros to facilitate the tedium of 
 * processing a track buffer by buffer.
 */

#ifndef ITERATOR_H
#define ITERATOR_H

#include <config.h>
#include <inttypes.h>
#include <gdk/gdk.h>
#include <audiofile.h>
#include "mem.h"
#include "sample.h"
#include "arbiter.h"

#define ITERATOR_GENERIC_INIT(frame_offset, frame_count) \
    AFframecount iter_read = 0, \
      iter_written = 0, \
      iter_frame_offset = (frame_offset), \
      iter_frame_count = 0, \
      iter_frames_processed = 0, \
      iter_frame_count_old = iter_frame_count = (frame_count); \
    int iter_escape = 0, iter_is_last_iteration = 0 

#define ITERATOR_INIT(frame_offset, frame_count) \
    ITERATOR_GENERIC_INIT(frame_offset, frame_count); \
    int32_t *int32_frame_bits = mem_alloc(EFFECT_BUF_SIZE * sizeof(int32_t))

#define ITERATORF_INIT(frame_offset, frame_count) \
    ITERATOR_GENERIC_INIT(frame_offset, frame_count); \
    float *float_frame_bits = mem_alloc(EFFECT_BUF_SIZE * sizeof(float))

#define ITERATOR_EXIT() \
    if(int32_frame_bits) \
      free(int32_frame_bits); 

#define ITERATORF_EXIT() \
    if(float_frame_bits) \
      free(float_frame_bits); 

#define ITERATOR_ESCAPE() \
    iter_escape = 1;

#define ITERATOR_READER(shl, track) \
    do { \
      iter_read = track_get_samples_as((track), \
                                       SAMPLE_TYPE_INT_32, \
                                       int32_frame_bits, \
                                       iter_frame_offset, \
                                       MIN(EFFECT_BUF_SIZE, \
                                       iter_frame_count)); \
      if(iter_read <= 0) { \
        ITERATOR_ESCAPE(); \
        break; \
      } \
      if(iter_read < EFFECT_BUF_SIZE) \
        iter_is_last_iteration = 1; \
      iter_written = iter_read; \
    } while(0)

#define ITERATORF_READER(shl, track) \
    do { \
      iter_read = track_get_samples_as((track), \
                                       SAMPLE_TYPE_FLOAT_32, \
                                       float_frame_bits, \
                                       iter_frame_offset, \
                                       MIN(EFFECT_BUF_SIZE, \
                                       iter_frame_count)); \
      if(iter_read <= 0) { \
        ITERATOR_ESCAPE(); \
        break; \
      } \
      if(iter_read < EFFECT_BUF_SIZE) \
        iter_is_last_iteration = 1; \
      iter_written = iter_read; \
    } while(0)

#define ITERATOR_CHECKER() \
    if(!int32_frame_bits) { \
      FAIL("failed to allocate iterator buffer\n"); \
      break; \
    }

#define ITERATORF_CHECKER() \
    if(!float_frame_bits) { \
      FAIL("failed to allocate iterator buffer\n"); \
      break; \
    }

/*
 * shl is optional. 
 */

#define ITERATOR_SKEL(shl, track, checker, reader, func) \
  do { \
    checker; \
    if((shl)) \
       if((shl)->cancel_requested) \
         break; \
    do { \
      reader; \
      if(!iter_escape) { \
        do { \
          func; \
          if((shl)) \
            view_set_progress((shl)->view, \
                              (float)iter_frames_processed / \
                              (float)(iter_frame_count_old)); \
        } while(0); \
        iter_frames_processed += iter_read; \
        iter_frame_offset += iter_written; \
        iter_frame_count -= iter_read; \
        arbiter_yield(); \
        if((shl) && (shl)->cancel_requested) \
          ITERATOR_ESCAPE(); \
      } \
    } while(!iter_escape && iter_read > 0 && iter_frame_count); \
    DEBUG("total: %ld\n", iter_frame_count_old); \
    if((shl)) \
      view_set_progress((shl)->view, 0); \
  } while(0) 

#define ITERATOR(shl, track, func) \
    ITERATOR_SKEL(shl, track, ITERATOR_CHECKER(), ITERATOR_READER(shl, track), func)

#define ITERATORF(shl, track, func) \
    ITERATOR_SKEL(shl, track, ITERATORF_CHECKER(), ITERATORF_READER(shl, track), func)

#endif /* ! ITERATOR_H */

