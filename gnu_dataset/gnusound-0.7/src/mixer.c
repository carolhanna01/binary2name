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
#include <math.h>
#include <errno.h>
#include <stdlib.h>
#include <ctype.h>
#include <sys/types.h>
#include <sys/stat.h>
#include <unistd.h>
#include <fcntl.h>
#include <audiofile.h>
#include <gnome.h>
#include "mem.h"
#include "sample.h"
#include "snd.h"
#include "mixer.h"

/**
 * Returns true when the channel plays solo.
 */
int
mixer_is_source_solo(mixer *m,
                     unsigned int source_channel) {
    if(source_channel >= m->source_tracks) {
        FAIL("cannot get source solo: source_channel %d out of bounds\n",
             source_channel);
        return 0;
    }
    return m->source_is_solo[source_channel] ? 1 : 0;
}

/**
 * Returns true when the channel is muted.
 */
int
mixer_is_source_mute(mixer *m,
                     unsigned int source_channel) {
    if(source_channel >= m->source_tracks) {
        FAIL("cannot get source solo: source_channel %d out of bounds\n",
             source_channel);
        return 0;
    }
    return m->source_is_mute[source_channel] ? 0 : 1;
}

/**
 * Toggles solo on a channel.
 */

void
mixer_toggle_source_solo(mixer *m,
                         unsigned int source_channel) {
    if(source_channel >= m->source_tracks) {
        FAIL("cannot set source solo: source_channel %d out of bounds\n",
             source_channel);
        return;
    }
    
    m->source_is_solo[source_channel] = ~m->source_is_solo[source_channel];
    DEBUG("source_channel: %d, new solo mask: %x\n", 
          source_channel, m->source_is_solo[source_channel]);
    m->num_solos += (m->source_is_solo[source_channel] ? 1 : -1);
}

/**
 * Mutes/unmutes a channel.
 */
void
mixer_toggle_source_mute(mixer *m,
                         unsigned int source_channel) {
    if(source_channel >= m->source_tracks) {
        FAIL("cannot set source mute: source_channel %d out of bounds\n",
             source_channel);
        return;
    }
    DEBUG("source_channel: %d, new mute mask: %x\n", 
          source_channel, m->source_is_mute[source_channel]);
    m->source_is_mute[source_channel] = ~m->source_is_mute[source_channel];
}

/**
 * Interleaves samples from the fb_sources buffer array onto the
 * fb_target buffer. The number of channels in the fb_sources and
 * fb_target channels is determined by the mixer output_channels
 * parameter. This is really a special case of the more general
 * mixer_mixi() function below. Used in file save.
 */

#define MIXI_UNITY(type) \
  for(i = 0; i < frame_count; i++) { \
      mul = i * m->output_channels; \
      for(j = 0; j < m->output_channels; j++) \
          ((type *)fb_target)[mul + j] = ((type **)fb_sources)[j][i]; \
  }

void
mixer_mixi_unity(mixer *m,
                 enum sample_type sample_type,
                 void *fb_target,
                 void *fb_sources[],
                 AFframecount frame_count) {
    register int i, j, mul;

    switch(sample_type) {
    case SAMPLE_TYPE_INT_8:
        MIXI_UNITY(int8_t);
        break;
    case SAMPLE_TYPE_INT_16:
        MIXI_UNITY(int16_t);
        break;
    case SAMPLE_TYPE_INT_32:
        MIXI_UNITY(int32_t);
        break;
    case SAMPLE_TYPE_FLOAT_32:
        MIXI_UNITY(float);
        break;
    }

}

#undef USE_OUTPUT_PEAKS
#ifdef USE_OUTPUT_PEAKS

/**
 * Mixes and interleaves the sources to the target buffer.
 * 
 * @param m The mixer object.
 * @param sample_type The type of the samples in the buffers.
 * @param fb_target The target buffer,must be big enough
 * to hold m->output_channels channels.
 * @param fb_sources Pointer array of input channels, must contain
 * m->source_tracks channels.
 * @param frame_count The number of frames to mix.
 */

#define MIXI(type, range)                       \
  for(i = 0; i < frame_count; i++) {            \
      for(j = 0; j < m->output_channels; j++) { \
          m->output_peaks[j] *= .9997;             \
          for(k = 0; k < m->source_tracks; k++) { \
              ((type *)fb_target)[(i * m->output_channels) + j] += \
                  (float)((int)((type **)fb_sources)[k][i] & \
                          (m->num_solos ?       \
                           m->source_is_solo[k] : \
                           m->source_is_mute[k])) * \
                  m->mixtable[j][k];            \
          } \
          current_peak = fabs((float)((type *)fb_target)[(i * m->output_channels) + j] / range); \
          if(current_peak > m->output_peaks[j]) \
              m->output_peaks[j] = current_peak; \
      } \
  }

inline void
mixer_mixi(mixer *m,
           enum sample_type sample_type,
           void *fb_target,
           void *fb_sources[],
           AFframecount frame_count) {
    int i, j, k;
    float current_peak;

    if(m->is_unity) {
        mixer_mixi_unity(m, sample_type, fb_target, fb_sources, frame_count);
        return;
    }

    memcpy(&m->output_peaks[MAX_TRACKS * 2],
           &m->output_peaks[MAX_TRACKS * 1],
           sizeof(float) * MAX_TRACKS);
    memcpy(&m->output_peaks[MAX_TRACKS * 1],
           &m->output_peaks[MAX_TRACKS],
           sizeof(float) * MAX_TRACKS);

    switch(sample_type) {
    case SAMPLE_TYPE_INT_8:
        MIXI(int8_t, 128.0f);
        break;
    case SAMPLE_TYPE_INT_16:
        MIXI(int16_t, 32768.0f);
        break;
    case SAMPLE_TYPE_INT_32:
        MIXI(int32_t, 2147483648.0f);
        break;
    case SAMPLE_TYPE_FLOAT_32:
        MIXI(float, 1.0);
        break;

    }
}

/**
 * Mixes the sources to the targets.
 * 
 * @param m The mixer object.
 * @param sample_type The type of the samples in the buffers.
 * @param fb_targets Pointer array of output channels, must be big enough
 * to hold m->output_channels channels.
 * @param fb_sources Pointer array of input channels, must contain
 * m->source_tracks channels.
 * @param frame_count The number of frames to mix.
 */

/* FIXME: don't do work for muted or silent channels. */

#define MIXN(type) \
  for(j = 0; j < m->output_channels; j++) { \
    for(k = 0; k < m->source_tracks; k++)  \
        for(i = 0; i < frame_count; i++) { \
            ((type **)fb_targets)[j][i] += \
                ((int)((type **)fb_sources)[k][i] & \
                 (m->num_solos ? \
                  m->source_is_solo[k] : \
                  m->source_is_mute[k])) * \
                m->mixtable[j][k]; \
      } \
  } \

void
mixer_mixn(mixer *m,
           enum sample_type sample_type,
           void *fb_targets[],
           void *fb_sources[],
           AFframecount frame_count) {
    AFframecount i;
    int j, k;
    switch(sample_type) {
    case SAMPLE_TYPE_INT_8:
        MIXN(int8_t);
        break;
    case SAMPLE_TYPE_INT_16:
        MIXN(int16_t);
        break;
    case SAMPLE_TYPE_INT_32:
        MIXN(int32_t);
        break;
    case SAMPLE_TYPE_FLOAT_32:
        MIXN(float);
        break;
    }
}

#else /* USE_OUTPUT_LEVELS */

/**
 * Mixes and interleaves the sources to the target buffer.
 * 
 * @param m The mixer object.
 * @param sample_type The type of the samples in the buffers.
 * @param fb_target The target buffer,must be big enough
 * to hold m->output_channels channels.
 * @param fb_sources Pointer array of input channels, must contain
 * m->source_tracks channels.
 * @param frame_count The number of frames to mix.
 */

#define MIXI(type)                              \
  for(i = 0; i < frame_count; i++) {            \
      for(j = 0; j < m->output_channels; j++) { \
          for(k = 0; k < m->source_tracks; k++) \
              ((type *)fb_target)[(i * m->output_channels) + j] += \
                  (float)((int)((type **)fb_sources)[k][i] & \
                          (m->num_solos ?       \
                           m->source_is_solo[k] : \
                           m->source_is_mute[k])) * \
                  m->mixtable[j][k];            \
      } \
  }

inline void
mixer_mixi(mixer *m,
           enum sample_type sample_type,
           void *fb_target,
           void *fb_sources[],
           AFframecount frame_count) {
    int i, j, k;

    if(m->is_unity) {
        mixer_mixi_unity(m, sample_type, fb_target, fb_sources, frame_count);
        return;
    }
    
    switch(sample_type) {
    case SAMPLE_TYPE_INT_8:
        MIXI(int8_t);
        break;
    case SAMPLE_TYPE_INT_16:
        MIXI(int16_t);
        break;
    case SAMPLE_TYPE_INT_32:
        MIXI(int32_t);
        break;
    case SAMPLE_TYPE_FLOAT_32:
        MIXI(float);
        break;

    }
}

/**
 * Mixes the sources to the targets.
 * 
 * @param m The mixer object.
 * @param sample_type The type of the samples in the buffers.
 * @param fb_targets Pointer array of output channels, must be big enough
 * to hold m->output_channels channels.
 * @param fb_sources Pointer array of input channels, must contain
 * m->source_tracks channels.
 * @param frame_count The number of frames to mix.
 */

/* FIXME: don't do work for muted or silent channels. */

#define MIXN(type) \
  for(j = 0; j < m->output_channels; j++) { \
    for(k = 0; k < m->source_tracks; k++)  \
        for(i = 0; i < frame_count; i++) { \
            ((type **)fb_targets)[j][i] += \
                ((int)((type **)fb_sources)[k][i] & \
                 (m->num_solos ? \
                  m->source_is_solo[k] : \
                  m->source_is_mute[k])) * \
                m->mixtable[j][k]; \
      } \
  } \

void
mixer_mixn(mixer *m,
           enum sample_type sample_type,
           void *fb_targets[],
           void *fb_sources[],
           AFframecount frame_count) {
    AFframecount i;
    int j, k;
    switch(sample_type) {
    case SAMPLE_TYPE_INT_8:
        MIXN(int8_t);
        break;
    case SAMPLE_TYPE_INT_16:
        MIXN(int16_t);
        break;
    case SAMPLE_TYPE_INT_32:
        MIXN(int32_t);
        break;
    case SAMPLE_TYPE_FLOAT_32:
        MIXN(float);
        break;
    }
}

#endif

void
mixer_insert_source_tracks(mixer *m,
                           mixer *src,
                           track_map_t map) {
    int i, j, k = 0, count = 0, max_tracks = MAX_TRACKS;
    for(i = 0; i < max_tracks; i++)
        if((1 << i) & map)
            count++;

    if(count + m->source_tracks >= max_tracks) {
        error_set(ERROR(m),
                  "Inserting %d tracks would exceed maximum tracks",
                  count);
        return;
    }

    /*
    if(src && src->output_channels != m->output_channels) {
        error_set(ERROR(m),
                  "Source mixer has different number of output channels "
                  "(%d) than target mixer (%d).",
                  src->output_channels, m->output_channels);
        return;
    }
    */

    if(src)
        mixer_dump(src);

    for(i = 0, k = 0; i < max_tracks - 1; i++) {
        if((1 << i) & map) {
            for(j = 0; j < m->output_channels; j++) {
                DEBUG("inserting source track %d for output channel %d\n",
                      i, j);
                memmove(&m->mixtable[j][i+1], &m->mixtable[j][i], 
                        (max_tracks - i - 1) * sizeof(m->mixtable[j][i]));
                if(src && j < src->output_channels && k < src->source_tracks)
                    m->mixtable[j][i] = src->mixtable[j][k];
            }

            /* Insert solo and mute masks. */

            memmove(&m->source_is_solo[i+1], &m->source_is_solo[i],
                    (max_tracks - i - 1) * sizeof(m->source_is_solo[i]));
            memmove(&m->source_is_mute[i+1], &m->source_is_mute[i],
                    (max_tracks - i - 1) * sizeof(m->source_is_mute[i]));

            m->source_is_solo[i] = 0;
            m->source_is_mute[i] = 0xFFFFFFFF;

            if(src && i < src->source_tracks) {
                m->source_is_solo[i] = src->source_is_solo[k];
                m->source_is_mute[i] = src->source_is_mute[k];
                
                if(m->source_is_solo[i])
                    m->num_solos++;
            }

            k++;
        }
    }
    m->source_tracks += count;
}

/* FIXME: doesn't adjust source_is_solo & source_is_mute */

mixer *
mixer_delete_source_tracks(mixer *m,
                           track_map_t map) {
    int i, j, k = 0, count = 0, max_tracks = MAX_TRACKS;
    mixer *del_mix;
    
    for(i = 0; i < max_tracks; i++)
        if((1 << i) & map)
            count++;
    
    if(m->source_tracks - count < 0) {
        error_set(ERROR(m),
                  "Cannot delete %d tracks, only %d available",
                  count, m->source_tracks);
        return NULL;
    }

    /* Create mixer to store what we're about to delete. */

    del_mix = mixer_new(m->output_channels, count);
    if(!del_mix) {
        error_set(ERROR(m), "Cannot create new mixer");
        return NULL;
    }

    /* 
     * Deleting a source track is equivalent to removing a column by
     * moving the subsequent columns to the left, ie. we overwrite the
     * tracks we need to delete.
     */

    for(i = 0, k = 0; i < max_tracks - 1; i++) {
        if((1 << i) & map) {

            /* Copy and delete the mute and solo masks. */

            del_mix->source_is_solo[k] = m->source_is_solo[i];
            del_mix->source_is_mute[k] = m->source_is_mute[i];
            if(m->source_is_solo[i]) {
                m->num_solos--;
                del_mix->num_solos++;
            }
            memmove(&m->source_is_solo[i], &m->source_is_solo[i+1],
                    (max_tracks - i - 1) * sizeof(m->source_is_solo[i]));
            memmove(&m->source_is_mute[i], &m->source_is_mute[i+1],
                    (max_tracks - i - 1) * sizeof(m->source_is_mute[i]));

            /* Loop through each row, delete a column from each. */

            for(j = 0; j < m->output_channels; j++) {
                DEBUG("deleting source track %d for output channel %d\n",
                      i, j);
                del_mix->mixtable[j][k] = m->mixtable[j][i];
                memmove(&m->mixtable[j][i], &m->mixtable[j][i+1], 
                        (max_tracks - i - 1) * sizeof(m->mixtable[j][i]));
            }

            /* 
             * Since we just deleted a track we need to shift the map
             * right so that referentiality is preserved. 
             */

            map >>= 1;
            i--;
            k++;
        }
    }
    m->source_tracks -= count;
    mixer_dump(del_mix);
    return del_mix;
}

/**
 * Deletes the specified output channels from the mixer.
 * Used in mixdown.
 * @param m The mixer.
 * @param map Mask specifying which output channels to delete.
 * @return Mixer object containing the deleted output channels,
 * or NULL on error.
 */

/* FIXME: doesn't account for output_flags & output_peaks. */

mixer *
mixer_delete_output_channels(mixer *m,
                             track_map_t map) {
    int i, j, count = 0, max_tracks = MAX_TRACKS;
    mixer *del_mix;
    float *tmp;

    for(i = 0; i < max_tracks; i++)
        if((1 << i) & map)
            count++;

    DEBUG("count: %d\n", count);

    if(m->output_channels - count < 0) {
        error_set(ERROR(m),
                  "Cannot delete %d output channels, only %d available",
                  count, m->output_channels);
        return NULL;
    }

    /* Create mixer to store what we're about to delete. */

    del_mix = mixer_new(count, m->source_tracks);
    if(!del_mix) {
        error_set(ERROR(m), "Cannot create new mixer");
        return NULL;
    }

    /* 
     * Deleting an output channel is equivalent to removing a row by
     * moving the subsequent rows up.
     */

    for(i = 0, j = 0; i < max_tracks - 1; i++) {
        if((1 << i) & map) {
            tmp = del_mix->mixtable[j];
            del_mix->mixtable[j++] = m->mixtable[i];
            memmove(&m->mixtable[i], &m->mixtable[i+1], 
                    sizeof(m->mixtable[i]) * (max_tracks - i - 1));
            m->mixtable[max_tracks - 1] = tmp;

            /* 
             * Since we just deleted a channel we need to move the map
             * right so that referentiality is preserved. 
             */

            map >>= 1;
            i--;
        }
    }
    m->output_channels -= count;
    mixer_dump(del_mix);
    return del_mix;
}

/**
 * Moves a track.
 * @param m The mixer.
 * @param from The track to move.
 * @param to Where to move the track to.
 * @return 0 on success, non-zero on error.
 */

int
mixer_move_source_track(mixer *m,
                        int from,
                        int to) {
    int i, j, max_tracks = MAX_TRACKS;
    float tmp[max_tracks];

    if(from < 0 || from >= m->source_tracks) {
        error_set(ERROR(m),
                  "Source track out of bounds");
        return 1;
    }

    if(to < 0 || to >= m->source_tracks) {
        error_set(ERROR(m),
                  "Destination track out of bounds");
        return 1;
    }

    if(from == to)
        return 0;

    /* Store the source track column because we will overwrite it. */

    for(i = 0; i < m->output_channels; i++) 
        tmp[i] = m->mixtable[i][from];

    /* Move preceding or subsequent tracks down or up respectively. */

    if(from > to) {
        for(i = 0; i < m->output_channels; i++)
            for(j = from; j > to; j--)
                m->mixtable[i][j] = m->mixtable[i][j-1];
    } else {
        for(i = 0; i < m->output_channels; i++)
            for(j = from; j < to; j++)
                m->mixtable[i][j] = m->mixtable[i][j+1];
    }

    /* Move the stored track into its destination column. */

    for(i = 0; i < m->output_channels; i++)
        m->mixtable[i][to] = tmp[i];

    return 0;
}

float **
mixer_mixtable_new(int output_channels,
                   int source_tracks) {
    int i;
    float **mt = NULL;
    
    mt = mem_alloc(output_channels * sizeof(float *));
    if(!mt) {
        FAIL("could not allocate mix table (%"CONVSPEC_SIZE_T" bytes)\n", 
             output_channels * sizeof(float));
            return NULL;
    }
    //    DEBUG("allocated %d bytes for mixtable\n",
    //          output_channels * sizeof(float));
    for(i = 0; i < output_channels; i++) {
        mt[i] = mem_alloc(source_tracks * sizeof(float));
        if(!mt[i]) {
            for(i--; i + 1 > 0; i--)
                mem_free(mt[i]);
            mem_free(mt);
            FAIL("could not allocate mix table rows\n");
            return NULL;
        }
    }
    return mt;
}

void
mixer_mixtable_destroy(float **mt,
                       int output_channels) {
    int i;
    for(i = 0; i < output_channels; i++) {
        mem_free(mt[i]);
    }
    mem_free(mt);
    //    DEBUG("freed %d channel mixtable\n", output_channels);
}

void 
mixer_dump(mixer *m) {
    int i, j;
    DEBUG("output_channels: %d, source_tracks: %d\n",
          m->output_channels, m->source_tracks);
    for(i = 0; i < m->output_channels; i++) {
        INFO("%2i:", i);
        for(j = 0; j < m->source_tracks; j++) 
            INFO(" %.3f (mute: %x)", m->mixtable[i][j], m->source_is_mute[j]);
        INFO("\n");
    }
    
}

void
mixer_init(mixer *m) {
    int i, j;
    if(!m->output_channels)
        return;
    for(i = 0; i < m->output_channels; i++) 
        for(j = 0; j < m->source_tracks; j++) 
            m->mixtable[i][j] = 0;
}

mixer *
mixer_clone(mixer *m) {
    int i, j;
    mixer *copy = mixer_new(m->output_channels, m->source_tracks);
    if(!copy)
        return NULL;
    copy->is_unity = m->is_unity;
    copy->num_solos = m->num_solos;
    for(i = 0; i < m->source_tracks; i++)
        copy->source_is_solo[i] = m->source_is_solo[i];
    for(i = 0; i < m->source_tracks; i++)
        copy->source_is_mute[i] = m->source_is_mute[i];
    for(i = 0; i < m->output_channels; i++)
        for(j = 0; j < m->source_tracks; j++) 
            copy->mixtable[i][j] = m->mixtable[i][j];
    return copy;
}

/**
 * Configures the mixer for a different number of output channels and
 * source tracks.
 */

void
mixer_configure(mixer *m,
                int output_channels,
                int source_tracks) {
    int i, j;

    if(output_channels > MAX_TRACKS ||
       source_tracks > MAX_TRACKS) {
        FAIL("illegal configure request, output_channels: %d, source_tracks: %d\n",
             output_channels, source_tracks);
        abort();
    }

    //    DEBUG("m->output_channels: %d, output_channels: %d, m->source_tracks: %d, source_tracks: %d\n",
    //          m->output_channels, output_channels, m->source_tracks, source_tracks);
    for(i = 0; i < output_channels; i++) {
        for(j = m->source_tracks; j < source_tracks; j++) {
            //            DEBUG("[%d][%d] = 0\n", i, j);
            m->mixtable[i][j] = i == j ? 1 : 0;
        }
    }
    m->output_channels = output_channels;
    m->source_tracks = source_tracks;
    //    mixer_dump(m);

//    DEBUG("output_channels: %d, source_tracks: %d\n", 
//          m->output_channels, m->source_tracks);
}

void
mixer_destroy(mixer *m) {
    DEBUG("destroying mixer\n");
    mixer_mixtable_destroy(m->mixtable, MAX_TRACKS);
    mem_free(m->source_is_mute);
    /* No need to free source_is_solo, it's a contiguous block. */
    mem_free(m->output_flags);
    mem_free(m->output_peaks);
    mem_free(m);    
}

mixer *
mixer_new(int output_channels,
          int source_tracks) {
    int *output_flags;
    float *output_peaks;
    int32_t *ptr;
    int i;
    mixer *m = mem_calloc(sizeof(mixer), 1);
    if(!m) {
        FAIL("could not allocate mixer object.\n");
        return NULL;
    }

    output_flags = mem_calloc(1, sizeof(int) * MAX_TRACKS);
    if(!output_flags) {
        FAIL("could not allocate output flags\n");
        mem_free(m);
        return NULL;
    }

    output_peaks = mem_calloc(3, sizeof(float) * MAX_TRACKS);
    if(!output_peaks) {
        FAIL("could not allocate output peaks\n");
        mem_free(output_flags);
        mem_free(m);
        return NULL;
    }

    /* Allocate once for both source_is_solo and source_is_mute
       arrays, make source_is_mute point at the beginning. */

    ptr = mem_alloc(2 * sizeof(int32_t) * MAX_TRACKS);
    if(!ptr) {
        FAIL("could not allocate source mute & solo tables.\n");
        mem_free(output_peaks);
        mem_free(output_flags);
        mem_free(m);
        return NULL;
    }

    m->mixtable = mixer_mixtable_new(MAX_TRACKS, 
                                     MAX_TRACKS);

    if(!m->mixtable) {
        FAIL("could not allocate mixtable.\n");
        mem_free(output_peaks);
        mem_free(output_flags);
        mem_free(ptr);
        mem_free(m);
        return NULL;
    }

    m->output_flags = output_flags;
    m->output_peaks = output_peaks;

    m->source_is_mute = ptr;
    m->source_is_solo = &ptr[MAX_TRACKS];
    for(i = 0; i < MAX_TRACKS; i++) {
        /* Unmute all source channels. */
        m->source_is_mute[i] = 0xFFFFFFFF;
        /* Unsolo all source channels. */
        m->source_is_solo[i] = 0;
    }
    m->num_solos = 0;
    
    mixer_configure(m, output_channels, source_tracks);
    mixer_init(m);
    error_init((struct error *)m);

    return m;
}

void 
mixer_buffers_free(int tracks,
                   void *fb_downmix,
                   void **fb_sources) {
    int i;
    for(i = 0; i < tracks; i++) {
        if(fb_sources[i])
            mem_free(fb_sources[i]);
        fb_sources[i] = NULL;
    }
    DEBUG("freed mixer buffers %p\n", fb_downmix);
    if(fb_downmix) 
        mem_free(fb_downmix);
}

void *
mixer_buffers_alloc(int frame_width,
                    int tracks,
                    void **fb_downmix,
                    void **fb_sources,
                    AFframecount frame_count) {
    int i;
    size_t fb_downmix_sz;
    
    for(i = 0; i < tracks; i++)
        fb_sources[i] = NULL;

    /*
     * We reserve some extra space in front of the buffer and fill it
     * with pointers so the buffer can be used as a pointer array as
     * well.
     */

    fb_downmix_sz = (frame_width * tracks * frame_count) + 
        sizeof(void *) * tracks;
    *fb_downmix = mem_alloc(fb_downmix_sz);
    if(!*fb_downmix) {
        FAIL("could not get memory for downmix buffer (%d tracks).\n", tracks);
        return NULL;
    }

    /* Setup pointers so fb_downmix can act as pointer array. */

    for(i = 0; i < tracks; i++)
        ((void **)(*fb_downmix))[i] = *fb_downmix + (i * frame_width * frame_count) + 
            (sizeof(void *) * tracks);

    for(i = 0; i < tracks; i++) {
        fb_sources[i] = mem_alloc(frame_width * frame_count);
        if(fb_sources[i] == NULL) {
            FAIL("could not get memory for track buffer %d\n", i);
            for(i--; i + 1 > 0; i--) {
                mem_free(fb_sources[i]);
                fb_sources[i] = NULL;
            }
            mem_free(*fb_downmix);
            *fb_downmix = NULL;
            return NULL;
        }
    }
    DEBUG("mixer buffers allocated: muxbuf: %ld bytes@%p, srcbufs: %ld bytes (* %d)\n",
          frame_width * tracks * frame_count, *fb_downmix, frame_width * frame_count, tracks);

    return *fb_downmix;
}


int
mixer_save(mixer *m,
           const char *path) {
    int i, j, argc = m->source_tracks;
    char key[512], *argv[argc];
    
    for(i = 0; i < argc; i++) {
        argv[i] = mem_alloc(32);
        if(!argv[i]) { 
            FAIL("not enough memory to save mixer, could not allocate 32 bytes for argv %d.\n", i);
            for(i--; i; i--)
                mem_free(argv[i]);
            return 1;
        }
    }

    for(i = 0; i < m->output_channels; i++) {
        snprintf(key, 512, "=%s=/Mixer/%d", path, i);
        for(j = 0; j < m->source_tracks; j++) 
            snprintf(argv[j], 32, "%f", m->mixtable[i][j]);
        gnome_config_set_vector(key, m->source_tracks, 
                                (const char * const *) argv);
    }
    
    snprintf(key, 512, "=%s=/Mixer/Source Is Mute", path);
    for(j = 0; j < m->source_tracks; j++) 
        snprintf(argv[j], 32, "%d", m->source_is_mute[j] ? 0 : 1);
    gnome_config_set_vector(key, m->source_tracks, 
                            (const char * const *) argv);

    snprintf(key, 512, "=%s=/Mixer/Source Is Solo", path);
    for(j = 0; j < m->source_tracks; j++) 
        snprintf(argv[j], 32, "%d", m->source_is_solo[j] ? 1 : 0);
    gnome_config_set_vector(key, m->source_tracks, 
                            (const char * const *) argv);
    for(i = 0; i < argc; i++)
        if(argv[i])
            mem_free(argv[i]);

    return 0;
}

int
mixer_load(mixer *m,
           const char *path) {
    unsigned int i, j, argc, r = 0;
    char **argv;
    char tmp[512];
    mixer_init(m);
    for(i = 0; i < m->output_channels; i++) {
        snprintf(tmp, 512, "=%s=/Mixer/%i", path, i);
        gnome_config_get_vector(tmp, &argc, &argv);
        for(j = 0; j < argc; j++) {
            if(j < m->source_tracks) 
                m->mixtable[i][j] = atof(argv[j]);
            else {
                r = 1;
                FAIL("source channel %d (value: %f) out of bounds.\n",
                     j, atof(argv[j]));
            }
        }
        for(j = 0; j < argc; j++) 
            g_free(argv[j]);
        g_free(argv);
    }
    
    /* "Source Is Mute" settings are stored on disk basically opposite
       from how they are stored in memory: a 1 on disk means a 0 in
       memory and a 0 on disk means 0xFFFFFFFF in memory. Reason: to
       make it easier for readers of the config file to understand. */

    snprintf(tmp, 512, "=%s=/Mixer/Source Is Mute", path);
    gnome_config_get_vector(tmp, &argc, &argv);
    for(j = 0; j < argc; j++) {
        if(j < m->source_tracks) 
            m->source_is_mute[j] = (atoi(argv[j]) ? 0 : 0xFFFFFFFF);
        else {
            r = 1;
            FAIL("is muted channel %d (value: %d) out of bounds.\n",
                 j, atoi(argv[j]));
        }
    }

    for(j = 0; j < argc; j++) 
        g_free(argv[j]);
    g_free(argv);

    /* For "Source Is Solo" it's the same story as for "Source Is
       Mute" above. */

    snprintf(tmp, 512, "=%s=/Mixer/Source Is Solo", path);
    gnome_config_get_vector(tmp, &argc, &argv);
    for(j = 0; j < argc; j++) {
        if(j < m->source_tracks) 
            m->source_is_solo[j] = (atoi(argv[j]) ? 0xFFFFFFFF : 0);
        else {
            r = 1;
            FAIL("is muted channel %d (value: %d) out of bounds.\n",
                 j, atoi(argv[j]));
        }
    }

    for(j = 0; j < argc; j++) 
        g_free(argv[j]);
    g_free(argv);
    
    return r;
}

/***********************************************************************/
/* Below code deprecated in favor of gnome_config_* (see file.c).      */
/***********************************************************************/

int
mixer_save_compat(mixer *m,
                  const char *path) {
    int i, j;
    FILE *out;
    
    out = fopen(path, "w");
    if(out == NULL) {
        DEBUG("could not open %s for writing\n", path);
        return 1;
    }

    for(i = 0; i < m->output_channels; i++) {
        fprintf(out, "%2i:", i);
        for(j = 0; j < m->source_tracks; j++) 
            fprintf(out, " %.3f", m->mixtable[i][j]);
        fprintf(out, "\n");
    }

    if(fclose(out)) {
        FAIL("error closing %s\n", path);
        return 1;
    }

    return 0;
}

/*
 * Parses a target specification line:
 * [0-9]+:( [0-9]+.[0-9]+)*
 * \____/ \_______________/
 *   |            |
 *   |            +-> source channel amplification factor(s)
 *   +-> target channel
 * where : and . stand for themselves, ( and ) denote grouping.
 * @return zero on succes.
 */
int
mixer_parse_line(mixer *m,
                 char *line) {
    char *token, *endptr;
    int target, source;
    double amp;
    while(*line != '\0' && isspace(*line)) 
        line++;
    token = strsep(&line, ":");
    //    DEBUG("token: %s, remaining: %s\n", token, line);
    if(token == NULL) {
        FAIL("parse error: expected [0-9]+: on line %s\n",
             line);
        return 1;
    }

    target = (int)strtol(token, &endptr, 10);
    //    DEBUG("target: %d\n", target);
    if(!(*token != '\0' && *endptr == '\0')) {
        FAIL("invalid token %s, expected integer\n", token);
        return 1;
    }

    if(target > m->output_channels || target < 0) {
        FAIL("target specification out of range: %d\n", target);
        return 1;
    }

    /* Now the source channels. */

    source = 0;
    while(*line != '\0' && isspace(*line)) 
        line++;
    for(token = strsep(&line, " "); token; token = strsep(&line, " "), source++) {
        //        DEBUG("token: %s, remaining: %s\n", token, line);
        amp = strtod(token, &endptr);
        if(!(*token != '\0' && *endptr == '\0')) {
            FAIL("invalid token %s, expected double\n", token);
            return 1;
        }
        while(line && *line != '\0' && isspace(*line)) 
            line++;
        if(amp > 1 || amp < 0) {
            FAIL("amplification factor out of range: %f\n", amp);
            continue;
        }
        
        //        DEBUG("target: %d, source: %d, amp: %f\n",
        //              target, source, amp);

        m->mixtable[target][source] = amp;
    }
    return 0;
}

/* 
 * @return zero on success.
 */
int
mixer_load_compat(mixer *m,
                  const char *path) {
    int fd, r;
    char *buf, *line;
    struct stat stats;

    r = stat(path, &stats);

    if(r == -1) {
        FAIL("unable to stat %s: %s\n", path, strerror(errno));
        return 1;
    }
    
    buf = mem_alloc(stats.st_size + 1);    
    if(!buf) {
        FAIL("unable to allocate %ld bytes for reading %s\n", 
             stats.st_size + 1, path);
        return 1;
    }

    fd = open(path, O_RDONLY);
    if(fd == -1) {
        FAIL("unable to open %s: %s\n", path, strerror(errno));
        mem_free(buf);
        return 1;
    }
    
    r = read(fd, buf, stats.st_size);
    if(r == -1) {
        FAIL("error while reading %s: %s\n", path, strerror(errno));
        mem_free(buf);
        close(fd);
        return 1;
    }
    close(fd);

    if(r != stats.st_size) 
        FAIL("while reading %s: expected %ld bytes, got %d bytes\n", 
             path, stats.st_size, r);

    buf[r] = '\0';
    line = strtok(buf, "\n\r");
    while(line) {
        //        DEBUG("read line: %s\n", line);
        if(line[0] == '#') {
            //            DEBUG("skipping comment\n");
            line = strtok(NULL, "\n\r");
            continue;
        }
        
        r = mixer_parse_line(m, line);
        if(r) {
            mem_free(buf);
            return r;
        }
        line = strtok(NULL, "\n\r");
    }        
    
    mem_free(buf);
    return 0;
}

