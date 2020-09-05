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
 * A copy of the GNU General Public License can be found in the file
 * LICENSE in the top directory of the source distribution. If not,
 * write to the Free Software * Foundation, Inc., 675 Mass Ave,
 * Cambridge, MA 02139, USA.
 *
 */

#ifndef SAMPLE_H
#define SAMPLE_H

#include <config.h>
#include <audiofile.h>

#if USE_MMX_MINMAX == 2
#define PEAK_MIN_FUNC min_int8_mmx2
#define PEAK_MAX_FUNC max_int8_mmx2
#define MIN8_FUNC min_int8_mmx2
#define MAX8_FUNC max_int8_mmx2
#define MIN16_FUNC min_int16_mmx2
#define MAX16_FUNC max_int16_mmx2
#elif USE_MMX_MINMAX == 3
#define PEAK_MIN_FUNC min_int8_3dnow
#define PEAK_MAX_FUNC max_int8_3dnow
#define MIN8_FUNC min_int8_3dnow
#define MAX8_FUNC max_int8_3dnow
#define MIN16_FUNC min_int16_3dnow
#define MAX16_FUNC max_int16_3dnow
#else
#define PEAK_MIN_FUNC peak_funcs.peak_min_func
#define PEAK_MAX_FUNC peak_funcs.peak_max_func
#define MIN8_FUNC peak_funcs.min8_func
#define MAX8_FUNC peak_funcs.max8_func
#define MIN16_FUNC peak_funcs.min16_func
#define MAX16_FUNC peak_funcs.max16_func
#endif

struct peak_func_table {
    graph_bits_unit_t (*peak_min_func)(const peak_unit_t *a, int count);
    graph_bits_unit_t (*peak_max_func)(const peak_unit_t *a, int count);
    int16_t (*min16_func)(const int16_t *a, int count);
    int16_t (*max16_func)(const int16_t *a, int count);
    int8_t (*min8_func)(const int8_t *a, int count);
    int8_t (*max8_func)(const int8_t *a, int count);
};

enum sample_type {
    SAMPLE_TYPE_INT_8,
    SAMPLE_TYPE_INT_16,
    SAMPLE_TYPE_INT_32,
    SAMPLE_TYPE_FLOAT_32
};

int
sample_init();

inline graph_bits_unit_t
sample_to_peak(enum sample_type sample_type,
               frame_bits_t frame_bits);

void
sample_calc_rms(rms_unit_t *avgs,
                enum sample_type type,
                const void *buf,
                AFframecount count,
                float scale);

void
sample_scale_peaks(peak_unit_t *lows,
                   peak_unit_t *highs,
                   const peak_unit_t *src_lows,
                   const peak_unit_t *src_highs,
                   AFframecount count,
                   float scale);

void 
sample_scale_samples(peak_unit_t *lows,
                     peak_unit_t *highs,
                     enum sample_type sample_type,
                     const void *buf,
                     AFframecount count,
                     float scale);

const char *
sample_get_description(enum sample_type type);    

int
sample_get_width(enum sample_type type);

void
sample_convert(enum sample_type src_type,
               enum sample_type dst_type,
               const void *src,
               void *dst,
               AFframecount frame_count);

void
sample_interleave(enum sample_type sample_type,
                  void *fb_i,
                  void *fb_n[],
                  int channels,
                  track_map_t skip_channel_map,
                  AFframecount frame_count);

void
sample_deinterleave(enum sample_type sample_type,
                    const void *fb_i,
                    void *fb_n,
                    int channel,
                    int channels,
                    AFframecount frame_count);

void
sample_deinterleave_all(enum sample_type sample_type,
                        void *fb_i,
                        void **fb_n,
                        int channels,
                        AFframecount frame_count);

#endif /* SAMPLE_H */
