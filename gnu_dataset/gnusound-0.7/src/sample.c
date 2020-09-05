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

#include <config.h>
#include <assert.h>
#include <stdlib.h>
#include <audiofile.h>
#include "lib/minmax.h"
#include "sample.h"
#include "arbiter.h"

static struct peak_func_table peak_funcs;

/* If not defined, sample_calc_rms calculates absolute averages. */

#define ENABLE_RMS

/**
 * Calculates the root-mean-square for a range of samples.
 * @param avgs Will receive the averages.
 * @param buf The samples.
 * @param count How many samples there are in buf.
 * @param type The type of the samples in buf.
 * @param scale The scaling factor: how many samples produce
 * a single average value.
 */

#ifndef ENABLE_RMS
#define RMS_INT(type, shift_c) \
    for(j = 0; j < q; j++) \
        sum += ABS(((type *)buf)[j]); \
    shift = shift_c

#else
#define RMS_INT(type, shift_c) \
    for(j = 0; j < q; j++) \
        fsum += (((type *)buf)[j] >> shift_c) * (((type *)buf)[j] >> shift_c)
#endif

void
sample_calc_rms(rms_unit_t *rms,
                enum sample_type type,
                const void *buf,
                AFframecount count,
                float scale) {
    long long sum = 0;
    double fsum = 0;
    int dst_idx = 0;
    int sample_width = sample_get_width(type);
    AFframecount i, j, q;
    AFframecount segment_size = scale * (int)(1 / HRES_MIN);
#ifndef ENABLE_RMS
    int shift = 0;
#endif

    count <<= HRES_MIN_SHIFT_BITS;

    for(i = 0; i < count; i += segment_size) {

        q = MIN(segment_size, count - i) >> HRES_MIN_SHIFT_BITS;
        sum = 0;
        fsum = 0;

        switch(type) {
        case SAMPLE_TYPE_INT_8:
            RMS_INT(int8_t, 0);
            break;
        case SAMPLE_TYPE_INT_16:
            RMS_INT(int16_t, 8);
            break;
        case SAMPLE_TYPE_INT_32:
            RMS_INT(int32_t, 24);
            break;
        case SAMPLE_TYPE_FLOAT_32:
            for(j = 0; j < q; j++) 
#ifdef ENABLE_RMS
                fsum += ((float *)buf)[j] * ((float *)buf)[j];
#else
                fsum += ABS(((float *)buf)[j]);
#endif
            break;
        default:
            abort();
        }

#ifdef ENABLE_RMS
        if(type == SAMPLE_TYPE_FLOAT_32)
            rms[dst_idx++] = (int)(floor(sqrt(fsum / q) * 128));
        else
            rms[dst_idx++] = (int)(floor(sqrt(fsum / q)));
#else
        if(type == SAMPLE_TYPE_FLOAT_32)
            rms[dst_idx++] = (fsum / q) * 128;
        else 
            rms[dst_idx++] = (sum / q) >> shift;
#endif

        buf += (sample_width * q);
    }
    
}

inline graph_bits_unit_t
sample_to_graph(enum sample_type sample_type,
                void *sample) {
    switch(sample_type) {
    case SAMPLE_TYPE_INT_8:
        return *((int8_t *)sample);
    case SAMPLE_TYPE_INT_16:
        return *((int16_t *)sample) >> 8;
    case SAMPLE_TYPE_INT_32:
        return *((int32_t *)sample) >> 24;
    case SAMPLE_TYPE_FLOAT_32:
        return *((float *)sample) / 1.0f;
    }
    FAIL("sample_type: %d\n", sample_type);
    assert(0 && "Unknown sample type");
}

void
sample_scale_peaks(peak_unit_t *lows,
                   peak_unit_t *highs,
                   const peak_unit_t *src_lows,
                   const peak_unit_t *src_highs,
                   AFframecount count,
                   float scale) {
    peak_unit_t l, h;
    int src_units_per_dst_unit = ceil(scale / PEAK_HRES);
    int src_idx = 0, dst_idx = 0, q;
    AFframecount i;

    for(i = 0; i < count; i += scale) {
        q = MIN(src_units_per_dst_unit, (count - i) / PEAK_HRES);
        l = PEAK_MIN_FUNC(&src_lows[src_idx], q);
        h = PEAK_MAX_FUNC(&src_highs[src_idx], q); 
        lows[dst_idx] = l;
        highs[dst_idx] = h;
        dst_idx++;
        src_idx += q;
    }
}

static void
sample_peaks_int_32(peak_unit_t *low,
                    peak_unit_t *high,
                    const int32_t *buf,
                    AFframecount count) {
    int i;
    int32_t s;
    peak_unit_t l = 0, h = 0;

    /* For 32 bit int samples we don't have a fast function. */

    l = h = s = buf[0] >> 24;
    
    for(i = 1; i < count; i++) {
                
        /* Get sample value, compare it to min. and max., swap
           if necessary. */
        
        s = buf[i] >> 24;
        
        if(s < l)
            l = s;
        else if(s >= h)
            h = s;
    }

    *low = l;
    *high = h;
}

static void
sample_peaks_float_32(peak_unit_t *low,
                      peak_unit_t *high,
                      const float *buf,
                      AFframecount count) {
    int i;
    int32_t s;
    peak_unit_t l = 0, h = 0;

    l = h = s = (int)(buf[0] * PEAK_HRES);
    
    for(i = 1; i < count; i++) {
                
        /* Get sample value, compare it to min. and max., swap
           if necessary. */
        
        s = (int)(buf[i] * PEAK_HRES);
        
        if(s < l)
            l = s;
        else if(s >= h)
            h = s;
    }

    *low = l;
    *high = h;
}

void 
sample_scale_samples(peak_unit_t *lows,
                     peak_unit_t *highs,
                     enum sample_type sample_type,
                     const void *src_buf,
                     AFframecount count,
                     float scale) {
    int i, q;
    int segment_size = scale * (int)(1 / HRES_MIN);
    int sample_width = sample_get_width(sample_type);
    int dst_idx = 0;
    peak_unit_t l = 0, h = 0;
    const void *buf = src_buf;
    AFframecount offset;

    /*
     * The scaling factor is multiplied by (1 / HRES_MIN) so that it
     * is always integer. The proper indices can then be obtained by
     * shifting right by HRES_MIN_SHIFT_BITS. This way floating point
     * operations are avoided.
     */
    
    count <<= HRES_MIN_SHIFT_BITS;

    for(i = 0; i < count; i += segment_size) {
        
        q = MIN(segment_size, count - i) >> HRES_MIN_SHIFT_BITS;
        q = MAX(q, 1);

        switch(sample_type) {
        case SAMPLE_TYPE_INT_8:
            l = MIN8_FUNC(buf, q);
            h = MAX8_FUNC(buf, q);
            break;
        case SAMPLE_TYPE_INT_16:
            l = MIN16_FUNC(buf, q) >> 8;
            h = MAX16_FUNC(buf, q) >> 8;
            break;
        case SAMPLE_TYPE_INT_32:
            sample_peaks_int_32(&l, &h, buf, q);
            break;
        case SAMPLE_TYPE_FLOAT_32:
            sample_peaks_float_32(&l, &h, buf, q);
            break;
        }

        lows[dst_idx] = l;
        highs[dst_idx] = h;

        dst_idx++;
        buf = src_buf + (sample_width * (i >> HRES_MIN_SHIFT_BITS));

    }

}


const char *
sample_get_description(enum sample_type type) {
    switch(type) {
    case SAMPLE_TYPE_INT_8:
        return "8 bit int";
    case SAMPLE_TYPE_INT_16:
        return "16 bit int";
    case SAMPLE_TYPE_INT_32:
        return "32 bit int";
    case SAMPLE_TYPE_FLOAT_32:
        return "32 bit float";
    default:
        FAIL("Unknown sample type!\n");
        abort();
    }
}
    
int
sample_get_width(enum sample_type type) {
    switch(type) {
    case SAMPLE_TYPE_INT_8:
        return 1;
    case SAMPLE_TYPE_INT_16:
        return 2;
    case SAMPLE_TYPE_INT_32:
        return 4;
    case SAMPLE_TYPE_FLOAT_32:
        return sizeof(float);
    default:
        FAIL("Unknown sample type!\n");
        abort();
    }
}

/**
 * Determines whether a buffer consists entirely of silence.
 * @param sample_type The sample type in sample_bits.
 * @param sample_bits The buffer.
 * @param count Number of samples in the buffer.
 */

#define BUFFER_IS_SILENCE(T) \
  if(((T *)sample_bits)[0] == 0 && \
     ((T *)sample_bits)[MAX(0, count - 1)] == 0 && \
     ((T *)sample_bits)[MAX(0, count / 2)] == 0) { \
      for(i = 0; i < count; i++) \
          if(((T *)sample_bits)[i] != 0) \
              return 0; \
      return 1; \
  } \
  return 0;

int
sample_buffer_is_silence(enum sample_type sample_type,
                         const void *sample_bits,
                         AFframecount count) {
    AFframecount i;

    g_return_val_if_fail(count >= 0, 0);
    
    if(count == 0)
        return 1;
    
    switch(sample_type) {
    case SAMPLE_TYPE_INT_8:
        BUFFER_IS_SILENCE(int8_t);
    case SAMPLE_TYPE_INT_16:
        BUFFER_IS_SILENCE(int16_t);
    case SAMPLE_TYPE_INT_32:
        BUFFER_IS_SILENCE(int32_t);
    case SAMPLE_TYPE_FLOAT_32:
        BUFFER_IS_SILENCE(float);
    default:
        FAIL("Unknown sample type!\n");
        abort();
    }
    return 0;
}

/*
 * Converts a buffer with sample data from one data type to another.
 * FIXME: When converting from a bigger to a smaller word-length, this
 * function truncates the low bits. Dithering would provide better
 * quality in some cases.
 *
 * @param src_type The type of the samples in the src buffer.
 * @param dst_type The type of the samples in the dst buffer.
 * @param src Where to get the samples to convert.
 * @param dst Where to store the converted samples.
 * @param frame_count The number of samples to convert.
 */

#define INTS_TO_FLOATS(src_type, srcbuf, dstbuf, count, range) \
  for(i = 0; i < count; i++) \
      ((float *)dstbuf)[i] = ((src_type *)srcbuf)[i] * (1.0f / range); \

#define FLOATS_TO_INTS(dst_type, srcbuf, dstbuf, count, range) \
  for(i = 0; i < count; i++) \
      ((dst_type *)dstbuf)[i] = FLOAT_TO_INT(((float *)srcbuf)[i] * range);

#define INTS_TO_INTS(src_type, dst_type, srcbuf, dstbuf, count, shiftdir, shift) \
  for(i = 0; i < count; i++) \
      ((dst_type *)dstbuf)[i] = ((src_type *)srcbuf)[i] shiftdir shift;

void
sample_convert(enum sample_type src_type,
               enum sample_type dst_type,
               const void *src,
               void *dst,
               AFframecount frame_count) {
    int i;
    if(src_type == dst_type) {
        memcpy(dst, src, frame_count * sample_get_width(src_type));
        return;
    }
    
    switch(src_type) {
    case SAMPLE_TYPE_INT_8:
        switch(dst_type) {
        case SAMPLE_TYPE_INT_16:
            INTS_TO_INTS(int8_t, int16_t, src, dst, frame_count, <<, 8);
            break;
        case SAMPLE_TYPE_INT_32:
            INTS_TO_INTS(int8_t, int32_t, src, dst, frame_count, <<, 24);
            break;
        case SAMPLE_TYPE_FLOAT_32:
            INTS_TO_FLOATS(int8_t, src, dst, frame_count, 127.5f);
            break;
        default:
            /* not reached */
            break;
        }
        break;
    case SAMPLE_TYPE_INT_16:
        switch(dst_type) {
        case SAMPLE_TYPE_INT_8:
            INTS_TO_INTS(int16_t, int8_t, src, dst, frame_count, >>, 8);
            break;
        case SAMPLE_TYPE_INT_32:
            INTS_TO_INTS(int16_t, int32_t, src, dst, frame_count, <<, 16);
            break;
        case SAMPLE_TYPE_FLOAT_32:
            INTS_TO_FLOATS(int16_t, src, dst, frame_count, 32767.5f);
            break;
        default:
            /* not reached */
            break;
        }
        break;
    case SAMPLE_TYPE_INT_32:
        switch(dst_type) {
        case SAMPLE_TYPE_INT_8:
            INTS_TO_INTS(int32_t, int8_t, src, dst, frame_count, >>, 24);
            break;
        case SAMPLE_TYPE_INT_16:
            INTS_TO_INTS(int32_t, int16_t, src, dst, frame_count, >>, 16);
            break;
        case SAMPLE_TYPE_FLOAT_32:
            INTS_TO_FLOATS(int32_t, src, dst, frame_count, 2147483647.5f);
            break;
        default:
            /* not reached */
            break;
        }
        break;
    case SAMPLE_TYPE_FLOAT_32:
        switch(dst_type) {
        case SAMPLE_TYPE_INT_8:
            FLOATS_TO_INTS(int8_t, src, dst, frame_count, 127.0f);
            break;
        case SAMPLE_TYPE_INT_16:
            FLOATS_TO_INTS(int16_t, src, dst, frame_count, 32767.0f);
            break;
        case SAMPLE_TYPE_INT_32:
            FLOATS_TO_INTS(int32_t, src, dst, frame_count, 2147483647.0f);
            break;
        default:
            /* not reached */
            break;
        }
        break;
    default:
        /* not reached */
        break;
    }
}

/**
 * Interleaves the samples in fb_sources onto fb_target.
 * 
 * @param sample_type The sample type.
 * @param fb_i Where the interleaved frames will be stored. Must be
 * big enough to hold (snd_get_sample_width(stype) * frame_count) samples.
 * @param fb_n The non-interleaved source tracks.
 * @param channels The number of channels.
 * @param skip_channel_map Bitmap specifying which channels to skip.
 * @param frame_count The number of frames to interleave.
 */

#define INTERLEAVE(type) \
  for(i = 0; i < channels; i++) { \
      if((1 << i) & skip_channel_map) \
          continue; \
      for(j = 0; j < frame_count; j++) \
           ((type *)fb_i)[(j * channels) + i] = ((type **)fb_n)[i][j]; \
  } \

void
sample_interleave(enum sample_type sample_type,
                  void *fb_i,
                  void *fb_n[],
                  int channels,
                  track_map_t skip_channel_map,
                  AFframecount frame_count) {
    int i, j = 0;
    switch(sample_type) {
    case SAMPLE_TYPE_INT_8:
        INTERLEAVE(int8_t);
        break;
    case SAMPLE_TYPE_INT_16:
        INTERLEAVE(int16_t);
        break;
    case SAMPLE_TYPE_INT_32:
        INTERLEAVE(int32_t);
        break;
    case SAMPLE_TYPE_FLOAT_32:
        INTERLEAVE(float);
        break;
    }
}

/**
 * Extracts a channel from the interleaved buffer fb_i and stores
 * it in the buffer fb_n.
 * 
 * @param sample_type The sample type.
 * @param fb_i The buffer containing the interleaved data.
 * @param fb_n The buffer which will receive the channel.
 * @param channel The channel to retrieve.
 * @param channels The number of channels in fb_i.
 * @param frame_count The number of frames to deinterleave.
 */

#define DEINTERLEAVE(type) \
  for(i = 0; i < frame_count - 8; i += 8) { \
      ((type *)fb_n)[i] = ((type *)fb_i)[src_index]; \
      src_index += channels; \
      ((type *)fb_n)[i+1] = ((type *)fb_i)[src_index]; \
      src_index += channels; \
      ((type *)fb_n)[i+2] = ((type *)fb_i)[src_index]; \
      src_index += channels; \
      ((type *)fb_n)[i+3] = ((type *)fb_i)[src_index]; \
      src_index += channels; \
      ((type *)fb_n)[i+4] = ((type *)fb_i)[src_index]; \
      src_index += channels; \
      ((type *)fb_n)[i+5] = ((type *)fb_i)[src_index]; \
      src_index += channels; \
      ((type *)fb_n)[i+6] = ((type *)fb_i)[src_index]; \
      src_index += channels; \
      ((type *)fb_n)[i+7] = ((type *)fb_i)[src_index]; \
      src_index += channels; \
  } \
  for(; i < frame_count; i++, src_index += channels) \
      ((type *)fb_n)[i] = ((type *)fb_i)[src_index]; \

void
sample_deinterleave(enum sample_type sample_type,
                    const void *fb_i,
                    void *fb_n,
                    int channel,
                    int channels,
                    AFframecount frame_count) {
    register int i, src_index = channel;
    switch(sample_type) {
    case SAMPLE_TYPE_INT_8:
        DEINTERLEAVE(int8_t);
        break;
    case SAMPLE_TYPE_INT_16:
        DEINTERLEAVE(int16_t);
        break;
    case SAMPLE_TYPE_INT_32:
        DEINTERLEAVE(int32_t);
        break;
    case SAMPLE_TYPE_FLOAT_32:
        DEINTERLEAVE(float);
        break;
    }
}

#if 0
/**
 * Extracts all channels from the interleaved buffer fb_i and stores
 * them in the buffers given by fb_n.
 * 
 * @param sample_type The sample type.
 * @param fb_i The buffer containing the interleaved samples.
 * @param fb_n The buffers which will receive the deinterleaved samples.
 * @param channels The number of channels in fb_i.
 * @param frame_count The number of frames to deinterleave.
 */

#define DEINTERLEAVE_ALL(type) \
    for(i = 0, k = 0; i < frame_count; i++, k += channels) \
        for(j = 0; j < channels; j++)  \
            ((type **)fb_n)[j][i] = ((type *)fb_i)[j+k]; \

void
sample_deinterleave_all(enum sample_type sample_type,
                        void *fb_i,
                        void **fb_n,
                        int channels,
                        AFframecount frame_count) {
    register int i, j, k;
    switch(sample_type) {
    case SAMPLE_TYPE_INT_8:
        DEINTERLEAVE_ALL(int8_t);
        break;
    case SAMPLE_TYPE_INT_16:
        DEINTERLEAVE_ALL(int16_t);
        break;
    case SAMPLE_TYPE_INT_32:
        DEINTERLEAVE_ALL(int32_t);
        break;
    case SAMPLE_TYPE_FLOAT_32:
        DEINTERLEAVE_ALL(float);
        break;
    }
}
#endif

int
sample_init() {
    CpuCaps *cpu_caps;

    peak_funcs.peak_max_func = max_int8_c;
    peak_funcs.peak_min_func = min_int8_c;
    peak_funcs.max8_func = max_int8_c;
    peak_funcs.min8_func = min_int8_c;
    peak_funcs.max16_func = max_int16_c;
    peak_funcs.min16_func = min_int16_c;
    
    cpu_caps = arbiter_get_cpu_caps();

#if USE_MMX_MINMAX == 3
    DEBUG("using static 3DNow! minmax\n");
#elif USE_MMX_MINMAX == 2
    DEBUG("using static MMX2 minmax\n");
#elif USE_MMX_MINMAX == 1
    if(gCpuCaps.hasMMX2) {
        DEBUG("using MMX2 minmax\n");
        peak_funcs.peak_max_func = max_int8_mmx2;
        peak_funcs.peak_min_func = min_int8_mmx2;
        peak_funcs.max8_func = max_int8_mmx2;
        peak_funcs.min8_func = min_int8_mmx2;
        peak_funcs.max16_func = max_int16_mmx2;
        peak_funcs.min16_func = min_int16_mmx2;
    } else if(gCpuCaps.has3DNow) {
        DEBUG("using 3DNow! minmax\n");
        peak_funcs.peak_max_func = max_int8_3dnow;
        peak_funcs.peak_min_func = min_int8_3dnow;
        peak_funcs.max8_func = max_int8_3dnow;
        peak_funcs.min8_func = min_int8_3dnow;
        peak_funcs.max16_func = max_int16_3dnow;
        peak_funcs.min16_func = min_int16_3dnow;
    }
#else
    DEBUG("Using plain C minmax\n");
#endif /* USE_MMX_MINMAX */

    return 0;
}

/****************************************************************/

/* old rubbish (see above for new rubbish) */

#if 0

/*
 * This function is probably the one that eats most of the CPU time in
 * the program, so optimizations here yield big rewards.
 */

void 
sample_scale_samples_or_peaks(graph_bits_unit_t *graph_bits_low,
                              graph_bits_unit_t *graph_bits_high,
                              void *source_bits_low,
                              void *source_bits_high,
                              AFframecount frame_count,
                              enum sample_type sample_type,
                              float segment_size,
                              AFframecount src_off,
                              AFframecount dst_off,
                              int source_is_graph) {
    AFframecount c, q, r;
    graph_bits_unit_t cur_high = 0, cur_low = 0, l = 0, h = 0;
    graph_bits_unit_t *gbp_low = (graph_bits_unit_t *)source_bits_low,
        *gbp_high = (graph_bits_unit_t *)source_bits_high;
    frame_bits_t fbp = (frame_bits_t)source_bits_low;
    void *p;
    int k;
    int i;
    int i_segment_size = segment_size * (int)(1 / HRES_MIN);
    int sample_width = sample_get_width(sample_type);
    frame_count *= (int)(1 / HRES_MIN);

    if(source_is_graph) {
        for(i = 0, c = 0; 
            i < frame_count; 
            i += i_segment_size, c++) {
            q = MIN(i_segment_size, frame_count - i) >> HRES_MIN_SHIFT_BITS;
            r = (AFframecount)(src_off+(i >> HRES_MIN_SHIFT_BITS));
            l = PEAK_MIN_FUNC(&gbp_low[r], q);
            h = PEAK_MAX_FUNC(&gbp_high[r], q);
            graph_bits_low[dst_off+c] = l;
            graph_bits_high[dst_off+c] = h;
        }
    } else {
        for(i = 0, c = 0; 
            i < frame_count; 
            i += i_segment_size, c++) {
            p = fbp + (sample_width * (size_t)(src_off+(i >> HRES_MIN_SHIFT_BITS)));
            q = MIN(i_segment_size, frame_count - i) >> HRES_MIN_SHIFT_BITS;
            switch(sample_type) {
            case SAMPLE_TYPE_INT_8:
                l = MIN8_FUNC(p, q);
                h = MAX8_FUNC(p, q);
                break;
            case SAMPLE_TYPE_INT_16:
                l = MIN16_FUNC(p, q) >> 8;
                h = MAX16_FUNC(p, q) >> 8;
                break;
            case SAMPLE_TYPE_INT_32:
                /* For 32 bit int samples we don't have a fast function. */
                for(k = 0; k < segment_size && c+k < frame_count; k++) {
                    cur_low = cur_high = 
                        sample_to_graph(sample_type, fbp + 
                                        (sample_width * (src_off+(i >> HRES_MIN_SHIFT_BITS)+k)));
                    
                    /* Low/high algorithm. */
                    
                    if(!k || (cur_low < l))
                        l = cur_low;
                    if(!k || (cur_high >= h))
                        h = cur_high;
                }
                break;
            case SAMPLE_TYPE_FLOAT_32:
                FAIL("NOT IMPLEMENTED\n");
                break;
            }
            graph_bits_low[dst_off+c] = l;
            graph_bits_high[dst_off+c] = h;
        }
    }
}

void 
sample_scale_samples(graph_bits_unit_t *lows,
                     graph_bits_unit_t *highs,
                     frame_bits_t frame_bits,
                     AFframecount frame_count,
                     enum sample_type sample_type,
                     float hres) {
    sample_scale_samples_or_peaks(lows,
                                  highs,
                                  frame_bits,
                                  frame_bits,
                                  frame_count,
                                  sample_type,
                                  hres,
                                  0,
                                  0,
                                  0);
    
}

#endif

