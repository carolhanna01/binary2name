/*
 * MMX optimized minimum/maximum functions.
 * Copyright (C) 2002-2004  Pascal Haakmat
 *
 * This program is free software; you can redistribute it and/or
 * modify it under the terms of the GNU General Public License as
 * published by the Free Software Foundation; either version 2 of the
 * License, or (at your option) any later version.
 *
 * This program is distributed in the hope that it will be useful, but
 * WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
 * General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with this program; if not, write to the Free Software
 * Foundation, Inc., 675 Mass Ave, Cambridge, MA 02139, USA.
 *
 * Statistics for compilation options: -O2
 * Results for int16.
 *
 * C:
 *      Runs | Elems | Time
 * -----------------------------
 *     10000 |    64 | 0.006302
 *    100000 |    64 | 0.060498
 *   1000000 |    64 | 0.624079
 *     10000 |  1024 | 0.086681
 *    100000 |  1024 | 0.876180
 *   1000000 |  1024 | 8.699330
 *     10000 |  4096 | 0.342442
 *    100000 |  4096 | 3.425953
 *   1000000 |  4096 | 34.249983
 *
 * MMX:
 *
 *      Runs | Elems | Time
 * -----------------------------
 *     10000 |    64 | 0.001497
 *    100000 |    64 | 0.015307
 *   1000000 |    64 | 0.140745
 *     10000 |  1024 | 0.009910
 *    100000 |  1024 | 0.093061
 *   1000000 |  1024 | 0.965179
 *     10000 |  4096 | 0.034865
 *    100000 |  4096 | 0.354665
 *   1000000 |  4096 | 3.483273
 *
 * MMX w/prefetch + pairing + checking:
 *
 *      Runs | Elems | Time
 * -----------------------------
 *     10000 |  4096 | 0.027310
 *    100000 |  4096 | 0.267258
 *   1000000 |  4096 | 2.674745
 */

#include <stdlib.h> /* defines int16_t and int8_t */
#include <config.h>

#ifdef HAVE_ARCH_X86

#define MINMAX16_MMX(MIN_OR_MAX_FUNC, MIN_OR_MAX_C_FUNC, COMPARATOR, LOOP_LABEL, PINSN, PREFETCHINSN, MOVINSN) \
int16_t                                         \
MIN_OR_MAX_FUNC(const int16_t *a,               \
                int count) {                    \
    int i, aligned_count, aligned_count2;       \
    int16_t tail_extreme, extreme, extremes[4]  = { 0, 0, 0, 0 }; \
                                                \
    /* Setup compares first 16 elements,        \
       then loop start compares 8 more elements, \
       so we can't compare less than 24 elements. */ \
                                                \
    if(count < 24)                              \
        return MIN_OR_MAX_C_FUNC(a, count);     \
                                                \
    /* Comparing 8 elements per iteration so align on 8 element \
       boundary. Because setup compares 16 elements subtract 16 \
       elements from loop count. */             \
                                                \
    aligned_count = aligned_count2 = count & ~7; \
    aligned_count -= 16;                        \
                                                \
    __asm__ __volatile__ (PREFETCHINSN" 128(%0)\n" \
             "movq (%0),%%mm6\n"                \
             "movq 8(%0),%%mm7\n"               \
             "movq 16(%0),%%mm0\n"              \
             "movq 24(%0),%%mm1\n"              \
             PINSN" %%mm6, %%mm0\n"             \
             PINSN" %%mm7, %%mm1\n"             \
             "add $32,%0\n"                     \
             LOOP_LABEL":\n"                    \
             "movq (%0),%%mm6\n"                \
             "movq 8(%0),%%mm7\n"               \
             PINSN" %%mm6, %%mm0\n"             \
             PINSN" %%mm7, %%mm1\n"             \
             "add $16,%0\n"                     \
             "subl $8,%1\n"                     \
             "jnz "LOOP_LABEL"\n"               \
             PINSN" %%mm1,%%mm0\n"              \
             MOVINSN" %%mm0,(%2)\n"             \
             "emms"                             \
             :                                  \
             : "q" (a), "q" (aligned_count), "q" (extremes) \
             : "memory", "cc");                 \
    extreme = extremes[0];                      \
    for(i = 1; i < 4; i++)                      \
        if(extremes[i] COMPARATOR extreme)      \
            extreme = extremes[i];              \
                                                \
    if(count - aligned_count2) {                \
        tail_extreme = MIN_OR_MAX_C_FUNC(&a[aligned_count2], \
                               count - aligned_count2); \
        extreme = tail_extreme COMPARATOR extreme ? tail_extreme : extreme; \
    }                                           \
                                                \
    return extreme;                             \
}

#define MINMAX8_MMX(MIN_OR_MAX_FUNC, MIN_OR_MAX_C_FUNC, COMPARATOR, LOOP_LABEL, PINSN, PREFETCHINSN, MOVINSN) \
int8_t                                          \
MIN_OR_MAX_FUNC(const int8_t *a,                \
                int count) {                    \
    int i, aligned_count, aligned_count2;       \
    int8_t tail_extreme, extreme, extremes[8]  = { 0, 0, 0, 0, 0, 0, 0, 0 }; \
    int8_t masks[8] = { 128, 128, 128, 128, 128, 128, 128, 128 }; \
                                                \
    /* Setup compares first 32 elements,        \
       then loop start compares 16 more elements, \
       so we can't compare less than 48 elements. */ \
                                                \
    if(count < 48)                              \
        return MIN_OR_MAX_C_FUNC(a, count);     \
                                                \
    /* Comparing 16 elements per iteration so align on 16 element \
       boundary. Because setup compares 32 elements subtract 32 \
       elements from loop count. */             \
                                                \
    aligned_count = aligned_count2 = count & ~15; \
    aligned_count -= 32;                        \
                                                \
    __asm__ __volatile__ (PREFETCHINSN" 128(%0)\n" \
             "movq (%0),%%mm6\n"                \
             "movq 8(%0),%%mm7\n"               \
             "movq 16(%0),%%mm0\n"              \
             "movq 24(%0),%%mm1\n"              \
             "pxor (%3),%%mm6\n"                \
             "pxor (%3),%%mm7\n"                \
             "pxor (%3),%%mm0\n"                \
             "pxor (%3),%%mm1\n"                \
             PINSN" %%mm6, %%mm0\n"             \
             PINSN" %%mm7, %%mm1\n"             \
             "add $32,%0\n"                     \
             LOOP_LABEL":\n"                    \
             "movq (%0),%%mm6\n"                \
             "movq 8(%0),%%mm7\n"               \
             "pxor (%3),%%mm6\n"                \
             "pxor (%3),%%mm7\n"                \
             PINSN" %%mm6, %%mm0\n"             \
             PINSN" %%mm7, %%mm1\n"             \
             "add $16,%0\n"                     \
             "subl $16,%1\n"                    \
             "jnz "LOOP_LABEL"\n"               \
             PINSN" %%mm1,%%mm0\n"              \
             MOVINSN" %%mm0,(%2)\n"                \
             "emms"                             \
             :                                  \
             : "q" (a), "q" (aligned_count), "q" (extremes), "q" (masks) \
             : "memory", "cc");                 \
    extreme = (int8_t)(extremes[0] ^ 128);      \
    for(i = 1; i < 8; i++) {                    \
        extremes[i] ^= 128;                     \
        if(extremes[i] COMPARATOR extreme)      \
            extreme = extremes[i];              \
    }                                           \
                                                \
    if(count - aligned_count2) {                \
        tail_extreme = MIN_OR_MAX_C_FUNC(&a[aligned_count2], \
                               count - aligned_count2); \
        extreme = tail_extreme COMPARATOR extreme ? tail_extreme : extreme; \
    }                                           \
                                                \
    return extreme;                             \
}

#endif /* HAVE_ARCH_X86 */

#define MINMAX_C(FUNC_NAME, COMPARATOR, INT8_OR_INT16) \
INT8_OR_INT16                                   \
FUNC_NAME(const INT8_OR_INT16 *a,               \
          int count) {                          \
    int i;                                      \
    INT8_OR_INT16 extreme = a[0];               \
    for(i = 1; i < count; i++)                  \
        if(a[i] COMPARATOR extreme)             \
            extreme = a[i];                     \
    return extreme;                             \
}

MINMAX_C(min_int16_c, <, int16_t)
MINMAX_C(max_int16_c, >, int16_t)
MINMAX_C(min_int8_c, <, int8_t)
MINMAX_C(max_int8_c, >, int8_t)

#ifdef HAVE_ARCH_X86
MINMAX8_MMX(min_int8_mmx2, min_int8_c, <, "min_int8_mmx2_loop", "pminub", "prefetchnta", "movntq")
MINMAX8_MMX(max_int8_mmx2, max_int8_c, >, "max_int8_mmx2_loop", "pmaxub", "prefetchnta", "movntq")
MINMAX16_MMX(min_int16_mmx2, min_int16_c, <, "min_int16_mmx2_loop", "pminsw", "prefetchnta", "movntq")
MINMAX16_MMX(max_int16_mmx2, max_int16_c, >, "max_int16_mmx2_loop", "pmaxsw", "prefetchnta", "movntq")
MINMAX8_MMX(min_int8_3dnow, min_int8_c, <, "min_int8_3dnow_loop", "pminub", "prefetch", "movq")
MINMAX8_MMX(max_int8_3dnow, max_int8_c, >, "max_int8_3dnow_loop", "pmaxub", "prefetch", "movq")
MINMAX16_MMX(min_int16_3dnow, min_int16_c, <, "min_int16_3dnow_loop", "pminsw", "prefetch", "movq")
MINMAX16_MMX(max_int16_3dnow, max_int16_c, >, "max_int16_3dnow_loop", "pmaxsw", "prefetch", "movq")
#endif /* HAVE_ARCH_X86 */
