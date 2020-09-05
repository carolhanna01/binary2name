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

#ifndef MINMAX_H
#define MINMAX_H

#include <stdlib.h>
#include <config.h>

int8_t
min_int8_c(const int8_t *a,
           int count);
int8_t
max_int8_c(const int8_t *a,
           int count);
int8_t
min_int8_mmx2(const int8_t *a,
              int count);
int8_t
max_int8_mmx2(const int8_t *a,
              int count);
int8_t
min_int8_3dnow(const int8_t *a,
              int count);
int8_t
max_int8_3dnow(const int8_t *a,
               int count);
int16_t
min_int16_c(const int16_t *a,
            int count);
int16_t
max_int16_c(const int16_t *a,
            int count);
int16_t
min_int16_mmx2(const int16_t *a,
               int count);
int16_t
max_int16_mmx2(const int16_t *a,
               int count);
int16_t
min_int16_3dnow(const int16_t *a,
                int count);
int16_t
max_int16_3dnow(const int16_t *a,
                int count);

#ifndef HAVE_ARCH_X86
#warning "Not compiling MMX/MMX2/3DNow! optimized min/max."
#define min_int8_mmx2 min_int8_c
#define min_int16_mmx2 min_int16_c
#define min_int8_3dnow min_int8_c
#define min_int16_3dnow min_int16_c
#define max_int8_mmx2 max_int8_c
#define max_int16_mmx2 max_int16_c
#define max_int8_3dnow max_int8_c
#define max_int16_3dnow max_int16_c
#endif

#endif /* MINMAX_H */
