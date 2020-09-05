/*
 * GNUsound - a sound editor for GNOME.
 * Copyright (C) 2005  Pascal Haakmat <a.haakmat@chello.nl>
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

#ifndef EMERGENCY_H
#define EMERGENCY_H

#include <config.h>
#include <endian.h>
#include <signal.h>
#ifdef HAVE_BYTESWAP_H
#include <byteswap.h>
#endif

extern sig_atomic_t is_emergency;

#define EMERGENCY_BUF_SIZE      512

/*
 * These definitions adapted from wav.c in the libsndfile package.
 */

/* Definitions for Microsoft WAVE format */

#if __BYTE_ORDER == __LITTLE_ENDIAN
# define COMPOSE_ID(a,b,c,d)	((a) | ((b)<<8) | ((c)<<16) | ((d)<<24))
# define LE_SHORT(v)		(v)
# define LE_INT(v)		(v)
#elif __BYTE_ORDER == __BIG_ENDIAN
# define COMPOSE_ID(a,b,c,d)	((d) | ((c)<<8) | ((b)<<16) | ((a)<<24))
# define LE_SHORT(v)		bswap_16(v)
# define LE_INT(v)		bswap_32(v)
#else
# error "Wrong endian"
#endif

#define RIFF_MARKER             COMPOSE_ID('R','I','F','F')
#define WAVE_MARKER             COMPOSE_ID('W','A','V','E')
#define fmt_MARKER              COMPOSE_ID('f','m','t',' ')
#define data_MARKER             COMPOSE_ID('d','a','t','a')
#define fact_MARKER             COMPOSE_ID('f','a','c','t')

#define WAVE_FORMAT_PCM         (0x0001)
#define WAVE_FORMAT_IEEE_FLOAT  (0x0003)

typedef struct {
    u_int32_t samples ;
} FACT_CHUNK;

typedef struct {
    u_int16_t format ;
    u_int16_t channels ;
    u_int32_t samplerate ;
    u_int32_t bytespersec ;
    u_int16_t blockalign ;
    u_int16_t bitwidth ;
} MIN_WAV_FMT ;

/*
 * End of definitions from wav.c.
 */

void
emergency_get_sighandlers();

void
emergency_exit();

int
emergency_init();

#endif /* EMERGENCY_H */
