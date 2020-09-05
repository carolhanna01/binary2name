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

#include <stdint.h>

const char *
find_extension(const char *path);

double 
tv_diff_secs(const struct timeval *start,
             const struct timeval *stop);

int
match_wildcard(const char *pattern, 
               const char *str);

char *
printable_byte_count(int64_t sz);

int
searchpath(const char *suffix,
           const char *prefixes,
           char *buf,
           size_t bufsz);

char *
wordwrap(char *s,
         int line_length);

char *
sym2words(char *sym);

char *
findfile(const char *search_paths, 
         const char *filename,
         int mode);


#ifndef HAVE_VASPRINTF
int
vasprintf(char **strp,
          const char *fmt,
          va_list ap);
#endif

#ifndef HAVE_ASPRINTF
int
asprintf(char **strp,
         const char *fmt, ...);
#endif
