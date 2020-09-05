/*
 * Copyright (C) 1998-1999 Ian Jackson
 *
 * This is free software; you can redistribute it and/or modify it
 * under the terms of the GNU General Public License as published by
 * the Free Software Foundation; either version 2 of the License, or
 * (at your option) any later version.
 *
 * This program is distributed in the hope that it will be useful, but
 * WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
 * General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with userv-utils; if not, write to the Free Software
 * Foundation, 59 Temple Place - Suite 330, Boston, MA 02111-1307, USA.
 *
 * $Id: ucgi.h,v 1.2 1999/11/09 23:04:32 ian Exp $
 */

#ifndef UCGI_H
#define UCGI_H

#include <stdlib.h>

#define MAX_ARGS 1024
#define MAX_USERNAME_LEN 1024
#define MAX_SCRIPTPATH_LEN 1024
#define MAX_ENVVAR_VALUE (1024*1024)

void syserror(const char *m);
void error(const char *m);
void *xmalloc(size_t sz);
void xsetenv(const char *en, const char *ev, int overwrite);

extern const char *const envok[];
extern const int nenvok;
extern int debugmode;

#endif
