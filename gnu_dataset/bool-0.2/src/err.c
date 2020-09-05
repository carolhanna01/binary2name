/* err.c - Error routines.
   Copyright (C) 2001 Free Software Foundation, Inc.

   This program is free software; you can redistribute it and/or
   modify it under the terms of the GNU General Public License
   as published by the Free Software Foundation; either version 2
   of the License, or (at your option) any later version.

   This program is distributed in the hope that it will be useful,
   but WITHOUT ANY WARRANTY; without even the implied warranty of
   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
   GNU General Public License for more details.

   You should have received a copy of the GNU General Public License
   along with this program; if not, write to the Free Software
   Foundation, Inc., 59 Temple Place - Suite 330, Boston, MA
   02111-1307, USA.  */

/* Written by Marc Tardif <intmktg@cam.org>.  */

#include <string.h>
#include <stdlib.h>
#include <stdio.h>
#include "config.h"

#include <errno.h>
#ifndef errno
extern int errno;
#endif

#include "options.h"

#ifndef HAVE_STRERROR
extern int sys_nerr;
extern char *sys_errlist[];
# define strerror(E) (0 <= (E) && (E) < sys_nerr \
    ? sys_errlist[E] \
    : "Unknown system error")
#endif

void
err_message (const char *msg)
{
  if (opt.suppress_errors)
    return;

  if (errno)
    fprintf (stderr, "bool: %s: %s\n", msg, strerror (errno));
  else
    fprintf (stderr, "bool: %s\n", msg);
}

void
err_fatal (const char *msg)
{
  fprintf (stderr, "bool: %s\n", msg);
  exit (2);
}

