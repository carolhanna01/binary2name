/* mkpass.c: -*- C -*-  DESCRIPTIVE TEXT. */

/* Author: Brian J. Fox (bfox@ai.mit.edu) Thu Feb  1 14:58:22 1996.

   This file is part of <Meta-HTML>(tm), a system for the rapid deployment
   of Internet and Intranet applications via the use of the Meta-HTML
   language.

   Copyright (c) 1995, 1996, Brian J. Fox (bfox@ai.mit.edu).
   Copyright (c) 1996, Universal Access Inc. (http://www.ua.com).

   Meta-HTML is free software; you can redistribute it and/or modify
   it under the terms of the UAI Free Software License as published
   by Universal Access Inc.; either version 1, or (at your option) any
   later version.

   This program is distributed in the hope that it will be useful,
   but WITHOUT ANY WARRANTY; without even the implied warranty of
   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
   UAI Free Software License for more details.

   You should have received a copy of the UAI Free Software License
   along with this program; if you have not, you may obtain one by
   writing to:

   Universal Access Inc.
   129 El Paseo Court
   Santa Barbara, CA
   93101  */

#if defined (HAVE_CONFIG_H)
#  include <config.h>
#endif

#include <stdio.h>
#include <unistd.h>
#include <stdlib.h>
#include <stdarg.h>
#include <string.h>
#include <bprintf/bprintf.h>
#include <xmalloc/xmalloc.h>
#if defined (HAVE_CRYPT_H)
#  include <crypt.h>
#endif

/* Create a password from cleartext. */
static char *
create_password (char *clear, char *salt)
{
  int length = (13 * ((strlen (clear) + 7) / 8));
  char *encrypted = (char *)xmalloc (1 + length);
  char *clear_p = clear;

  encrypted[0] = '\0';

  while (length > 0)
    {
      char chunk[9];
      char *temp;

      strncpy (chunk, clear_p, 8);
      chunk[8] = (char)0;

      temp = crypt (chunk, salt);
      strcat (encrypted, temp);

      clear_p += 8;
      length -= 13;
    }

  return (encrypted);
}

int
main (int argc, char *argv[])
{
  char salt[3] = { 'c', 'd', '\0' };

  if (argc >= 2)
    {
      int arg_index = 1;

      if (strcmp (argv[1], "--salt") == 0)
	{
	  arg_index = 3;
	  salt[0] = argv[2][0];
	  salt[1] = argv[2][1];
	}

      fprintf (stdout, "%s", create_password (argv[arg_index], salt));
    }
  return (0);
}

