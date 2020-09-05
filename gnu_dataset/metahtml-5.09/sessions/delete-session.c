/* create-session.c: -*- C -*-  DESCRIPTIVE TEXT. */

/* Author: Brian J. Fox (bfox@ua.com) Sat Jul  1 16:34:18 1995.

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

#include <stdio.h>
#include <unistd.h>
#include <stdlib.h>
#include <stdarg.h>
#include <string.h>
#include "session.h"
#include "parseargs.h"

/* Delete the session with the arguments given. */
int
main (int argc, char *argv[])
{
  register int i;
  int num_deleted = 0;

  parse_session_arguments (&argc, argv);

  if (argc < 2)
    {
      fprintf (stderr, "Sorry, but %s requires an argument.\n", argv[0]);
      exit (1);
    }

  for (i = 1; i < argc; i++)
    {
      if (session_end (argv[i]) != 0)
	fprintf (stderr, "Can't delete session %s...\n", argv[i]);
      else
	num_deleted++;
    }

  printf ("Sessions Deleted = %d\n", num_deleted);

  return (num_deleted == 0);
}
