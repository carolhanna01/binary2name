/* dbget.c: -*- C -*-  Get a database entry. */

/* Author: Brian J. Fox (bfox@ai.mit.edu) Wed May  8 17:48:07 1996.

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
#include "database.h"

static char *progname;

static void
usage (void)
{
  fprintf (stderr, "Usage: %s dbfile.db key-string\n", progname);
  exit (1);
}

int
main (int argc, char *argv[])
{
  DBFILE db;
  char *temp;

  progname = argv[0];
  if ((temp = strrchr (progname, '/')) != (char *)NULL)
    progname = temp + 1;

  if (argc < 2)
    usage ();

  db = database_open (argv[1], DB_WRITER);
  if (!db)
    {
      fprintf (stderr, "%s: Couldn't open `%s' to read a record\n",
	       progname, argv[1]);
    }
  else
    {
      DBOBJ *key = database_setkey (argv[2]);
      DBOBJ *content = database_fetch (db, key);

      if (content)
	{
	  char *string = (char *)malloc (2 + content->length);
	  memcpy (string, content->data, content->length);
	  string[content->length] = '\0';

	  fprintf (stdout, "%s\n", string);
	}
    }

  return (0);
}
