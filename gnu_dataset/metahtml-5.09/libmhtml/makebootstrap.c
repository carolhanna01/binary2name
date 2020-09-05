/* makebootstrap.c: -*- C -*-  Standalone program creates a C file which
   when called installs packages and functions which were defined in the
   input files to this program. */

/*  Copyright (c) 1996 Brian J. Fox
    Author: Brian J. Fox (bfox@ai.mit.edu) Wed Jan 29 10:20:38 1997.  */

#include "mhtmlstd.h"

static void
usage (void)
{
  fprintf (stderr, "Usage: makebootstrap [bootstrap libraries ...]\n");
  exit (2);
}

static int bootstrap_code_len = 0;

static void
dump_library (char *filename)
{
  int fd = os_open (filename, O_RDONLY, 0666);

  if (fd > -1)
    {
      struct stat finfo;

      if (stat (filename, &finfo) != -1)
	{
	  char *buffer = (char *)malloc (1 + (int)finfo.st_size);
	  register int i;
	  int chars_this_line = 0;

	  read (fd, buffer, (size_t)finfo.st_size);

	  for (i = 0; i < finfo.st_size; i++)
	    {
	      /* Start a new line if that is the right thing. */

	      if (!chars_this_line)
		fprintf (stdout, "\n  ");

	      fprintf (stdout, "'\\%03o', ", (unsigned char)buffer[i]);

	      chars_this_line++;
	      if (chars_this_line == 9)
		{
		  fprintf (stdout, "\n");
		  chars_this_line = 0;
		}
	    }

	  free (buffer);
	  bootstrap_code_len += i;
	}
    }
}
	      
static void
open_library (void)
{
  fprintf (stdout, "char bootstrap_code[] = {\n");
}

static void
close_library (void)
{

  fprintf (stdout, " 0 };\nint bootstrap_code_len = %d;\n",
	   bootstrap_code_len);
}

int
main (int argc, char *argv[])
{
  int arg_index = 1;

  if (argc < 2)
    usage ();

  open_library ();

  while (arg_index < argc)
    dump_library (argv[arg_index++]);

  close_library ();
  exit (0);
}


  
