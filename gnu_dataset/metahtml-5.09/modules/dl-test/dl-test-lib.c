/* dl-test-lib.c: -*- C -*-  This becoms a dynamic library.. */

/*  Copyright (c) 1998 Brian J. Fox
    Author: Brian J. Fox (bfox@ai.mit.edu) Wed Jul  8 12:11:40 1998.  */

#include <stdio.h>
#include <unistd.h>
#include <stdlib.h>

extern int necessary_symbol;

void
shared_lib_function (char *arg)
{
  fprintf (stderr, "In dl-test-lib.so, got arg: `%s'\n", arg);
  fprintf (stderr, "In dl-test-lib.so, necessary_symbol: `%d'\n",
	   necessary_symbol);
}
