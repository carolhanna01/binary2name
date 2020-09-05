/* dl-test-core.c: -*- C -*-  Test dynamic loading on this system. */

/*  Copyright (c) 1998 Brian J. Fox
    Author: Brian J. Fox (bfox@ai.mit.edu) Wed Jul  8 12:05:37 1998.  */

#include <stdio.h>
#include <unistd.h>
#include <dlfcn.h>

typedef void Function (char *);

int necessary_symbol = 4;

int
main (int argc, char *argv[])
{
  void *handle = (void *)dlopen ("./dl-test-lib.so", RTLD_LAZY);

  if (handle == (void *)NULL)
    fprintf (stderr, "Error after dlopen(): %s\n", dlerror ());
  else
    {
      Function *func = (Function *)NULL;
      func = (Function *)dlsym (handle, "shared_lib_function");

      if (func == (Function *)NULL)
	{
	  fprintf (stderr, "Error after dlsym(): %s\n", dlerror ());
	  dlclose (handle);
	}
      else
	(*func) ("From within dl-test-core.c");
    }

  return (0);
}
