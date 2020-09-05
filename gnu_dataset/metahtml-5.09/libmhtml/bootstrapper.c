/* bootstrapper.c: -*- C -*-  Load Meta-HTML defuns from bootstrap_code. */

/*  Copyright (c) 1996 Brian J. Fox
    Author: Brian J. Fox (bfox@ai.mit.edu) Wed Jan 29 10:49:30 1997.  */

#include "language.h"
#include "symdump.h"

#if defined (__cplusplus)
extern "C"
{
#endif

extern unsigned char bootstrap_code[];
extern int bootstrap_code_len;

void
bootstrap_metahtml (int call_initializer_p)
{
  if (bootstrap_code_len)
    {
      int sd = symdump_open_string_data ();
      void *ignore = (void *)xmalloc (4 * bootstrap_code_len);
      free (ignore);

      if (sd != -1)
	{
	  Package *pack;
	  char *temp;

	  symdump_write_string_data
	    (sd, bootstrap_code_len, &bootstrap_code[0]);
	  symdump_seek_string_data (sd, 0);
	  symdump_set_string_data_buffer_size (sd, bootstrap_code_len);
	  while ((pack = symbol_load_package (sd)) != (Package *)NULL);
	  if (mhtml_user_keywords == (Package *)NULL)
	    mhtml_user_keywords =
	      symbol_get_package_hash ("*user-functions*", 577);
	  bprintf_free_buffer (symdump_close_string_data (sd));

	  /* Always call <bootstrapper::system-initialize>. */
	  temp = mhtml_evaluate_string ("<bootstrapper::system-initialize>");
	  xfree (temp);

	  /* Optionally call <bootstrapper::initialize> */
	  if (call_initializer_p)
	    {
	      temp = mhtml_evaluate_string ("<bootstrapper::initialize>");
	      xfree (temp);
	    }
	}
    }
}

#if defined (__cplusplus)
}
#endif
