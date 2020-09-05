/* debugfuncs.c: -*- C -*-  Functions which facilitate debugging Meta-HTML. */

/*  Copyright (c) 1997 Brian J. Fox
    Author: Brian J. Fox (bfox@ai.mit.edu) Tue Jul 22 13:59:02 1997.

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

#include "language.h"

#if defined (__cplusplus)
extern "C"
{
#endif
/************************************************************/
/*							    */
/*		   Debugger Facilitation Functions	    */
/*							    */
/************************************************************/

static void pf_debugging_on (PFunArgs);
static void pf_page_debug (PFunArgs);
static void pf_debugging_output (PFunArgs);
static void pf_system_error_output (PFunArgs);

static PFunDesc func_table[] =
{
  { "DEBUGGING-ON",	0, 0, pf_debugging_on },
  { "PAGE-DEBUG",	0, 0, pf_page_debug },
  { "DEBUGGING-OUTPUT",	0, 0, pf_debugging_output },
  { "SYSTEM-ERROR-OUTPUT", 0, 0, pf_system_error_output },

  { (char *)NULL,	0, 0, (PFunHandler *)NULL }
};

PACKAGE_INITIALIZER (initialize_debugger_functions)
DEFINE_SECTION (DEBUGGING-COMMANDS, debug; variables; trouble-shooting,
"Debugging a CGI application executing under a running Web server can be
quite problematic.  <Meta-HTML> provides the following functions, macros,
and variables in an effort to alleviate the problems associated with this
situation.",
"In addition to the functions and variables above, <Meta-HTML> has a complete
source language debugger called <code>mdb</code>, which can be used to
interactively execute expressions, to place breakpoints in files and then
run them examining local and global variables, and to single-step through
source code that you have written in <Meta-HTML>.  Instructions on the
use of the debugger is beyong the scope of <i>this</i> manual -- please see
<b>MDB: A User's Guide to Debugging <Meta-HTML> Programs</b> for more
information specific to the debugger.")

DEFUN (pf_debugging_on, &optional function-name=level...,
"Turns on debugging for the <var function-name>s mentioned, setting
the level of output to <var level>.  <var level> is a number between
<code>0</code> (the least amount of debugging info) and
<code>10</code> (the maximum amount of debugging info).

The output is placed into the <Meta-HTML> internal debugger buffer, and
can be placed into an output page by simply placing the tag
<tag DEBUGGING-OUTPUT> somewhere in the page, or can be explicity
retrieved using <example code><debugging-output retrieve></example>.")
{
  if (vars)
    {
      register int i;
      Symbol **symbols = symbols_of_package (vars);
      Symbol *sym;

      for (i = 0; (sym = symbols[i]) != (Symbol *)NULL; i++)
	{
	  mhtml_set_debugging_on (sym);
	}
    }
}

DEFUN (pf_page_debug, &rest body,
"Cause <var body> to be inserted into the debugger output.

The output is placed into the <Meta-HTML> internal debugger buffer, and
can be placed into an output page by simply placing the tag
<tag DEBUGGING-OUTPUT> somewhere in the page, or can be explicity
retrieved using <example code><debugging-output retrieve></example>.")
{
  char *value;

  value = mhtml_evaluate_string (body->buffer ? body->buffer : "");
  if (!empty_string_p (value))
    page_debug ("%s", value);
  if (value) free (value);
}

DEFUN (pf_debugging_output, &optional action...,
"<Meta-HTML> stores debugging information in an internal buffer.  You
may directly place information into this buffer using the <funref
language-operators page-debug> command, and you may retrieve or clear
this buffer using <example code><debugging-output></example>.

Possible values for <var action> are:
<ul>
<li> <b>retrieve</b><br>
Inserts the current contents of the debugging buffer into the page at
the current location.
<li> <b>clear</b><br>
Empties the debugging buffer of all stored information.
</ul>

If you place  <example code><debugging-output></example> into your
page without passing any arguments, <Meta-HTML> treats this invocation
specially; it marks the location at which any debugging statements
which have been collected during the processing of the entire page
should be placed.

We recommend that you always place this tag somewhere in the output
page, whenever that output is an HTML document, as opposed to a
standalone script.")
{
  register int i = 0;
  char *text;
  static char *token_name = "<DEBUGGING-OUTPUT>";

  while ((text = get_positional_arg (vars, i)) != (char *)NULL)
    {
      char *arg = mhtml_evaluate_string (text);

      if (!empty_string_p (arg))
	{
	  if (strcasecmp (arg, "clear") == 0)
	    {
	      page_debug_clear ();
	    }
	  else if (strcasecmp (arg, "retrieve") == 0)
	    {
	      char *contents = page_debug_buffer ();

	      if (contents != (char *)NULL)
		{
		  bprintf_insert (page, start, "%s", contents);
		  *newstart = start + strlen (contents) - 1;
		}
	    }
	}
      xfree (arg);
      i++;
    }

  if (i == 0)
    {
      bprintf_insert (page, start, token_name);
      *newstart = start + strlen (token_name) - 1;
    }
}

DEFUN (pf_system_error_output, &optional action...,
"<Meta-HTML> stores system error information in an internal buffer.
Such information may be the results of an error which occurred during
the execution of an external command (such as with <funref
file-operators cgi-exec>), or other information which is generated by
the inability of <Meta-HTML> to access a particular file (such as with
the <funref file-operators require> command).

You may retrieve or clear this buffer using <example
code><system-error-output></example>.

Possible values for <var action> are:
<ul>
<li> <b>retrieve</b><br>
Inserts the current contents of the system error buffer into the page at
the current location.
<li> <b>clear</b><br>
Empties the system error buffer of all stored information.
</ul>

If you place  <example code><system-error-output></example> into your
page without passing any arguments, <Meta-HTML> treats this invocation
specially; it marks the location at which any system errors
which have been collected during the processing of the entire page
should be placed.

We recommend that you always place this tag somewhere in the output
page, whenever that output is an HTML document, as opposed to a
standalone script.")
{
  register int i = 0;
  char *text;
  static char *token_name = "<SYSTEM-ERROR-OUTPUT>";

  while ((text = get_positional_arg (vars, i)) != (char *)NULL)
    {
      char *arg = mhtml_evaluate_string (text);

      if (!empty_string_p (arg))
	{
	  if (strcasecmp (arg, "clear") == 0)
	    {
	      page_syserr_clear ();
	    }
	  else if (strcasecmp (arg, "retrieve") == 0)
	    {
	      char *contents = page_syserr_buffer ();

	      if (contents != (char *)NULL)
		{
		  bprintf_insert (page, start, "%s", contents);
		  *newstart = start + strlen (contents) - 1;
		}
	    }
	}
      xfree (arg);
      i++;
    }

  if (i == 0)
    {
      bprintf_insert (page, start, token_name);
      *newstart = start + strlen (token_name) - 1;
    }
}

#if defined (__cplusplus)
}
#endif
