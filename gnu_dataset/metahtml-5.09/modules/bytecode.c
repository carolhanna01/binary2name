/* bytecode.c: -*- C -*-  First cut at byte code executor. */

/*  Copyright (c) 1998 Brian J. Fox
    Author: Brian J. Fox (bfox@ai.mit.edu) Mon Sep 14 00:09:07 1998

    This file is part of <Meta-HTML>(tm), a system for the rapid
    deployment of Internet and Intranet applications via the use of
    the Meta-HTML language.

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

#include "modules.h"

#if defined (__cplusplus)
extern "C"
{
#endif

static void pf_bytecode (PFunArgs);

static PFunDesc ftab[] =
{
  /*   tag           complex? debug_level          code    */
  { "%%BYTECODE",          0,       0,             pf_bytecode },
  { (char *)NULL,       0,       0,             (PFunHandler *)NULL }
};

MODULE_INITIALIZE ("bytecode", ftab)

DEFINE_SECTION (BYTECODE-EXECUTION, bytecode; compiler,
"A first cut at handling bytecodes in <Meta-HTML>.", "")

DEFUNX (pf_%%bytecode, code-string,
"Execute the bytecode in <var code-string>")

static int
run_bytecode (char *codestring, char **outputp, int *output_len)
{
  WORKING HERE;
}

static void
pf_bytecode (PFunArgs)
{
  char *codestring = mhtml_evaluate_string (get_positional_arg (vars, 0));
  char *output = (char *)NULL;
  int length = 0;
  int error_code = run_bytecode (codestring, &output, &length);

  if (error_code == 0)
    {
      if (length != 0)
	{
	  bprintf_insert_binary (page, start, output, length);
	  free (output);
	}
    }
  xfree (codestring);
}

#if defined (__cplusplus)
}
#endif
