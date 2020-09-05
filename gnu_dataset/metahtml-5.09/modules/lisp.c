/* lisp.c: -*- C -*-  SOme functions for handling lists and alists. */

/*  Copyright (c) 1998 Brian J. Fox
    Author: Brian J. Fox (bfox@ai.mit.edu) Tue Mar  3 01:02:24 1998.

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

static void pf_cons (PFunArgs);
static void pf_list (PFunArgs);
static void pf_car (PFunArgs);
static void pf_cdr (PFunArgs);
static void pf_cadr (PFunArgs);
static void pf_cdar (PFunArgs);
static void pf_cddr (PFunArgs);
static void pf_caar (PFunArgs);
static void pf_assoc (PFunArgs);

static PFunDesc ftab[] =
{
  /*   tag	     complex? debug_level	   code    */
  { "CONS",		0,	 0,		pf_cons },
  { "LIST",		0,	 0,		pf_list },
  { "CAR",		0,	 0,		pf_car },
  { "CDR",		0,	 0,		pf_cdr },
  { "CADR",		0,	 0,		pf_cadr },
  { "CDAR",		0,	 0,		pf_cdar },
  { "CDDR",		0,	 0,		pf_cddr },
  { "CAAR",		0,	 0,		pf_caar },
  { "ASSOC",		0,	 0,		pf_assoc },
  { (char *)NULL,	0,	 0,		(PFunHandler *)NULL }
};

void
module_initialize (void)
{
  static int called = 0;

  if (!called)
    {
      register int i;
      Symbol *sym, *funcnames;

      called++;
      funcnames = symbol_intern ("modules::syms-of-lisp");

      /* Install the names and pointers. */
      for (i = 0; ftab[i].tag != (char *)NULL; i++)
	{
	  sym = symbol_intern_in_package (mhtml_function_package, ftab[i].tag);
	  symbol_add_value (funcnames, ftab[i].tag);
	  sym->type = symtype_FUNCTION;
	  sym->values = (char **)(&ftab[i]);
	}
      LEFT_BRACKET = '(';
      RIGHT_BRACKET = ')';
    }
}

static char *
get_lisp_positional_arg (Package *vars, int which)
{
  char *raw_arg = get_positional_arg (vars, which);
  char *final = (char *)NULL;

  if (raw_arg)
    {
      if (*raw_arg == '<')
	final = mhtml_evaluate_string (raw_arg);
      else if (*raw_arg == '\'')
	final = mhtml_evaluate_string (raw_arg + 1);
      else
	{
	  Symbol *sym = symbol_lookup (raw_arg);

	  if (sym == (Symbol *)NULL)
	    final = strdup (raw_arg);
	  else
	    {
	      switch (sym->type)
		{
		case symtype_STRING:
		  if (sym->values_index)
		    final = strdup (sym->values[0]);
		  break;

		case symtype_BINARY:
		  {
		    Datablock *d = (Datablock *)(sym->values);
		    if (d->length)
		      final = strdup (d->data);
		  }
		  break;

		default:
		  break;
		}
	    }
	}
    }
  return (final);
}

DEFINE_SECTION (LISP-MODULE, lisp;scheme;lists,
"The functions in this module treat their arguments substantially
differently than in the rest of Meta-HTML.  It is important to read
and understand the following paragraphs if you are to use this module.

If an argument begins with an open angle bracket (\"<\"), then it is
evaluated, and that is the argument that is passed to the lisp
function.  If an argument begins with a single quote (\"'\"), then the
value which immediately follows it is read using the lisp reader, and
the resultant value is passed to the lisp function.  Otherwise, the
argument is looked up as a variable name, and if it exists, the value
of that variable (either standard or binary) is what is passed to the
lisp function.  If the variable doesn't exist, then the variable name
is passed.

This is not optimal behavior (it allows for too much ambiguity), but
it can make writing programs which rely heavily on lisp-like syntax
much easier to read.", "")

DEFUN (pf_cons, &optional car cdr,
"Create a two element list with first element <var car> and second element
<var cdr>.  <var car> and <var cdr> default to the empty list \"()\" if not
specified.

Please see <funref lisp-module lisp-module-synopsis> for details of how
the arguments to this function are parsed.
<example>
<cons 'this 'that>            --> (\"this\" . \"that\")
<cons 'this <cons 'that '()>> --> (\"this\" \"that\")
</example>")
{
  char *arg1 = get_lisp_positional_arg (vars, 0);
  char *arg2 = get_lisp_positional_arg (vars, 1);
  WispObject *car, *cdr, *cons;
  char *result;

  if (arg1 == (char *)NULL) arg1 = strdup ("()");
  if (arg2 == (char *)NULL) arg2 = strdup ("()");
  
  car = wisp_from_string (arg1);
  cdr = wisp_from_string (arg2);

  if ((NIL_P (car)) && (NIL_P (cdr)))
    cons = NIL;
  else
    cons = make_cons (car, cdr);

  result = string_from_wisp (cons);

  bprintf_insert (page, start, "%s", result);

  free (arg1);
  free (arg2);
  gc_wisp_objects ();
}

DEFUN (pf_list, &rest objects,
"Return a lisp list consisting of the objects passed.

Please see <funref lisp-module lisp-module-synopsis> for details of how
the arguments to this function are parsed.

<example>
<list 'this 'that '(the other thing)>  --> (this that (the other thing))
</example>")
{
  register int i = 0;
  WispObject *list = NIL;
  char *arg, **array = (char **)NULL;
  int array_index = 0, array_slots = 0;

  while ((arg = get_lisp_positional_arg (vars, i)) != (char *)NULL)
    {
      if (array_index + 2 >= array_slots)
	array = (char **)
	  xrealloc (array, (array_slots += 10) * sizeof (char *));

      {
	char *new_elt = (char *)xmalloc (5 + strlen (arg));
	sprintf (new_elt, "\"%s\"", arg);
	free (arg);
	arg = new_elt;
      }

      array[array_index++] = arg;
      i++;
    }

  for (i = (array_index - 1); i > -1; i--)
    {
      WispObject *car = string_to_wisp (array[i]);
      free (array[i]);
      car = make_cons (car, list);
      list = car;
    }

  xfree (array);

  {
    char *result = string_from_wisp (list);
    int len = strlen (result);
    bprintf_insert (page, start, "%s", result);
    *newstart += len;
  }
  gc_wisp_objects ();
}

DEFUN (pf_car, list,
"Returns the first element of <var list>.
<example>
<car \"(this that and the other thing)\"> --> \"this\"
<car \"((this that) and more stuff)\">    --> (\"this\" \"that\")
</example>")
{
  char *string = get_lisp_positional_arg (vars, 0);
  WispObject *list = string ? wisp_from_string (string) : (WispObject *)NULL;
  char *result = (char *)NULL;

  if ((list != (WispObject *)NULL) &&
      (CONS_P (list)) && (list != NIL))
    result = string_from_wisp (CAR (list));

  if (result != (char *)NULL)
    {
      int len = strlen (result);
      bprintf_insert (page, start, "%s", result);
      *newstart = start + len;
    }

  gc_wisp_objects ();
  xfree (string);
}

DEFUN (pf_cdr, list,
"Returns everything bu the first element of <var list>.
<example>
<cdr \"(this that stuff)\">     --> (\"that\" \"stuff\")
<cdr \"((this that) stuff)\">   --> (\"stuff\")
</example>")
{
  char *string = get_lisp_positional_arg (vars, 0);
  WispObject *list = string ? wisp_from_string (string) : (WispObject *)NULL;
  char *result = (char *)NULL;

  if ((list != (WispObject *)NULL) &&
      (CONS_P (list)) && (list != NIL))
    result = string_from_wisp (CDR (list));

  if (result != (char *)NULL)
    {
      int len = strlen (result);
      bprintf_insert (page, start, "%s", result);
      *newstart = start + len;
    }

  gc_wisp_objects ();
  xfree (string);
}

DEFUN (pf_cadr, list,
"Returns the <funref lisp-module car> of the <funref lisp-module cdr> of
<var list>.
<example>
<cadr \"(this that stuff)\">     --> \"that\"
<cadr \"((this that) stuff)\">   --> \"stuff\"
</example>")
{
  char *string = get_lisp_positional_arg (vars, 0);
  WispObject *list = string ? wisp_from_string (string) : (WispObject *)NULL;
  char *result = (char *)NULL;

  if ((list != (WispObject *)NULL) &&
      ((LIST_P (list)) && (list != NIL)) &&
      ((CDR (list) != NIL) && (CONS_P (CDR (list)))))
    result = string_from_wisp (CADR (list));

  if (result != (char *)NULL)
    {
      int len = strlen (result);
      bprintf_insert (page, start, "%s", result);
      *newstart = start + len;
    }

  gc_wisp_objects ();
  xfree (string);
}

DEFUN (pf_cdar, list,
"Returns the <funref lisp-module cdr> of the <funref lisp-module car> of
<var list>.
<example>
<cdar \"(this that stuff)\">     -->
<cdar \"((this that) stuff)\">   --> (\"that\")
</example>")
{
  char *string = get_lisp_positional_arg (vars, 0);
  WispObject *list = string ? wisp_from_string (string) : (WispObject *)NULL;
  char *result = (char *)NULL;

  if ((list != (WispObject *)NULL) &&
      ((LIST_P (list)) && (list != NIL)) &&
      ((CAR (list) != NIL) && (CONS_P (CAR (list)))))
    result = string_from_wisp (CDAR (list));

  if (result != (char *)NULL)
    {
      int len = strlen (result);
      bprintf_insert (page, start, "%s", result);
      *newstart = start + len;
    }

  gc_wisp_objects ();
  xfree (string);
}

DEFUN (pf_cddr, list,
"Returns the <funref lisp-module cdr> of the <funref lisp-module cdr> of
<var list>.
<example>
<cddr \"(this that stuff)\">     --> (\"stuff\")
<cddr \"((this that) stuff)\">   --> ()
</example>")
{
  char *string = get_lisp_positional_arg (vars, 0);
  WispObject *list = string ? wisp_from_string (string) : (WispObject *)NULL;
  char *result = (char *)NULL;

  if ((list != (WispObject *)NULL) &&
      ((LIST_P (list)) && (list != NIL)) &&
      ((CDR (list) != NIL) && (CONS_P (CDR (list)))))
    result = string_from_wisp (CDDR (list));

  if (result != (char *)NULL)
    {
      int len = strlen (result);
      bprintf_insert (page, start, "%s", result);
      *newstart = start + len;
    }

  gc_wisp_objects ();
  xfree (string);
}

DEFUN (pf_caar, list,
"Returns the <funref lisp-module car> of the <funref lisp-module car> of
<var list>.
<example>
<caar \"(this that stuff)\">     --> 
<caar \"((this that) stuff)\">   --> \"this\"
</example>")
{
  char *string = get_lisp_positional_arg (vars, 0);
  WispObject *list = string ? wisp_from_string (string) : (WispObject *)NULL;
  char *result = (char *)NULL;

  if ((list != (WispObject *)NULL) &&
      ((LIST_P (list)) && (list != NIL)) &&
      ((CAR (list) != NIL) && (CONS_P (CAR (list)))))
    result = string_from_wisp (CAAR (list));

  if (result != (char *)NULL)
    {
      int len = strlen (result);
      bprintf_insert (page, start, "%s", result);
      *newstart = start + len;
    }

  gc_wisp_objects ();
  xfree (string);
}

static void
pf_assoc (PFunArgs)
{
  char *key = get_lisp_positional_arg (vars, 0);
  char *string = get_lisp_positional_arg (vars, 1);
  WispObject *list = string ? wisp_from_string (string) : NIL;
  char *result = (char *)NULL;

  if ((CONS_P (list)) && (list != NIL) && (CONS_P (CAR (list))))
    result = string_from_wisp (assoc (key, list));

  if (result != (char *)NULL)
    {
      int len = strlen (result);
      bprintf_insert (page, start, "%s", result);
      *newstart = *newstart + len;
    }
  gc_wisp_objects ();
  xfree (string);
}

#if defined (__cplusplus)
}
#endif
