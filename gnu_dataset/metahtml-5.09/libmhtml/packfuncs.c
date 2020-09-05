/* packfuncs.c: -*- C -*-  Functions which manipulate packages. */

/*  Copyright (c) 1997 Brian J. Fox
    Author: Brian J. Fox (bfox@ai.mit.edu) Tue Jul 22 22:01:08 1997.

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

static void pf_package_names (PFunArgs);
static void pf_package_vars (PFunArgs);
static void pf_package_delete (PFunArgs);
static void pf_in_package (PFunArgs);
static void pf_with_local_package (PFunArgs);
static void pf_package_to_alist (PFunArgs);
static void pf_alist_to_package (PFunArgs);
static void pf_alist_package_names (PFunArgs);
static void pf_alist_package_delete (PFunArgs);

/* The "convenience" functions follow. */
static void pf_alist_get_var (PFunArgs);
static void pf_alist_set_var (PFunArgs);
static void pf_alist_unset_var (PFunArgs);
static void pf_alist_var_exists (PFunArgs);
static void pf_alist_defvar (PFunArgs);

/************************************************************/
/*							    */
/*		  Package Manipulation Functions	    */
/*							    */
/************************************************************/

static PFunDesc func_table[] =
{
  { "PACKAGE-NAMES",		0, 0, pf_package_names },
  { "PACKAGE-VARS",		0, 0, pf_package_vars },
  { "PACKAGE-DELETE",		0, 0, pf_package_delete },
  { "IN-PACKAGE",		1, 0, pf_in_package },
  { "WITH-LOCAL-PACKAGE",	1, 0, pf_with_local_package },
  { "PACKAGE-TO-ALIST",		0, 0, pf_package_to_alist },
  { "ALIST-TO-PACKAGE",		0, 0, pf_alist_to_package },

  { "ALIST-PACKAGE-NAMES",	0, 0, pf_alist_package_names },
  { "ALIST-PACKAGE-DELETE",	0, 0, pf_alist_package_delete },
  { "ALIST-GET-VAR",		0, 0, pf_alist_get_var },
  { "ALIST-SET-VAR",		0, 0, pf_alist_set_var },
  { "ALIST-DEFVAR",		0, 0, pf_alist_defvar },
  { "ALIST-UNSET-VAR",		0, 0, pf_alist_unset_var },
  { "ALIST-VAR-EXISTS",		0, 0, pf_alist_var_exists },

  { (char *)NULL,		0, 0, (PFunHandler *)NULL }
};

PACKAGE_INITIALIZER (initialize_package_functions)
DEFINE_SECTION (PACKAGES, variables; module; package; group, 
"<i>Packages</i> are repositories which contain symbols and their values.

Each time a symbol is referenced, a package must first be found, and
then the symbol may be found within that package. You indicate which
package to look the symbol up in by supplying a package <i>prefix</i>.
When the prefix is not supplied, the symbol is looked up in the
current package.",
"For example, a full reference to the symbol <var bar> stored in the
<var foo> package looks like: 

<example>
   FOO::BAR
</example>

There are very few commands specifically for dealing with packages,
because most of the operations are performed implicitly, rather than
explicitly.  To create a package, simply give the package name as part
of the symbol, in the place where the symbol is normally used.

<example>
   <set-var foo::bar = \"Hello\">
</example>

This has the effect of creating the package <var foo> if it didn't
already exist.

The majority of the functions documented here perform package maintenance, as
opposed to variable manipulation.  There are functions for querying a package
about its contens, for deleting an entire package, for exporting and importing
packages to and from sessions, for copying the contents of packages, and for
converting packages from an internal representation to a printable
representation, called an <i>alist</i>.")

DEFUN (pf_package_names, ,
"Return a newline separated list of all of the named packages
which are currently defined.  Because the list is newline separated,
the result can easily be assigned to an array variable:

<example>
<set-var all-packages[]=<package-names>>
</example>")
{
  if (AllPackages)
    {
      register int i;
      Package *pack;

      for (i = 0; (pack = AllPackages[i]) != (Package *)NULL; i++)
	if (pack->name != (char *)NULL)
	  {
	    bprintf_insert (page, start, "%s\n", pack->name);
	    start += strlen (pack->name) + 1;
	  }

      *newstart = start;
    }
}

DEFUN (pf_package_vars, &optional package-name &key strip=true,
"Returns a newline separated list of the fully qualified variable
names found in the package named by <var package-name>, or in the
current package if <var package-name> is not given.  When <var
strip=true> is supplied, the returned variable names have the package
prefix stripped off, making them <i>not</i> fully qualified.  The
names are not returned in any significant order.  Because the list is
newline separated, the results can easily be assigned to an array
variable:

<complete-example>
<set-var foo::bar=baz>
<set-var foo::baz=bar>
<set-var names[]=<package-vars foo>>
<get-var names[1]>
</complete-example>")
{
  register int pos = 0;
  char *strip = get_value (vars, "STRIP");
  char *name;

  if ((CurrentPackage != (Package *)NULL) &&
      (get_positional_arg (vars, 0) == (char *)NULL))
    {
      Symbol **symbols = symbols_of_package (CurrentPackage);

      if (symbols != (Symbol **)NULL)
	{
	  register int i;

	  for (i = 0; symbols[i] != (Symbol *)NULL; i++)
	    {
	      bprintf_insert (page, start, "%s\n", symbols[i]->name);
	      start += symbols[i]->name_len + 1;
	    }

	  free (symbols);
	}
    }

  while ((name = get_positional_arg (vars, pos)) != (char *)NULL)
    {
      Package *pack = (Package *)NULL;

      pos++;

      name = mhtml_evaluate_string (name);

      if (!empty_string_p (name))
	pack = symbol_lookup_package (name);

      if (pack)
	{
	  Symbol **symbols = symbols_of_package (pack);

	  if (symbols != (Symbol **)NULL)
	    {
	      register int i;

	      for (i = 0; symbols[i] != (Symbol *)NULL; i++)
		{
		  if (((pack->name != (char *)NULL) && (pack->name[0] != '\0'))
		      && (strip == (char *)NULL))
		    {
		      bprintf_insert (page, start, "%s::%s\n",
				      pack->name,
				      symbols[i]->preserved_name ?
				      symbols[i]->preserved_name :
				      symbols[i]->name);
		      start += pack->name_len + 3 + symbols[i]->name_len;
		    }
		  else
		    {
		      bprintf_insert (page, start, "%s\n",
				      symbols[i]->preserved_name ?
				      symbols[i]->preserved_name :
				      symbols[i]->name);
		      start += symbols[i]->name_len + 1;
		    }
		}

	      free (symbols);
	    }
	}

      if (name) free (name);
    }

  *newstart = start;
}

DEFUN (pf_package_delete, package-name...,
"Remove the definition of the packages named by <var package-name>s,
and all of the variables defined within them.")
{
  char **names = get_vars_names (vars);

  if (names != (char **)NULL)
    {
      register int i;

      for (i = 0; names[i] != (char *)NULL; i++)
	{
	  char *name = names[i];

	  name = mhtml_evaluate_string (name);

	  if (name)
	    {
	      pagefunc_destroy_package (name);
	      free (name);
	    }
	}
    }
}

DEFMACRO (pf_in_package, package-name,
"Evaluate <var body> in an environment where variables which are not
specifically prefixed with a package name are looked up and stored
within <var package-name>.

The special package name <code>\"local\"</code> creates an anonymous
package within which to work.  The contents of local packages are only
accessible within the expressions surrounded by the
<code>in-package</code> operator.")
{
  int jump_again = 0;
  char *packname = mhtml_evaluate_string (get_positional_arg (vars, 0));
  char *result = (char *)NULL;

  if (empty_string_p (packname))
    {
      if (packname) free (packname);
      packname = strdup ("DEFAULT");
    }

  if (strcasecmp (packname, "local") == 0)
    {
      free ((char *)packname);
      packname = (char *)NULL;
    }

  {
    PageEnv *page_environ = pagefunc_save_environment ();

    symbol_push_package (symbol_get_package (packname));

    if ((jump_again = setjmp (page_jmp_buffer)) == 0)
      result = mhtml_evaluate_string (body->buffer);

    symbol_pop_package ();

    pagefunc_restore_environment (page_environ);
  }

  if (result != (char *)NULL)
    {
      if (jump_again == 0)
	bprintf_insert (page, start, "%s", (char *)result);
      free ((char *)result);
    }

  if (packname) free (packname);
  if (jump_again) longjmp (page_jmp_buffer, 1);
}

DEFMACRO (pf_with_local_package, ,
"Shorthand for <example code><in-package local> <i>body</i>
</in-package></example>")
{
  int jump_again = 0;
  char *result = (char *)NULL;

  {
    PageEnv *page_environ = pagefunc_save_environment ();

    symbol_push_package (symbol_get_package ((char *)NULL));

    if ((jump_again = setjmp (page_jmp_buffer)) == 0)
      result = mhtml_evaluate_string (body->buffer);

    symbol_pop_package ();
    pagefunc_restore_environment (page_environ);
  }

  if (result != (char *)NULL)
    {
      if (jump_again == 0)
	bprintf_insert (page, start, "%s", (char *)result);
      free ((char *)result);
    }
  if (jump_again) longjmp (page_jmp_buffer, 1);
}

DEFUN (pf_package_to_alist, &optional package,
"Returns a Lisp readable string containing the names and values of the variables in <var package>.  If <var strip=true> is supplied, the package name is removed from the variables before placing them in the list.  See the following code sequence:

<complete-example>
<set-var
  foo::bar=baz
  foo::array[0]=Elt-0
  foo::array[1]=Elt-1>

  The contents of Foo: <package-to-alist foo>
The stripped contents: <package-to-alist foo strip=true>
</complete-example>")
{
  char *packname = mhtml_evaluate_string (get_positional_arg (vars, 0));
  char *strip = get_value (vars, "STRIP");
  char *result = (char *)NULL;
  Package *package = (Package *)NULL;

  if (!empty_string_p (packname))
    package = symbol_lookup_package (packname);
  else
    package = CurrentPackage;

  if (packname != (char *)NULL) free (packname);

  if (package != (Package *)NULL)
    result = package_to_alist (package, (strip != (char *)NULL));

  if (result)
    {
      bprintf_insert (page, start, "%s", result);
      *newstart += strlen (result);
      free (result);
    }
}

DEFUN (pf_alist_to_package, alist &optional package,
"Takes the textual list representation of a package, and creates (or
modifies) the package named by <var package-name>.

<code>alist-to-package</code> is the inverse of the <funref packages
package-to-alist> function -- given an \"alist\" (short for
`association list') you can create a package, and vice-versa.  The
following expression is one way to copy all of the variables from the
package <code>FOO</code> into the package <code>BAR</code>:

<example>
<alist-to-package <package-to-alist foo> bar>
</example>")
{
  char *alist = mhtml_evaluate_string (get_positional_arg (vars, 0));
  char *packname = mhtml_evaluate_string (get_positional_arg (vars, 1));

  if (!empty_string_p (alist))
    {
      Package *from = (Package *)NULL;
      Package *to = (Package *)NULL;

      from = alist_to_package (alist);

      if (!empty_string_p (packname))
	to = symbol_get_package (packname);
      else
	to = CurrentPackage;

      if (from && to)
	{
	  Symbol **symbols = symbols_of_package (from);

	  if (symbols != (Symbol **)NULL)
	    {
	      register int i;
	      Symbol *sym, *copy;

	      for (i = 0; (sym = symbols[i]) != (Symbol *)NULL; i++)
		{
		  char *sym_name = sym->name;
		  char *temp = strstr (sym_name, "::");

		  if (temp)
		    sym_name = temp + 2;

		  copy = symbol_copy (sym, to);
		  if (temp)
		    symbol_rename (copy, sym_name);
		}
	      free (symbols);
	    }
	}

      symbol_destroy_package (from);
    }

  xfree (alist);
  xfree (packname);
}

static Package *
get_alist_var (Package *vars)
{
  char *varname = mhtml_evaluate_string (get_positional_arg (vars, 0));
  Package *resultpack = (Package *)NULL;

  if (!empty_string_p (varname))
    {
      register int i;
      char *alist_text;

      for (i = 0; whitespace (varname[i]); i++);

      if (varname[i] == '(')
	alist_text = varname + i;
      else
	alist_text = pagefunc_get_variable (varname);

      if (empty_string_p (alist_text))
	resultpack = symbol_get_package ((char *)NULL);
      else
	resultpack = alist_to_package (alist_text);
    }

  xfree (varname);
  return (resultpack);
}
    
static void
set_alist_var (Package *vars, Package *value)
{
  char *varname = mhtml_evaluate_string (get_positional_arg (vars, 0));

  if (!empty_string_p (varname))
    {
      char *alist_text = package_to_alist (value, 0);
      pagefunc_set_variable (varname, alist_text);
      xfree (alist_text);
    }

  xfree (varname);

  if (value != (Package *)NULL)
    symbol_destroy_package (value);
}

DEFUN (pf_alist_defvar, alistvar name value,
"<b>DEF</b>ault the value of the <b>VAR</b>iable named by <var name>
to <var value>, in the association list referenced by <alistvar>.

<code>defvar</code> assigns <var value> to <var name> if, and only if,
<var name> has a non-empty value.")
{
  Package *alist = get_alist_var (vars);
  char *name = mhtml_evaluate_string (get_positional_arg (vars, 1));

  if ((alist != (Package *)NULL) && (!empty_string_p (name)))
    {
      char *current_value = forms_get_tag_value_in_package (alist, name);

      if (empty_string_p (current_value))
	{
	  char *new_value = mhtml_evaluate_string (get_positional_arg
						   (vars, 2));
	  if (!empty_string_p (new_value))
	    forms_set_tag_value_in_package (alist, name, new_value);
	  xfree (new_value);
	}

      set_alist_var (vars, alist);
    }
  else if (alist != (Package *)NULL)
    symbol_destroy_package (alist);

  xfree (name);
}

DEFUN (pf_alist_unset_var, alistvar &rest names...,
"Make <var name>s be non-existent in the association list specified
by <var alistvar>.")
{
  Package *alist = get_alist_var (vars);
  register int i;
  char *name;

  if (alist != (Package *)NULL)
    {
      for (i = 1; (name = get_positional_arg (vars, i)) != (char *)NULL; i++)
	{
	  char *varname = mhtml_evaluate_string (name);

	  if (!empty_string_p (varname))
	    {
	      Symbol *sym = symbol_remove_in_package (alist, varname);
	      if (sym) symbol_free (sym);
	    }

	  xfree (varname);
	}

      set_alist_var (vars, alist);
    }
}

DEFUN (pf_alist_var_exists, alistvar name,
"<code>var-exists</code> checks for the <i>existence</i> of
the variable named by <var varname>, in the association list specified by
<var alistvar>, and returns <code>true</code> if that variable does in
fact exist.

The existence of a variable has nothing to do with its value -- a
variable exists if it is present within the list, whether or not it
has a value.")
{
  Package *alist = get_alist_var (vars);
  char *arg = mhtml_evaluate_string (get_positional_arg (vars, 1));
  int set_p = 0;

  if ((!empty_string_p (arg)) &&
      (alist != ((Package *)NULL)) &&
      (symbol_lookup_in_package (alist, arg) != (Symbol *)NULL))
    set_p++;

  xfree (arg);

  if (set_p)
    {
      bprintf_insert (page, start, "true");
      *newstart += 4;
    }

  symbol_destroy_package (alist);
}

DEFUN (pf_alist_set_var, alistvar &optional name=value...,
"Gives the variable <var name> the value of <var value> in the association
list specified by <var alistvar>. Any number of name/value pairs may be given,
and whitespace is not significant.  Where <var =value> is omitted, the
value is the empty string.

<example>
<alist-set-var myalist foo=bar bar=baz>
<alist-get-var myalist foo>    --> bar
<alist-get-var myalist bar>    --> baz
<alist-get-var myalist <alist-get-var myalist foo>>    --> baz
</example>")
{
  Package *alist = get_alist_var (vars);
  char *func = "alist-set-var";

  if ((alist != (Package *)NULL))
    {
      char **names = get_vars_names (vars);
      char **vals = get_vars_vals (vars);

      if (names != (char **)NULL)
	{
	  register int i;
	  char *sym_name;

	  for (i = 1; (sym_name = names[i]) != (char *)NULL; i++)
	    {
	      char *name = mhtml_evaluate_string (sym_name);
	      char *value = vals[i];
	      int free_value = 0;

	      if (debug_level >= 5)
		{
		  if (value)
		    page_debug ("<%s \"%s\"=\"%s\">", func, sym_name, value);
		  else
		    page_debug ("<%s \"%s\">", func, sym_name);
		}

	      if (value == (char *)NULL)
		{
		  if (debug_level)
		    page_debug ("<%s %s ...> missing `='", func, sym_name);
		}
	      else
		{
		  value = mhtml_evaluate_string (value);
		  if (value) free_value++;
		}

	      if (debug_level >= 6)
		page_debug ("--> <%s \"%s\"=\"%s\">",
			    func, name ? name : "", value ? value : "");

	      if (name)
		forms_set_tag_value_in_package (alist, name, value);

	      if (free_value) free (value);
	      if (name != sym_name) free (name);
	    }
	}

      set_alist_var (vars, alist);
    }
}

DEFUN (pf_alist_get_var, alistvar &optional name...,
"Return the value of the <var name>s given from the association list
specified by <var alistvar>.  Each <var name> is a
variable name which has had a value assigned to it with <funref
variables alist-set-var>, or was created implicity via
<funref packages alist-to-package>.

The values are returned in the order in which the <var name>s appear.")
{
  Package *alist = get_alist_var (vars);
  register int i;
  char *name;

  for (i = 1; (name = get_positional_arg (vars, i)) != (char *)NULL; i++)
    {
      char *insertion;
      char *value;

      insertion = mhtml_evaluate_string (name);
      value = forms_get_tag_value_in_package (alist, insertion);

      if (debug_level > 5)
	page_debug ("<get-var \"%s\">", insertion ? insertion : "");

      if (value)
	{
	  int len = strlen (value);
	  bprintf_insert_binary (page, start, value, len);
	  start += len;
	}
      else
	{
	  if (debug_level > 9)
	    page_debug ("<get-var \"%s\">: Unbound Variable \"%s\"!",
			insertion, insertion);
	}

      if (debug_level > 5)
	page_debug ("--> `%s'", value ? value : "");

      xfree (insertion);

      *newstart = start;
    }

  symbol_destroy_package (alist);
}

DEFUN (pf_alist_package_names, alistvar,
"Return a newline separated list of all of the packages which are
defined within the association list stored within <var alistvar>.
Because the list is newline separated, the result can easily be
assigned to an array variable:")
{
  Package *alist = get_alist_var (vars);

  if (alist != (Package *)NULL)
    {
      register int i;
      Symbol **syms = symbols_of_package (alist);
      char **values = (char **)NULL;
      int values_slots = 0;
      int values_index = 0;

      for (i = 0; syms[i] != (Symbol *)NULL; i++)
	{
	  char *name = syms[i]->name;
	  char *dots = name ? strstr (name, "::") : (char *)NULL;

	  if (dots != (char *)NULL)
	    {
	      register int j, found = 0;
	      int len = (int) (dots - name);

	      for (j = 0; j < values_index; j++)
		if (strncmp (values[j], name, len) == 0)
		  {
		    found = 1;
		    break;
		  }

	      if (!found)
		{
		  char *pack = (char *)xmalloc (1 + len);

		  strncpy (pack, name, len);
		  pack[len] = '\0';

		  if (values_index + 2 > values_slots)
		    values = (char **)xrealloc
		      (values, (values_slots += 10) * sizeof (char *));

		  values[values_index++] = pack;
		  values[values_index] = (char *)NULL;
		}
	    }
	}

      if (values != (char **)NULL)
	{
	  for (i = 0; i < values_index; i++)
	    {
	      bprintf_insert (page, start, "%s\n", values[i]);
	      start += 1 + strlen (values[i]);
	      free (values[i]);
	    }

	  free (values);
	  *newstart = start;
	}

      symbol_destroy_package (alist);
    }
}

DEFUN (pf_alist_package_delete, alistvar &rest packages[],
"For each variable in the association list within <var alistvar>, remove
the variable from the association list if it is prefixed with one of the
specifed package names.  For example, given that the variable <var alist>
contained the alist:

<example>
((\"FOO::BAR\" . \"bar\") (\"FOO::BAZ\" . \"baz\") (\"BAR::X\" . \"val\"))
</example>

then calling:

<example>
<alist-package-delete alist foo>
<get-var-once alist>
</example>

produces:

<example>
((\"BAR::X\" . \"val\"))
</example>")
{
  Package *alist = get_alist_var (vars);

  if (alist != (Package *)NULL)
    {
      Symbol **syms = symbols_of_package (alist);

      if (syms != (Symbol **)NULL)
	{
	  register int i, j;
	  char **packnames = (char **)NULL;
	  int packnames_index = 0;
	  int packnames_slots = 0;
	  char *arg;

	  /* Collect the names of the packages passed as parameters. */
	  for (i = 1;
	       (arg = (mhtml_evaluate_string (get_positional_arg (vars, i))))
		 != (char *)NULL;
	       i++)
	    {
	      /* Force to all upper case. */
	      for (j = 0; arg[j] != '\0'; j++)
		if (islower (arg[j]))
		  arg[j] = toupper (arg[j]);

	      /* Append it to our list of package names to check. */
	      if (packnames_index + 2 > packnames_slots)
		packnames = (char **)xrealloc
		  (packnames, (packnames_slots += 10) * sizeof (char *));

	      packnames[packnames_index++] = arg;
	      packnames[packnames_index] = (char *)NULL;
	    }

	  /* Okay, have all of the arguments.  Now figure out which variables
	     to remove. */
	  for (i = 0; syms[i] != (Symbol *)NULL; i++)
	    {
	      char *name = syms[i]->name;
	      char *dots = name ? strstr (name, "::") : (char *)NULL;

	      if (dots != (char *)NULL)
		{
		  int len = (int) (dots - name);

		  for (j = 0; j < packnames_index; j++)
		    if (strncmp (packnames[j], name, len) == 0)
		      {
			Symbol *sym = symbol_remove_in_package (alist, name);
			symbol_free (sym);
		      }
		}
	    }

	  /* All variables removed.  Replace the package. */
	  set_alist_var (vars, alist);

	  /* Free the memory used to store the package names. */
	  for (i = 0; i < packnames_index; i++)
	    free (packnames[i]);
	  xfree (packnames);
	}
      else
	symbol_destroy_package (alist);
    }
}

#if defined (__cplusplus)
}
#endif

