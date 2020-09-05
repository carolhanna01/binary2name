/* arrayfuncs.c: -*- C -*-  Functions specifically for operating on arrays. */

/* Author: Brian J. Fox (bfox@ai.mit.edu) Sat Jul 20 17:22:47 1996.

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

static void pf_array_size (PFunArgs);
static void pf_array_member (PFunArgs);
static void pf_array_append (PFunArgs);
static void pf_array_add_unique (PFunArgs);
static void pf_array_shift (PFunArgs);
static void pf_foreach (PFunArgs);
static void pf_array_concat (PFunArgs);
static void pf_sort (PFunArgs);

static PFunDesc func_table[] =
{
  /*   tag	     complex? debug_level	   code    */
  { "ARRAY-SIZE",	0,	0,		pf_array_size },
  { "ARRAY-MEMBER",	0,	0,		pf_array_member },
  { "ARRAY-APPEND",	0,	0,		pf_array_append },
  { "ARRAY-ADD-UNIQUE",	0,	0,		pf_array_add_unique },
  { "ARRAY-SHIFT",	0,	0,		pf_array_shift },
  { "ARRAY-CONCAT",	0,	0,		pf_array_concat },
  { "FOREACH",		1,	0,		pf_foreach },
  { "SORT",		0,	0,		pf_sort },
  { (char *)NULL,	0,	0,		(PFunHandler *)NULL }
};

PACKAGE_INITIALIZER (initialize_array_functions)
DEFINE_SECTION (ARRAYS, variables; arrays; multiple values, 
"<meta-html> allows the use of <i>array</i> variables as well as single element variables.  In fact, all string variables in <meta-html> can be treated as array variables -- there is no special command for creating such variables.
",
"Array variable values are referenced by placing the array index directly after the variable name, enclosed in square brackets (<b>[</b> and <b>]</b>).  Thus, a reference to the 3rd element of the array <var foo> looks like:

<example>
   foo[3]
</example>

When an array reference is made without any containing index, the reference refers to the entire array.  So, to get the value of the entire array stored in <var foo>, you would write:

<example>
   <get-var foo[]>
</example>

In order to ease the writing of array references which rely on a variable index, a variable name seen as an array reference index is automatically looked up as if you had written <example code><get-var VAR></example>.  Finally, multiple values may be given in a <funref variables set-var> command by separating those values with newline characters.  The following sequence of commands illustrates the typical use of array variables.

<complete-example>
<set-var array[] =
       \"value-zero
        value-one
        value-two
        value-three\">
    <set-var i=0>
    <while <get-var array[i]>>
      The value of array[<get-var i>] is `<get-var array[i]>'.<br>
      <increment i>
    </while>
</complete-example>
")

DEFUN (pf_array_size, arrayvar,
"Returns the number of elements in the array referenced by the
variable <var arrayvar>.

Examples:

<complete-example>
<set-var array[]=\"this\">
<array-size array>
</complete-example>

and,

<complete-example>
<array-shift 4 array>
<array-size array>
</complete-example>")
{
  char *array_name = mhtml_evaluate_string (get_positional_arg (vars, 0));
  int result = 0;

  if (!empty_string_p (array_name))
    {
      Symbol *sym = symbol_lookup (array_name);

      if ((sym != (Symbol *)NULL) && (sym->type == symtype_STRING))
	result = sym->values_index;
    }

  bprintf_insert (page, start, "%d", result);
  xfree (array_name);
}

DEFUN (pf_array_member, item arrayvar &key caseless=true,
"Look up (and return) the index of <var item> in the contents of the
array referenced by <var arrayvar>.

If <var item> is not found, then <code>array-member</code> returns the
empty string.

If <var caseless> is non-empty, then the comparison is done without
regard to character case.  Otherwise, character case is significant in
the location of the item.

<complete-example>
<set-var array[] =
  <prog
     this
     another
     multi word
     thing>>
<array-member \"multi word\" array>
</complete-example>")
{
  char *item = mhtml_evaluate_string (get_positional_arg (vars, 0));
  char *array = mhtml_evaluate_string (get_positional_arg (vars, 1));
  int result = -1;

  if (!empty_string_p (array))
    {
      char **values = symbol_get_values (array);

      if (values != (char **)NULL)
	{
	  register int i;
	  char *caseless;
	  int caseless_p = 0;

	  caseless = mhtml_evaluate_string (get_value (vars, "caseless"));
	  if (!empty_string_p (caseless)) caseless_p++;
	  xfree (caseless);

	  if (item ==(char *)NULL) item = strdup ("");

	  for (i = 0; values[i] != (char *)NULL; i++)
	    if ((caseless && (strcasecmp (item, values[i]) == 0)) ||
		(!caseless && (strcmp (item, values[i]) == 0)))
	      {
		result = i;
		break;
	      }
	}
    }

  if (result > -1)
    bprintf_insert (page, start, "%d", result);

  xfree (item);
  xfree (array);
}

DEFUN (pf_array_append, item arrayvar,
"Add <var item> as the last array element of the contents
of <var arrayvar>.  This is especially useful in conjunction with
<funref arrays foreach> as a <i>collector</i>:

<example>
<foreach name allnames>
  <if <satifies-criteria <get-var name>>
     <array-append <get-var name> useful-names>>
</foreach>
</example>

See also <funref arrays array-add-unique>.")
{
  char *item = mhtml_evaluate_string (get_positional_arg (vars, 0)); 
  char *array = mhtml_evaluate_string (get_positional_arg (vars, 1));

  if (!empty_string_p (array))
    {
      Symbol *sym = symbol_intern (array);

      if (item == (char *)NULL) item = strdup ("");
      symbol_add_value (sym, item);
    }

  xfree (array);
  xfree (item);
}

DEFUN (pf_array_add_unique, item arrayvar &key caseless=true,
"Add <var item> as the last array element of the contents
of <var arrayvar> if, and only if, <var item> is not already
a member of that array.

The comparison is a direct string-wise compare.  If <var CASELESS> is
non-empty, then a caseless string compare is done.

See also <funref arrays array-append>.")
{
  char *item = mhtml_evaluate_string (get_positional_arg (vars, 0));
  char *array = mhtml_evaluate_string (get_positional_arg (vars, 1));
  int result = -1;

  if (!empty_string_p (array))
    {
      char **values = symbol_get_values (array);

      if (item == (char *)NULL) item = strdup ("");

      if (values != (char **)NULL)
	{
	  register int i;
	  char *caseless;
	  int caseless_p = 0;

	  caseless = mhtml_evaluate_string (get_value (vars, "caseless"));
	  if (!empty_string_p (caseless)) caseless_p++;
	  xfree (caseless);

	  for (i = 0; values[i] != (char *)NULL; i++)
	    if ((caseless && (strcasecmp (item, values[i]) == 0)) ||
		(!caseless && (strcmp (item, values[i]) == 0)))
	      {
		result = i;
		break;
	      }
	}

      if (result == -1)
	{
	  Symbol *sym = symbol_intern (array);
	  symbol_add_value (sym, item);
	}
    }

  xfree (array);
  xfree (item);
}

DEFUN (pf_array_shift, amount arrayvar &key start,
"Shift the elements of <var arrayvar> the indicated amount.

If <var amount> is negative, the elements are shifted down
(i.e. towards zero), with the lowest number elements being lost.

If <var amount> is positive, the elements are shifted up, with
no loss at all -- instead empty elements are used to fill the
created space.

If the keyword argument <var start> is present, it indicates the
zero-based offset from which to start shifting.

Given the array:
<example>
<set-var array[] =
   <prog
       0
       1
       2>>
</example>

<set-var array[] =
   <prog
       0
       1
       2>>

then after executing <example code><array-shift 2 array></example>, 
the array looks like:
<example>
   \"\"
   \"\"
   \"0\"
   \"1\"
   \"2\"
</example>
and, a subsequent execution of
<example code><array-shift -3 array></example> leaves <var array>:
<example>
   \"1\"
   \"2\"
</example>")
{
  char *amount_txt =  mhtml_evaluate_string (get_positional_arg (vars, 0));
  char *array_var = mhtml_evaluate_string (get_positional_arg (vars, 1));
  char *start_var = mhtml_evaluate_string (get_value (vars, "start"));
  int startpos = 0;

  if (!empty_string_p (start_var))
    startpos = atoi (start_var);

  if (startpos < 0) startpos = 0;

  if (!empty_string_p (array_var))
    {
      Symbol *sym = symbol_lookup (array_var);

      if ((sym != (Symbol *)NULL) &&
	  (sym->type == symtype_STRING) &&
	  (sym->values_index > startpos))
	{
	  register int i;
	  int amount = atoi (amount_txt);
	  int newsize = sym->values_index + amount;

	  if (amount < 0)
	    {
	      if (newsize <= 0)
		{
		  for (i = 0; i < sym->values_index; i++)
		    {
		      free (sym->values[i]);
		      sym->values[i] = (char *)NULL;
		    }
		  free (sym->values);
		  sym->values = (char **)NULL;
		  sym->values_index = 0;
		}
	      else
		{
		  amount = -amount;

		  for (i = startpos; i < startpos + amount; i++)
		    free (sym->values[i]);

		  for (; i < sym->values_index; i++)
		    sym->values[i - amount] = sym->values[i];

		  sym->values[i - amount] = (char *)NULL;
		  sym->values_index = newsize;
		}
	    }
	  else if (amount > 0)
	    {
	      if (newsize >= sym->values_slots)
		sym->values = (char **)xrealloc
		  (sym->values, (newsize + 1) * sizeof (char *));

	      for (i = sym->values_index; i > startpos - 1; i--)
		sym->values[i + amount] = sym->values[i];

	      for (i = startpos; i < startpos + amount; i++)
		sym->values[i] = strdup ("");

	      sym->values_index = newsize;
	    }
	}
    }
  else if (debug_level)
    {
      page_debug ("--> array-shift: Needs an array to operate on");
    }
      
  xfree (amount_txt);
  xfree (array_var);
  xfree (start_var);
}

DEFMACRO (pf_foreach, elementvar arrayvar
	  &key start=x end=x step=x iter=var no-copy=true,
"Perform <var body> with <var elementvar> bound to successive memebers
of <var arrayvar>, starting with the element at <var start> (default
0), and ending at <var end> (default <example code><array-size
ARRAYVAR></example>), advancing by <var step> (default 1).

The <code>foreach</code> command is the basic array looping device in
<Meta-HTML>.  It is guaranteed to iterate over each element that you
specify, whether that element is the empty string or not.

If <var no-copy=true> is specified, the array is not copied before
iteration, so that changes that you make to the array take place
immediately, during the execution of the surrounding <tag foreach>.

Starting with the simple array:
<set-var array[]=\"0\\n1\\n2\\n3\\n4\\n5\\n6\\n7\\n8\\n9\">
<example>
<set-var array[]=\"0\\n1\\n2\\n3\\n4\\n5\\n6\\n7\\n8\\n9\">
</example>

we can print out the odd numbers of this array by using values for
both <var start> and <var step>:

<complete-example>
<foreach x array start=1 step=2> <get-var x>, </foreach>
</complete-example>

or, we can produce a \"countdown\" with a negative value for <var step>:

<complete-example>
<foreach x array step=-1> <get-var x>, </foreach> BOOM!
</complete-example>")
{
  char *element_var = mhtml_evaluate_string (get_positional_arg (vars, 0));
  char *array_var = mhtml_evaluate_string (get_positional_arg (vars, 1));
  char *start_arg = mhtml_evaluate_string (get_value (vars, "start"));
  char *end_arg = mhtml_evaluate_string (get_value (vars, "end"));
  char *step_arg = mhtml_evaluate_string (get_value (vars, "step"));
  char *iter_var = mhtml_evaluate_string (get_value (vars, "iter"));
  char *copy = mhtml_evaluate_string (get_value (vars, "copy"));
  char *no_copy = mhtml_evaluate_string (get_value (vars, "no-copy"));
  int start_index, end_index, step;

  if ((!empty_string_p (element_var)) && (!empty_string_p (array_var)))
    {
      Symbol *array = symbol_lookup (array_var);

      if ((array != (Symbol *)NULL) && (array->type == symtype_STRING) &&
	  (array->values_index > 0) && (array->values != (char **)NULL))
	{
	  register int i;

	  end_index = array->values_index;
	  start_index = 0;
	  step = 1;

	  if (!empty_string_p (step_arg))
	    {
	      step = atoi (step_arg);
	      if (step == 0) step = 1;

	      if (step < 0)
		{
		  end_index = 0;
		  start_index = array->values_index - 1;
		}
	    }

	  if (!empty_string_p (start_arg))
	    start_index = atoi (start_arg);

	  if (start_index < 0)
	    start_index = 0;

	  if (!empty_string_p (end_arg))
	    end_index = atoi (end_arg);

	  if (end_index > array->values_index)
	    end_index = array->values_index;

	  if (end_index < start_index)
	    {
	      if (step == 1)
		step = -1;
	    }

	  /* Final sanity check.  Make sure that START and END are within
	     the bounds of the final array. */
	  if ((step < 0) && start_index >= array->values_index)
	    start_index = array->values_index - 1;

	  if (((step < 0) && (end_index < start_index)) ||
	      ((step > 0) && (end_index > start_index)))
	    {
	      /* Maybe copy the array, so that functions which delete or
		 manipulate the contents of it do not disturb our pointers. */
	      int copyit = 1;
	      char **array_values;

	      if ((empty_string_p (copy)) && (empty_string_p (no_copy)))
		{
		  /* Set the default behavior. */
		  if (var_present_p (vars, "copy"))
		    copyit = 0;
		  else if (var_present_p (vars, "no-copy"))
		    copyit = 1;
		}
	      else if (!empty_string_p (no_copy))
		copyit = 0;


	      if (copyit)
		array_values = symbol_copy_array (array->values);
	      else
		array_values = array->values;

	      for (i = start_index;
		   (((start_index < end_index) && (i < end_index)) ||
		    ((start_index > end_index) && (i >= end_index)));
		   i += step)
		{
		  Symbol *element = symbol_remove (element_var);
		  PAGE *code;
		  int line = parser_current_lineno;

		  if (element) symbol_free (element);
		  element = symbol_intern (element_var);
		  symbol_add_value (element, array_values[i]);
		  code = page_copy_page (body);

		  if (!empty_string_p (iter_var))
		    mhtml_set_numeric_variable (iter_var, i);

		  page_process_page_internal (code);
		  parser_current_lineno = line;

		  if (code != (PAGE *)NULL)
		    {
		      int broken = (code->attachment != (void *)NULL);

		      if (code->bindex != 0)
			{
			  bprintf_insert (page, start, "%s", code->buffer);
			  start += (code->bindex);
			}

		      page_free_page (code);
		      if (broken) break;
		    }
		}

	      if (copyit)
		symbol_free_array (array_values);
	    }

	  *newstart = start;
	}
    }

  xfree (element_var);
  xfree (array_var);
  xfree (start_arg);
  xfree (end_arg);
  xfree (step_arg);
  xfree (iter_var);
  xfree (no_copy);
  xfree (copy);
}

DEFUN (pf_array_concat, receiver &optional contributor...,
"Appends the contents of each <var contributor> array to the 
end of <var receiver>.

Both <var receiver> and each <var contributor> are variable names whose
values are treated as arrays.

For a single <var contributor>, <code>array-concat</code>
could have been defined as:

<example>
<defsubst array-concat>
  <foreach item %1>
     <array-append <get-var item> %0>
  </foreach>
</defsubst>
</example>")
{
  char *arrayname = mhtml_evaluate_string (get_positional_arg (vars, 0));

  if (!empty_string_p (arrayname))
    {
      Symbol *arraysym = symbol_intern (arrayname);

      if (arraysym->type == symtype_STRING)
	{
	  register int i, arg_index = 0;
	  int offset;
	  char *arg;

	  for (arg_index = 1;
	       (arg = mhtml_evaluate_string
		(get_positional_arg (vars, arg_index))) != (char *)NULL;
	       arg_index++)
	    {
	      if (!empty_string_p (arg))
		{
		  Symbol *sym = symbol_lookup (arg);

		  if ((sym != (Symbol *)NULL) &&
		      (sym->type == symtype_STRING) &&
		      (sym->values_index > 0))
		    {
		      if ((sym->values_index + arraysym->values_index + 1)
			  > arraysym->values_slots)
			arraysym->values =
			  (char **)xrealloc
			  (arraysym->values,
			   (arraysym->values_slots += (sym->values_index + 10))
			   * sizeof (char *));

		      offset = arraysym->values_index;
		      for (i = 0; i < sym->values_index; i++)
			arraysym->values[offset++] = strdup (sym->values[i]);
		      arraysym->values_index = offset;
		      arraysym->values[offset] = (char *)NULL;
		    }
		}
	      xfree (arg);
	    }
	}
    }
  xfree (arrayname);
}

static char *mhtml_sort_function_name = (char *)NULL;
static int mhtml_sort_is_caseless = 0;
static int mhtml_sort_is_descending = 0;
static int mhtml_sort_is_numeric = 0;

static int
sort_with_function (const void *item1, const void *item2)
{
  char *string1 = *(char **)item1;
  char *string2 = *(char **)item2;
  int should_free = 0;
  int result = 0;

  if (mhtml_sort_function_name)
    {
      PAGE *page = page_create_page ();

      if (string1)
	{
	  char *temp = quote_for_setvar (string1);
	  bprintf (page, "<%s %s>", mhtml_sort_function_name, temp);
	  string1 = mhtml_evaluate_string (page->buffer);
	}
      page->bindex = 0;

      if (string2)
	{
	  char *temp = quote_for_setvar (string2);
	  bprintf (page, "<%s %s>", mhtml_sort_function_name, temp);
	  page->buffer[page->bindex] = '\0';
	  string2 = mhtml_evaluate_string (page->buffer);
	}

      page_free_page (page);
      should_free++;
    }

  if (string1 && !string2)
    result = 1;
  else if (string2 && !string1)
    result = -1;
  else if (!string1 && !string2)
    result = 0;
  else if (mhtml_sort_is_numeric)
    {
      double x = strtod (string1, (char **)NULL);
      double y = strtod (string2, (char **)NULL);

      if (x != y)
	result = (x > y) ? 1 : -1;
    }
  else if (mhtml_sort_is_caseless)
    result = strcasecmp (string1, string2);
  else
    result = strcmp (string1, string2);

  if (should_free)
    {
      xfree (string1);
      xfree (string2);
    }

  if (result && mhtml_sort_is_descending)
    result = -result;

  return (result);
}

DEFUN (pf_sort, arrayvar &optional sort-fun &key caseless=true
       sortorder=[ascending|descending] number=true,
"Sort the contents of the array <var arrayvar>.

The elements are sorted in place -- this function has no return value.

If <var caseless=true> is given, then the comparison of the elements of the array is done without regards to case.

If <var sortorder=reverse> is given, then the results are returned in descending order, instead of ascending order.  The default is to order the elements in ascending order.

If <var numeric=true> is given, then the elements of <var arrayvar> are treated as numeric entities, whether they are or not.  The default is to treat the elements as character strings, which can have unexpected results when sorting numeric quantities (\"11\" is less then \"2\" when sorting alphabetically!)

Finally, you may supply a sorting function, whose name is passed as <var sort-fun>.  This function will be called on each element just before comparison, and the results of that function will be used for the comparison instead of the element itself.  This allows you to create a collating sort, or to sort on complex weighting features, or anything else that you can conceive of.

Examples:

Given the array:
<unset-var array>
<set-var array[0] = 1
         array[1] = 2
         array[3] = 3
         array[4] = 4
         array[5] = 20>
<example>
<set-var array[0] = 1
         array[1] = 2
         array[3] = 3
         array[4] = 4
         array[5] = 20>
</example>
then,

<complete-example>
<sort array>
<foreach x array> <get-var x> </foreach>
</complete-example>
while
<complete-example>
<sort array numeric=true>
<foreach x array> <get-var x> </foreach>
</complete-example>

Sorting strings:
<complete-example>
<set-var array[]=\"a\\nb\\nc\\nd\\ne\\nf\\nA\\nB\\nC\\nD\\nE\\nF\">
<sort array sortorder=descending>
<foreach x array> <get-var x> </foreach>
</complete-example>

Without regards to case:
<complete-example>
<sort array caseless=true>
<foreach x array> <get-var x> </foreach>
</complete-example>

Finally, here is an example which sorts a list
of words based upon the percentage of vowels
present in each word, using a sort function
which calculates that value for each string:

<complete-example>
<defun vowel-percentage string>
  <set-var x =
    <subst-in-string <downcase <get-var string>> \"([^aeiou])\" \"\">>
  <percent <string-length <get-var x>>
           <string-length <get-var string>>>
</defun>
.blank
<set-var words[]=
  <prog
    Brian
    Fox
    sorts
    elegant
    strings
    beautifully>>
.blank
<sort words vowel-percentage numeric=true sortorder=descending>
.blank
<foreach word words>
  <get-var word> (<vowel-percentage <get-var word>>)<br>
</foreach>
</complete-example>")
{
  char *sortvar = mhtml_evaluate_string (get_positional_arg (vars, 0));

  if (!empty_string_p (sortvar))
    {
      Symbol *sym = symbol_lookup (sortvar);

      /* If there is anything to sort... */
      if ((sym != (Symbol *)NULL) && (sym->values_index != 0))
	{
	  char *sorter = mhtml_evaluate_string (get_positional_arg (vars, 1));
	  int caseless = 0, descending = 0, numeric = 0;
	  char *temp;

	  temp = mhtml_evaluate_string (get_value (vars, "caseless"));
	  if (!empty_string_p (temp))
	    caseless = 1;
	  xfree (temp);

	  temp = mhtml_evaluate_string (get_value (vars, "descending"));
	  if (!empty_string_p (temp))
	    descending = 1;
	  xfree (temp);

	  temp = mhtml_evaluate_string (get_value (vars, "numeric"));
	  if (!empty_string_p (temp))
	    numeric = 1;
	  xfree (temp);

	  /* Support "sortorder=[reverse,ascending,descending]" syntax. */
	  temp = mhtml_evaluate_string (get_value (vars, "sortorder"));

	  if (!empty_string_p (temp))
	    {
	      if ((strcasecmp (temp, "descending") == 0) ||
		  (strcasecmp (temp, "reverse") == 0))
		descending++;
	    }
	  xfree (temp);

	  mhtml_sort_function_name = (char *)NULL;

	  if (!empty_string_p (sorter))
	    mhtml_sort_function_name = sorter;

	  mhtml_sort_is_numeric = numeric;
	  mhtml_sort_is_caseless = caseless;
	  mhtml_sort_is_descending = descending;

	  qsort ((void *)sym->values, sym->values_index, sizeof (char *),
		 sort_with_function);

	  if (sorter) free (sorter);
	}
    }

  if (sortvar) free (sortvar);
}

#if defined (__cplusplus)
}
#endif
