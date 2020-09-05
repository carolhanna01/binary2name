/* parser.h: -*- C -*-  Functions which make it easy to parse Meta-HTML. */

/* Author: Brian J. Fox (bfox@ai.mit.edu) Tue Sep 26 22:17:39 1995.

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

#if !defined (_LIBMHTML_PARSER_H_)
#define _LIBMHTML_PARSER_H_

#define MHTML_INCLUDE_IS_RELATIVE 1
#define MHTML_INCLUDE_RECURSION_LIMIT 10

#if defined (LANGUAGE_DEFINITIONS_FILE) || defined (COMPILING_PARSER_C)
#  define LIBMHTML_INTERNAL 1
#endif

#if defined (__cplusplus)
extern "C"
{
extern char LEFT_BRACKET, RIGHT_BRACKET;
}
#else
extern char LEFT_BRACKET, RIGHT_BRACKET;
#endif

#if defined (METAHTML_PROFILER)
# include "profiler.h"
#endif

#if defined (__cplusplus)
extern "C"
{
#endif

typedef struct
{
  int type;		/* Either user_MACRO, user_SUBST, or user_DEFUN. */
  int flags;		/* Interesting bits about this function. */
  int debug_level;	/* How much debugging to do. */
  char *name;		/* The name of this macro, function, or subst. */
  char *body;		/* The body of this macro function, or subst. */
  char *packname;	/* Default package for the scope of this function. */
  char **named_parameters; /* Variables to bind during function invocation. */
  char **documentation;	/* The first set of comments that follow the defun. */
#if defined (METAHTML_PROFILER)
  PROFILE_INFO *profile_info;
#endif
} UserFunction;

/* Return a pointer to the UserFunction structure describing the user level
   function named by NAME, or NULL if no such function exists. */
extern UserFunction *mhtml_find_user_function (char *name);

#undef whitespace
#define whitespace(c) \
	(((c) == ' ') || ((c) == '\t') || ((c) == '\r') || ((c) == '\n'))

#undef return_sequence
#define return_sequence(c, c1) \
     (((c) == '\n') || (((c) == '\r') && ((c1) == '\n')))

#undef make_gcable
#define make_gcable forms_gc_remember

#if defined (LIBMHTML_INTERNAL)
#if !defined (PACKAGE_INITIALIZER_EXTRA_CODE)
#  define PACKAGE_INITIALIZER_EXTRA_CODE
#endif

#define PACKAGE_INITIALIZER(initializer_function_name)			\
void									\
initializer_function_name (Package *package)				\
{									\
  register int i;							\
  register Symbol *sym;							\
									\
  for (i = 0; func_table[i].tag != (char *)NULL; i++)			\
    {									\
      sym = symbol_intern_in_package (package, func_table[i].tag);	\
      sym->type = symtype_FUNCTION;					\
      sym->values = (char **) (&func_table[i]);				\
    }									\
   PACKAGE_INITIALIZER_EXTRA_CODE					\
}

/* The following defines are all used as markers for the `createdoc' program.
   A couple of them produce output in the C source file in which they appear,
   most notably the commands which define a function or macro. */
#define DOC_SECTION(secname)
#define DEFINE_SECTION(secname, keywords, short_info, long_info)
#define DEFVAR(name, documentation)
#define DEFUN(name, args, documentation) static void name (PFunArgs)
#define DEFUNX(name, args, documentation)
#define DEFMACRO(name, args, documentation) static void name (PFunArgs)
#define DEFMACROX(name, args, documentation)

/* A pointer to a function which takes a Package as an argument, and
   initializes that package with Meta-HTML function pointers. */
typedef void METAHTML_INITFUNC (Package *);

#define var_present_p(pack, name) \
     (symbol_lookup_in_package (pack, name) != (Symbol *)NULL)

/* Globally known variable holds onto the reserved words. */
extern Package *mhtml_function_package;

/* Globally known variable holds onto the user defined function names. */
extern Package *mhtml_user_keywords;

/* The "DEFAULT" package. */
extern Package *PageVars;

/* What a Meta-HTML builtin function takes as arguments:
   PAGE is the entire page, as passed to page_process_page_internal ().
   BODY is the contents of the tag body, exclusive of the tags.
   VARS is the list of variables found in the start tag.
   START is the absolute start of this tag in the page.
   END is the absolute end of this tag in the page.
   NEWSTART is a pointer to an integer.  Change this to reset the
            location of the parser pointer.
   DEBUG_LEVEL is an integer which says how much debugging to do for
               this function. */
#define PFunArgs PAGE *page, PAGE *body, Package *vars, \
		 int start, int end, int *newstart, int debug_level
#define PassPFunArgs page, body, vars, start, end, newstart, debug_level

typedef void PFunHandler (PFunArgs);

/* A structure which is used to describe a binary primitive. */
typedef struct
{
  char *tag;		/* The name of the function. */
  int complexp;		/* Non-zero indicates <foo> ... </foo>.  In addition,
			   a value of -1 here indicates a "weak" complex tag;
			   the closing tag does not have to be present in
			   that case. */
  int debug_level;	/* How much debugging to do. */
  PFunHandler *fun;	/* The code that handles this PAGEFUNC. */
#if defined (METAHTML_PROFILER)
  PROFILE_INFO *profile_info;
#endif
} PFunDesc;

/* Return the primitive descriptor for TAG, or NULL if there is none. */
extern PFunDesc *pagefunc_get_descriptor (char *tag);

/* Bind variables in the current package only for the duration of body. */
extern void mhtml_let (PFunArgs);

/* Execute the subst, function or macro described by UF. */
extern void mhtml_execute_function (UserFunction *uf, PFunArgs, char *attr);

typedef struct
{
  jmp_buf env;
  PAGE *page;
  int offset;
} PageEnv;

extern PageEnv *pagefunc_save_environment (void);
extern void pagefunc_restore_environment (PageEnv *env);
extern jmp_buf page_jmp_buffer;

/* Variable specifically for the mdb debugger.  This allows the setting
   of a callback function when an mdb breakpoint is hit. */
typedef int DEBUGGER_CALLBACK_FUNCTION
   (PFunArgs, PFunDesc *desc, UserFunction *uf, char *open_body);
extern DEBUGGER_CALLBACK_FUNCTION *mhtml_debug_callback_function;
typedef void MHTML_PARSER_CALLBACK_FUNCTION (void);
extern MHTML_PARSER_CALLBACK_FUNCTION *mhtml_parser_callback_function;
extern int mhtml_parser_interrupted;

#endif /* LIBMHTML_INTERNAL */

#define user_MACRO 1
#define user_SUBST 2
#define user_DEFUN 3

/* Flags that can be associated with a UserFunction. */
#define user_WHITESPACE_DELETED	0x01
#define user_WEAK_MACRO		0x02
#define user_ACCEPT_KEYWORDS	0x04
#define user_WHITESPACE_KEPT	0x08

extern char *metahtml_copyright_string;

/* The parser's idea of what the current line number is. */
extern int parser_current_lineno;

/* Gets 1 when mhtml::inhibit-comment-parsing has a value, 0 otherwise. */
extern int mhtml_inhibit_comment_parsing;

/* Gets 1 when mhtml::decimal-places has a value, 0 otherwise. */
extern int mhtml_decimal_notify;
extern int mhtml_decimal_places;

/* Gets 1 when mhtml::warn-on-redefine-primitive has a value, 0 otherwise. */
extern int mhtml_warn_on_redefine_primitive;

/* Gets 1 when mhtml::warn-on-redefine-function has a value, 0 otherwise. */
extern int mhtml_warn_on_redefine;

/* Given that we are about to define a new function, check to see if
   the definition would overwrite an existing definition.  We only
   perform this check if the user has set one of the variables
   mhtml::warn-on-redefine-primitive or mhtml::warn-on-redefine. */
extern void mhtml_maybe_warn_redefine (char *name, int type);

/* Globally known variable holds onto to the reserved words. */
extern Package *mhtml_function_package;

/* Macro writing and processing. */
extern Package *mhtml_user_keywords;

/* Actually process PAGE in place.  The result of processing PAGE is placed
   within PAGE.  This is likely to change shortly, when we pre-parse the
   PAGE and write sequential output to a different destination. */
extern void page_process_page_internal (PAGE *page);
extern void page_process_page (volatile PAGE *page);
extern void page_return_this_page (PAGE *page);

extern void pagefunc_initialize_notifiers (void);
extern int page_check_syntax (PAGE *page);
extern char *get_value (Package *package, char *name);
extern char **get_vars_names (Package *package);
extern char **get_vars_vals (Package *package);
extern void pagefunc_set_variable (char *tag, char *value);
extern void mhtml_set_numeric_variable (char *name, int value);
extern void mhtml_set_numeric_variable_in_package (Package *p,
						   char *name, int value);
extern void pagefunc_set_variable_readonly (char *tag, char *value);
extern char *pagefunc_get_variable (char *tag);
extern char *get_one_of (Package *package, char *tag, ...);
extern char *get_positional_arg (Package *package, int position);
extern char *read_sexp_1 (char *string, int *start, int stop_at_equals_p,
			  int one_list);
extern char *read_sexp (char *string, int *start, int stop_at_equals_p);
extern char *quote_for_setvar (char *string);

/* If you want to delete a package, you should probably call this function
   rather than calling symbol_destroy_package () from symbols.c.  This 
   allows the engine to reset a bunch of internal variables if necessary. */
extern void pagefunc_destroy_package (char *package_name);

/* Gather arguments from STRING and return a newly consed anonymous package
   containing those arguments.  If second arg ALLOW_ASSIGNMENTS_P is non-zero,
   allow equals signs to indicate keyword values. */
extern Package *pagefunc_snarf_vars (char *string, int allow_assignments_p);

/* Return non-zero if STRING is non-zero or all whitespace. */
extern int empty_string_p (char *string);

/* Read STRING, and convert the contents to a list of variables in PACKAGE. */
extern Package *alist_to_package (char *string);

/* Convert PACKAGE to an ASCII readable string -- an alist representing
   the contents of PACKAGE.  If STRIP is non-zero, the package name prefix
   is not prepended to each variable name in the alist, otherwise, the
   package name appears before each variable.  If PACKAGE is anonymous,
   no package name is associated with the variables. */
extern char *package_to_alist (Package *package, int strip);

/* Evaluate the string BODY in the current environment, returning the results
   as a newly consed string, or NULL if BODY was NULL. */
extern char *mhtml_evaluate_string (char *body);

/* Add or replace a function of TYPE with NAME, BODY in the
   *user-functions* package. The definition is modified by variable
   names and values specified in the package passed in VARS. */
extern void mhtml_add_user_function (int type, char *name, char *body,
				     Package *vars);

/* Canonicalize the filename INPUT such that it is a complete and
   valid path to a file. */
extern char *mhtml_canonicalize_file_name
	(char *input, char *incpref, char *relpref, char **web_relative_name);

/* Same, but auto-handle the include and relative prefixes dependent on the
   setting of isp::web-relative-pathnames.  Also evaluates ARGUMENT, so you
   should call this function like:

   fname = mhtml_canonicalize_file_name_argument (get_positional_arg (vars, 1))

   and xfree() FNAME when you are done with it. */
extern char *mhtml_canonicalize_file_name_argument (char *argument);

/* Set the debugging level for the function named in SYM to
   be the value of SYM. */
extern void mhtml_set_debugging_on (Symbol *sym);

/* Deliver a string which looks like the string that might have been
   passed to a function.  PACKAGE is the package returned from
   PAGEFUNC_SNARF_VARS. */
extern char *mhtml_funargs (Package *pack);

/* Returns non-zero if STRING consists exclusively of all digits.
   A decimal point is NOT a digit. */
extern int mhtml_all_digits (char *string);

extern char *mhtml_base64decode (char *encoded, int *len);
extern char *mhtml_base64encode (char *data, int length, int shortlines);

extern PAGE *parser_top_page (void);
extern int number_p (char *string);
extern int float_p (char *string);
extern int integer_p (char *string, int base);

extern PAGE *get_after_page_return_buffer (void);

#if defined (__cplusplus)
}
#endif

#endif /*_LIBMHTML_PARSER_H_ */
