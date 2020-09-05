dnl This file is part of grecs
dnl Copyright (C) 2007-2018 Sergey Poznyakoff
dnl
dnl Grecs is free software; you can redistribute it and/or modify
dnl it under the terms of the GNU General Public License as published by
dnl the Free Software Foundation; either version 3, or (at your option)
dnl any later version.
dnl
dnl Grecs is distributed in the hope that it will be useful,
dnl but WITHOUT ANY WARRANTY; without even the implied warranty of
dnl MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
dnl GNU General Public License for more details.
dnl
dnl You should have received a copy of the GNU General Public License
dnl along with Grecs.  If not, see <http://www.gnu.org/licenses/>.
divert(-1)
changequote([<,>])
changecom(/*,*/)

dnl Diversion channels
define([<__DIV_OPTKEYS__>],1)dnl	option keys
define([<__DIV_OPTDECL__>],2)dnl	struct option declarations
define([<__DIV_HELPDECL__>],3)dnl	grecs_opthelp declarations
define([<__DIV_SWITCH__>],4)dnl		switch statement body
define([<__DIV_STATIC__>],5)dnl		module static data

dnl Special macros that can be set during expansion
pushdef([<__GETOPT_DEFINED__>])
pushdef([<__OPTIONS_END_DEFINED__>])
pushdef([<__GETOPT_LASTOPT_LABEL__>])

define([<__GETOPT_LONG_TAG>])
define([<__GETOPT_SHORT_TAG>])

define([<WITH_DIVERSION>],[<ifelse([<$#>],0,[<[<$0>]>],[<dnl
pushdef([<__GETOPT_DIVERSION__>],divnum)dnl
divert([<$1>])$2dnl
divert(__GETOPT_DIVERSION__)dnl
popdef([<__GETOPT_DIVERSION__>])>])>])

dnl _getopt_mangle_option(NAME)
dnl ---------------------------
dnl Convert NAME to a valid m4 identifier, by replacing invalid characters
dnl with underscores, and prepend the _GETOPT_OPTION_ suffix to it.
define([<_getopt_mangle_option>],
  [<[<_GETOPT_OPTION_>]patsubst($1, [<[^a-zA-Z0-9_]>], [<_>])>])

dnl _getopt_set_option(NAME[=VAL])
dnl ------------------------------
dnl Set option NAME.  
define([<_getopt_set_option>],
  [<ifelse(index([<$1>],=),-1,[<define(_getopt_mangle_option($1))>],
    [<define(regexp([<$1>],\([^=]+\)=\(.*\),
             [<_getopt_mangle_option(\1),[<\2>]>]))>])>])

dnl _getopt_get_option(NAME[,DEFAULT])
dnl ----------------------------------
define([<_getopt_get_option>],
  [<_getopt_if_option_set($1,[<indir(_getopt_mangle_option($1))>],[<$2>])>])
  
dnl _getopt_if_option_set(NAME,IF-SET,IF-NOT-SET)
dnl ---------------------------------------------
dnl Check if option NAME is set.
define([<_getopt_if_option_set>],
  [<ifdef(_getopt_mangle_option([<$1>]),[<$2>],[<$3>])>])

dnl _getopt_if_option_null(NAME,IF-NULL,IF-NOT-NULL)
dnl ------------------------------------------------
dnl Check if option NAME is set.
define([<_getopt_if_option_null>],
  [<ifelse(indir(_getopt_mangle_option([<$1>])),,[<$2>],[<$3>])>])

dnl _getopt_option_switch(NAME1,IF-SET1,[NAME2,IF-SET2,[...]],[IF-NOT-SET])
dnl ------------------------------------------------------------------------
dnl If NAME1 is set, run IF-SET1.  Otherwise, if NAME2 is set, run IF-SET2.
dnl Continue the process for all name-if-set pairs within [...].  If none
dnl of the options is set, run IF-NOT-SET.
define([<_getopt_option_switch>],
  [<ifelse([<$4>],,[<_getopt_if_option_set($@)>],
           [<$3>],,[<_getopt_if_option_set($@)>],
     [<_getopt_if_option_set([<$1>],[<$2>],
                             [<_getopt_option_switch(shift(shift($@)))>])>])>])

dnl _getopt_if_option_val(NAME,val,IF-TRUE,IF-FALSE)
dnl ------------------------------------------------
dnl Check if option NAME is set.
define([<_getopt_if_option_val>],
  [<ifelse(_getopt_get_option([<$1>]),[<$2>],[<$3>],[<$4>])>])

define([<__getopt_switch_option_val>],
[<ifelse([<$#>],2,[<$2>],dnl
         [<$#>],3,[<ifelse([<$1>],[<$2>],[<$3>])>],dnl
[<ifelse([<$1>],[<$2>],[<$3>],dnl
[<__getopt_switch_option_val([<$1>],shift(shift(shift($@))))>])>])>])
	            
dnl _getopt_switch_option_val(NAME,val1,IF-VAL1,val2,IF-VAL2...,ELSE)
dnl -----------------------------------------------------------------
dnl Check if option NAME is set.
define([<_getopt_switch_option_val>],
[<pushdef([<val>],[<_getopt_get_option($1)>])dnl
__getopt_switch_option_val(val, shift($@))[<>]dnl
popdef([<val>])>])

dnl _getopt_set_options(OPTION[,OPTION...])
dnl ---------------------------------------
dnl Set options given as arguments.
define([<_getopt_set_options>],
  [<ifelse([<$1>],,,
     [<_getopt_set_option([<$1>])
       _getopt_set_options(shift($@))>])>])

dnl __getopt_format_authors(name[,name...])
dnl ---------------------------------------
define([<__getopt_format_authors>],dnl
	[<ifelse([<$1>],,NULL,[<"$1",
__getopt_format_authors(shift($@))>])>])
       
dnl __getopt_upcase(ARGS...)
dnl ------------------------
dnl Concatenate and convert ARGS to upper case.
dnl
define([<__getopt_upcase>], [<translit([<$*>], [<a-z>], [<A-Z>])>])

dnl __getopt_concat(ARGS...)
dnl ------------------------	
dnl Concatenate arguments, inserting ", " between each pair of them.
dnl
define([<__getopt_concat>],[<ifelse([<$#>],1,[<$1>],[<$1, __getopt_concat(shift($@))>])>])

dnl __getopt_flushleft(ARGS...)
dnl ---------------------------
dnl Concatenate ARGS and remove any leading whitespace
dnl
define([<__getopt_flushleft>],
 [<patsubst([<__getopt_concat($*)>], [<^[	]+>])>])

dnl __getopt_chop(ARGS...)
dnl ----------------------
dnl Concatenate ARGS and remove any trailing whitespace
dnl
define([<__getopt_chop>],
 [<patsubst([<$*>], [<[	]+$>])>])

dnl __getopt_escape(ARGS...)
dnl ------------------------
dnl Concatenate ARGS and escape any occurrences of double-quotes with
dnl backslashes.
dnl
define([<__getopt_escape>],
[<patsubst([<__getopt_concat($*)>],[<[\"]>],[<\\\&>])>])

dnl __getopt_prep(ARG)
dnl ------------------
dnl Prepare ARG for including in C strings: replace newlines and any 
dnl preceding and following whitespace by a single space character, remove
dnl leading whitespace, and escape double-quotes.
dnl
define([<__getopt_prep>],
 [<__getopt_escape(__getopt_flushleft(patsubst([<$1>],[<[	]*
+[	]*>],[< >])))>])

dnl __GETOPT_SHORT_OPTS
dnl -------------------
dnl Accumulator for the 3rd argument of getopt_long
dnl
define([<__GETOPT_SHORT_OPTS>],[<_getopt_if_option_set([<nopermute>],+)>])

dnl GROUP(STRING)
dnl -------------
dnl Begin a named group of options
dnl
define([<GROUP>],[<ifelse([<$#>],0,[<[<$0>]>],[<dnl
WITH_DIVERSION(__DIV_HELPDECL__,[<dnl
	{ NULL, NULL, 0, N_("__getopt_prep([<$1>])") },
>])>])>])	

# __getopt_quote(args) - convert args to single-quoted string
define([<__getopt_quote>], [<ifelse([<$*>], [<>], [<>], [<[<$*>]>])>])
define([<__getopt_dquote>], [<ifelse([<$*>], [<>], [<>], [<[<$@>]>])>])

# __getopt_ifempty(LIST, IF-TRUE [, IF-FALSE])
# --------------------------------------------
# Expands to IF-TRUE if LIST is not empty, otherwise to IF-FALSE.
#
define([<__getopt_ifempty>], [<ifelse([<$1>],[<>],[<$2>],[<$3>])>])

define([<__GATHER_OPTIONS>],[<
pushdef([<__GETOPT_KEY>],ifelse([<$2>],,[<OPTION_>]__getopt_upcase(patsubst($1,-,_)),'$2'))
ifelse([<$2>],,[<
WITH_DIVERSION(__DIV_OPTKEYS__,[<dnl
	__GETOPT_KEY,
>])>])
define([<__GETOPT_SELECTOR>],ifdef([<__GETOPT_SELECTOR>],__GETOPT_SELECTOR) case __GETOPT_KEY:)
ifelse([<$1>],,,[<
WITH_DIVERSION(__DIV_OPTDECL__,[<dnl
	{ "$1", __GETOPT_ARGTYPE, 0, __GETOPT_KEY },
>])>])
dnl
define([<__GETOPT_SHORT_OPTS>],__getopt_dquote(__GETOPT_SHORT_OPTS[<>]dnl
ifelse([<$2>],,,[<$2>]ifelse(__GETOPT_ARGTYPE,[<no_argument>],,__GETOPT_ARGTYPE,[<required_argument>],:,__GETOPT_ARGTYPE,[<optional_argument>],::))))
dnl
ifelse([<$1>],,,dnl
[<define([<__GETOPT_LONG_TAG>],
__getopt_ifempty(__getopt_quote(__GETOPT_LONG_TAG),__getopt_dquote(__getopt_dquote(--$1)),[<__getopt_quote(__getopt_dquote(__GETOPT_LONG_TAG), __getopt_dquote([< --$1>]))>]))>])
ifelse([<$2>],,,dnl
[<define([<__GETOPT_SHORT_TAG>],
__getopt_ifempty(__getopt_quote(__GETOPT_SHORT_TAG),__getopt_dquote(__getopt_dquote(-$2)),[<__getopt_quote(__getopt_dquote(__GETOPT_SHORT_TAG), __getopt_dquote([< -$2>]))>]))>])
popdef([<__GETOPT_KEY>])
>])

dnl OPTION(long-opt, short-opt, [arg], [descr])
dnl -------------------------------------------
dnl Introduce a command line option. Arguments:
dnl   long-opt     Long option.
dnl   short-opt    Short option (a single char)
dnl   (At least one of long-opt or short-opt must be present)
dnl
dnl   Optional arguments:
dnl   arg          Option argument.
dnl   descr        Option description
dnl
dnl If arg is absent, the option does not take any arguments. If arg is
dnl enclosed in square brackets, the option takes an optional argument.
dnl Otherwise, the argument is required.
dnl
dnl If descr is not given the option will not appear in the --help and
dnl --usage outputs.
dnl
define([<OPTION>],[<ifelse([<$#>],0,[<[<$0>]>],[<
pushdef([<__GETOPT_LONG_TAG>])
pushdef([<__GETOPT_SHORT_TAG>])
pushdef([<__GETOPT_SELECTOR>])
pushdef([<__GETOPT_ARGNAME>],[<$3>])
pushdef([<__GETOPT_HIDDEN>],ifelse([<$4>],,1,0))
pushdef([<__GETOPT_DOCSTRING>],[<__getopt_prep([<$4>])>])
pushdef([<__GETOPT_ARGTYPE>],[<ifelse([<$3>],,[<no_argument>],dnl
patsubst([<$3>],[<\[.*\]>]),,[<optional_argument>],dnl
[<required_argument>])>])
__GATHER_OPTIONS($@)
define([<BEGIN>],[<__GETOPT_API_BEGIN>])
>])>])

dnl ALIAS(long-opt, short-opt)
dnl --------------------------
dnl Declare aliases for the previous OPTION statement.
dnl   long-opt     Long option.
dnl   short-opt    Short option (a single char)
dnl   (At least one of long-opt or short-opt must be present)
dnl An OPTION statement may be followed by any number of ALIAS statements.
dnl
define([<ALIAS>],[<ifelse([<$#>],0,[<[<$0>]>],[<
__GATHER_OPTIONS($1,$2)
>])>])

dnl BEGIN
dnl -----
dnl Start an action associated with the declared option. Must follow OPTION
dnl statement, with optional ALIAS statements in between.
dnl
define([<__GETOPT_API_BEGIN>],[<
undefine([<BEGIN>])
define([<END>],[<__GETOPT_API_END>])
define([<LASTOPT>],[<__GETOPT_API_LASTOPT>])
ifelse(__GETOPT_HIDDEN,1,,[<
WITH_DIVERSION(__DIV_HELPDECL__,[<dnl
	{
#ifdef HAVE_GETOPT_LONG
	  "__getopt_ifempty(__getopt_quote(__GETOPT_SHORT_TAG),__getopt_quote(__GETOPT_LONG_TAG),[<__getopt_quote(__GETOPT_SHORT_TAG)__getopt_ifempty(__getopt_quote(__GETOPT_LONG_TAG),,[<, __getopt_quote(__GETOPT_LONG_TAG)>])>])",
#else
	  "__GETOPT_SHORT_TAG",
#endif
				   ifelse(__GETOPT_ARGNAME,,[<NULL, 0>],
[<ifelse(__GETOPT_ARGTYPE,[<optional_argument>],
[<N_(>]"[<patsubst(__GETOPT_ARGNAME,[<\[\(.*\)\]>],[<\1>])>][<"), 1>],[<N_("__GETOPT_ARGNAME"), 0>])>]), N_("__GETOPT_DOCSTRING") },
>])>])
popdef([<__GETOPT_ARGTYPE>])
popdef([<__GETOPT_ARGNAME>])
popdef([<__GETOPT_DOCSTRING>])
popdef([<__GETOPT_HIDDEN>])
divert(__DIV_SWITCH__)dnl
popdef([<__GETOPT_LONG_TAG>])dnl
popdef([<__GETOPT_SHORT_TAG>])dnl
	__GETOPT_SELECTOR
	  {
>])

dnl END
dnl ---
dnl Finish the associated action
dnl
define([<__GETOPT_API_END>],[<
	     break;
	  }
divert(-1)
undefine([<END>])
undefine([<ALIAS>])
undefine([<LASTOPT>])
popdef([<__GETOPT_SELECTOR>])>])

dnl OPTNODE(name, value)
dnl --------------------
define([<OPTNODE>],[<ifelse([<$#>],0,[<[<$0>]>],[<dnl
WITH_DIVERSION(__DIV_STATIC__,[<
static struct grecs_node *cmdline_tree;
>]);
do {
   struct grecs_node *node = grecs_node_from_path($1, $2);
   if (!cmdline_tree)
	cmdline_tree = node;
   else
	grecs_node_bind(cmdline_tree, node, 0);
} while(0)
>])>])

dnl LASTOPT()
dnl ---------
dnl Stop option processing (as if "--" has been encountered)
define([<__GETOPT_API_LASTOPT>],[<dnl
pushdef([<__GETOPT_LASTOPT_LABEL__>],[<lastoptlab:
popdef([<__GETOPT_LASTOPT_LABEL__>])>])dnl
goto lastoptlab>])

dnl GETOPT(argc, argv, [long_index], [onerr])
dnl -----------------------------------------
dnl Emit option parsing code. Arguments:
dnl
dnl  argc        Name of the 1st argument to getopt_long.
dnl  argv        Name of the 2nd argument to getopt_long.
dnl  long_index  5th argument to getopt_long.  If not given,
dnl              NULL will be passed.
dnl  onerr       Action to take if parsing fails.
dnl
define([<GETOPT>],[<ifelse([<$#>],0,[<[<$0>]>],[<dnl
pushdef([<__GETOPT_DEFINED__>],1)
 {
  int c;

  optind = 0;
#ifdef HAVE_GETOPT_LONG
  while ((c = getopt_long($1, $2, "__GETOPT_SHORT_OPTS",
			  long_options, NULL)) != EOF)
#else
  while ((c = getopt($1, $2, "__GETOPT_SHORT_OPTS")) != EOF)
#endif
    {
      switch (c)
	{
	default:
	   ifelse([<$4>],,,[<$4;>])dnl
	   exit(EX_USAGE);
	undivert(__DIV_SWITCH__)
	}
    }
__GETOPT_LASTOPT_LABEL__()dnl
  ifelse([<$3>],,[<
    if (optind < argc) {
	fprintf(stderr, "%s: unexpected arguments\n", $2[0]);
	exit(EX_USAGE);
    }
>],[<$3 = optind;>])
ifelse(__GETOPT_CMDLINE_TREE_NEEDED__,1,[<
  if (cmdline_tree)
    {
      struct grecs_node *rn = grecs_node_create(grecs_node_root, NULL);
      rn->down = cmdline_tree;
      cmdline_tree = rn;
    }
>])dnl
 }
>])>])

define([<_getopt_sc_array>],
  [<ifelse([<$1>],,,[<[<"$1", >]_getopt_sc_array(shift($@))>])>])

define([<STDFUNC>],[<ifelse([<$#>],0,[<[<$0>]>],[<
WITH_DIVERSION(__DIV_STATIC__,[<
ifelse([<$3>],,,[<
static char *subcommands[] = {
	_getopt_sc_array($3)
	NULL
};>])
#ifndef PACKAGE_URL
# define PACKAGE_URL NULL
#endif
static struct grecs_proginfo proginfo = {
	$2, /* progname */
	ifelse([<$3>],,NULL,subcommands), /* subcommands */
	ifelse([<$4>],,"",N_("$4")), /* docstring */
	ifelse([<$5>],,"",N_("$5")), /* args_doc */
	opthelp, /* opthelp */
	sizeof(opthelp)/sizeof(opthelp[0]), /* optcount */
	NULL, /* print_help_hook */
	NULL, /* print_version_hook */
	PACKAGE_NAME,    /* package */
	PACKAGE_VERSION, /* version */
	NULL, /* license */
_getopt_if_option_set([<copyright_year>],dnl
	"[<_getopt_get_option(copyright_year)>]",NULL),/* copyright_year */
_getopt_if_option_set([<copyright_holder>],dnl
	"[<_getopt_get_option(copyright_holder)>]",NULL), /* copyright_holder */
	NULL, /* *authors */
	"<" PACKAGE_BUGREPORT ">", /* bug_address */
	PACKAGE_URL, /* url */
	NULL  /* epilogue */
};

_getopt_if_option_set([<program_version>],dnl
[<const char *_getopt_if_option_null(program_version,program_version,[<_getopt_get_option(program_version)>]) = $2 " (" PACKAGE_NAME ") " PACKAGE_VERSION;
>])>])>])>])

dnl OPTIONS_BEGIN(PROGNAME, DOCSTR, [ARGDOC], [OPTS...])
dnl ----------------------------------------------------
dnl Begins option definition section. Arguments:
dnl   PROGNAME    -  canonical program name,
dnl   DOCSTR      -  usage string to be displayed in --help output.
dnl   ARGDOC      -  docstring for arguments
dnl
dnl Optional arguments (OPTS):
dnl   nostdincl   -  don't emit C include statements for standard include files
dnl   nopermute   -  don't permute arguments, i.e. don't accept intermixed
dnl                  optional and positional arguments
dnl
dnl   gnu         -  emit code for --help (-h), --version (-V) and --usage
dnl                  options.
dnl Options bellow modify the effect of the 'gnu' option:
dnl
dnl   nousage     -  disable support for the --usage option
dnl   noversion   -  disable support for the --version option
dnl
dnl   copyright_year=Y
dnl               Year, or range, or list of years to display in
dnl               the copyright statement. Default is 2012 (why?)
dnl   copyright_holder=NAME
dnl               Copyright holder name. Default is 'Free Software Foundation,
dnl               inc.'
dnl   program_version=V
dnl               Version string. Defaults to PACKAGE_VERSION autoconf define.
dnl
define([<OPTIONS_BEGIN>],[<ifelse([<$#>],0,[<[<$0>]>],
    [<divert(-1)
     _getopt_set_options(shift(shift(shift($@))))
     pushdef([<__GETOPT_EXPECT_ARGS__>],ifelse(__getopt_quote($3),,0,1))
     _getopt_if_option_set([<gnu>],
        [<STDFUNC([<$1 " (" PACKAGE_NAME ") " PACKAGE_VERSION>],
	          [<$1>], [<>], [<$2>], [<$3>])>])>])>])


define([<OPTIONS_COMMAND_BEGIN>],[<ifelse([<$#>],0,[<[<$0>]>],
   [<divert(-1)
     _getopt_set_options(shift(shift(shift(shift($@)))))
     pushdef([<__GETOPT_EXPECT_ARGS__>],ifelse(__getopt_quote($3),,0,1))
     _getopt_if_option_set([<gnu>],
        [<STDFUNC([<$1 " (" PACKAGE_NAME ") " PACKAGE_VERSION>],
	          [<$1>], [<$2>], [<$3>], [<$4>])>])
>])>])

define([<OPTIONS_END>],[<
pushdef([<__OPTIONS_END_DEFINED__>],1)
_getopt_if_option_set([<gnu>],[<
	 GROUP([<Other options>])
	 OPTION([<help>],h,,[<Give this help list>])
	 BEGIN
		grecs_print_help(&proginfo);
		exit(0);
	 END
_getopt_if_option_set([<nousage>],,[<
         OPTION([<usage>],,,[<Give a short usage message>])
	 BEGIN
		grecs_print_usage(&proginfo);
		exit(0);
	 END>])
_getopt_if_option_set([<noversion>],,[<	 
	 OPTION([<version>],V,,[<Print program version>])
	 BEGIN
	        /* Give version */
		grecs_print_version(&proginfo, stdout);
		exit(0);
	 END>])>])
divert(0)dnl
_getopt_if_option_set([<nostdincl>],,
[<#ifdef HAVE_CONFIG_H
# include <config.h>
#endif
#ifdef HAVE_GETOPT_H
# include <getopt.h>
#endif
#include <unistd.h>
#include <stdlib.h>
>])dnl
#include <grecs.h>
#include <grecs/opt.h>
#ifndef [<EX_USAGE>]
# define [<EX_USAGE>] 64
#endif
/* Option codes */
enum {
	_OPTION_INIT=255,
undivert(__DIV_OPTKEYS__)dnl
	MAX_OPTION
};
#ifdef HAVE_GETOPT_LONG
static struct option long_options[] = {
undivert(__DIV_OPTDECL__)dnl
	{0, 0, 0, 0}
};
#endif
_getopt_if_option_set([<gnu>],[<
static struct grecs_opthelp opthelp[] = {
undivert(__DIV_HELPDECL__)dnl
};
>],[<WITH_DIVERSION(-1,[<undivert(__DIV_HELPDECL__)>])>])dnl
undivert(__DIV_STATIC__)
>])

m4wrap([<ifelse(__OPTIONS_END_DEFINED__,1,,[<OPTIONS_END()>])dnl
ifelse(__GETOPT_DEFINED__,1,,[<divert(0)dnl
ifelse(__GETOPT_EXPECT_ARGS__,1,[<
int
parse_options(int argc, char **argv, int *idx)
{
  GETOPT(argc, argv, *idx)
  return 0;
}
>],[<
int
parse_options(int argc, char **argv)
{
  GETOPT(argc, argv)
  return 0;
}
>])
divert(-1)
popdef([<__GETOPT_DEFINED__>])
popdef([<__GETOPT_EXPECT_ARGS__>])
popdef([<__OPTIONS_END_DEFINED__>])
popdef([<__GETOPT_LASTOPT_LABEL__>])>])>])

divert(0)dnl
/* -*- buffer-read-only: t -*- vi: set ro:
   THIS FILE IS GENERATED AUTOMATICALLY.  PLEASE DO NOT EDIT.
*/
