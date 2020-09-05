#line 539 "../grecs/build-aux/getopt.m4"
/* -*- buffer-read-only: t -*- vi: set ro:
   THIS FILE IS GENERATED AUTOMATICALLY.  PLEASE DO NOT EDIT.
*/
#line 1 "cmdline.opt"
/* This file is part of GNU Dico. -*- c -*-
   Copyright (C) 1998-2019 Sergey Poznyakoff

   GNU Dico is free software; you can redistribute it and/or modify
   it under the terms of the GNU General Public License as published by
   the Free Software Foundation; either version 3, or (at your option)
   any later version.

   GNU Dico is distributed in the hope that it will be useful,
   but WITHOUT ANY WARRANTY; without even the implied warranty of
   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
   GNU General Public License for more details.

   You should have received a copy of the GNU General Public License
   along with GNU Dico.  If not, see <http://www.gnu.org/licenses/>. */

#include <dicod.h>
#include <stdlib.h>
#include <stdio.h>
#include <string.h>
#include <ctype.h>
#include <getopt.h>
#include <gettext.h>

static struct grecs_txtacc *pp_cmd_acc;

#line 188 "cmdline.opt"
#ifdef HAVE_CONFIG_H
#line 188
# include <config.h>
#line 188
#endif
#line 188
#ifdef HAVE_GETOPT_H
#line 188
# include <getopt.h>
#line 188
#endif
#line 188
#include <unistd.h>
#line 188
#include <stdlib.h>
#line 188
#include <grecs.h>
#line 188
#include <grecs/opt.h>
#line 188
#ifndef EX_USAGE
#line 188
# define EX_USAGE 64
#line 188
#endif
#line 188
/* Option codes */
#line 188
enum {
#line 188
	_OPTION_INIT=255,
#line 65 "cmdline.opt"
	OPTION_CONFIG,
#line 77 "cmdline.opt"
	OPTION_STDERR,
#line 83 "cmdline.opt"
	OPTION_SYSLOG,
#line 111 "cmdline.opt"
	OPTION_NO_TRANSCRIPT,
#line 124 "cmdline.opt"
	OPTION_SOURCE_INFO,
#line 130 "cmdline.opt"
	OPTION_TRACE_GRAMMAR,
#line 136 "cmdline.opt"
	OPTION_TRACE_LEX,
#line 144 "cmdline.opt"
	OPTION_CONFIG_HELP,
#line 153 "cmdline.opt"
	OPTION_PREPROCESSOR,
#line 159 "cmdline.opt"
	OPTION_NO_PREPROCESSOR,
#line 188 "cmdline.opt"
	OPTION_USAGE,
#line 188 "cmdline.opt"
	MAX_OPTION
#line 188
};
#line 188
#ifdef HAVE_GETOPT_LONG
#line 188
static struct option long_options[] = {
#line 43 "cmdline.opt"
	{ "lint", no_argument, 0, 't' },
#line 49 "cmdline.opt"
	{ "inetd", no_argument, 0, 'i' },
#line 55 "cmdline.opt"
	{ "runtest", no_argument, 0, 'r' },
#line 65 "cmdline.opt"
	{ "config", required_argument, 0, OPTION_CONFIG },
#line 71 "cmdline.opt"
	{ "foreground", no_argument, 0, 'f' },
#line 77 "cmdline.opt"
	{ "stderr", no_argument, 0, OPTION_STDERR },
#line 83 "cmdline.opt"
	{ "syslog", no_argument, 0, OPTION_SYSLOG },
#line 89 "cmdline.opt"
	{ "single-process", no_argument, 0, 's' },
#line 95 "cmdline.opt"
	{ "load-dir", required_argument, 0, 'L' },
#line 105 "cmdline.opt"
	{ "transcript", no_argument, 0, 'T' },
#line 111 "cmdline.opt"
	{ "no-transcript", no_argument, 0, OPTION_NO_TRANSCRIPT },
#line 117 "cmdline.opt"
	{ "debug", required_argument, 0, 'x' },
#line 124 "cmdline.opt"
	{ "source-info", no_argument, 0, OPTION_SOURCE_INFO },
#line 130 "cmdline.opt"
	{ "trace-grammar", no_argument, 0, OPTION_TRACE_GRAMMAR },
#line 136 "cmdline.opt"
	{ "trace-lex", no_argument, 0, OPTION_TRACE_LEX },
#line 144 "cmdline.opt"
	{ "config-help", no_argument, 0, OPTION_CONFIG_HELP },
#line 153 "cmdline.opt"
	{ "preprocessor", required_argument, 0, OPTION_PREPROCESSOR },
#line 159 "cmdline.opt"
	{ "no-preprocessor", no_argument, 0, OPTION_NO_PREPROCESSOR },
#line 165 "cmdline.opt"
	{ "include-dir", required_argument, 0, 'I' },
#line 171 "cmdline.opt"
	{ "define", required_argument, 0, 'D' },
#line 188 "cmdline.opt"
	{ "help", no_argument, 0, 'h' },
#line 188 "cmdline.opt"
	{ "usage", no_argument, 0, OPTION_USAGE },
#line 188 "cmdline.opt"
	{ "version", no_argument, 0, 'V' },
#line 188 "cmdline.opt"
	{0, 0, 0, 0}
#line 188
};
#line 188
#endif
#line 188

#line 188
static struct grecs_opthelp opthelp[] = {
#line 36 "cmdline.opt"
	{ NULL, NULL, 0, N_("Select program mode") },
#line 39 "cmdline.opt"
	{
#line 39
#ifdef HAVE_GETOPT_LONG
#line 39
	  "-E",
#line 39
#else
#line 39
	  "-E",
#line 39
#endif
#line 39
				   NULL, 0, N_("preprocess configuration file and exit") },
#line 45 "cmdline.opt"
	{
#line 45
#ifdef HAVE_GETOPT_LONG
#line 45
	  "-t, --lint",
#line 45
#else
#line 45
	  "-t",
#line 45
#endif
#line 45
				   NULL, 0, N_("check configuration file syntax and exit") },
#line 51 "cmdline.opt"
	{
#line 51
#ifdef HAVE_GETOPT_LONG
#line 51
	  "-i, --inetd",
#line 51
#else
#line 51
	  "-i",
#line 51
#endif
#line 51
				   NULL, 0, N_("inetd mode") },
#line 57 "cmdline.opt"
	{
#line 57
#ifdef HAVE_GETOPT_LONG
#line 57
	  "-r, --runtest",
#line 57
#else
#line 57
	  "-r",
#line 57
#endif
#line 57
				   NULL, 0, N_("run unit tests for module; subsequent arguments are treated as module name and unit test arguments; the -- marker introduces module initialization arguments") },
#line 63 "cmdline.opt"
	{ NULL, NULL, 0, N_("Modifiers") },
#line 67 "cmdline.opt"
	{
#line 67
#ifdef HAVE_GETOPT_LONG
#line 67
	  "--config",
#line 67
#else
#line 67
	  "",
#line 67
#endif
#line 67
				   N_("FILE"), 0, N_("read this configuration file") },
#line 73 "cmdline.opt"
	{
#line 73
#ifdef HAVE_GETOPT_LONG
#line 73
	  "-f, --foreground",
#line 73
#else
#line 73
	  "-f",
#line 73
#endif
#line 73
				   NULL, 0, N_("operate in foreground") },
#line 79 "cmdline.opt"
	{
#line 79
#ifdef HAVE_GETOPT_LONG
#line 79
	  "--stderr",
#line 79
#else
#line 79
	  "",
#line 79
#endif
#line 79
				   NULL, 0, N_("output diagnostic to stderr") },
#line 85 "cmdline.opt"
	{
#line 85
#ifdef HAVE_GETOPT_LONG
#line 85
	  "--syslog",
#line 85
#else
#line 85
	  "",
#line 85
#endif
#line 85
				   NULL, 0, N_("output diagnostic to syslog (default)") },
#line 91 "cmdline.opt"
	{
#line 91
#ifdef HAVE_GETOPT_LONG
#line 91
	  "-s, --single-process",
#line 91
#else
#line 91
	  "-s",
#line 91
#endif
#line 91
				   NULL, 0, N_("single-process mode") },
#line 97 "cmdline.opt"
	{
#line 97
#ifdef HAVE_GETOPT_LONG
#line 97
	  "-L, --load-dir",
#line 97
#else
#line 97
	  "-L",
#line 97
#endif
#line 97
				   N_("DIR"), 0, N_("prepend DIR to the module load path") },
#line 103 "cmdline.opt"
	{ NULL, NULL, 0, N_("Debugging") },
#line 107 "cmdline.opt"
	{
#line 107
#ifdef HAVE_GETOPT_LONG
#line 107
	  "-T, --transcript",
#line 107
#else
#line 107
	  "-T",
#line 107
#endif
#line 107
				   NULL, 0, N_("enable session transcript") },
#line 113 "cmdline.opt"
	{
#line 113
#ifdef HAVE_GETOPT_LONG
#line 113
	  "--no-transcript",
#line 113
#else
#line 113
	  "",
#line 113
#endif
#line 113
				   NULL, 0, N_("disable session transcript") },
#line 119 "cmdline.opt"
	{
#line 119
#ifdef HAVE_GETOPT_LONG
#line 119
	  "-x, --debug",
#line 119
#else
#line 119
	  "-x",
#line 119
#endif
#line 119
				   N_("NUMBER"), 0, N_("set debug verbosity level") },
#line 126 "cmdline.opt"
	{
#line 126
#ifdef HAVE_GETOPT_LONG
#line 126
	  "--source-info",
#line 126
#else
#line 126
	  "",
#line 126
#endif
#line 126
				   NULL, 0, N_("include source line information in the debugging output") },
#line 132 "cmdline.opt"
	{
#line 132
#ifdef HAVE_GETOPT_LONG
#line 132
	  "--trace-grammar",
#line 132
#else
#line 132
	  "",
#line 132
#endif
#line 132
				   NULL, 0, N_("trace parsing of configuration file") },
#line 138 "cmdline.opt"
	{
#line 138
#ifdef HAVE_GETOPT_LONG
#line 138
	  "--trace-lex",
#line 138
#else
#line 138
	  "",
#line 138
#endif
#line 138
				   NULL, 0, N_("trace config file lexer") },
#line 142 "cmdline.opt"
	{ NULL, NULL, 0, N_("Additional help") },
#line 146 "cmdline.opt"
	{
#line 146
#ifdef HAVE_GETOPT_LONG
#line 146
	  "--config-help",
#line 146
#else
#line 146
	  "",
#line 146
#endif
#line 146
				   NULL, 0, N_("show configuration file summary") },
#line 151 "cmdline.opt"
	{ NULL, NULL, 0, N_("Preprocessor control") },
#line 155 "cmdline.opt"
	{
#line 155
#ifdef HAVE_GETOPT_LONG
#line 155
	  "--preprocessor",
#line 155
#else
#line 155
	  "",
#line 155
#endif
#line 155
				   N_("PROG"), 0, N_("use PROG as a preprocessor for config file") },
#line 161 "cmdline.opt"
	{
#line 161
#ifdef HAVE_GETOPT_LONG
#line 161
	  "--no-preprocessor",
#line 161
#else
#line 161
	  "",
#line 161
#endif
#line 161
				   NULL, 0, N_("do not use external preprocessor") },
#line 167 "cmdline.opt"
	{
#line 167
#ifdef HAVE_GETOPT_LONG
#line 167
	  "-I, --include-dir",
#line 167
#else
#line 167
	  "-I",
#line 167
#endif
#line 167
				   N_("DIR"), 0, N_("add the directory DIR to the list of directories to be searched for preprocessor include files") },
#line 173 "cmdline.opt"
	{
#line 173
#ifdef HAVE_GETOPT_LONG
#line 173
	  "-D, --define",
#line 173
#else
#line 173
	  "-D",
#line 173
#endif
#line 173
				   N_("SYMBOL[=VALUE]"), 0, N_("define a preprocessor symbol") },
#line 188 "cmdline.opt"
	{ NULL, NULL, 0, N_("Other options") },
#line 188 "cmdline.opt"
	{
#line 188
#ifdef HAVE_GETOPT_LONG
#line 188
	  "-h, --help",
#line 188
#else
#line 188
	  "-h",
#line 188
#endif
#line 188
				   NULL, 0, N_("Give this help list") },
#line 188 "cmdline.opt"
	{
#line 188
#ifdef HAVE_GETOPT_LONG
#line 188
	  "--usage",
#line 188
#else
#line 188
	  "",
#line 188
#endif
#line 188
				   NULL, 0, N_("Give a short usage message") },
#line 188 "cmdline.opt"
	{
#line 188
#ifdef HAVE_GETOPT_LONG
#line 188
	  "-V, --version",
#line 188
#else
#line 188
	  "-V",
#line 188
#endif
#line 188
				   NULL, 0, N_("Print program version") },
#line 188 "cmdline.opt"
};
#line 27 "cmdline.opt"

#line 27

#line 27
#ifndef PACKAGE_URL
#line 27
# define PACKAGE_URL NULL
#line 27
#endif
#line 27
static struct grecs_proginfo proginfo = {
#line 27
	"dicod", /* progname */
#line 27
	NULL, /* subcommands */
#line 27
	N_("GNU dictionary server"), /* docstring */
#line 27
	"", /* args_doc */
#line 27
	opthelp, /* opthelp */
#line 27
	sizeof(opthelp)/sizeof(opthelp[0]), /* optcount */
#line 27
	NULL, /* print_help_hook */
#line 27
	NULL, /* print_version_hook */
#line 27
	PACKAGE_NAME,    /* package */
#line 27
	PACKAGE_VERSION, /* version */
#line 27
	NULL, /* license */
#line 27
	"2005-2017",/* copyright_year */
#line 27
	"Free Software Foundation, Inc.", /* copyright_holder */
#line 27
	NULL, /* *authors */
#line 27
	"<" PACKAGE_BUGREPORT ">", /* bug_address */
#line 27
	PACKAGE_URL, /* url */
#line 27
	NULL  /* epilogue */
#line 27
};
#line 27

#line 27
const char *program_version = "dicod" " (" PACKAGE_NAME ") " PACKAGE_VERSION;
#line 188 "cmdline.opt"

#line 188


void
get_options(int *pargc, char **pargv[], struct dicod_conf_override *conf)
{
    int i;
    int argc = *pargc;
    char **argv = *pargv;
    
    
#line 197
 {
#line 197
  int c;
#line 197

#line 197
  optind = 0;
#line 197
#ifdef HAVE_GETOPT_LONG
#line 197
  while ((c = getopt_long(argc, argv, "EtirfsL:Tx:I:D:hV",
#line 197
			  long_options, NULL)) != EOF)
#line 197
#else
#line 197
  while ((c = getopt(argc, argv, "EtirfsL:Tx:I:D:hV")) != EOF)
#line 197
#endif
#line 197
    {
#line 197
      switch (c)
#line 197
	{
#line 197
	default:
#line 197
	   exit(EX_USAGE);	   exit(EX_USAGE);
#line 197
	#line 39 "cmdline.opt"
	 case 'E':
#line 39
	  {
#line 39

   mode = MODE_PREPROC;

#line 41
	     break;
#line 41
	  }
#line 45 "cmdline.opt"
	 case 't':
#line 45
	  {
#line 45

   config_lint_option = 1;

#line 47
	     break;
#line 47
	  }
#line 51 "cmdline.opt"
	 case 'i':
#line 51
	  {
#line 51

   mode = MODE_INETD;

#line 53
	     break;
#line 53
	  }
#line 57 "cmdline.opt"
	 case 'r':
#line 57
	  {
#line 57

   mode = MODE_TEST;
   log_to_stderr = 1;
   goto lastoptlab;

#line 61
	     break;
#line 61
	  }
#line 67 "cmdline.opt"
	 case OPTION_CONFIG:
#line 67
	  {
#line 67

   config_file = optarg;

#line 69
	     break;
#line 69
	  }
#line 73 "cmdline.opt"
	 case 'f':
#line 73
	  {
#line 73

   foreground = 1;

#line 75
	     break;
#line 75
	  }
#line 79 "cmdline.opt"
	 case OPTION_STDERR:
#line 79
	  {
#line 79

   log_to_stderr = 1;

#line 81
	     break;
#line 81
	  }
#line 85 "cmdline.opt"
	 case OPTION_SYSLOG:
#line 85
	  {
#line 85

   log_to_stderr = 0;

#line 87
	     break;
#line 87
	  }
#line 91 "cmdline.opt"
	 case 's':
#line 91
	  {
#line 91

   single_process = 1;

#line 93
	     break;
#line 93
	  }
#line 97 "cmdline.opt"
	 case 'L':
#line 97
	  {
#line 97

   prepend_load_path = xdico_list_create();
   dico_list_set_free_item(prepend_load_path, dicod_free_item, NULL);
   xdico_list_append(prepend_load_path, xstrdup(optarg));

#line 101
	     break;
#line 101
	  }
#line 107 "cmdline.opt"
	 case 'T':
#line 107
	  {
#line 107

   conf->transcript = 1;

#line 109
	     break;
#line 109
	  }
#line 113 "cmdline.opt"
	 case OPTION_NO_TRANSCRIPT:
#line 113
	  {
#line 113

   conf->transcript = 0;

#line 115
	     break;
#line 115
	  }
#line 119 "cmdline.opt"
	 case 'x':
#line 119
	  {
#line 119

   debug_level_str = optarg;
   debug_level = atoi(optarg);

#line 122
	     break;
#line 122
	  }
#line 126 "cmdline.opt"
	 case OPTION_SOURCE_INFO:
#line 126
	  {
#line 126

   debug_source_info = 1;

#line 128
	     break;
#line 128
	  }
#line 132 "cmdline.opt"
	 case OPTION_TRACE_GRAMMAR:
#line 132
	  {
#line 132

   grecs_gram_trace(1);

#line 134
	     break;
#line 134
	  }
#line 138 "cmdline.opt"
	 case OPTION_TRACE_LEX:
#line 138
	  {
#line 138

   grecs_lex_trace(1);

#line 140
	     break;
#line 140
	  }
#line 146 "cmdline.opt"
	 case OPTION_CONFIG_HELP:
#line 146
	  {
#line 146

   config_help();
   exit(0);

#line 149
	     break;
#line 149
	  }
#line 155 "cmdline.opt"
	 case OPTION_PREPROCESSOR:
#line 155
	  {
#line 155

   grecs_preprocessor = optarg;

#line 157
	     break;
#line 157
	  }
#line 161 "cmdline.opt"
	 case OPTION_NO_PREPROCESSOR:
#line 161
	  {
#line 161

   grecs_preprocessor = NULL;

#line 163
	     break;
#line 163
	  }
#line 167 "cmdline.opt"
	 case 'I':
#line 167
	  {
#line 167

   grecs_preproc_add_include_dir(optarg); 

#line 169
	     break;
#line 169
	  }
#line 173 "cmdline.opt"
	 case 'D':
#line 173
	  {
#line 173

   char *p;

   if (!pp_cmd_acc)
       pp_cmd_acc = grecs_txtacc_create();
   grecs_txtacc_grow(pp_cmd_acc, " \"-D", 4);
   for (p = optarg; *p; p++) {
       if (*p == '\\' || *p == '"')
	   grecs_txtacc_grow_char(pp_cmd_acc, '\\');
       grecs_txtacc_grow_char(pp_cmd_acc, *p);
   }
   grecs_txtacc_grow_char(pp_cmd_acc, '"');			

#line 185
	     break;
#line 185
	  }
#line 188 "cmdline.opt"
	 case 'h':
#line 188
	  {
#line 188

#line 188
		grecs_print_help(&proginfo);
#line 188
		exit(0);
#line 188
	 
#line 188
	     break;
#line 188
	  }
#line 188 "cmdline.opt"
	 case OPTION_USAGE:
#line 188
	  {
#line 188

#line 188
		grecs_print_usage(&proginfo);
#line 188
		exit(0);
#line 188
	 
#line 188
	     break;
#line 188
	  }
#line 188 "cmdline.opt"
	 case 'V':
#line 188
	  {
#line 188

#line 188
	        /* Give version */
#line 188
		grecs_print_version(&proginfo, stdout);
#line 188
		exit(0);
#line 188
	 
#line 188
	     break;
#line 188
	  }

#line 197 "cmdline.opt"
	}
#line 197
    }
#line 197
lastoptlab:
#line 197
  i = optind;
#line 197
 }
#line 197

    if (pp_cmd_acc && grecs_preprocessor) {
	char *args, *cmd;

	grecs_txtacc_grow_char(pp_cmd_acc, 0);
	args = grecs_txtacc_finish(pp_cmd_acc, 0);
	cmd = grecs_malloc(strlen(grecs_preprocessor) +
			   strlen(args) + 1);
	strcpy(cmd, grecs_preprocessor);
	strcat(cmd, args);
	grecs_preprocessor = cmd;
    }
    grecs_txtacc_free(pp_cmd_acc);

    if (mode == MODE_TEST) {
	*pargc = argc - i;
	*pargv = argv + i;
    } else if (i < argc) {
	fprintf(stderr, "%s: unexpected arguments\n", argv[0]);
	exit(EX_USAGE);
    }
}
