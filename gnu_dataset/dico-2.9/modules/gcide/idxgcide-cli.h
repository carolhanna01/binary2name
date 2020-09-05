#line 539 "../../grecs/build-aux/getopt.m4"
/* -*- buffer-read-only: t -*- vi: set ro:
   THIS FILE IS GENERATED AUTOMATICALLY.  PLEASE DO NOT EDIT.
*/
#line 1 "idxgcide-cli.opt"
/* This file is part of GNU Dico. -*- c -*-
   Copyright (C) 2012-2019 Sergey Poznyakoff

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

#line 67 "idxgcide-cli.opt"
#ifdef HAVE_CONFIG_H
#line 67
# include <config.h>
#line 67
#endif
#line 67
#ifdef HAVE_GETOPT_H
#line 67
# include <getopt.h>
#line 67
#endif
#line 67
#include <unistd.h>
#line 67
#include <stdlib.h>
#line 67
#include <grecs.h>
#line 67
#include <grecs/opt.h>
#line 67
#ifndef EX_USAGE
#line 67
# define EX_USAGE 64
#line 67
#endif
#line 67
/* Option codes */
#line 67
enum {
#line 67
	_OPTION_INIT=255,
#line 67 "idxgcide-cli.opt"
	OPTION_USAGE,
#line 67 "idxgcide-cli.opt"
	MAX_OPTION
#line 67
};
#line 67
#ifdef HAVE_GETOPT_LONG
#line 67
static struct option long_options[] = {
#line 24 "idxgcide-cli.opt"
	{ "debug", no_argument, 0, 'd' },
#line 30 "idxgcide-cli.opt"
	{ "dry-run", no_argument, 0, 'n' },
#line 37 "idxgcide-cli.opt"
	{ "verbose", no_argument, 0, 'v' },
#line 43 "idxgcide-cli.opt"
	{ "page-size", required_argument, 0, 'p' },
#line 67 "idxgcide-cli.opt"
	{ "help", no_argument, 0, 'h' },
#line 67 "idxgcide-cli.opt"
	{ "usage", no_argument, 0, OPTION_USAGE },
#line 67 "idxgcide-cli.opt"
	{ "version", no_argument, 0, 'V' },
#line 67 "idxgcide-cli.opt"
	{0, 0, 0, 0}
#line 67
};
#line 67
#endif
#line 67

#line 67
static struct grecs_opthelp opthelp[] = {
#line 26 "idxgcide-cli.opt"
	{
#line 26
#ifdef HAVE_GETOPT_LONG
#line 26
	  "-d, --debug",
#line 26
#else
#line 26
	  "-d",
#line 26
#endif
#line 26
				   NULL, 0, N_("debug mode") },
#line 32 "idxgcide-cli.opt"
	{
#line 32
#ifdef HAVE_GETOPT_LONG
#line 32
	  "-n, --dry-run",
#line 32
#else
#line 32
	  "-n",
#line 32
#endif
#line 32
				   NULL, 0, N_("dry run: do nothing, but print everything") },
#line 39 "idxgcide-cli.opt"
	{
#line 39
#ifdef HAVE_GETOPT_LONG
#line 39
	  "-v, --verbose",
#line 39
#else
#line 39
	  "-v",
#line 39
#endif
#line 39
				   NULL, 0, N_("increase verbosity") },
#line 45 "idxgcide-cli.opt"
	{
#line 45
#ifdef HAVE_GETOPT_LONG
#line 45
	  "-p, --page-size",
#line 45
#else
#line 45
	  "-p",
#line 45
#endif
#line 45
				   N_("NUMBER"), 0, N_("set index page size") },
#line 67 "idxgcide-cli.opt"
	{ NULL, NULL, 0, N_("Other options") },
#line 67 "idxgcide-cli.opt"
	{
#line 67
#ifdef HAVE_GETOPT_LONG
#line 67
	  "-h, --help",
#line 67
#else
#line 67
	  "-h",
#line 67
#endif
#line 67
				   NULL, 0, N_("Give this help list") },
#line 67 "idxgcide-cli.opt"
	{
#line 67
#ifdef HAVE_GETOPT_LONG
#line 67
	  "--usage",
#line 67
#else
#line 67
	  "",
#line 67
#endif
#line 67
				   NULL, 0, N_("Give a short usage message") },
#line 67 "idxgcide-cli.opt"
	{
#line 67
#ifdef HAVE_GETOPT_LONG
#line 67
	  "-V, --version",
#line 67
#else
#line 67
	  "-V",
#line 67
#endif
#line 67
				   NULL, 0, N_("Print program version") },
#line 67 "idxgcide-cli.opt"
};
#line 17 "idxgcide-cli.opt"

#line 17

#line 17
#ifndef PACKAGE_URL
#line 17
# define PACKAGE_URL NULL
#line 17
#endif
#line 17
static struct grecs_proginfo proginfo = {
#line 17
	"idxgcide", /* progname */
#line 17
	NULL, /* subcommands */
#line 17
	N_("standalone indexer for GCIDE dictionaries"), /* docstring */
#line 17
	N_("DICTDIR [IDXDIR]"), /* args_doc */
#line 17
	opthelp, /* opthelp */
#line 17
	sizeof(opthelp)/sizeof(opthelp[0]), /* optcount */
#line 17
	NULL, /* print_help_hook */
#line 17
	NULL, /* print_version_hook */
#line 17
	PACKAGE_NAME,    /* package */
#line 17
	PACKAGE_VERSION, /* version */
#line 17
	NULL, /* license */
#line 17
	"2012-2017",/* copyright_year */
#line 17
	"Free Software Foundation, Inc.", /* copyright_holder */
#line 17
	NULL, /* *authors */
#line 17
	"<" PACKAGE_BUGREPORT ">", /* bug_address */
#line 17
	PACKAGE_URL, /* url */
#line 17
	NULL  /* epilogue */
#line 17
};
#line 17

#line 67 "idxgcide-cli.opt"

#line 67


void
get_options(int argc, char *argv[], int *index)
{
    
#line 72
 {
#line 72
  int c;
#line 72

#line 72
  optind = 0;
#line 72
#ifdef HAVE_GETOPT_LONG
#line 72
  while ((c = getopt_long(argc, argv, "dnvp:hV",
#line 72
			  long_options, NULL)) != EOF)
#line 72
#else
#line 72
  while ((c = getopt(argc, argv, "dnvp:hV")) != EOF)
#line 72
#endif
#line 72
    {
#line 72
      switch (c)
#line 72
	{
#line 72
	default:
#line 72
	   exit(EX_USAGE);	   exit(EX_USAGE);
#line 72
	#line 26 "idxgcide-cli.opt"
	 case 'd':
#line 26
	  {
#line 26

   yy_flex_debug = 1;

#line 28
	     break;
#line 28
	  }
#line 32 "idxgcide-cli.opt"
	 case 'n':
#line 32
	  {
#line 32

   dry_run_option = 1;
   verbose_option++;

#line 35
	     break;
#line 35
	  }
#line 39 "idxgcide-cli.opt"
	 case 'v':
#line 39
	  {
#line 39

   verbose_option++;

#line 41
	     break;
#line 41
	  }
#line 45 "idxgcide-cli.opt"
	 case 'p':
#line 45
	  {
#line 45

   char *p;
   idx_header.ihdr_pagesize = strtoul(optarg, &p, 10);
   switch (*p) {
   case 0:
       break;
   case 'g':
   case 'G':
       idx_header.ihdr_pagesize <<= 10;
   case 'm':
   case 'M':
       idx_header.ihdr_pagesize <<= 10;
   case 'k':
   case 'K':
       idx_header.ihdr_pagesize <<= 10;
       break;
   default:
       dico_log(L_ERR, 0, _("not a valid size: %s"), optarg);
       exit(EX_USAGE);
   }

#line 65
	     break;
#line 65
	  }
#line 67 "idxgcide-cli.opt"
	 case 'h':
#line 67
	  {
#line 67

#line 67
		grecs_print_help(&proginfo);
#line 67
		exit(0);
#line 67
	 
#line 67
	     break;
#line 67
	  }
#line 67 "idxgcide-cli.opt"
	 case OPTION_USAGE:
#line 67
	  {
#line 67

#line 67
		grecs_print_usage(&proginfo);
#line 67
		exit(0);
#line 67
	 
#line 67
	     break;
#line 67
	  }
#line 67 "idxgcide-cli.opt"
	 case 'V':
#line 67
	  {
#line 67

#line 67
	        /* Give version */
#line 67
		grecs_print_version(&proginfo, stdout);
#line 67
		exit(0);
#line 67
	 
#line 67
	     break;
#line 67
	  }

#line 72 "idxgcide-cli.opt"
	}
#line 72
    }
#line 72
  *index = optind;
#line 72
 }
#line 72

}

