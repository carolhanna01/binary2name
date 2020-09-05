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

#include <dico-priv.h>
#include <getopt.h>
#include <sysexits.h>

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
#line 31 "cmdline.opt"
	OPTION_HOST,
#line 49 "cmdline.opt"
	OPTION_SOURCE,
#line 70 "cmdline.opt"
	OPTION_LEVDIST,
#line 125 "cmdline.opt"
	OPTION_SASL,
#line 131 "cmdline.opt"
	OPTION_NOSASL,
#line 150 "cmdline.opt"
	OPTION_AUTOLOGIN,
#line 175 "cmdline.opt"
	OPTION_TIME_STAMP,
#line 182 "cmdline.opt"
	OPTION_SOURCE_INFO,
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
#line 31 "cmdline.opt"
	{ "host", required_argument, 0, OPTION_HOST },
#line 37 "cmdline.opt"
	{ "port", required_argument, 0, 'p' },
#line 43 "cmdline.opt"
	{ "database", required_argument, 0, 'd' },
#line 49 "cmdline.opt"
	{ "source", required_argument, 0, OPTION_SOURCE },
#line 57 "cmdline.opt"
	{ "match", no_argument, 0, 'm' },
#line 63 "cmdline.opt"
	{ "strategy", required_argument, 0, 's' },
#line 70 "cmdline.opt"
	{ "levdist", required_argument, 0, OPTION_LEVDIST },
#line 80 "cmdline.opt"
	{ "dbs", no_argument, 0, 'D' },
#line 86 "cmdline.opt"
	{ "strategies", no_argument, 0, 'S' },
#line 92 "cmdline.opt"
	{ "serverhelp", no_argument, 0, 'H' },
#line 98 "cmdline.opt"
	{ "info", required_argument, 0, 'i' },
#line 105 "cmdline.opt"
	{ "serverinfo", no_argument, 0, 'I' },
#line 111 "cmdline.opt"
	{ "quiet", no_argument, 0, 'q' },
#line 119 "cmdline.opt"
	{ "noauth", no_argument, 0, 'a' },
#line 125 "cmdline.opt"
	{ "sasl", no_argument, 0, OPTION_SASL },
#line 131 "cmdline.opt"
	{ "nosasl", no_argument, 0, OPTION_NOSASL },
#line 137 "cmdline.opt"
	{ "user", required_argument, 0, 'u' },
#line 143 "cmdline.opt"
	{ "key", required_argument, 0, 'k' },
#line 150 "cmdline.opt"
	{ "autologin", required_argument, 0, OPTION_AUTOLOGIN },
#line 156 "cmdline.opt"
	{ "client", required_argument, 0, 'c' },
#line 163 "cmdline.opt"
	{ "transcript", no_argument, 0, 't' },
#line 169 "cmdline.opt"
	{ "verbose", no_argument, 0, 'v' },
#line 175 "cmdline.opt"
	{ "time-stamp", no_argument, 0, OPTION_TIME_STAMP },
#line 182 "cmdline.opt"
	{ "source-info", no_argument, 0, OPTION_SOURCE_INFO },
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
#line 29 "cmdline.opt"
	{ NULL, NULL, 0, N_("Server selection") },
#line 33 "cmdline.opt"
	{
#line 33
#ifdef HAVE_GETOPT_LONG
#line 33
	  "--host",
#line 33
#else
#line 33
	  "",
#line 33
#endif
#line 33
				   N_("SERVER"), 0, N_("connect to this server") },
#line 39 "cmdline.opt"
	{
#line 39
#ifdef HAVE_GETOPT_LONG
#line 39
	  "-p, --port",
#line 39
#else
#line 39
	  "-p",
#line 39
#endif
#line 39
				   N_("SERVICE"), 0, N_("specify port to connect to") },
#line 45 "cmdline.opt"
	{
#line 45
#ifdef HAVE_GETOPT_LONG
#line 45
	  "-d, --database",
#line 45
#else
#line 45
	  "-d",
#line 45
#endif
#line 45
				   N_("NAME"), 0, N_("select a database to search") },
#line 51 "cmdline.opt"
	{
#line 51
#ifdef HAVE_GETOPT_LONG
#line 51
	  "--source",
#line 51
#else
#line 51
	  "",
#line 51
#endif
#line 51
				   N_("ADDR"), 0, N_("set a source address for TCP connections") },
#line 55 "cmdline.opt"
	{ NULL, NULL, 0, N_("Operation modes") },
#line 59 "cmdline.opt"
	{
#line 59
#ifdef HAVE_GETOPT_LONG
#line 59
	  "-m, --match",
#line 59
#else
#line 59
	  "-m",
#line 59
#endif
#line 59
				   NULL, 0, N_("match instead of define") },
#line 65 "cmdline.opt"
	{
#line 65
#ifdef HAVE_GETOPT_LONG
#line 65
	  "-s, --strategy",
#line 65
#else
#line 65
	  "-s",
#line 65
#endif
#line 65
				   N_("NAME"), 0, N_("select a strategy for matching; implies --match") },
#line 73 "cmdline.opt"
	{
#line 73
#ifdef HAVE_GETOPT_LONG
#line 73
	  "--levdist",
#line 73
#else
#line 73
	  "",
#line 73
#endif
#line 73
				   N_("N"), 0, N_("set maximum Levenshtein distance") },
#line 82 "cmdline.opt"
	{
#line 82
#ifdef HAVE_GETOPT_LONG
#line 82
	  "-D, --dbs",
#line 82
#else
#line 82
	  "-D",
#line 82
#endif
#line 82
				   NULL, 0, N_("show available databases") },
#line 88 "cmdline.opt"
	{
#line 88
#ifdef HAVE_GETOPT_LONG
#line 88
	  "-S, --strategies",
#line 88
#else
#line 88
	  "-S",
#line 88
#endif
#line 88
				   NULL, 0, N_("show available search strategies") },
#line 94 "cmdline.opt"
	{
#line 94
#ifdef HAVE_GETOPT_LONG
#line 94
	  "-H, --serverhelp",
#line 94
#else
#line 94
	  "-H",
#line 94
#endif
#line 94
				   NULL, 0, N_("show server help") },
#line 100 "cmdline.opt"
	{
#line 100
#ifdef HAVE_GETOPT_LONG
#line 100
	  "-i, --info",
#line 100
#else
#line 100
	  "-i",
#line 100
#endif
#line 100
				   N_("DBNAME"), 0, N_("show information about database DBNAME") },
#line 107 "cmdline.opt"
	{
#line 107
#ifdef HAVE_GETOPT_LONG
#line 107
	  "-I, --serverinfo",
#line 107
#else
#line 107
	  "-I",
#line 107
#endif
#line 107
				   NULL, 0, N_("show information about the server") },
#line 113 "cmdline.opt"
	{
#line 113
#ifdef HAVE_GETOPT_LONG
#line 113
	  "-q, --quiet",
#line 113
#else
#line 113
	  "-q",
#line 113
#endif
#line 113
				   NULL, 0, N_("do not print the normal dico welcome") },
#line 117 "cmdline.opt"
	{ NULL, NULL, 0, N_("Authentication") },
#line 121 "cmdline.opt"
	{
#line 121
#ifdef HAVE_GETOPT_LONG
#line 121
	  "-a, --noauth",
#line 121
#else
#line 121
	  "-a",
#line 121
#endif
#line 121
				   NULL, 0, N_("disable authentication") },
#line 127 "cmdline.opt"
	{
#line 127
#ifdef HAVE_GETOPT_LONG
#line 127
	  "--sasl",
#line 127
#else
#line 127
	  "",
#line 127
#endif
#line 127
				   NULL, 0, N_("enable SASL authentication (default)") },
#line 133 "cmdline.opt"
	{
#line 133
#ifdef HAVE_GETOPT_LONG
#line 133
	  "--nosasl",
#line 133
#else
#line 133
	  "",
#line 133
#endif
#line 133
				   NULL, 0, N_("disable SASL authentication") },
#line 139 "cmdline.opt"
	{
#line 139
#ifdef HAVE_GETOPT_LONG
#line 139
	  "-u, --user",
#line 139
#else
#line 139
	  "-u",
#line 139
#endif
#line 139
				   N_("NAME"), 0, N_("set user name for authentication") },
#line 146 "cmdline.opt"
	{
#line 146
#ifdef HAVE_GETOPT_LONG
#line 146
	  "-k, --key",
#line 146
#else
#line 146
	  "-k",
#line 146
#endif
#line 146
				   N_("STRING"), 0, N_("set shared secret for authentication") },
#line 152 "cmdline.opt"
	{
#line 152
#ifdef HAVE_GETOPT_LONG
#line 152
	  "--autologin",
#line 152
#else
#line 152
	  "",
#line 152
#endif
#line 152
				   N_("NAME"), 0, N_("set the name of autologin file to use") },
#line 158 "cmdline.opt"
	{
#line 158
#ifdef HAVE_GETOPT_LONG
#line 158
	  "-c, --client",
#line 158
#else
#line 158
	  "-c",
#line 158
#endif
#line 158
				   N_("STRING"), 0, N_("additional text for client command") },
#line 162 "cmdline.opt"
	{ NULL, NULL, 0, N_("Debugging") },
#line 165 "cmdline.opt"
	{
#line 165
#ifdef HAVE_GETOPT_LONG
#line 165
	  "-t, --transcript",
#line 165
#else
#line 165
	  "-t",
#line 165
#endif
#line 165
				   NULL, 0, N_("enable session transcript") },
#line 171 "cmdline.opt"
	{
#line 171
#ifdef HAVE_GETOPT_LONG
#line 171
	  "-v, --verbose",
#line 171
#else
#line 171
	  "-v",
#line 171
#endif
#line 171
				   NULL, 0, N_("increase debugging verbosity level") },
#line 177 "cmdline.opt"
	{
#line 177
#ifdef HAVE_GETOPT_LONG
#line 177
	  "--time-stamp",
#line 177
#else
#line 177
	  "",
#line 177
#endif
#line 177
				   NULL, 0, N_("include time stamp in the debugging output") },
#line 184 "cmdline.opt"
	{
#line 184
#ifdef HAVE_GETOPT_LONG
#line 184
	  "--source-info",
#line 184
#else
#line 184
	  "",
#line 184
#endif
#line 184
				   NULL, 0, N_("include source line information in the debugging output") },
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
#line 21 "cmdline.opt"

#line 21

#line 21
#ifndef PACKAGE_URL
#line 21
# define PACKAGE_URL NULL
#line 21
#endif
#line 21
static struct grecs_proginfo proginfo = {
#line 21
	"dico", /* progname */
#line 21
	NULL, /* subcommands */
#line 21
	N_("GNU dictionary client program"), /* docstring */
#line 21
	N_("[URL-or-WORD]"), /* args_doc */
#line 21
	opthelp, /* opthelp */
#line 21
	sizeof(opthelp)/sizeof(opthelp[0]), /* optcount */
#line 21
	NULL, /* print_help_hook */
#line 21
	NULL, /* print_version_hook */
#line 21
	PACKAGE_NAME,    /* package */
#line 21
	PACKAGE_VERSION, /* version */
#line 21
	NULL, /* license */
#line 21
	"2005-2017",/* copyright_year */
#line 21
	"Free Software Foundation, Inc.", /* copyright_holder */
#line 21
	NULL, /* *authors */
#line 21
	"<" PACKAGE_BUGREPORT ">", /* bug_address */
#line 21
	PACKAGE_URL, /* url */
#line 21
	NULL  /* epilogue */
#line 21
};
#line 21

#line 188 "cmdline.opt"

#line 188


void
get_options (int argc, char *argv[], int *index)
{
    
#line 193
 {
#line 193
  int c;
#line 193

#line 193
  optind = 0;
#line 193
#ifdef HAVE_GETOPT_LONG
#line 193
  while ((c = getopt_long(argc, argv, "p:d:ms:DSHi:Iqau:k:c:tvhV",
#line 193
			  long_options, NULL)) != EOF)
#line 193
#else
#line 193
  while ((c = getopt(argc, argv, "p:d:ms:DSHi:Iqau:k:c:tvhV")) != EOF)
#line 193
#endif
#line 193
    {
#line 193
      switch (c)
#line 193
	{
#line 193
	default:
#line 193
	   	   exit(EX_USAGE);
#line 193
	#line 33 "cmdline.opt"
	 case OPTION_HOST:
#line 33
	  {
#line 33

  xdico_assign_string(&dico_url.host, optarg);

#line 35
	     break;
#line 35
	  }
#line 39 "cmdline.opt"
	 case 'p':
#line 39
	  {
#line 39

  xdico_assign_string(&dico_url.port, optarg);

#line 41
	     break;
#line 41
	  }
#line 45 "cmdline.opt"
	 case 'd':
#line 45
	  {
#line 45

  xdico_assign_string(&dico_url.req.database, optarg);  

#line 47
	     break;
#line 47
	  }
#line 51 "cmdline.opt"
	 case OPTION_SOURCE:
#line 51
	  {
#line 51

  source_addr = optarg;

#line 53
	     break;
#line 53
	  }
#line 59 "cmdline.opt"
	 case 'm':
#line 59
	  {
#line 59

  mode = mode_match;   

#line 61
	     break;
#line 61
	  }
#line 65 "cmdline.opt"
	 case 's':
#line 65
	  {
#line 65

  xdico_assign_string(&dico_url.req.strategy, optarg);
  mode = mode_match;

#line 68
	     break;
#line 68
	  }
#line 73 "cmdline.opt"
	 case OPTION_LEVDIST:
#line 73
	  {
#line 73

  char *p;
  levenshtein_threshold = strtoul(optarg, &p, 10);
  if (*p)
      dico_die(1, L_ERR, 0, _("%s: invalid number"), optarg);

#line 78
	     break;
#line 78
	  }
#line 82 "cmdline.opt"
	 case 'D':
#line 82
	  {
#line 82

  mode = mode_dbs;  

#line 84
	     break;
#line 84
	  }
#line 88 "cmdline.opt"
	 case 'S':
#line 88
	  {
#line 88

  mode = mode_strats;

#line 90
	     break;
#line 90
	  }
#line 94 "cmdline.opt"
	 case 'H':
#line 94
	  {
#line 94

  mode = mode_help;

#line 96
	     break;
#line 96
	  }
#line 100 "cmdline.opt"
	 case 'i':
#line 100
	  {
#line 100

  mode = mode_info;
  dico_url.req.database = optarg;

#line 103
	     break;
#line 103
	  }
#line 107 "cmdline.opt"
	 case 'I':
#line 107
	  {
#line 107

  mode = mode_server;

#line 109
	     break;
#line 109
	  }
#line 113 "cmdline.opt"
	 case 'q':
#line 113
	  {
#line 113

  quiet_option = 1;

#line 115
	     break;
#line 115
	  }
#line 121 "cmdline.opt"
	 case 'a':
#line 121
	  {
#line 121

  noauth_option = 1;

#line 123
	     break;
#line 123
	  }
#line 127 "cmdline.opt"
	 case OPTION_SASL:
#line 127
	  {
#line 127

  sasl_enable(1);

#line 129
	     break;
#line 129
	  }
#line 133 "cmdline.opt"
	 case OPTION_NOSASL:
#line 133
	  {
#line 133

  sasl_enable(0);

#line 135
	     break;
#line 135
	  }
#line 139 "cmdline.opt"
	 case 'u':
#line 139
	  {
#line 139

  default_cred.user = optarg;

#line 141
	     break;
#line 141
	  }
#line 146 "cmdline.opt"
	 case 'k':
#line 146
	  {
#line 146

  default_cred.pass = optarg;

#line 148
	     break;
#line 148
	  }
#line 152 "cmdline.opt"
	 case OPTION_AUTOLOGIN:
#line 152
	  {
#line 152

  autologin_file = optarg;

#line 154
	     break;
#line 154
	  }
#line 158 "cmdline.opt"
	 case 'c':
#line 158
	  {
#line 158

  client = optarg;

#line 160
	     break;
#line 160
	  }
#line 165 "cmdline.opt"
	 case 't':
#line 165
	  {
#line 165

  transcript = 1;

#line 167
	     break;
#line 167
	  }
#line 171 "cmdline.opt"
	 case 'v':
#line 171
	  {
#line 171

  debug_level++;

#line 173
	     break;
#line 173
	  }
#line 177 "cmdline.opt"
	 case OPTION_TIME_STAMP:
#line 177
	  {
#line 177

  int n = 1;
  dico_stream_ioctl(debug_stream, DICO_DBG_CTL_SET_TS, &n);

#line 180
	     break;
#line 180
	  }
#line 184 "cmdline.opt"
	 case OPTION_SOURCE_INFO:
#line 184
	  {
#line 184

  debug_source_info = 1;

#line 186
	     break;
#line 186
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

#line 193 "cmdline.opt"
	}
#line 193
    }
#line 193
  *index = optind;
#line 193
 }
#line 193

}

static char gplv3_text[] = "\
   GNU Dico is free software; you can redistribute it and/or modify\n\
   it under the terms of the GNU General Public License as published by\n\
   the Free Software Foundation; either version 3, or (at your option)\n\
   any later version.\n\
\n\
   GNU Dico is distributed in the hope that it will be useful,\n\
   but WITHOUT ANY WARRANTY; without even the implied warranty of\n\
   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the\n\
   GNU General Public License for more details.\n\
\n\
   You should have received a copy of the GNU General Public License\n\
   along with GNU Dico.  If not, see <http://www.gnu.org/licenses/>.\n";

void
ds_warranty(int argc, char **argv)
{
    grecs_print_version_only(&proginfo, stdout);
    putchar('\n');
    printf("%s", gplv3_text);
}

void
shell_banner()
{
    grecs_print_version(&proginfo, stdout);
    printf("%s\n\n", _("Type ? for help summary"));
}

