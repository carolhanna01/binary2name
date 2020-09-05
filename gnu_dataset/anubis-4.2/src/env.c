/* -*- buffer-read-only: t -*- vi: set ro:
   THIS FILE IS GENERATED AUTOMATICALLY.  PLEASE DO NOT EDIT.
*/

#line 1 "env.opt"
/* -*- c -*-
   This file is part of GNU Anubis.
   Copyright (C) 2001-2014 The Anubis Team.

   GNU Anubis is free software; you can redistribute it and/or modify
   it under the terms of the GNU General Public License as published by
   the Free Software Foundation; either version 3 of the License, or
   (at your option) any later version.

   GNU Anubis is distributed in the hope that it will be useful,
   but WITHOUT ANY WARRANTY; without even the implied warranty of
   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
   GNU General Public License for more details.

   You should have received a copy of the GNU General Public License
   along with GNU Anubis.  If not, see <http://www.gnu.org/licenses/>.
*/

#include "headers.h"
#include <grp.h>
#include <getopt.h>
#include "extern.h"
#include "rcfile.h"

#ifdef HAVE_PAM
pam_handle_t *pamh;
static struct pam_conv conv = {
  misc_conv,
  NULL
};
#endif /* HAVE_PAM */

static char *pidfile;

#line 35 "env.opt"

#line 35
void print_help(void);
#line 35
void print_usage(void);
#line 164 "env.opt"

#line 164
/* Option codes */
#line 164
enum {
#line 164
	_OPTION_INIT=255,
#line 164
	#line 84 "env.opt"

#line 84
	OPTION_FROM,
#line 119 "env.opt"

#line 119
	OPTION_ALTRC,
#line 126 "env.opt"

#line 126
	OPTION_NORC,
#line 139 "env.opt"

#line 139
	OPTION_SHOW_CONFIG_OPTIONS,
#line 145 "env.opt"

#line 145
	OPTION_LOCATION_COLUMN,
#line 151 "env.opt"

#line 151
	OPTION_RELAX_PERM_CHECK,
#line 157 "env.opt"

#line 157
	OPTION_PID_FILE,
#line 164 "env.opt"

#line 164
	OPTION_USAGE,
#line 164 "env.opt"

#line 164
	OPTION_VERSION,

#line 164 "env.opt"
	MAX_OPTION
#line 164
};
#line 164
static struct option long_options[] = {
#line 164
	#line 37 "env.opt"

#line 37
	{ "bind", required_argument, 0, 'b' },
#line 48 "env.opt"

#line 48
	{ "remote-mta", required_argument, 0, 'r' },
#line 55 "env.opt"

#line 55
	{ "local-mta", required_argument, 0, 'l' },
#line 69 "env.opt"

#line 69
	{ "mode", required_argument, 0, 'm' },
#line 78 "env.opt"

#line 78
	{ "foreground", no_argument, 0, 'f' },
#line 84 "env.opt"

#line 84
	{ "from", required_argument, 0, OPTION_FROM },
#line 90 "env.opt"

#line 90
	{ "stdio", no_argument, 0, 'i' },
#line 99 "env.opt"

#line 99
	{ "silent", no_argument, 0, 's' },
#line 105 "env.opt"

#line 105
	{ "verbose", no_argument, 0, 'v' },
#line 111 "env.opt"

#line 111
	{ "debug", no_argument, 0, 'D' },
#line 119 "env.opt"

#line 119
	{ "altrc", required_argument, 0, OPTION_ALTRC },
#line 126 "env.opt"

#line 126
	{ "norc", no_argument, 0, OPTION_NORC },
#line 132 "env.opt"

#line 132
	{ "check-config", required_argument, 0, 'c' },
#line 139 "env.opt"

#line 139
	{ "show-config-options", no_argument, 0, OPTION_SHOW_CONFIG_OPTIONS },
#line 145 "env.opt"

#line 145
	{ "location-column", no_argument, 0, OPTION_LOCATION_COLUMN },
#line 151 "env.opt"

#line 151
	{ "relax-perm-check", no_argument, 0, OPTION_RELAX_PERM_CHECK },
#line 157 "env.opt"

#line 157
	{ "pid-file", required_argument, 0, OPTION_PID_FILE },
#line 164 "env.opt"

#line 164
	{ "help", no_argument, 0, 'h' },
#line 164 "env.opt"

#line 164
	{ "usage", no_argument, 0, OPTION_USAGE },
#line 164 "env.opt"

#line 164
	{ "version", no_argument, 0, OPTION_VERSION },

#line 164 "env.opt"
	{0, 0, 0, 0}
#line 164
};
#line 164
static struct opthelp {
#line 164
        const char *opt;
#line 164
        const char *arg;
#line 164
        int is_optional;
#line 164
        const char *descr;
#line 164
} opthelp[] = {
#line 164
	#line 41 "env.opt"

#line 41
	{ "-b, --bind", N_("[HOST:]PORT"), 0, N_("Specify the TCP port on which GNU Anubis listens for connections; the default HOST is INADDR_ANY, and default PORT is 24 (private mail system)") },
#line 51 "env.opt"

#line 51
	{ "-r, --remote-mta", N_("[HOST:]PORT"), 0, N_("Specify a remote SMTP host name or IP address; the default is 25") },
#line 59 "env.opt"

#line 59
	{ "-l, --local-mta", N_("FILE"), 0, N_("Execute a local SMTP server, which works on standard input and output (inetd-type program); this option excludes the `--remote-mta' option") },
#line 72 "env.opt"

#line 72
	{ "-m, --mode", N_("MODE"), 0, N_("Select operation mode; MODE is one of \"transparent\", \"auth\" or \"mda\"") },
#line 80 "env.opt"

#line 80
	{ "-f, --foreground", NULL, 0, N_("Foreground mode") },
#line 86 "env.opt"

#line 86
	{ "--from", N_("EMAIL"), 0, N_("Specify sender address (implies MDA mode)") },
#line 93 "env.opt"

#line 93
	{ "-i, --stdio", NULL, 0, N_("Use the SMTP protocol (OMP/Tunnel) as described in RFC 821 on standard input and output") },
#line 97 "env.opt"

#line 97
	{ NULL, NULL, 0, N_("Output options") },
#line 100 "env.opt"

#line 100
	{ "-s, --silent", NULL, 0, N_("Work silently") },
#line 106 "env.opt"

#line 106
	{ "-v, --verbose", NULL, 0, N_("Work noisily") },
#line 112 "env.opt"

#line 112
	{ "-D, --debug", NULL, 0, N_("Debug mode") },
#line 117 "env.opt"

#line 117
	{ NULL, NULL, 0, N_("Miscellaneous options") },
#line 121 "env.opt"

#line 121
	{ "--altrc", N_("FILE"), 0, N_("Specify alternate system configuration file") },
#line 128 "env.opt"

#line 128
	{ "--norc", NULL, 0, N_("Ignore system configuration file") },
#line 134 "env.opt"

#line 134
	{ "-c, --check-config", N_("DEBUG-LEVEL"), 0, N_("Run the configuration file syntax checker") },
#line 141 "env.opt"

#line 141
	{ "--show-config-options", NULL, 0, N_("Print a list of configuration options used to build GNU Anubis") },
#line 147 "env.opt"

#line 147
	{ "--location-column", NULL, 0, N_("Print location column numbers in parser diagnostics") },
#line 153 "env.opt"

#line 153
	{ "--relax-perm-check", NULL, 0, N_("Do not check user configuration file permissions") },
#line 159 "env.opt"

#line 159
	{ "--pid-file", N_("FILE"), 0, N_("Store the PID of the running daemon in FILE") },
#line 164 "env.opt"

#line 164
	{ NULL, NULL, 0, N_("Other options") },
#line 164 "env.opt"

#line 164
	{ "-h, --help", NULL, 0, N_("Give this help list") },
#line 164 "env.opt"

#line 164
	{ "--usage", NULL, 0, N_("Give a short usage message") },
#line 164 "env.opt"

#line 164
	{ "--version", NULL, 0, N_("Print program version") },

#line 164 "env.opt"
};
#line 35 "env.opt"

#line 35
const char *program_version = "anubis" " (" PACKAGE_STRING ")";
#line 35
static char doc[] = N_("SMTP message submission daemon.");
#line 35
static char args_doc[] = N_("");
#line 35
static char *after_desc = NULL;
#line 35
const char *program_bug_address = "<" PACKAGE_BUGREPORT ">";
#line 35
		    
#line 35
#define DESCRCOLUMN 30
#line 35
#define RMARGIN 79
#line 35
#define GROUPCOLUMN 2
#line 35
#define USAGECOLUMN 13
#line 35
		    
#line 35
static void
#line 35
indent (size_t start, size_t col)
#line 35
{
#line 35
  for (; start < col; start++)
#line 35
    putchar (' ');
#line 35
}
#line 35
		    
#line 35
static void
#line 35
print_option_descr (const char *descr, size_t lmargin, size_t rmargin)
#line 35
{
#line 35
  while (*descr)
#line 35
    {
#line 35
      size_t s = 0;
#line 35
      size_t i;
#line 35
      size_t width = rmargin - lmargin;
#line 35
      
#line 35
      while (*descr && (*descr == ' ' || *descr == '\t'))
#line 35
	descr++;
#line 35
      for (i = 0; ; i++)
#line 35
	{
#line 35
	  if (descr[i] == '\n')
#line 35
	    {
#line 35
	      s = i;
#line 35
	      break;
#line 35
	    }
#line 35
	  if (descr[i] == 0 || (descr[i] == ' ' || descr[i] == '\t'))
#line 35
	    {
#line 35
	      if (i > width)
#line 35
		break;
#line 35
	      s = i;
#line 35
	      if (descr[i] == 0)
#line 35
		break;
#line 35
	    }
#line 35
	}
#line 35
      if (s > width)
#line 35
	while (!(descr[s] == ' ' || descr[s] == '\t'))
#line 35
	  s--;
#line 35
      printf ("%*.*s\n", s, s, descr);
#line 35
      descr += s;
#line 35
      if (*descr)
#line 35
	{
#line 35
	  indent (0, lmargin);
#line 35
	  descr++;
#line 35
	}
#line 35
    }
#line 35
}
#line 35

#line 35
void
#line 35
print_help(void)
#line 35
{
#line 35
  unsigned i;
#line 35
  
#line 35
  printf ("%s %s [%s]... %s\n", _("Usage:"), "anubis", _("OPTION"),
#line 35
	  gettext (args_doc)); 
#line 35
  if (doc[0])
#line 35
    print_option_descr(gettext (doc), 0, RMARGIN);
#line 35
  putchar ('\n');
#line 35

#line 35
  for (i = 0; i < sizeof (opthelp) / sizeof (opthelp[0]); i++)
#line 35
    {
#line 35
      unsigned n;
#line 35
      if (opthelp[i].opt)
#line 35
	{
#line 35
	  n = printf ("  %s", opthelp[i].opt);
#line 35
	  if (opthelp[i].arg)
#line 35
	    {
#line 35
	      char *cb, *ce;
#line 35
	      if (strlen (opthelp[i].opt) == 2)
#line 35
		{
#line 35
		  if (!opthelp[i].is_optional)
#line 35
		    {
#line 35
		      putchar (' ');
#line 35
		      n++;
#line 35
		    }
#line 35
		}
#line 35
	      else
#line 35
		{
#line 35
		  putchar ('=');
#line 35
		  n++;
#line 35
		}
#line 35
	      if (opthelp[i].is_optional)
#line 35
		{
#line 35
		  cb = "[";
#line 35
		  ce = "]";
#line 35
		}
#line 35
	      else
#line 35
		cb = ce = "";
#line 35
	      n += printf ("%s%s%s", cb, gettext (opthelp[i].arg), ce);
#line 35
	    }
#line 35
	  if (n >= DESCRCOLUMN)
#line 35
	    {
#line 35
	      putchar ('\n');
#line 35
	      n = 0;
#line 35
	    }
#line 35
	  indent (n, DESCRCOLUMN);
#line 35
	  print_option_descr (gettext (opthelp[i].descr), DESCRCOLUMN, RMARGIN);
#line 35
	}
#line 35
      else
#line 35
	{
#line 35
	  if (i)
#line 35
	    putchar ('\n');
#line 35
	  indent (0, GROUPCOLUMN);
#line 35
	  print_option_descr (gettext (opthelp[i].descr),
#line 35
			      GROUPCOLUMN, RMARGIN);
#line 35
	  putchar ('\n');
#line 35
	}
#line 35
    }
#line 35
  
#line 35
  putchar ('\n');
#line 35
  print_option_descr (_("Mandatory or optional arguments to long options are also mandatory or optional for any corresponding short options."), 0, RMARGIN);
#line 35
  putchar ('\n');
#line 35

#line 35
  if (after_desc)
#line 35
    {
#line 35
      print_option_descr (gettext (after_desc), 0, RMARGIN);
#line 35
    }	
#line 35
  printf (_("Report bugs to %s.\n"), program_bug_address);
#line 35
}
#line 35

#line 35
void
#line 35
print_usage(void)
#line 35
{
#line 35
  unsigned i;
#line 35
  int f = 0;
#line 35
  unsigned n;
#line 35
  char buf[RMARGIN+1];
#line 35

#line 35
#define FLUSH                        do                                   {                              	  buf[n] = 0;              	  printf ("%s\n", buf);    	  n = USAGECOLUMN;         	  memset (buf, ' ', n);        }                                while (0)
#line 35
#define ADDC(c)   do { if (n == RMARGIN) FLUSH; buf[n++] = c; } while (0)
#line 35

#line 35
  n = snprintf (buf, sizeof buf, "%s %s ", _("Usage:"), "anubis");
#line 35

#line 35
  /* Print a list of short options without arguments. */
#line 35
  for (i = 0; i < sizeof (opthelp) / sizeof (opthelp[0]); i++)
#line 35
    {
#line 35
      if (opthelp[i].opt && opthelp[i].descr && opthelp[i].opt[1] != '-'
#line 35
	  && opthelp[i].arg == NULL)
#line 35
	{
#line 35
	  if (f == 0)
#line 35
	    {
#line 35
	      ADDC('[');
#line 35
	      ADDC('-');
#line 35
	      f = 1;
#line 35
	    }
#line 35
	  ADDC(opthelp[i].opt[1]);
#line 35
	}
#line 35
    }
#line 35
  if (f)
#line 35
    ADDC(']');
#line 35

#line 35
  /* Print a list of short options with arguments. */
#line 35
  for (i = 0; i < sizeof (opthelp) / sizeof (opthelp[0]); i++)
#line 35
    {
#line 35
      if (opthelp[i].opt && opthelp[i].descr && opthelp[i].opt[1] != '-'
#line 35
	  && opthelp[i].arg)
#line 35
	{
#line 35
	  size_t len = 5 
#line 35
	                + strlen (opthelp[i].arg)
#line 35
			   + (opthelp[i].is_optional ? 2 : 1);
#line 35
	  if (n + len > RMARGIN) FLUSH;
#line 35
	  buf[n++] = ' '; 
#line 35
	  buf[n++] = '['; 
#line 35
	  buf[n++] = '-';
#line 35
	  buf[n++] = opthelp[i].opt[1];
#line 35
	  if (opthelp[i].is_optional)
#line 35
	    {
#line 35
	      buf[n++] = '[';
#line 35
	      strcpy (&buf[n], opthelp[i].arg);
#line 35
	      n += strlen (opthelp[i].arg);
#line 35
	      buf[n++] = ']';
#line 35
	    }
#line 35
	  else
#line 35
	    {
#line 35
	      buf[n++] = ' ';
#line 35
	      strcpy (&buf[n], opthelp[i].arg);
#line 35
	      n += strlen (opthelp[i].arg);
#line 35
	    }
#line 35
	  buf[n++] = ']';
#line 35
	}
#line 35
    }
#line 35

#line 35
  /* Print a list of long options */
#line 35
  for (i = 0; i < sizeof (opthelp) / sizeof (opthelp[0]); i++)
#line 35
    {
#line 35
      if (opthelp[i].opt && opthelp[i].descr)
#line 35
	{
#line 35
	  size_t len;
#line 35
	  const char *longopt;
#line 35

#line 35
	  if (opthelp[i].opt[1] == '-')
#line 35
	    longopt = opthelp[i].opt;
#line 35
	  else if (opthelp[i].opt[2] == ',')
#line 35
	    longopt = opthelp[i].opt + 4;
#line 35
	  else
#line 35
	    continue;
#line 35

#line 35
	  len = 3 + strlen (longopt)
#line 35
	          + (opthelp[i].arg ? 1 + strlen (opthelp[i].arg)
#line 35
		      + (opthelp[i].is_optional ? 2 : 0) : 0);
#line 35
	  if (n + len > RMARGIN) FLUSH;
#line 35
	  buf[n++] = ' '; 
#line 35
	  buf[n++] = '['; 
#line 35
	  strcpy (&buf[n], longopt);
#line 35
	  n += strlen (longopt);
#line 35
	  if (opthelp[i].arg)
#line 35
	    {
#line 35
	      buf[n++] = '=';
#line 35
	      if (opthelp[i].is_optional)
#line 35
		{
#line 35
		  buf[n++] = '[';
#line 35
		  strcpy (&buf[n], opthelp[i].arg);
#line 35
		  n += strlen (opthelp[i].arg);
#line 35
		  buf[n++] = ']';
#line 35
		}
#line 35
	      else
#line 35
		{
#line 35
		  strcpy (&buf[n], opthelp[i].arg);
#line 35
		  n += strlen (opthelp[i].arg);
#line 35
		}
#line 35
	    }
#line 35
	  buf[n++] = ']';
#line 35
	}
#line 35
    }
#line 35
  FLUSH;
#line 35
  
#line 35
}
#line 35

#line 35
const char version_etc_copyright[] =
#line 35
  /* Do *not* mark this string for translation.  %s is a copyright
     symbol suitable for this locale. */
#line 35
  "Copyright %s 2001-2014 The Anubis Team.";
#line 35

#line 35
void
#line 35
print_version_only(const char *program_version, FILE *stream)
#line 35
{
#line 35
  fprintf (stream, "%s\n", program_version);
#line 35
  /* TRANSLATORS: Translate "(C)" to the copyright symbol
     (C-in-a-circle), if this symbol is available in the user's
     locale.  Otherwise, do not translate "(C)"; leave it as-is.  */
#line 35
  fprintf (stream, version_etc_copyright, _("(C)"));
#line 35
  fputc ('\n', stream);
#line 35
}
#line 35

#line 35
void
#line 35
print_version(const char *program_version, FILE *stream)
#line 35
{
#line 35
  print_version_only(program_version, stream);
#line 35
	
#line 35
  fputs (_("License GPLv3+: GNU GPL version 3 or later <http://gnu.org/licenses/gpl.html>\nThis is free software: you are free to change and redistribute it.\nThere is NO WARRANTY, to the extent permitted by law.\n\n"),
#line 35
	 stream);
#line 35
}
#line 35

#line 164 "env.opt"

#line 164


int x_argc;
char **x_argv;

void
get_options (int argc, char *argv[])
{
  
#line 172
 {
#line 172
  int c;
#line 172

#line 172
  while ((c = getopt_long(argc, argv, "b:r:l:m:fisvDc:h",
#line 172
                          long_options, NULL)) != EOF)
#line 172
    {
#line 172
      switch (c)
#line 172
        {
#line 172
        default:
#line 172
	   exit(1);
#line 172
	#line 41 "env.opt"
	 case 'b':
#line 41
          {
#line 41

	  parse_mtahost (optarg, &session.anubis, &session.anubis_port);
	  if (session.anubis && strlen (session.anubis) != 0)
	    topt |= T_NAMES;
	  rc_disable_keyword (CF_INIT | CF_SUPERVISOR, "bind");

#line 46
             break;
#line 46
          }
#line 51 "env.opt"
	 case 'r':
#line 51
          {
#line 51

	  parse_mtaport (optarg, &session.mta, &session.mta_port);

#line 53
             break;
#line 53
          }
#line 59 "env.opt"
	 case 'l':
#line 59
          {
#line 59

	  int rc;
	  int argc;
	  if ((rc = argcv_get (optarg, "", "#", &argc, &session.execargs)))
	     anubis_error (EX_SOFTWARE, rc, _("argcv_get failed"));
	  session.execpath = strdup (session.execargs[0]);
	  topt |= T_LOCAL_MTA;
	  rc_disable_keyword (CF_INIT | CF_SUPERVISOR, "local-mta");

#line 67
             break;
#line 67
          }
#line 72 "env.opt"
	 case 'm':
#line 72
          {
#line 72

	  if (anubis_set_mode (optarg))
            anubis_error (1, 0, _("invalid mode: %s"), optarg);
	  rc_disable_keyword (CF_INIT | CF_SUPERVISOR, "mode");

#line 76
             break;
#line 76
          }
#line 80 "env.opt"
	 case 'f':
#line 80
          {
#line 80

	  topt |= T_FOREGROUND_INIT;

#line 82
             break;
#line 82
          }
#line 86 "env.opt"
	 case OPTION_FROM:
#line 86
          {
#line 86

	  assign_string (&from_address, optarg);

#line 88
             break;
#line 88
          }
#line 93 "env.opt"
	 case 'i':
#line 93
          {
#line 93

	  topt |= T_STDINOUT;

#line 95
             break;
#line 95
          }
#line 100 "env.opt"
	 case 's':
#line 100
          {
#line 100

	  options.termlevel = SILENT;
	  rc_disable_keyword (CF_INIT | CF_SUPERVISOR, "termlevel");

#line 103
             break;
#line 103
          }
#line 106 "env.opt"
	 case 'v':
#line 106
          {
#line 106

	  options.termlevel = VERBOSE;
	  rc_disable_keyword (CF_INIT | CF_SUPERVISOR, "termlevel");

#line 109
             break;
#line 109
          }
#line 112 "env.opt"
	 case 'D':
#line 112
          {
#line 112

	  options.termlevel = DEBUG;
	  rc_disable_keyword (CF_INIT | CF_SUPERVISOR, "termlevel");

#line 115
             break;
#line 115
          }
#line 121 "env.opt"
	 case OPTION_ALTRC:
#line 121
          {
#line 121

	  options.altrc = optarg;
	  topt |= T_ALTRC;

#line 124
             break;
#line 124
          }
#line 128 "env.opt"
	 case OPTION_NORC:
#line 128
          {
#line 128

	  topt |= T_NORC;

#line 130
             break;
#line 130
          }
#line 134 "env.opt"
	 case 'c':
#line 134
          {
#line 134

	  rc_set_debug_level (optarg);
	  topt |= T_CHECK_CONFIG;

#line 137
             break;
#line 137
          }
#line 141 "env.opt"
	 case OPTION_SHOW_CONFIG_OPTIONS:
#line 141
          {
#line 141

	  print_config_options ();

#line 143
             break;
#line 143
          }
#line 147 "env.opt"
	 case OPTION_LOCATION_COLUMN:
#line 147
          {
#line 147

	  topt |= T_LOCATION_COLUMN;

#line 149
             break;
#line 149
          }
#line 153 "env.opt"
	 case OPTION_RELAX_PERM_CHECK:
#line 153
          {
#line 153

	  topt |= T_RELAX_PERM_CHECK;

#line 155
             break;
#line 155
          }
#line 159 "env.opt"
	 case OPTION_PID_FILE:
#line 159
          {
#line 159

	  pidfile = optarg;
  	  rc_disable_keyword (CF_INIT | CF_SUPERVISOR, "termlevel");

#line 162
             break;
#line 162
          }
#line 164 "env.opt"
	 case 'h':
#line 164
          {
#line 164

#line 164
		print_help ();
#line 164
                exit (0);
#line 164
	 
#line 164
             break;
#line 164
          }
#line 164 "env.opt"
	 case OPTION_USAGE:
#line 164
          {
#line 164

#line 164
		print_usage ();
#line 164
		exit (0);
#line 164
	 
#line 164
             break;
#line 164
          }
#line 164 "env.opt"
	 case OPTION_VERSION:
#line 164
          {
#line 164

#line 164
		/* Give version */
#line 164
		print_version(program_version, stdout);
#line 164
		exit (0);
#line 164
         
#line 164
             break;
#line 164
          }

#line 172 "env.opt"
        }
#line 172
    }
#line 172
  
#line 172
 }   
#line 172

  x_argc = argc - optind;
  x_argv = argv + optind;

  if (from_address)  /* Force MDA mode */
    anubis_mode = anubis_mda;
}

/*********************
 Get a home directory
**********************/

void
get_homedir (char *user, char *buf, int maxsize)
{
  struct passwd *pwd;
  memset (buf, 0, maxsize);

  if (user == 0)
    return;

  pwd = getpwnam (user);
  if (pwd)
    strncpy (buf, (char *) pwd->pw_dir, maxsize - 1);
  else
    {
      char *p = getenv ("HOME");
      if (p)
	strncpy (buf, p, maxsize - 1);
      else
	strncpy (buf, "", 1);
    }
  return;
}

/*****************************
 Get a real user name (login)
******************************/

void
anubis_getlogin (char **buf)
{
  struct passwd *pwd;

  pwd = getpwuid (getuid ());
  assign_string (buf, pwd ? pwd->pw_name : getlogin ());
}

/*******************
 Check current RUID
********************/

int
check_superuser (void)
{
  if (getuid () == 0)
    return 1;			/* a super-user */
  return 0;
}

/*******************************************
 Set USER's RGID, RUID, and home directory.
********************************************/

/* Change to the given uid/gid. Clear the supplementary group list.
   On success returns 0.
   On failure returns 1 (or exits, depending on topt settings. See
   anubis_error) */
static int
change_privs (uid_t uid, gid_t gid)
{
  int rc = 0;
  gid_t emptygidset[1];

  /* Reset group permissions */
  emptygidset[0] = gid ? gid : getegid();
  if (geteuid() == 0 && setgroups(1, emptygidset))
    {
      anubis_error (0, errno,
		    _("setgroups(1, %lu) failed"),
		    (u_long) emptygidset[0]);
      rc = 1;
    }

  /* Switch to the user's gid. On some OSes the effective gid must
     be reset first */

#if defined(HAVE_SETEGID)
  if ((rc = setegid(gid)) < 0)
    anubis_error (0, errno, _("setegid(%lu) failed"), (u_long) gid);
#elif defined(HAVE_SETREGID)
  if ((rc = setregid(gid, gid)) < 0)
    anubis_error (0, errno, _("setregid(%lu,%lu) failed"),
		  (u_long) gid, (u_long) gid);
#elif defined(HAVE_SETRESGID)
  if ((rc = setresgid(gid, gid, gid)) < 0)
    anubis_error (0, errno, _("setresgid(%lu,%lu,%lu) failed"),
		  (u_long) gid,
		  (u_long) gid,
		  (u_long) gid);
#endif

  if (rc == 0 && gid != 0)
    {
      if ((rc = setgid(gid)) < 0 && getegid() != gid) 
	anubis_error (0, errno, _("setgid(%lu) failed"), (u_long) gid);
      if (rc == 0 && getegid() != gid)
	{
	  anubis_error (0, errno, _("cannot set effective gid to %lu"),
			(u_long) gid);
	  rc = 1;
	}
    }

  /* Now reset uid */
  if (rc == 0 && uid != 0)
    {
      uid_t euid;

      if (setuid(uid)
	  || geteuid() != uid
	  || (getuid() != uid
	      && (geteuid() == 0 || getuid() == 0)))
	{
			
#if defined(HAVE_SETREUID)
	  if (geteuid() != uid)
	    {
	      if (setreuid(uid, -1) < 0)
		{
		  anubis_error (0, errno, _("setreuid(%lu,-1) failed"),
				(u_long) uid);
		  rc = 1;
		}
	      if (setuid(uid) < 0)
		{
		  anubis_error (0, errno, _("second setuid(%lu) failed"),
				(u_long) uid);
		  rc = 1;
		}
	    }
	  else
#endif
	    {
	      anubis_error (0, errno, _("setuid(%lu) failed"), (u_long) uid);
	      rc = 1;
	    }
	}
	

      euid = geteuid();
      if (uid != 0 && setuid(0) == 0)
	{
	  anubis_error (0, 0, _("seteuid(0) succeeded when it should not"));
	  rc = 1;
	}
      else if (uid != euid && setuid(euid) == 0)
	{
	  anubis_error (0, 0, _("cannot drop non-root setuid privileges"));
	  rc = 1;
	}
    }
  return rc;
}

void
anubis_changeowner (const char *user)
{
#ifdef HAVE_PAM
  int pam_retval;
#endif
  struct passwd *pwd;

  if (user == NULL || check_superuser () == 0)
    return;

#ifdef HAVE_PAM
  pam_retval = pam_start ("anubis", user, &conv, &pamh);
  if (pam_retval == PAM_SUCCESS)
    pam_retval = pam_authenticate (pamh, 0);
  if (pam_retval == PAM_SUCCESS)
    pam_retval = pam_acct_mgmt (pamh, 0);
  if (pam_retval == PAM_SUCCESS)
    pam_retval = pam_open_session (pamh, 0);
  if (pam_retval == PAM_SUCCESS)
    info (VERBOSE, _("PAM: Session opened (restrictions applied)."));
  else
    {
      info (NORMAL, _("PAM: Not authenticated to use GNU Anubis."));
      quit (EXIT_FAILURE);
    }
#endif /* HAVE_PAM */

  pwd = getpwnam (user);
  if (pwd)
    {
      if (change_privs (pwd->pw_uid, pwd->pw_gid))
	quit (EXIT_FAILURE);
	
      chdir (pwd->pw_dir);
      info (VERBOSE, _("UID:%d (%s), GID:%d, EUID:%d, EGID:%d"),
	    (int) getuid (), pwd->pw_name, (int) getgid (),
	    (int) geteuid (), (int) getegid ());
    }
  return;
}

int
check_username (char *user)
{
  struct passwd *pwd;

  if (user == NULL)
    return 0;

  pwd = getpwnam (user);
  if (pwd == 0)
    {
      int i = 0;
      int digits = 0;
      int len = strlen (user);

      for (i = len - 1; i >= 0; i--)
	{
	  if (isdigit ((u_char) user[i]))
	    digits++;
	}
      if (digits == len)
	{
	  int uid = atoi (user);
	  pwd = getpwuid (uid);
	  if (pwd != 0)
	    strncpy (user, (char *) pwd->pw_name, 64);
	  else
	    {
	      info (NORMAL, _("Invalid user ID: %s"), user);
	      return 0;		/* FALSE */
	    }
	}
      else
	{
	  info (NORMAL, _("Invalid user name: %s"), user);
	  return 0;		/* FALSE */
	}
    }
  return 1;			/* TRUE */
}

/*************************
 Check a file permissions
**************************/

int
check_filemode (char *path)
{
  struct stat st;

  if (path == 0)
    return 0;

  if (stat (path, &st) == -1)
    return 0;
  if ((st.st_mode & S_IRWXG) || (st.st_mode & S_IRWXO))
    {
      anubis_error (0, 0, _("Wrong permissions on %s. Set 0600."), path);
      return 0;			/* FALSE */
    }
  return 1;			/* TRUE */
}

/*************************
 Check does a file exist?
**************************/

int
check_filename (char *path, time_t *timep)
{
  struct stat st;

  if (path == NULL)
    return 0;

  if (stat (path, &st) == -1)
    {
      anubis_error (0, errno, "%s", path);
      return 0;			/* FALSE */
    }
  if (!(st.st_mode & S_IFREG) || !(st.st_mode & S_IFLNK))
    {
      anubis_error (0, 0,
		    _("%s is not a regular file or a symbolic link."), path);
      return 0;			/* FALSE */
    }

  if (timep)
    {
      time_t mtime = *timep;
      *timep = st.st_mtime;
      return st.st_mtime > mtime;
    }
  return 1;			/* TRUE */
}

/* Select working mode */
int
anubis_set_mode (char *modename)
{
  if (strcmp (modename, "transparent") == 0)
    anubis_mode = anubis_transparent;
  else if (strcmp (modename, "proxy") == 0)
    anubis_mode = anubis_proxy; 
#if WITH_GSASL
  else if (strcmp (modename, "auth") == 0)
    anubis_mode = anubis_authenticate;
#endif
  else if (strcmp (modename, "mda") == 0)
    anubis_mode = anubis_mda;
  else
    return 1;
  return 0;
}

void
write_pid_file (void)
{
  FILE *fp;
  
  if (!pidfile)
    pidfile = "/var/run/" DEFAULT_PIDFILE;
  fp = fopen (pidfile, "w");
  if (!fp)
    anubis_error (0, errno, _("Cannot open pid file '%s'"), pidfile);
  else 
    {
      fprintf (fp, "%ld\n", (unsigned long) getpid ());
      fclose (fp);
    }
}
/* EOF */
