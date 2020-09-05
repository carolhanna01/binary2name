/* -*- buffer-read-only: t -*- vi: set ro:
   THIS FILE IS GENERATED AUTOMATICALLY.  PLEASE DO NOT EDIT.
*/

#line 1 "usr.opt"
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
#include <anubisusr.h>

#line 20 "usr.opt"

#line 20
void print_help(void);
#line 20
void print_usage(void);
#line 71 "usr.opt"

#line 71
/* Option codes */
#line 71
enum {
#line 71
	_OPTION_INIT=255,
#line 71
	#line 40 "usr.opt"

#line 40
	OPTION_TLS_PRIORITIES,
#line 71 "usr.opt"

#line 71
	OPTION_USAGE,
#line 71 "usr.opt"

#line 71
	OPTION_VERSION,

#line 71 "usr.opt"
	MAX_OPTION
#line 71
};
#line 71
static struct option long_options[] = {
#line 71
	#line 24 "usr.opt"

#line 24
	{ "disable-tls", no_argument, 0, 'd' },
#line 32 "usr.opt"

#line 32
	{ "tls-cafile", required_argument, 0, 'C' },
#line 40 "usr.opt"

#line 40
	{ "tls-priorities", required_argument, 0, OPTION_TLS_PRIORITIES },
#line 47 "usr.opt"

#line 47
	{ "file", required_argument, 0, 'f' },
#line 53 "usr.opt"

#line 53
	{ "mechanism", required_argument, 0, 'm' },
#line 59 "usr.opt"

#line 59
	{ "netrc", required_argument, 0, 'n' },
#line 65 "usr.opt"

#line 65
	{ "verbose", no_argument, 0, 'v' },
#line 71 "usr.opt"

#line 71
	{ "help", no_argument, 0, 'h' },
#line 71 "usr.opt"

#line 71
	{ "usage", no_argument, 0, OPTION_USAGE },
#line 71 "usr.opt"

#line 71
	{ "version", no_argument, 0, OPTION_VERSION },

#line 71 "usr.opt"
	{0, 0, 0, 0}
#line 71
};
#line 71
static struct opthelp {
#line 71
        const char *opt;
#line 71
        const char *arg;
#line 71
        int is_optional;
#line 71
        const char *descr;
#line 71
} opthelp[] = {
#line 71
	#line 26 "usr.opt"

#line 26
	{ "-d, --disable-tls", NULL, 0, N_("Disable TLS encryption.") },
#line 34 "usr.opt"

#line 34
	{ "-C, --tls-cafile", N_("FILE"), 0, N_("Use given CA file.") },
#line 41 "usr.opt"

#line 41
	{ "--tls-priorities", N_("PRIO"), 0, N_("Set TLS priorities") },
#line 49 "usr.opt"

#line 49
	{ "-f, --file", N_("FILE"), 0, N_("Set user configuration file name.") },
#line 55 "usr.opt"

#line 55
	{ "-m, --mechanism", N_("MECH"), 0, N_("Restrict allowed SASL mechanisms.") },
#line 61 "usr.opt"

#line 61
	{ "-n, --netrc", N_("FILE"), 0, N_("Set .netrc file name.") },
#line 67 "usr.opt"

#line 67
	{ "-v, --verbose", NULL, 0, N_("Verbose output. Multiple options increase the verbosity. Maximum is 3.") },
#line 71 "usr.opt"

#line 71
	{ NULL, NULL, 0, N_("Other options") },
#line 71 "usr.opt"

#line 71
	{ "-h, --help", NULL, 0, N_("Give this help list") },
#line 71 "usr.opt"

#line 71
	{ "--usage", NULL, 0, N_("Give a short usage message") },
#line 71 "usr.opt"

#line 71
	{ "--version", NULL, 0, N_("Print program version") },

#line 71 "usr.opt"
};
#line 20 "usr.opt"

#line 20
const char *program_version = "anubisusr" " (" PACKAGE_STRING ")";
#line 20
static char doc[] = N_("Synchronize local and remote copies of the user's RC file.");
#line 20
static char args_doc[] = N_("[URL]");
#line 20
static char *after_desc = NULL;
#line 20
const char *program_bug_address = "<" PACKAGE_BUGREPORT ">";
#line 20
		    
#line 20
#define DESCRCOLUMN 30
#line 20
#define RMARGIN 79
#line 20
#define GROUPCOLUMN 2
#line 20
#define USAGECOLUMN 13
#line 20
		    
#line 20
static void
#line 20
indent (size_t start, size_t col)
#line 20
{
#line 20
  for (; start < col; start++)
#line 20
    putchar (' ');
#line 20
}
#line 20
		    
#line 20
static void
#line 20
print_option_descr (const char *descr, size_t lmargin, size_t rmargin)
#line 20
{
#line 20
  while (*descr)
#line 20
    {
#line 20
      size_t s = 0;
#line 20
      size_t i;
#line 20
      size_t width = rmargin - lmargin;
#line 20
      
#line 20
      while (*descr && (*descr == ' ' || *descr == '\t'))
#line 20
	descr++;
#line 20
      for (i = 0; ; i++)
#line 20
	{
#line 20
	  if (descr[i] == '\n')
#line 20
	    {
#line 20
	      s = i;
#line 20
	      break;
#line 20
	    }
#line 20
	  if (descr[i] == 0 || (descr[i] == ' ' || descr[i] == '\t'))
#line 20
	    {
#line 20
	      if (i > width)
#line 20
		break;
#line 20
	      s = i;
#line 20
	      if (descr[i] == 0)
#line 20
		break;
#line 20
	    }
#line 20
	}
#line 20
      if (s > width)
#line 20
	while (!(descr[s] == ' ' || descr[s] == '\t'))
#line 20
	  s--;
#line 20
      printf ("%*.*s\n", s, s, descr);
#line 20
      descr += s;
#line 20
      if (*descr)
#line 20
	{
#line 20
	  indent (0, lmargin);
#line 20
	  descr++;
#line 20
	}
#line 20
    }
#line 20
}
#line 20

#line 20
void
#line 20
print_help(void)
#line 20
{
#line 20
  unsigned i;
#line 20
  
#line 20
  printf ("%s %s [%s]... %s\n", _("Usage:"), "anubisusr", _("OPTION"),
#line 20
	  gettext (args_doc)); 
#line 20
  if (doc[0])
#line 20
    print_option_descr(gettext (doc), 0, RMARGIN);
#line 20
  putchar ('\n');
#line 20

#line 20
  for (i = 0; i < sizeof (opthelp) / sizeof (opthelp[0]); i++)
#line 20
    {
#line 20
      unsigned n;
#line 20
      if (opthelp[i].opt)
#line 20
	{
#line 20
	  n = printf ("  %s", opthelp[i].opt);
#line 20
	  if (opthelp[i].arg)
#line 20
	    {
#line 20
	      char *cb, *ce;
#line 20
	      if (strlen (opthelp[i].opt) == 2)
#line 20
		{
#line 20
		  if (!opthelp[i].is_optional)
#line 20
		    {
#line 20
		      putchar (' ');
#line 20
		      n++;
#line 20
		    }
#line 20
		}
#line 20
	      else
#line 20
		{
#line 20
		  putchar ('=');
#line 20
		  n++;
#line 20
		}
#line 20
	      if (opthelp[i].is_optional)
#line 20
		{
#line 20
		  cb = "[";
#line 20
		  ce = "]";
#line 20
		}
#line 20
	      else
#line 20
		cb = ce = "";
#line 20
	      n += printf ("%s%s%s", cb, gettext (opthelp[i].arg), ce);
#line 20
	    }
#line 20
	  if (n >= DESCRCOLUMN)
#line 20
	    {
#line 20
	      putchar ('\n');
#line 20
	      n = 0;
#line 20
	    }
#line 20
	  indent (n, DESCRCOLUMN);
#line 20
	  print_option_descr (gettext (opthelp[i].descr), DESCRCOLUMN, RMARGIN);
#line 20
	}
#line 20
      else
#line 20
	{
#line 20
	  if (i)
#line 20
	    putchar ('\n');
#line 20
	  indent (0, GROUPCOLUMN);
#line 20
	  print_option_descr (gettext (opthelp[i].descr),
#line 20
			      GROUPCOLUMN, RMARGIN);
#line 20
	  putchar ('\n');
#line 20
	}
#line 20
    }
#line 20
  
#line 20
  putchar ('\n');
#line 20
  print_option_descr (_("Mandatory or optional arguments to long options are also mandatory or optional for any corresponding short options."), 0, RMARGIN);
#line 20
  putchar ('\n');
#line 20

#line 20
  if (after_desc)
#line 20
    {
#line 20
      print_option_descr (gettext (after_desc), 0, RMARGIN);
#line 20
    }	
#line 20
  printf (_("Report bugs to %s.\n"), program_bug_address);
#line 20
}
#line 20

#line 20
void
#line 20
print_usage(void)
#line 20
{
#line 20
  unsigned i;
#line 20
  int f = 0;
#line 20
  unsigned n;
#line 20
  char buf[RMARGIN+1];
#line 20

#line 20
#define FLUSH                        do                                   {                              	  buf[n] = 0;              	  printf ("%s\n", buf);    	  n = USAGECOLUMN;         	  memset (buf, ' ', n);        }                                while (0)
#line 20
#define ADDC(c)   do { if (n == RMARGIN) FLUSH; buf[n++] = c; } while (0)
#line 20

#line 20
  n = snprintf (buf, sizeof buf, "%s %s ", _("Usage:"), "anubisusr");
#line 20

#line 20
  /* Print a list of short options without arguments. */
#line 20
  for (i = 0; i < sizeof (opthelp) / sizeof (opthelp[0]); i++)
#line 20
    {
#line 20
      if (opthelp[i].opt && opthelp[i].descr && opthelp[i].opt[1] != '-'
#line 20
	  && opthelp[i].arg == NULL)
#line 20
	{
#line 20
	  if (f == 0)
#line 20
	    {
#line 20
	      ADDC('[');
#line 20
	      ADDC('-');
#line 20
	      f = 1;
#line 20
	    }
#line 20
	  ADDC(opthelp[i].opt[1]);
#line 20
	}
#line 20
    }
#line 20
  if (f)
#line 20
    ADDC(']');
#line 20

#line 20
  /* Print a list of short options with arguments. */
#line 20
  for (i = 0; i < sizeof (opthelp) / sizeof (opthelp[0]); i++)
#line 20
    {
#line 20
      if (opthelp[i].opt && opthelp[i].descr && opthelp[i].opt[1] != '-'
#line 20
	  && opthelp[i].arg)
#line 20
	{
#line 20
	  size_t len = 5 
#line 20
	                + strlen (opthelp[i].arg)
#line 20
			   + (opthelp[i].is_optional ? 2 : 1);
#line 20
	  if (n + len > RMARGIN) FLUSH;
#line 20
	  buf[n++] = ' '; 
#line 20
	  buf[n++] = '['; 
#line 20
	  buf[n++] = '-';
#line 20
	  buf[n++] = opthelp[i].opt[1];
#line 20
	  if (opthelp[i].is_optional)
#line 20
	    {
#line 20
	      buf[n++] = '[';
#line 20
	      strcpy (&buf[n], opthelp[i].arg);
#line 20
	      n += strlen (opthelp[i].arg);
#line 20
	      buf[n++] = ']';
#line 20
	    }
#line 20
	  else
#line 20
	    {
#line 20
	      buf[n++] = ' ';
#line 20
	      strcpy (&buf[n], opthelp[i].arg);
#line 20
	      n += strlen (opthelp[i].arg);
#line 20
	    }
#line 20
	  buf[n++] = ']';
#line 20
	}
#line 20
    }
#line 20

#line 20
  /* Print a list of long options */
#line 20
  for (i = 0; i < sizeof (opthelp) / sizeof (opthelp[0]); i++)
#line 20
    {
#line 20
      if (opthelp[i].opt && opthelp[i].descr)
#line 20
	{
#line 20
	  size_t len;
#line 20
	  const char *longopt;
#line 20

#line 20
	  if (opthelp[i].opt[1] == '-')
#line 20
	    longopt = opthelp[i].opt;
#line 20
	  else if (opthelp[i].opt[2] == ',')
#line 20
	    longopt = opthelp[i].opt + 4;
#line 20
	  else
#line 20
	    continue;
#line 20

#line 20
	  len = 3 + strlen (longopt)
#line 20
	          + (opthelp[i].arg ? 1 + strlen (opthelp[i].arg)
#line 20
		      + (opthelp[i].is_optional ? 2 : 0) : 0);
#line 20
	  if (n + len > RMARGIN) FLUSH;
#line 20
	  buf[n++] = ' '; 
#line 20
	  buf[n++] = '['; 
#line 20
	  strcpy (&buf[n], longopt);
#line 20
	  n += strlen (longopt);
#line 20
	  if (opthelp[i].arg)
#line 20
	    {
#line 20
	      buf[n++] = '=';
#line 20
	      if (opthelp[i].is_optional)
#line 20
		{
#line 20
		  buf[n++] = '[';
#line 20
		  strcpy (&buf[n], opthelp[i].arg);
#line 20
		  n += strlen (opthelp[i].arg);
#line 20
		  buf[n++] = ']';
#line 20
		}
#line 20
	      else
#line 20
		{
#line 20
		  strcpy (&buf[n], opthelp[i].arg);
#line 20
		  n += strlen (opthelp[i].arg);
#line 20
		}
#line 20
	    }
#line 20
	  buf[n++] = ']';
#line 20
	}
#line 20
    }
#line 20
  FLUSH;
#line 20
  
#line 20
}
#line 20

#line 20
const char version_etc_copyright[] =
#line 20
  /* Do *not* mark this string for translation.  %s is a copyright
     symbol suitable for this locale. */
#line 20
  "Copyright %s 2001-2014 The Anubis Team.";
#line 20

#line 20
void
#line 20
print_version_only(const char *program_version, FILE *stream)
#line 20
{
#line 20
  fprintf (stream, "%s\n", program_version);
#line 20
  /* TRANSLATORS: Translate "(C)" to the copyright symbol
     (C-in-a-circle), if this symbol is available in the user's
     locale.  Otherwise, do not translate "(C)"; leave it as-is.  */
#line 20
  fprintf (stream, version_etc_copyright, _("(C)"));
#line 20
  fputc ('\n', stream);
#line 20
}
#line 20

#line 20
void
#line 20
print_version(const char *program_version, FILE *stream)
#line 20
{
#line 20
  print_version_only(program_version, stream);
#line 20
	
#line 20
  fputs (_("License GPLv3+: GNU GPL version 3 or later <http://gnu.org/licenses/gpl.html>\nThis is free software: you are free to change and redistribute it.\nThere is NO WARRANTY, to the extent permitted by law.\n\n"),
#line 20
	 stream);
#line 20
}
#line 20

#line 71 "usr.opt"

#line 71


void
usr_get_options (int argc, char *argv[], int *index)
{
    
#line 76
 {
#line 76
  int c;
#line 76

#line 76
  while ((c = getopt_long(argc, argv, "dC:f:m:n:vh",
#line 76
                          long_options, NULL)) != EOF)
#line 76
    {
#line 76
      switch (c)
#line 76
        {
#line 76
        default:
#line 76
	   exit(1);
#line 76
	#line 26 "usr.opt"
	 case 'd':
#line 26
          {
#line 26

#ifdef HAVE_TLS
       enable_tls = 0;
#endif

#line 30
             break;
#line 30
          }
#line 34 "usr.opt"
	 case 'C':
#line 34
          {
#line 34

#ifdef HAVE_TLS
       secure.cafile = optarg;
#endif

#line 38
             break;
#line 38
          }
#line 41 "usr.opt"
	 case OPTION_TLS_PRIORITIES:
#line 41
          {
#line 41

#ifdef HAVE_TLS
       secure.prio = optarg;
#endif

#line 45
             break;
#line 45
          }
#line 49 "usr.opt"
	 case 'f':
#line 49
          {
#line 49

       rcfile_name = optarg;

#line 51
             break;
#line 51
          }
#line 55 "usr.opt"
	 case 'm':
#line 55
          {
#line 55

       add_mech (optarg);

#line 57
             break;
#line 57
          }
#line 61 "usr.opt"
	 case 'n':
#line 61
          {
#line 61

       netrc_name = optarg;

#line 63
             break;
#line 63
          }
#line 67 "usr.opt"
	 case 'v':
#line 67
          {
#line 67

       verbose++;

#line 69
             break;
#line 69
          }
#line 71 "usr.opt"
	 case 'h':
#line 71
          {
#line 71

#line 71
		print_help ();
#line 71
                exit (0);
#line 71
	 
#line 71
             break;
#line 71
          }
#line 71 "usr.opt"
	 case OPTION_USAGE:
#line 71
          {
#line 71

#line 71
		print_usage ();
#line 71
		exit (0);
#line 71
	 
#line 71
             break;
#line 71
          }
#line 71 "usr.opt"
	 case OPTION_VERSION:
#line 71
          {
#line 71

#line 71
		/* Give version */
#line 71
		print_version(program_version, stdout);
#line 71
		exit (0);
#line 71
         
#line 71
             break;
#line 71
          }

#line 76 "usr.opt"
        }
#line 76
    }
#line 76
  *index = optind;
#line 76
 }   
#line 76
;
}
