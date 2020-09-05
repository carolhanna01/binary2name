/* -*- buffer-read-only: t -*- vi: set ro:
   THIS FILE IS GENERATED AUTOMATICALLY.  PLEASE DO NOT EDIT.
*/

#line 1 "adm.opt"
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
#include <anubisadm.h>

#line 20 "adm.opt"

#line 20
void print_help(void);
#line 20
void print_usage(void);
#line 99 "adm.opt"

#line 99
/* Option codes */
#line 99
enum {
#line 99
	_OPTION_INIT=255,
#line 99
	#line 99 "adm.opt"

#line 99
	OPTION_USAGE,
#line 99 "adm.opt"

#line 99
	OPTION_VERSION,

#line 99 "adm.opt"
	MAX_OPTION
#line 99
};
#line 99
static struct option long_options[] = {
#line 99
	#line 38 "adm.opt"

#line 38
	{ "create", no_argument, 0, 'c' },
#line 44 "adm.opt"

#line 44
	{ "list", no_argument, 0, 'l' },
#line 50 "adm.opt"

#line 50
	{ "add", no_argument, 0, 'a' },
#line 56 "adm.opt"

#line 56
	{ "modify", no_argument, 0, 'm' },
#line 62 "adm.opt"

#line 62
	{ "remove", no_argument, 0, 'r' },
#line 70 "adm.opt"

#line 70
	{ "authid", required_argument, 0, 'i' },
#line 78 "adm.opt"

#line 78
	{ "password", required_argument, 0, 'p' },
#line 85 "adm.opt"

#line 85
	{ "user", required_argument, 0, 'u' },
#line 92 "adm.opt"

#line 92
	{ "rcfile", required_argument, 0, 'f' },
#line 99 "adm.opt"

#line 99
	{ "help", no_argument, 0, 'h' },
#line 99 "adm.opt"

#line 99
	{ "usage", no_argument, 0, OPTION_USAGE },
#line 99 "adm.opt"

#line 99
	{ "version", no_argument, 0, OPTION_VERSION },

#line 99 "adm.opt"
	{0, 0, 0, 0}
#line 99
};
#line 99
static struct opthelp {
#line 99
        const char *opt;
#line 99
        const char *arg;
#line 99
        int is_optional;
#line 99
        const char *descr;
#line 99
} opthelp[] = {
#line 99
	#line 37 "adm.opt"

#line 37
	{ NULL, NULL, 0, N_("Administration commands:") },
#line 40 "adm.opt"

#line 40
	{ "-c, --create", NULL, 0, N_("Creates the database.") },
#line 46 "adm.opt"

#line 46
	{ "-l, --list", NULL, 0, N_("List contents of existing database.") },
#line 52 "adm.opt"

#line 52
	{ "-a, --add", NULL, 0, N_("Add a new record.") },
#line 58 "adm.opt"

#line 58
	{ "-m, --modify", NULL, 0, N_("Modify existing record.") },
#line 64 "adm.opt"

#line 64
	{ "-r, --remove", NULL, 0, N_("Remove existing record.") },
#line 68 "adm.opt"

#line 68
	{ NULL, NULL, 0, N_("Options:") },
#line 74 "adm.opt"

#line 74
	{ "-i, --authid", N_("STRING"), 0, N_("Specify the authid to operate upon. This option is mandatory with --add, --modify and --remove. It is optional when used with --list.") },
#line 81 "adm.opt"

#line 81
	{ "-p, --password", N_("STRING"), 0, N_("Specify the password for the authid. Mandatory with --add, --modify and --remove.") },
#line 88 "adm.opt"

#line 88
	{ "-u, --user", N_("STRING"), 0, N_("Specify the system user name corresponding to the given authid. Optional for --add, --modify and --remove.") },
#line 95 "adm.opt"

#line 95
	{ "-f, --rcfile", N_("STRING"), 0, N_("Specify the rc file to be used for this authid. Optional for --add, --modify and --remove.") },
#line 99 "adm.opt"

#line 99
	{ NULL, NULL, 0, N_("Other options") },
#line 99 "adm.opt"

#line 99
	{ "-h, --help", NULL, 0, N_("Give this help list") },
#line 99 "adm.opt"

#line 99
	{ "--usage", NULL, 0, N_("Give a short usage message") },
#line 99 "adm.opt"

#line 99
	{ "--version", NULL, 0, N_("Print program version") },

#line 99 "adm.opt"
};
#line 20 "adm.opt"

#line 20
const char *program_version = "anubisadm" " (" PACKAGE_STRING ")";
#line 20
static char doc[] = N_("Interface for GNU Anubis database administration.");
#line 20
static char args_doc[] = N_("URL");
#line 20
static char *after_desc = N_("EXAMPLES:\n\n  1. Create the GDBM database from a plaintext file:\n\n example$ anubisadm --create gdbm:/etc/anubis.db < plaintext\n\n 2. Add SMTP authid \"test\" with password \"guessme\" and map it to the system account \"gray\":\n\n example$ anubisadm --add --authid test --password guessme --user gray gdbm:/etc/anubis.db\n\n 3. List the database:\n\n example$ anubisadm --list gdbm:/etc/anubis.db\n\n 4. List only the record with authid \"test\":\n\n example$ anubisadm --list --authid test gdbm:/etc/anubis.db \n\n");
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
  printf ("%s %s [%s]... %s\n", _("Usage:"), "anubisadm", _("OPTION"),
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
  n = snprintf (buf, sizeof buf, "%s %s ", _("Usage:"), "anubisadm");
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

#line 99 "adm.opt"

#line 99


void
adm_get_options (int argc, char *argv[], operation_fp *operation, int *index)
{
    
#line 104
 {
#line 104
  int c;
#line 104

#line 104
  while ((c = getopt_long(argc, argv, "clamri:p:u:f:h",
#line 104
                          long_options, NULL)) != EOF)
#line 104
    {
#line 104
      switch (c)
#line 104
        {
#line 104
        default:
#line 104
	   exit(1);
#line 104
	#line 40 "adm.opt"
	 case 'c':
#line 40
          {
#line 40

       *operation = op_create;

#line 42
             break;
#line 42
          }
#line 46 "adm.opt"
	 case 'l':
#line 46
          {
#line 46

	  *operation = op_list;

#line 48
             break;
#line 48
          }
#line 52 "adm.opt"
	 case 'a':
#line 52
          {
#line 52

	  *operation = op_add;

#line 54
             break;
#line 54
          }
#line 58 "adm.opt"
	 case 'm':
#line 58
          {
#line 58

	  *operation = op_modify;

#line 60
             break;
#line 60
          }
#line 64 "adm.opt"
	 case 'r':
#line 64
          {
#line 64

	  rcfile = optarg;

#line 66
             break;
#line 66
          }
#line 74 "adm.opt"
	 case 'i':
#line 74
          {
#line 74

	  authid = optarg;

#line 76
             break;
#line 76
          }
#line 81 "adm.opt"
	 case 'p':
#line 81
          {
#line 81

	  password = optarg;

#line 83
             break;
#line 83
          }
#line 88 "adm.opt"
	 case 'u':
#line 88
          {
#line 88

	  username = optarg;

#line 90
             break;
#line 90
          }
#line 95 "adm.opt"
	 case 'f':
#line 95
          {
#line 95

	  rcfile = optarg;

#line 97
             break;
#line 97
          }
#line 99 "adm.opt"
	 case 'h':
#line 99
          {
#line 99

#line 99
		print_help ();
#line 99
                exit (0);
#line 99
	 
#line 99
             break;
#line 99
          }
#line 99 "adm.opt"
	 case OPTION_USAGE:
#line 99
          {
#line 99

#line 99
		print_usage ();
#line 99
		exit (0);
#line 99
	 
#line 99
             break;
#line 99
          }
#line 99 "adm.opt"
	 case OPTION_VERSION:
#line 99
          {
#line 99

#line 99
		/* Give version */
#line 99
		print_version(program_version, stdout);
#line 99
		exit (0);
#line 99
         
#line 99
             break;
#line 99
          }

#line 104 "adm.opt"
        }
#line 104
    }
#line 104
  *index = optind;
#line 104
 }   
#line 104
;
}

