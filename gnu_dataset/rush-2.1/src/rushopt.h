/* -*- buffer-read-only: t -*- vi: set ro:
   THIS FILE IS GENERATED AUTOMATICALLY.  PLEASE DO NOT EDIT.
*/

#line 1 "rushopt.opt"
/* This file is part of GNU Rush.
   Copyright (C) 2008-2019 Sergey Poznyakoff

   GNU Rush is free software; you can redistribute it and/or modify
   it under the terms of the GNU General Public License as published by
   the Free Software Foundation; either version 3, or (at your option)
   any later version.

   GNU Rush is distributed in the hope that it will be useful,
   but WITHOUT ANY WARRANTY; without even the implied warranty of
   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
   GNU General Public License for more details.

   You should have received a copy of the GNU General Public License
   along with GNU Rush.  If not, see <http://www.gnu.org/licenses/>. */

#include <error.h>

#line 19 "rushopt.opt"

#line 19
void print_help(void);
#line 19
void print_usage(void);
#line 100 "rushopt.opt"

#line 100
/* Option codes */
#line 100
enum {
#line 100
	_OPTION_INIT=255,
#line 100
	#line 32 "rushopt.opt"

#line 32
	OPTION_LINT,
#line 89 "rushopt.opt"

#line 89
	OPTION_SHOW_DEFAULT,
#line 100 "rushopt.opt"

#line 100
	OPTION_USAGE,
#line 100 "rushopt.opt"

#line 100
	OPTION_VERSION,

#line 100 "rushopt.opt"
	MAX_OPTION
#line 100
};
#line 100
static struct option long_options[] = {
#line 100
	#line 23 "rushopt.opt"

#line 23
	{ "debug", required_argument, 0, 'd' },
#line 30 "rushopt.opt"

#line 30
	{ "test", no_argument, 0, 't' },
#line 32 "rushopt.opt"

#line 32
	{ "lint", no_argument, 0, OPTION_LINT },
#line 43 "rushopt.opt"

#line 43
	{ "trace", no_argument, 0, 'x' },
#line 49 "rushopt.opt"

#line 49
	{ "user", required_argument, 0, 'u' },
#line 74 "rushopt.opt"

#line 74
	{ "dump", required_argument, 0, 'D' },
#line 81 "rushopt.opt"

#line 81
	{ "security-check", required_argument, 0, 'C' },
#line 89 "rushopt.opt"

#line 89
	{ "show-default", no_argument, 0, OPTION_SHOW_DEFAULT },
#line 100 "rushopt.opt"

#line 100
	{ "help", no_argument, 0, 'h' },
#line 100 "rushopt.opt"

#line 100
	{ "usage", no_argument, 0, OPTION_USAGE },
#line 100 "rushopt.opt"

#line 100
	{ "version", no_argument, 0, OPTION_VERSION },

#line 100 "rushopt.opt"
	{0, 0, 0, 0}
#line 100
};
#line 100
static struct opthelp {
#line 100
        const char *opt;
#line 100
        const char *arg;
#line 100
        int is_optional;
#line 100
        const char *descr;
#line 100
} opthelp[] = {
#line 100
	#line 25 "rushopt.opt"

#line 25
	{ "-d, --debug", N_("NUMBER"), 0, N_("Set debugging level.") },
#line 33 "rushopt.opt"

#line 33
	{ "-t, --test, --lint", NULL, 0, N_("Run in test mode.") },
#line 39 "rushopt.opt"

#line 39
	{ "-T", NULL, 0, N_("Scanner test.") },
#line 45 "rushopt.opt"

#line 45
	{ "-x, --trace", NULL, 0, N_("Print grammar and lexical analyzer traces") },
#line 51 "rushopt.opt"

#line 51
	{ "-u, --user", N_("NAME"), 0, N_("Supply user name in test mode.") },
#line 63 "rushopt.opt"

#line 63
	{ "-c", N_("COMMAND"), 0, N_("Execute COMMAND.") },
#line 69 "rushopt.opt"

#line 69
	{ "-i", NULL, 0, N_("Force interactive shell.") },
#line 76 "rushopt.opt"

#line 76
	{ "-D, --dump", N_("KEYWORDS"), 0, N_("Dump final request in test mode.") },
#line 83 "rushopt.opt"

#line 83
	{ "-C, --security-check", N_("CHECK"), 0, N_("Add or remove configuration security check.") },
#line 91 "rushopt.opt"

#line 91
	{ "--show-default", NULL, 0, N_("Show default configuration.") },
#line 100 "rushopt.opt"

#line 100
	{ NULL, NULL, 0, N_("Other options") },
#line 100 "rushopt.opt"

#line 100
	{ "-h, --help", NULL, 0, N_("Give this help list") },
#line 100 "rushopt.opt"

#line 100
	{ "--usage", NULL, 0, N_("Give a short usage message") },
#line 100 "rushopt.opt"

#line 100
	{ "--version", NULL, 0, N_("Print program version") },

#line 100 "rushopt.opt"
};
#line 19 "rushopt.opt"

#line 19
const char *program_version = "rush" " (" PACKAGE_STRING ")";
#line 19
static char doc[] = N_("rush - a restricted user shell.");
#line 19
static char args_doc[] = N_("[FILE]");
#line 19
const char *program_bug_address = "<" PACKAGE_BUGREPORT ">";
#line 19
		    
#line 19
#define DESCRCOLUMN 30
#line 19
#define RMARGIN 79
#line 19
#define GROUPCOLUMN 2
#line 19
#define USAGECOLUMN 13
#line 19
		    
#line 19
static void
#line 19
indent (size_t start, size_t col)
#line 19
{
#line 19
  for (; start < col; start++)
#line 19
    putchar (' ');
#line 19
}
#line 19
		    
#line 19
static void
#line 19
print_option_descr (const char *descr, size_t lmargin, size_t rmargin)
#line 19
{
#line 19
  while (*descr)
#line 19
    {
#line 19
      int s = 0;
#line 19
      int i;
#line 19
      size_t width = rmargin - lmargin;
#line 19

#line 19
      for (i = 0; ; i++)
#line 19
	{
#line 19
	  if (descr[i] == 0 || isspace (descr[i]))
#line 19
	    {
#line 19
	      if (i > width)
#line 19
		break;
#line 19
	      s = i;
#line 19
	      if (descr[i] == 0)
#line 19
		break;
#line 19
	    }
#line 19
	}
#line 19
      printf ("%*.*s\n", s, s, descr);
#line 19
      descr += s;
#line 19
      if (*descr)
#line 19
	{
#line 19
	  indent (0, lmargin);
#line 19
	  descr++;
#line 19
	}
#line 19
    }
#line 19
}
#line 19

#line 19
void
#line 19
print_help(void)
#line 19
{
#line 19
  unsigned i;
#line 19
  
#line 19
  printf ("%s %s [%s]...", _("Usage:"), "rush", _("OPTION"));
#line 19
  if (args_doc[0])
#line 19
	  printf(" %s", gettext(args_doc));
#line 19
  printf("\n");
#line 19
  if (doc[0])
#line 19
    print_option_descr(gettext (doc), 0, RMARGIN);
#line 19
  putchar ('\n');
#line 19

#line 19
  for (i = 0; i < sizeof (opthelp) / sizeof (opthelp[0]); i++)
#line 19
    {
#line 19
      unsigned n;
#line 19
      if (opthelp[i].opt)
#line 19
	{
#line 19
	  n = printf ("  %s", opthelp[i].opt);
#line 19
	  if (opthelp[i].arg)
#line 19
	    {
#line 19
	      char *cb, *ce;
#line 19
	      if (strlen (opthelp[i].opt) == 2)
#line 19
		{
#line 19
		  if (!opthelp[i].is_optional)
#line 19
		    {
#line 19
		      putchar (' ');
#line 19
		      n++;
#line 19
		    }
#line 19
		}
#line 19
	      else
#line 19
		{
#line 19
		  putchar ('=');
#line 19
		  n++;
#line 19
		}
#line 19
	      if (opthelp[i].is_optional)
#line 19
		{
#line 19
		  cb = "[";
#line 19
		  ce = "]";
#line 19
		}
#line 19
	      else
#line 19
		cb = ce = "";
#line 19
	      n += printf ("%s%s%s", cb, gettext (opthelp[i].arg), ce);
#line 19
	    }
#line 19
	  if (n >= DESCRCOLUMN)
#line 19
	    {
#line 19
	      putchar ('\n');
#line 19
	      n = 0;
#line 19
	    }
#line 19
	  indent (n, DESCRCOLUMN);
#line 19
	  print_option_descr (gettext (opthelp[i].descr), DESCRCOLUMN, RMARGIN);
#line 19
	}
#line 19
      else
#line 19
	{
#line 19
	  if (i)
#line 19
	    putchar ('\n');
#line 19
	  indent (0, GROUPCOLUMN);
#line 19
	  print_option_descr (gettext (opthelp[i].descr),
#line 19
			      GROUPCOLUMN, RMARGIN);
#line 19
	  putchar ('\n');
#line 19
	}
#line 19
    }
#line 19
  
#line 19
  putchar ('\n');
#line 19
  print_option_descr (_("Mandatory or optional arguments to long options are also mandatory or optional for any corresponding short options."), 0, RMARGIN);
#line 19
  putchar ('\n');
#line 19
  printf (_("Report bugs to %s.\n"), program_bug_address);
#line 19
}
#line 19

#line 19
static int
#line 19
cmpidx_short(const void *a, const void *b)
#line 19
{
#line 19
	struct opthelp const **opta = (struct opthelp const **)a;
#line 19
	struct opthelp const **optb = (struct opthelp const **)b;
#line 19

#line 19
	return (*opta)->opt[1] - (*optb)->opt[1];
#line 19
}
#line 19
  
#line 19
static int
#line 19
cmpidx_long(const void *a, const void *b)
#line 19
{
#line 19
	struct opthelp const **ap = (struct opthelp const **)a;
#line 19
	struct opthelp const **bp = (struct opthelp const **)b;
#line 19
	char const *opta, *optb;
#line 19
	size_t lena, lenb;
#line 19

#line 19
	if ((*ap)->opt[1] == '-')
#line 19
		opta = (*ap)->opt;
#line 19
	else
#line 19
		opta = (*ap)->opt + 4;
#line 19
	lena = strcspn(opta, ",");
#line 19
  
#line 19
	if ((*bp)->opt[1] == '-')
#line 19
		optb = (*bp)->opt;
#line 19
	else
#line 19
		optb = (*bp)->opt + 4;
#line 19
	lenb = strcspn(optb, ",");
#line 19
	return strncmp(opta, optb, lena > lenb ? lenb : lena);
#line 19
}
#line 19

#line 19
void
#line 19
print_usage(void)
#line 19
{
#line 19
	unsigned i;
#line 19
	unsigned n;
#line 19
	char *buf;
#line 19
	size_t bufsize;
#line 19
	unsigned nidx;
#line 19
	struct opthelp **optidx;
#line 19
	size_t optcount = sizeof(opthelp) / sizeof(opthelp[0]);
#line 19
  
#line 19
#define FLUSH do {						buf[n] = 0;					printf("%s\n", buf);				n = USAGECOLUMN;				memset(buf, ' ', n);			}  while (0)
#line 19
#define ADDC(c) do { if (n == RMARGIN) FLUSH; buf[n++] = c; } while (0)
#line 19

#line 19
	optidx = calloc(optcount, sizeof(optidx[0]));
#line 19
	if (!optidx)
#line 19
		abort();
#line 19
	bufsize = RMARGIN + 1;
#line 19
	buf = malloc(bufsize);
#line 19
	if (!buf)
#line 19
		abort();
#line 19
	
#line 19
	n = snprintf(buf, bufsize, "%s %s ", _("Usage:"), "rush");
#line 19
	
#line 19
	/* Print a list of short options without arguments. */
#line 19
	for (i = nidx = 0; i < optcount; i++)
#line 19
		if (opthelp[i].opt &&
#line 19
		    opthelp[i].descr &&
#line 19
		    opthelp[i].opt[1] != '-' &&
#line 19
		    opthelp[i].arg == NULL)
#line 19
			optidx[nidx++] = opthelp + i;
#line 19

#line 19
	if (nidx) {
#line 19
		qsort(optidx, nidx, sizeof(optidx[0]), cmpidx_short);
#line 19

#line 19
		ADDC('[');
#line 19
		ADDC('-');
#line 19
		for (i = 0; i < nidx; i++) {
#line 19
			ADDC(optidx[i]->opt[1]);
#line 19
		}
#line 19
		ADDC(']');
#line 19
	}
#line 19

#line 19
	/* Print a list of short options with arguments. */
#line 19
	for (i = nidx = 0; i < optcount; i++) {
#line 19
		if (opthelp[i].opt &&
#line 19
		    opthelp[i].descr &&
#line 19
		    opthelp[i].opt[1] != '-' &&
#line 19
		    opthelp[i].arg)
#line 19
			optidx[nidx++] = opthelp + i;
#line 19
	}
#line 19

#line 19
	if (nidx) {
#line 19
		qsort(optidx, nidx, sizeof(optidx[0]), cmpidx_short);
#line 19
    
#line 19
		for (i = 0; i < nidx; i++) {
#line 19
			struct opthelp *opt = optidx[i];
#line 19
			size_t len = 5 + strlen(opt->arg)
#line 19
				       + (opt->is_optional ? 2 : 1);
#line 19
      
#line 19
			if (n + len > RMARGIN)
#line 19
				FLUSH;
#line 19
			buf[n++] = ' ';
#line 19
			buf[n++] = '[';
#line 19
			buf[n++] = '-';
#line 19
			buf[n++] = opt->opt[1];
#line 19
			if (opt->is_optional) {
#line 19
				buf[n++] = '[';
#line 19
				strcpy(&buf[n], opt->arg);
#line 19
				n += strlen(opt->arg);
#line 19
				buf[n++] = ']';
#line 19
			} else {
#line 19
				buf[n++] = ' ';
#line 19
				strcpy(&buf[n], opt->arg);
#line 19
				n += strlen(opt->arg);
#line 19
			}
#line 19
			buf[n++] = ']';
#line 19
		}
#line 19
	}
#line 19
  
#line 19
	/* Print a list of long options */
#line 19
	for (i = nidx = 0; i < optcount; i++) {
#line 19
		if (opthelp[i].opt && opthelp[i].descr
#line 19
		    && (opthelp[i].opt[1] == '-' || opthelp[i].opt[2] == ','))
#line 19
			optidx[nidx++] = opthelp + i;
#line 19
	}
#line 19

#line 19
	if (nidx) {
#line 19
		qsort(optidx, nidx, sizeof(optidx[0]), cmpidx_long);
#line 19
	
#line 19
		for (i = 0; i < nidx; i++) {
#line 19
			struct opthelp *opt = optidx[i];
#line 19
			size_t len;
#line 19
			const char *longopt;
#line 19
	  
#line 19
			if (opt->opt[1] == '-')
#line 19
				longopt = opt->opt;
#line 19
			else if (opt->opt[2] == ',')
#line 19
				longopt = opt->opt + 4;
#line 19
			else
#line 19
				continue;
#line 19

#line 19
			len = 3 + strlen(longopt)
#line 19
				+ (opt->arg ? 1 + strlen(opt->arg)
#line 19
				+ (opt->is_optional ? 2 : 0) : 0);
#line 19
			if (n + len > RMARGIN) {
#line 19
				FLUSH;
#line 19
				/* Make sure we have enough buffer space if
                                   the string cannot be split */
#line 19
				if (n + len > bufsize) {
#line 19
					bufsize = n + len;
#line 19
					buf = realloc(buf, bufsize);
#line 19
					if (!buf)
#line 19
						abort();
#line 19
				}
#line 19
			}
#line 19
			buf[n++] = ' ';
#line 19
			buf[n++] = '[';
#line 19
			strcpy(&buf[n], longopt);
#line 19
			n += strlen(longopt);
#line 19
			if (opt->arg) {
#line 19
				buf[n++] = '=';
#line 19
				if (opt->is_optional) {
#line 19
					buf[n++] = '[';
#line 19
					strcpy(&buf[n], opt->arg);
#line 19
					n += strlen(opt->arg);
#line 19
					buf[n++] = ']';
#line 19
				} else {
#line 19
					strcpy(&buf[n], opt->arg);
#line 19
					n += strlen(opt->arg);
#line 19
				}
#line 19
			}
#line 19
			buf[n++] = ']';
#line 19
		}
#line 19
	}
#line 19

#line 19
	/* Print argument list */
#line 19
	if (args_doc[0]) {
#line 19
		char const *docstr = gettext(args_doc);
#line 19
		size_t len = strlen(docstr) + 1;
#line 19
		if (n + len <= RMARGIN) {
#line 19
			buf[n++] = ' ';
#line 19
			strcpy(buf + n, docstr);
#line 19
			n += len;
#line 19
		} else {
#line 19
			struct wordsplit ws;
#line 19

#line 19
			if (wordsplit(docstr, &ws, 
#line 19
				      WRDSF_SHOWERR |
#line 19
				      WRDSF_NOVAR |
#line 19
				      WRDSF_NOCMD |
#line 19
				      WRDSF_QUOTE |
#line 19
				      WRDSF_SQUEEZE_DELIMS))
#line 19
				abort();
#line 19

#line 19
			for (i = 0; i < ws.ws_wordc; i++) {
#line 19
				len = strlen(ws.ws_wordv[i]) + 1;
#line 19
				if (n + len > RMARGIN) {
#line 19
					FLUSH;
#line 19
					/* Make sure we have enough buffer
					   space if the string cannot be
					   split */
#line 19
					if (n + len > bufsize) {
#line 19
						bufsize = n + len;
#line 19
						buf = realloc(buf, bufsize);
#line 19
						if (!buf)
#line 19
							abort();
#line 19
					}
#line 19
				}
#line 19
				buf[n++] = ' ';
#line 19
				strcpy(buf + n, ws.ws_wordv[i]);
#line 19
				n += len;
#line 19
			}
#line 19
		}	
#line 19
	}
#line 19
	
#line 19
	FLUSH;
#line 19

#line 19
	free(optidx);
#line 19
	free(buf);
#line 19
}
#line 19

#line 19
const char version_etc_copyright[] =
#line 19
  /* Do *not* mark this string for translation.  %s is a copyright
     symbol suitable for this locale, and %d is the copyright
     year.  */
#line 19
  "Copyright %s 2008-2017 Sergey Poznyakoff";
#line 19

#line 19
void
#line 19
print_version_only(const char *program_version, FILE *stream)
#line 19
{
#line 19
	fprintf (stream, "%s\n", program_version);
#line 19
	/* TRANSLATORS: Translate "(C)" to the copyright symbol
	   (C-in-a-circle), if this symbol is available in the user's
	   locale.  Otherwise, do not translate "(C)"; leave it as-is.  */
#line 19
	fprintf (stream, version_etc_copyright, _("(C)"));
#line 19
	fputc ('\n', stream);
#line 19
}
#line 19

#line 19
void
#line 19
print_version(const char *program_version, FILE *stream)
#line 19
{
#line 19
	print_version_only(program_version, stream);
#line 19
	
#line 19
	fputs (_("License GPLv3+: GNU GPL version 3 or later <http://gnu.org/licenses/gpl.html>\nThis is free software: you are free to change and redistribute it.\nThere is NO WARRANTY, to the extent permitted by law.\n\n"),
#line 19
	       stream);
#line 19
	
#line 19
}
#line 19

#line 100 "rushopt.opt"

#line 100


void
get_options(int argc, char *argv[])
{
    
#line 105
 {
#line 105
  int optchar;
#line 105

#line 105

#line 105
  while ((optchar = getopt_long(argc, argv, "d:tTxu:c:iD:C:h",
#line 105
                                long_options, NULL)) != EOF)
#line 105
    {
#line 105
      switch (optchar)
#line 105
        {
#line 105
        default:
#line 105
	exit(1);
#line 105
       
#line 105
	#line 25 "rushopt.opt"
	 case 'd':
#line 25
          {
#line 25

	debug_level = atoi(optarg);
	debug_option = 1;

#line 28
             break;
#line 28
          }
#line 33 "rushopt.opt"
	 case 't': case OPTION_LINT:
#line 33
          {
#line 33

	lint_option = 1;

#line 35
             break;
#line 35
          }
#line 39 "rushopt.opt"
	 case 'T':
#line 39
          {
#line 39

       scanner_test = 1;

#line 41
             break;
#line 41
          }
#line 45 "rushopt.opt"
	 case 'x':
#line 45
          {
#line 45

       parser_traces++;

#line 47
             break;
#line 47
          }
#line 51 "rushopt.opt"
	 case 'u':
#line 51
          {
#line 51

	lint_option = 1;
	if (getuid())
		die(usage_error,
		    NULL,
		    _("the --user option is allowed "
		      "for the superuser only"));
	test_user_name = optarg;

#line 59
             break;
#line 59
          }
#line 63 "rushopt.opt"
	 case 'c':
#line 63
          {
#line 63

	command = optarg;

#line 65
             break;
#line 65
          }
#line 69 "rushopt.opt"
	 case 'i':
#line 69
          {
#line 69

	lint_option = 1;
	interactive = 1;

#line 72
             break;
#line 72
          }
#line 76 "rushopt.opt"
	 case 'D':
#line 76
          {
#line 76

	dump_option = optarg;
	lint_option = 1;

#line 79
             break;
#line 79
          }
#line 83 "rushopt.opt"
	 case 'C':
#line 83
          {
#line 83

	if (cfck_keyword(optarg))
		die(usage_error, NULL,
		    _("unknown keyword: %s"), optarg);

#line 87
             break;
#line 87
          }
#line 91 "rushopt.opt"
	 case OPTION_SHOW_DEFAULT:
#line 91
          {
#line 91

#ifdef RUSH_DEFAULT_CONFIG
	printf("%s\n", RUSH_DEFAULT_CONFIG);
	exit(0);
#else
	error(1, 0, _("No default configuration"));
#endif

#line 98
             break;
#line 98
          }
#line 100 "rushopt.opt"
	 case 'h':
#line 100
          {
#line 100

#line 100
		print_help ();
#line 100
                exit (0);
#line 100
	 
#line 100
             break;
#line 100
          }
#line 100 "rushopt.opt"
	 case OPTION_USAGE:
#line 100
          {
#line 100

#line 100
		print_usage ();
#line 100
		exit (0);
#line 100
	 
#line 100
             break;
#line 100
          }
#line 100 "rushopt.opt"
	 case OPTION_VERSION:
#line 100
          {
#line 100

#line 100
		/* Give version */
#line 100
		print_version(program_version, stdout);
#line 100
		exit (0);
#line 100
         
#line 100
             break;
#line 100
          }

#line 105 "rushopt.opt"
        }
#line 105
    }
#line 105
 }   
#line 105

}
