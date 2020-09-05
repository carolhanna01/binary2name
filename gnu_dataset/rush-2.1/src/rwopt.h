/* -*- buffer-read-only: t -*- vi: set ro:
   THIS FILE IS GENERATED AUTOMATICALLY.  PLEASE DO NOT EDIT.
*/

#line 1 "rwopt.opt"
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

#line 17 "rwopt.opt"

#line 17
void print_help(void);
#line 17
void print_usage(void);
#line 38 "rwopt.opt"

#line 38
/* Option codes */
#line 38
enum {
#line 38
	_OPTION_INIT=255,
#line 38
	#line 38 "rwopt.opt"

#line 38
	OPTION_USAGE,
#line 38 "rwopt.opt"

#line 38
	OPTION_VERSION,

#line 38 "rwopt.opt"
	MAX_OPTION
#line 38
};
#line 38
static struct option long_options[] = {
#line 38
	#line 20 "rwopt.opt"

#line 20
	{ "format", required_argument, 0, 'F' },
#line 26 "rwopt.opt"

#line 26
	{ "file", required_argument, 0, 'f' },
#line 32 "rwopt.opt"

#line 32
	{ "no-header", no_argument, 0, 'H' },
#line 38 "rwopt.opt"

#line 38
	{ "help", no_argument, 0, 'h' },
#line 38 "rwopt.opt"

#line 38
	{ "usage", no_argument, 0, OPTION_USAGE },
#line 38 "rwopt.opt"

#line 38
	{ "version", no_argument, 0, OPTION_VERSION },

#line 38 "rwopt.opt"
	{0, 0, 0, 0}
#line 38
};
#line 38
static struct opthelp {
#line 38
        const char *opt;
#line 38
        const char *arg;
#line 38
        int is_optional;
#line 38
        const char *descr;
#line 38
} opthelp[] = {
#line 38
	#line 22 "rwopt.opt"

#line 22
	{ "-F, --format", N_("STRING"), 0, N_("Use STRING instead of the default format.") },
#line 28 "rwopt.opt"

#line 28
	{ "-f, --file", N_("DIR"), 0, N_("Look for database files in DIR.") },
#line 34 "rwopt.opt"

#line 34
	{ "-H, --no-header", NULL, 0, N_("Do not display header line.") },
#line 38 "rwopt.opt"

#line 38
	{ NULL, NULL, 0, N_("Other options") },
#line 38 "rwopt.opt"

#line 38
	{ "-h, --help", NULL, 0, N_("Give this help list") },
#line 38 "rwopt.opt"

#line 38
	{ "--usage", NULL, 0, N_("Give a short usage message") },
#line 38 "rwopt.opt"

#line 38
	{ "--version", NULL, 0, N_("Print program version") },

#line 38 "rwopt.opt"
};
#line 17 "rwopt.opt"

#line 17
const char *program_version = "rushwho" " (" PACKAGE_STRING ")";
#line 17
static char doc[] = N_("rushwho - show listing of online Rush users.");
#line 17
static char args_doc[] = N_("");
#line 17
const char *program_bug_address = "<" PACKAGE_BUGREPORT ">";
#line 17
		    
#line 17
#define DESCRCOLUMN 30
#line 17
#define RMARGIN 79
#line 17
#define GROUPCOLUMN 2
#line 17
#define USAGECOLUMN 13
#line 17
		    
#line 17
static void
#line 17
indent (size_t start, size_t col)
#line 17
{
#line 17
  for (; start < col; start++)
#line 17
    putchar (' ');
#line 17
}
#line 17
		    
#line 17
static void
#line 17
print_option_descr (const char *descr, size_t lmargin, size_t rmargin)
#line 17
{
#line 17
  while (*descr)
#line 17
    {
#line 17
      int s = 0;
#line 17
      int i;
#line 17
      size_t width = rmargin - lmargin;
#line 17

#line 17
      for (i = 0; ; i++)
#line 17
	{
#line 17
	  if (descr[i] == 0 || isspace (descr[i]))
#line 17
	    {
#line 17
	      if (i > width)
#line 17
		break;
#line 17
	      s = i;
#line 17
	      if (descr[i] == 0)
#line 17
		break;
#line 17
	    }
#line 17
	}
#line 17
      printf ("%*.*s\n", s, s, descr);
#line 17
      descr += s;
#line 17
      if (*descr)
#line 17
	{
#line 17
	  indent (0, lmargin);
#line 17
	  descr++;
#line 17
	}
#line 17
    }
#line 17
}
#line 17

#line 17
void
#line 17
print_help(void)
#line 17
{
#line 17
  unsigned i;
#line 17
  
#line 17
  printf ("%s %s [%s]...", _("Usage:"), "rushwho", _("OPTION"));
#line 17
  if (args_doc[0])
#line 17
	  printf(" %s", gettext(args_doc));
#line 17
  printf("\n");
#line 17
  if (doc[0])
#line 17
    print_option_descr(gettext (doc), 0, RMARGIN);
#line 17
  putchar ('\n');
#line 17

#line 17
  for (i = 0; i < sizeof (opthelp) / sizeof (opthelp[0]); i++)
#line 17
    {
#line 17
      unsigned n;
#line 17
      if (opthelp[i].opt)
#line 17
	{
#line 17
	  n = printf ("  %s", opthelp[i].opt);
#line 17
	  if (opthelp[i].arg)
#line 17
	    {
#line 17
	      char *cb, *ce;
#line 17
	      if (strlen (opthelp[i].opt) == 2)
#line 17
		{
#line 17
		  if (!opthelp[i].is_optional)
#line 17
		    {
#line 17
		      putchar (' ');
#line 17
		      n++;
#line 17
		    }
#line 17
		}
#line 17
	      else
#line 17
		{
#line 17
		  putchar ('=');
#line 17
		  n++;
#line 17
		}
#line 17
	      if (opthelp[i].is_optional)
#line 17
		{
#line 17
		  cb = "[";
#line 17
		  ce = "]";
#line 17
		}
#line 17
	      else
#line 17
		cb = ce = "";
#line 17
	      n += printf ("%s%s%s", cb, gettext (opthelp[i].arg), ce);
#line 17
	    }
#line 17
	  if (n >= DESCRCOLUMN)
#line 17
	    {
#line 17
	      putchar ('\n');
#line 17
	      n = 0;
#line 17
	    }
#line 17
	  indent (n, DESCRCOLUMN);
#line 17
	  print_option_descr (gettext (opthelp[i].descr), DESCRCOLUMN, RMARGIN);
#line 17
	}
#line 17
      else
#line 17
	{
#line 17
	  if (i)
#line 17
	    putchar ('\n');
#line 17
	  indent (0, GROUPCOLUMN);
#line 17
	  print_option_descr (gettext (opthelp[i].descr),
#line 17
			      GROUPCOLUMN, RMARGIN);
#line 17
	  putchar ('\n');
#line 17
	}
#line 17
    }
#line 17
  
#line 17
  putchar ('\n');
#line 17
  print_option_descr (_("Mandatory or optional arguments to long options are also mandatory or optional for any corresponding short options."), 0, RMARGIN);
#line 17
  putchar ('\n');
#line 17
  printf (_("Report bugs to %s.\n"), program_bug_address);
#line 17
}
#line 17

#line 17
static int
#line 17
cmpidx_short(const void *a, const void *b)
#line 17
{
#line 17
	struct opthelp const **opta = (struct opthelp const **)a;
#line 17
	struct opthelp const **optb = (struct opthelp const **)b;
#line 17

#line 17
	return (*opta)->opt[1] - (*optb)->opt[1];
#line 17
}
#line 17
  
#line 17
static int
#line 17
cmpidx_long(const void *a, const void *b)
#line 17
{
#line 17
	struct opthelp const **ap = (struct opthelp const **)a;
#line 17
	struct opthelp const **bp = (struct opthelp const **)b;
#line 17
	char const *opta, *optb;
#line 17
	size_t lena, lenb;
#line 17

#line 17
	if ((*ap)->opt[1] == '-')
#line 17
		opta = (*ap)->opt;
#line 17
	else
#line 17
		opta = (*ap)->opt + 4;
#line 17
	lena = strcspn(opta, ",");
#line 17
  
#line 17
	if ((*bp)->opt[1] == '-')
#line 17
		optb = (*bp)->opt;
#line 17
	else
#line 17
		optb = (*bp)->opt + 4;
#line 17
	lenb = strcspn(optb, ",");
#line 17
	return strncmp(opta, optb, lena > lenb ? lenb : lena);
#line 17
}
#line 17

#line 17
void
#line 17
print_usage(void)
#line 17
{
#line 17
	unsigned i;
#line 17
	unsigned n;
#line 17
	char *buf;
#line 17
	size_t bufsize;
#line 17
	unsigned nidx;
#line 17
	struct opthelp **optidx;
#line 17
	size_t optcount = sizeof(opthelp) / sizeof(opthelp[0]);
#line 17
  
#line 17
#define FLUSH do {						buf[n] = 0;					printf("%s\n", buf);				n = USAGECOLUMN;				memset(buf, ' ', n);			}  while (0)
#line 17
#define ADDC(c) do { if (n == RMARGIN) FLUSH; buf[n++] = c; } while (0)
#line 17

#line 17
	optidx = calloc(optcount, sizeof(optidx[0]));
#line 17
	if (!optidx)
#line 17
		abort();
#line 17
	bufsize = RMARGIN + 1;
#line 17
	buf = malloc(bufsize);
#line 17
	if (!buf)
#line 17
		abort();
#line 17
	
#line 17
	n = snprintf(buf, bufsize, "%s %s ", _("Usage:"), "rushwho");
#line 17
	
#line 17
	/* Print a list of short options without arguments. */
#line 17
	for (i = nidx = 0; i < optcount; i++)
#line 17
		if (opthelp[i].opt &&
#line 17
		    opthelp[i].descr &&
#line 17
		    opthelp[i].opt[1] != '-' &&
#line 17
		    opthelp[i].arg == NULL)
#line 17
			optidx[nidx++] = opthelp + i;
#line 17

#line 17
	if (nidx) {
#line 17
		qsort(optidx, nidx, sizeof(optidx[0]), cmpidx_short);
#line 17

#line 17
		ADDC('[');
#line 17
		ADDC('-');
#line 17
		for (i = 0; i < nidx; i++) {
#line 17
			ADDC(optidx[i]->opt[1]);
#line 17
		}
#line 17
		ADDC(']');
#line 17
	}
#line 17

#line 17
	/* Print a list of short options with arguments. */
#line 17
	for (i = nidx = 0; i < optcount; i++) {
#line 17
		if (opthelp[i].opt &&
#line 17
		    opthelp[i].descr &&
#line 17
		    opthelp[i].opt[1] != '-' &&
#line 17
		    opthelp[i].arg)
#line 17
			optidx[nidx++] = opthelp + i;
#line 17
	}
#line 17

#line 17
	if (nidx) {
#line 17
		qsort(optidx, nidx, sizeof(optidx[0]), cmpidx_short);
#line 17
    
#line 17
		for (i = 0; i < nidx; i++) {
#line 17
			struct opthelp *opt = optidx[i];
#line 17
			size_t len = 5 + strlen(opt->arg)
#line 17
				       + (opt->is_optional ? 2 : 1);
#line 17
      
#line 17
			if (n + len > RMARGIN)
#line 17
				FLUSH;
#line 17
			buf[n++] = ' ';
#line 17
			buf[n++] = '[';
#line 17
			buf[n++] = '-';
#line 17
			buf[n++] = opt->opt[1];
#line 17
			if (opt->is_optional) {
#line 17
				buf[n++] = '[';
#line 17
				strcpy(&buf[n], opt->arg);
#line 17
				n += strlen(opt->arg);
#line 17
				buf[n++] = ']';
#line 17
			} else {
#line 17
				buf[n++] = ' ';
#line 17
				strcpy(&buf[n], opt->arg);
#line 17
				n += strlen(opt->arg);
#line 17
			}
#line 17
			buf[n++] = ']';
#line 17
		}
#line 17
	}
#line 17
  
#line 17
	/* Print a list of long options */
#line 17
	for (i = nidx = 0; i < optcount; i++) {
#line 17
		if (opthelp[i].opt && opthelp[i].descr
#line 17
		    && (opthelp[i].opt[1] == '-' || opthelp[i].opt[2] == ','))
#line 17
			optidx[nidx++] = opthelp + i;
#line 17
	}
#line 17

#line 17
	if (nidx) {
#line 17
		qsort(optidx, nidx, sizeof(optidx[0]), cmpidx_long);
#line 17
	
#line 17
		for (i = 0; i < nidx; i++) {
#line 17
			struct opthelp *opt = optidx[i];
#line 17
			size_t len;
#line 17
			const char *longopt;
#line 17
	  
#line 17
			if (opt->opt[1] == '-')
#line 17
				longopt = opt->opt;
#line 17
			else if (opt->opt[2] == ',')
#line 17
				longopt = opt->opt + 4;
#line 17
			else
#line 17
				continue;
#line 17

#line 17
			len = 3 + strlen(longopt)
#line 17
				+ (opt->arg ? 1 + strlen(opt->arg)
#line 17
				+ (opt->is_optional ? 2 : 0) : 0);
#line 17
			if (n + len > RMARGIN) {
#line 17
				FLUSH;
#line 17
				/* Make sure we have enough buffer space if
                                   the string cannot be split */
#line 17
				if (n + len > bufsize) {
#line 17
					bufsize = n + len;
#line 17
					buf = realloc(buf, bufsize);
#line 17
					if (!buf)
#line 17
						abort();
#line 17
				}
#line 17
			}
#line 17
			buf[n++] = ' ';
#line 17
			buf[n++] = '[';
#line 17
			strcpy(&buf[n], longopt);
#line 17
			n += strlen(longopt);
#line 17
			if (opt->arg) {
#line 17
				buf[n++] = '=';
#line 17
				if (opt->is_optional) {
#line 17
					buf[n++] = '[';
#line 17
					strcpy(&buf[n], opt->arg);
#line 17
					n += strlen(opt->arg);
#line 17
					buf[n++] = ']';
#line 17
				} else {
#line 17
					strcpy(&buf[n], opt->arg);
#line 17
					n += strlen(opt->arg);
#line 17
				}
#line 17
			}
#line 17
			buf[n++] = ']';
#line 17
		}
#line 17
	}
#line 17

#line 17
	/* Print argument list */
#line 17
	if (args_doc[0]) {
#line 17
		char const *docstr = gettext(args_doc);
#line 17
		size_t len = strlen(docstr) + 1;
#line 17
		if (n + len <= RMARGIN) {
#line 17
			buf[n++] = ' ';
#line 17
			strcpy(buf + n, docstr);
#line 17
			n += len;
#line 17
		} else {
#line 17
			struct wordsplit ws;
#line 17

#line 17
			if (wordsplit(docstr, &ws, 
#line 17
				      WRDSF_SHOWERR |
#line 17
				      WRDSF_NOVAR |
#line 17
				      WRDSF_NOCMD |
#line 17
				      WRDSF_QUOTE |
#line 17
				      WRDSF_SQUEEZE_DELIMS))
#line 17
				abort();
#line 17

#line 17
			for (i = 0; i < ws.ws_wordc; i++) {
#line 17
				len = strlen(ws.ws_wordv[i]) + 1;
#line 17
				if (n + len > RMARGIN) {
#line 17
					FLUSH;
#line 17
					/* Make sure we have enough buffer
					   space if the string cannot be
					   split */
#line 17
					if (n + len > bufsize) {
#line 17
						bufsize = n + len;
#line 17
						buf = realloc(buf, bufsize);
#line 17
						if (!buf)
#line 17
							abort();
#line 17
					}
#line 17
				}
#line 17
				buf[n++] = ' ';
#line 17
				strcpy(buf + n, ws.ws_wordv[i]);
#line 17
				n += len;
#line 17
			}
#line 17
		}	
#line 17
	}
#line 17
	
#line 17
	FLUSH;
#line 17

#line 17
	free(optidx);
#line 17
	free(buf);
#line 17
}
#line 17

#line 17
const char version_etc_copyright[] =
#line 17
  /* Do *not* mark this string for translation.  %s is a copyright
     symbol suitable for this locale, and %d is the copyright
     year.  */
#line 17
  "Copyright %s 2008-2017 Sergey Poznyakoff";
#line 17

#line 17
void
#line 17
print_version_only(const char *program_version, FILE *stream)
#line 17
{
#line 17
	fprintf (stream, "%s\n", program_version);
#line 17
	/* TRANSLATORS: Translate "(C)" to the copyright symbol
	   (C-in-a-circle), if this symbol is available in the user's
	   locale.  Otherwise, do not translate "(C)"; leave it as-is.  */
#line 17
	fprintf (stream, version_etc_copyright, _("(C)"));
#line 17
	fputc ('\n', stream);
#line 17
}
#line 17

#line 17
void
#line 17
print_version(const char *program_version, FILE *stream)
#line 17
{
#line 17
	print_version_only(program_version, stream);
#line 17
	
#line 17
	fputs (_("License GPLv3+: GNU GPL version 3 or later <http://gnu.org/licenses/gpl.html>\nThis is free software: you are free to change and redistribute it.\nThere is NO WARRANTY, to the extent permitted by law.\n\n"),
#line 17
	       stream);
#line 17
	
#line 17
}
#line 17

#line 38 "rwopt.opt"

#line 38


void
get_options(int argc, char *argv[])
{
	
#line 43
 {
#line 43
  int optchar;
#line 43

#line 43

#line 43
  while ((optchar = getopt_long(argc, argv, "F:f:Hh",
#line 43
                                long_options, NULL)) != EOF)
#line 43
    {
#line 43
      switch (optchar)
#line 43
        {
#line 43
        default:
#line 43
	exit(1);
#line 43
       
#line 43
	#line 22 "rwopt.opt"
	 case 'F':
#line 22
          {
#line 22

	format = optarg;

#line 24
             break;
#line 24
          }
#line 28 "rwopt.opt"
	 case 'f':
#line 28
          {
#line 28

	base_name = optarg;

#line 30
             break;
#line 30
          }
#line 34 "rwopt.opt"
	 case 'H':
#line 34
          {
#line 34

	display_header = 0;

#line 36
             break;
#line 36
          }
#line 38 "rwopt.opt"
	 case 'h':
#line 38
          {
#line 38

#line 38
		print_help ();
#line 38
                exit (0);
#line 38
	 
#line 38
             break;
#line 38
          }
#line 38 "rwopt.opt"
	 case OPTION_USAGE:
#line 38
          {
#line 38

#line 38
		print_usage ();
#line 38
		exit (0);
#line 38
	 
#line 38
             break;
#line 38
          }
#line 38 "rwopt.opt"
	 case OPTION_VERSION:
#line 38
          {
#line 38

#line 38
		/* Give version */
#line 38
		print_version(program_version, stdout);
#line 38
		exit (0);
#line 38
         
#line 38
             break;
#line 38
          }

#line 43 "rwopt.opt"
        }
#line 43
    }
#line 43
 }   
#line 43
;
}

