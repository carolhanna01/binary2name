/* -*- buffer-read-only: t -*- vi: set ro:
   THIS FILE IS GENERATED AUTOMATICALLY.  PLEASE DO NOT EDIT.
*/

#line 1 "rlopt.opt"
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

static int numeric_option;


#line 20 "rlopt.opt"

#line 20
void print_help(void);
#line 20
void print_usage(void);
#line 80 "rlopt.opt"

#line 80
/* Option codes */
#line 80
enum {
#line 80
	_OPTION_INIT=255,
#line 80
	#line 38 "rlopt.opt"

#line 38
	OPTION_FORWARD,
#line 80 "rlopt.opt"

#line 80
	OPTION_USAGE,
#line 80 "rlopt.opt"

#line 80
	OPTION_VERSION,

#line 80 "rlopt.opt"
	MAX_OPTION
#line 80
};
#line 80
static struct option long_options[] = {
#line 80
	#line 24 "rlopt.opt"

#line 24
	{ "format", required_argument, 0, 'F' },
#line 31 "rlopt.opt"

#line 31
	{ "file", required_argument, 0, 'f' },
#line 38 "rlopt.opt"

#line 38
	{ "forward", no_argument, 0, OPTION_FORWARD },
#line 45 "rlopt.opt"

#line 45
	{ "no-header", no_argument, 0, 'H' },
#line 52 "rlopt.opt"

#line 52
	{ "count", required_argument, 0, 'n' },
#line 80 "rlopt.opt"

#line 80
	{ "help", no_argument, 0, 'h' },
#line 80 "rlopt.opt"

#line 80
	{ "usage", no_argument, 0, OPTION_USAGE },
#line 80 "rlopt.opt"

#line 80
	{ "version", no_argument, 0, OPTION_VERSION },

#line 80 "rlopt.opt"
	{0, 0, 0, 0}
#line 80
};
#line 80
static struct opthelp {
#line 80
        const char *opt;
#line 80
        const char *arg;
#line 80
        int is_optional;
#line 80
        const char *descr;
#line 80
} opthelp[] = {
#line 80
	#line 26 "rlopt.opt"

#line 26
	{ "-F, --format", N_("STRING"), 0, N_("Use STRING instead of the default format.") },
#line 33 "rlopt.opt"

#line 33
	{ "-f, --file", N_("DIR"), 0, N_("Look for database files in DIR.") },
#line 40 "rlopt.opt"

#line 40
	{ "--forward", NULL, 0, N_("Show entries in chronological order.") },
#line 47 "rlopt.opt"

#line 47
	{ "-H, --no-header", NULL, 0, N_("Do not display header line.") },
#line 54 "rlopt.opt"

#line 54
	{ "-n, --count", N_("NUMBER"), 0, N_("Show at most NUM records.") },
#line 80 "rlopt.opt"

#line 80
	{ NULL, NULL, 0, N_("Other options") },
#line 80 "rlopt.opt"

#line 80
	{ "-h, --help", NULL, 0, N_("Give this help list") },
#line 80 "rlopt.opt"

#line 80
	{ "--usage", NULL, 0, N_("Give a short usage message") },
#line 80 "rlopt.opt"

#line 80
	{ "--version", NULL, 0, N_("Print program version") },

#line 80 "rlopt.opt"
};
#line 20 "rlopt.opt"

#line 20
const char *program_version = "rushlast" " (" PACKAGE_STRING ")";
#line 20
static char doc[] = N_("rushlast - show listing of last Rush logins.");
#line 20
static char args_doc[] = N_("[user [user...]]");
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
      int s = 0;
#line 20
      int i;
#line 20
      size_t width = rmargin - lmargin;
#line 20

#line 20
      for (i = 0; ; i++)
#line 20
	{
#line 20
	  if (descr[i] == 0 || isspace (descr[i]))
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
  printf ("%s %s [%s]...", _("Usage:"), "rushlast", _("OPTION"));
#line 20
  if (args_doc[0])
#line 20
	  printf(" %s", gettext(args_doc));
#line 20
  printf("\n");
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
  printf (_("Report bugs to %s.\n"), program_bug_address);
#line 20
}
#line 20

#line 20
static int
#line 20
cmpidx_short(const void *a, const void *b)
#line 20
{
#line 20
	struct opthelp const **opta = (struct opthelp const **)a;
#line 20
	struct opthelp const **optb = (struct opthelp const **)b;
#line 20

#line 20
	return (*opta)->opt[1] - (*optb)->opt[1];
#line 20
}
#line 20
  
#line 20
static int
#line 20
cmpidx_long(const void *a, const void *b)
#line 20
{
#line 20
	struct opthelp const **ap = (struct opthelp const **)a;
#line 20
	struct opthelp const **bp = (struct opthelp const **)b;
#line 20
	char const *opta, *optb;
#line 20
	size_t lena, lenb;
#line 20

#line 20
	if ((*ap)->opt[1] == '-')
#line 20
		opta = (*ap)->opt;
#line 20
	else
#line 20
		opta = (*ap)->opt + 4;
#line 20
	lena = strcspn(opta, ",");
#line 20
  
#line 20
	if ((*bp)->opt[1] == '-')
#line 20
		optb = (*bp)->opt;
#line 20
	else
#line 20
		optb = (*bp)->opt + 4;
#line 20
	lenb = strcspn(optb, ",");
#line 20
	return strncmp(opta, optb, lena > lenb ? lenb : lena);
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
	unsigned n;
#line 20
	char *buf;
#line 20
	size_t bufsize;
#line 20
	unsigned nidx;
#line 20
	struct opthelp **optidx;
#line 20
	size_t optcount = sizeof(opthelp) / sizeof(opthelp[0]);
#line 20
  
#line 20
#define FLUSH do {						buf[n] = 0;					printf("%s\n", buf);				n = USAGECOLUMN;				memset(buf, ' ', n);			}  while (0)
#line 20
#define ADDC(c) do { if (n == RMARGIN) FLUSH; buf[n++] = c; } while (0)
#line 20

#line 20
	optidx = calloc(optcount, sizeof(optidx[0]));
#line 20
	if (!optidx)
#line 20
		abort();
#line 20
	bufsize = RMARGIN + 1;
#line 20
	buf = malloc(bufsize);
#line 20
	if (!buf)
#line 20
		abort();
#line 20
	
#line 20
	n = snprintf(buf, bufsize, "%s %s ", _("Usage:"), "rushlast");
#line 20
	
#line 20
	/* Print a list of short options without arguments. */
#line 20
	for (i = nidx = 0; i < optcount; i++)
#line 20
		if (opthelp[i].opt &&
#line 20
		    opthelp[i].descr &&
#line 20
		    opthelp[i].opt[1] != '-' &&
#line 20
		    opthelp[i].arg == NULL)
#line 20
			optidx[nidx++] = opthelp + i;
#line 20

#line 20
	if (nidx) {
#line 20
		qsort(optidx, nidx, sizeof(optidx[0]), cmpidx_short);
#line 20

#line 20
		ADDC('[');
#line 20
		ADDC('-');
#line 20
		for (i = 0; i < nidx; i++) {
#line 20
			ADDC(optidx[i]->opt[1]);
#line 20
		}
#line 20
		ADDC(']');
#line 20
	}
#line 20

#line 20
	/* Print a list of short options with arguments. */
#line 20
	for (i = nidx = 0; i < optcount; i++) {
#line 20
		if (opthelp[i].opt &&
#line 20
		    opthelp[i].descr &&
#line 20
		    opthelp[i].opt[1] != '-' &&
#line 20
		    opthelp[i].arg)
#line 20
			optidx[nidx++] = opthelp + i;
#line 20
	}
#line 20

#line 20
	if (nidx) {
#line 20
		qsort(optidx, nidx, sizeof(optidx[0]), cmpidx_short);
#line 20
    
#line 20
		for (i = 0; i < nidx; i++) {
#line 20
			struct opthelp *opt = optidx[i];
#line 20
			size_t len = 5 + strlen(opt->arg)
#line 20
				       + (opt->is_optional ? 2 : 1);
#line 20
      
#line 20
			if (n + len > RMARGIN)
#line 20
				FLUSH;
#line 20
			buf[n++] = ' ';
#line 20
			buf[n++] = '[';
#line 20
			buf[n++] = '-';
#line 20
			buf[n++] = opt->opt[1];
#line 20
			if (opt->is_optional) {
#line 20
				buf[n++] = '[';
#line 20
				strcpy(&buf[n], opt->arg);
#line 20
				n += strlen(opt->arg);
#line 20
				buf[n++] = ']';
#line 20
			} else {
#line 20
				buf[n++] = ' ';
#line 20
				strcpy(&buf[n], opt->arg);
#line 20
				n += strlen(opt->arg);
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
	for (i = nidx = 0; i < optcount; i++) {
#line 20
		if (opthelp[i].opt && opthelp[i].descr
#line 20
		    && (opthelp[i].opt[1] == '-' || opthelp[i].opt[2] == ','))
#line 20
			optidx[nidx++] = opthelp + i;
#line 20
	}
#line 20

#line 20
	if (nidx) {
#line 20
		qsort(optidx, nidx, sizeof(optidx[0]), cmpidx_long);
#line 20
	
#line 20
		for (i = 0; i < nidx; i++) {
#line 20
			struct opthelp *opt = optidx[i];
#line 20
			size_t len;
#line 20
			const char *longopt;
#line 20
	  
#line 20
			if (opt->opt[1] == '-')
#line 20
				longopt = opt->opt;
#line 20
			else if (opt->opt[2] == ',')
#line 20
				longopt = opt->opt + 4;
#line 20
			else
#line 20
				continue;
#line 20

#line 20
			len = 3 + strlen(longopt)
#line 20
				+ (opt->arg ? 1 + strlen(opt->arg)
#line 20
				+ (opt->is_optional ? 2 : 0) : 0);
#line 20
			if (n + len > RMARGIN) {
#line 20
				FLUSH;
#line 20
				/* Make sure we have enough buffer space if
                                   the string cannot be split */
#line 20
				if (n + len > bufsize) {
#line 20
					bufsize = n + len;
#line 20
					buf = realloc(buf, bufsize);
#line 20
					if (!buf)
#line 20
						abort();
#line 20
				}
#line 20
			}
#line 20
			buf[n++] = ' ';
#line 20
			buf[n++] = '[';
#line 20
			strcpy(&buf[n], longopt);
#line 20
			n += strlen(longopt);
#line 20
			if (opt->arg) {
#line 20
				buf[n++] = '=';
#line 20
				if (opt->is_optional) {
#line 20
					buf[n++] = '[';
#line 20
					strcpy(&buf[n], opt->arg);
#line 20
					n += strlen(opt->arg);
#line 20
					buf[n++] = ']';
#line 20
				} else {
#line 20
					strcpy(&buf[n], opt->arg);
#line 20
					n += strlen(opt->arg);
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

#line 20
	/* Print argument list */
#line 20
	if (args_doc[0]) {
#line 20
		char const *docstr = gettext(args_doc);
#line 20
		size_t len = strlen(docstr) + 1;
#line 20
		if (n + len <= RMARGIN) {
#line 20
			buf[n++] = ' ';
#line 20
			strcpy(buf + n, docstr);
#line 20
			n += len;
#line 20
		} else {
#line 20
			struct wordsplit ws;
#line 20

#line 20
			if (wordsplit(docstr, &ws, 
#line 20
				      WRDSF_SHOWERR |
#line 20
				      WRDSF_NOVAR |
#line 20
				      WRDSF_NOCMD |
#line 20
				      WRDSF_QUOTE |
#line 20
				      WRDSF_SQUEEZE_DELIMS))
#line 20
				abort();
#line 20

#line 20
			for (i = 0; i < ws.ws_wordc; i++) {
#line 20
				len = strlen(ws.ws_wordv[i]) + 1;
#line 20
				if (n + len > RMARGIN) {
#line 20
					FLUSH;
#line 20
					/* Make sure we have enough buffer
					   space if the string cannot be
					   split */
#line 20
					if (n + len > bufsize) {
#line 20
						bufsize = n + len;
#line 20
						buf = realloc(buf, bufsize);
#line 20
						if (!buf)
#line 20
							abort();
#line 20
					}
#line 20
				}
#line 20
				buf[n++] = ' ';
#line 20
				strcpy(buf + n, ws.ws_wordv[i]);
#line 20
				n += len;
#line 20
			}
#line 20
		}	
#line 20
	}
#line 20
	
#line 20
	FLUSH;
#line 20

#line 20
	free(optidx);
#line 20
	free(buf);
#line 20
}
#line 20

#line 20
const char version_etc_copyright[] =
#line 20
  /* Do *not* mark this string for translation.  %s is a copyright
     symbol suitable for this locale, and %d is the copyright
     year.  */
#line 20
  "Copyright %s 2008-2017 Sergey Poznyakoff";
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
	
#line 20
}
#line 20

#line 80 "rlopt.opt"

#line 80


void
get_options(int argc, char *argv[])
{
	
#line 85
 {
#line 85
  int optchar;
#line 85

#line 85

#line 85
  while ((optchar = getopt_long(argc, argv, "F:f:Hn:0123456789h",
#line 85
                                long_options, NULL)) != EOF)
#line 85
    {
#line 85
      switch (optchar)
#line 85
        {
#line 85
        default:
#line 85
	exit(1);
#line 85
       
#line 85
	#line 26 "rlopt.opt"
	 case 'F':
#line 26
          {
#line 26

	numeric_option = 0;
	format = optarg;

#line 29
             break;
#line 29
          }
#line 33 "rlopt.opt"
	 case 'f':
#line 33
          {
#line 33

	numeric_option = 0;
	base_name = optarg;

#line 36
             break;
#line 36
          }
#line 40 "rlopt.opt"
	 case OPTION_FORWARD:
#line 40
          {
#line 40

	numeric_option = 0;
	forward = 1;

#line 43
             break;
#line 43
          }
#line 47 "rlopt.opt"
	 case 'H':
#line 47
          {
#line 47

	numeric_option = 0;
	display_header = 0;

#line 50
             break;
#line 50
          }
#line 54 "rlopt.opt"
	 case 'n':
#line 54
          {
#line 54

	char *p;
	numeric_option = 0;
	count = strtoul(optarg, &p, 10);
        if (*p) 
		error(1, 0, _("invalid number (%s)"), optarg);

#line 60
             break;
#line 60
          }
#line 72 "rlopt.opt"
	 case '0': case '1': case '2': case '3': case '4': case '5': case '6': case '7': case '8': case '9':
#line 72
          {
#line 72

	if (!numeric_option) {
		count = 0;
		numeric_option = 1;
	}
	count = count * 10 + optchar - '0';

#line 78
             break;
#line 78
          }
#line 80 "rlopt.opt"
	 case 'h':
#line 80
          {
#line 80

#line 80
		print_help ();
#line 80
                exit (0);
#line 80
	 
#line 80
             break;
#line 80
          }
#line 80 "rlopt.opt"
	 case OPTION_USAGE:
#line 80
          {
#line 80

#line 80
		print_usage ();
#line 80
		exit (0);
#line 80
	 
#line 80
             break;
#line 80
          }
#line 80 "rlopt.opt"
	 case OPTION_VERSION:
#line 80
          {
#line 80

#line 80
		/* Give version */
#line 80
		print_version(program_version, stdout);
#line 80
		exit (0);
#line 80
         
#line 80
             break;
#line 80
          }

#line 85 "rlopt.opt"
        }
#line 85
    }
#line 85
 }   
#line 85
;
}

