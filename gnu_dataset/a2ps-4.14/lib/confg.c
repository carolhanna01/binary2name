/* ANSI-C code produced by gperf version 3.0.3 */
/* Command-line: /usr/bin/gperf -t -D -L ANSI-C confg.gperf  */
/* Computed positions: -k'5' */

#if !((' ' == 32) && ('!' == 33) && ('"' == 34) && ('#' == 35) \
      && ('%' == 37) && ('&' == 38) && ('\'' == 39) && ('(' == 40) \
      && (')' == 41) && ('*' == 42) && ('+' == 43) && (',' == 44) \
      && ('-' == 45) && ('.' == 46) && ('/' == 47) && ('0' == 48) \
      && ('1' == 49) && ('2' == 50) && ('3' == 51) && ('4' == 52) \
      && ('5' == 53) && ('6' == 54) && ('7' == 55) && ('8' == 56) \
      && ('9' == 57) && (':' == 58) && (';' == 59) && ('<' == 60) \
      && ('=' == 61) && ('>' == 62) && ('?' == 63) && ('A' == 65) \
      && ('B' == 66) && ('C' == 67) && ('D' == 68) && ('E' == 69) \
      && ('F' == 70) && ('G' == 71) && ('H' == 72) && ('I' == 73) \
      && ('J' == 74) && ('K' == 75) && ('L' == 76) && ('M' == 77) \
      && ('N' == 78) && ('O' == 79) && ('P' == 80) && ('Q' == 81) \
      && ('R' == 82) && ('S' == 83) && ('T' == 84) && ('U' == 85) \
      && ('V' == 86) && ('W' == 87) && ('X' == 88) && ('Y' == 89) \
      && ('Z' == 90) && ('[' == 91) && ('\\' == 92) && (']' == 93) \
      && ('^' == 94) && ('_' == 95) && ('a' == 97) && ('b' == 98) \
      && ('c' == 99) && ('d' == 100) && ('e' == 101) && ('f' == 102) \
      && ('g' == 103) && ('h' == 104) && ('i' == 105) && ('j' == 106) \
      && ('k' == 107) && ('l' == 108) && ('m' == 109) && ('n' == 110) \
      && ('o' == 111) && ('p' == 112) && ('q' == 113) && ('r' == 114) \
      && ('s' == 115) && ('t' == 116) && ('u' == 117) && ('v' == 118) \
      && ('w' == 119) && ('x' == 120) && ('y' == 121) && ('z' == 122) \
      && ('{' == 123) && ('|' == 124) && ('}' == 125) && ('~' == 126))
/* The character set is not based on ISO-646.  */
#error "gperf generated tables don't work with this execution character set. Please report a bug to <bug-gnu-gperf@gnu.org>."
#endif

#line 1 "confg.gperf"
	/* -*- C -*- */
/*
 * confg.c
 *
 * Read and understanding everything about the options
 * & (dynamic) configuration of a2ps.
 * Copyright (c) 1988-1993 Miguel Santana
 * Copyright (c) 1995-1999 Akim Demaille, Miguel Santana
 */

/*
 * This file is part of a2ps.
 *
 * This program is free software; you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation; either version 3, or (at your option)
 * any later version.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with this program; see the file COPYING.  If not, write to
 * the Free Software Foundation, 59 Temple Place - Suite 330,
 * Boston, MA 02111-1307, USA.
 */

/************************************************************************/
/*									*/
/*			I n c l u d e   f i l e s			*/
/*                                                                      */
/************************************************************************/
#include "a2ps.h"
#include "routines.h"
#include "message.h"
#include "media.h"
#include "jobs.h"
#include "getshline.h"
#include "pathwalk.h"
#include "confg.h"
#include "useropt.h"
#include "path-concat.h"
#include "printers.h"
#include "options.h"
#include "metaseq.h"
#include "quotearg.h"
#include "dirname.h"

extern char *program_name;

/*
 * Hooks used
 */
config_hook delegation_hook = NULL;
config_hook toc_entry_hook = NULL;

enum keyword_e
{
  AppendLibraryPath,
  DefaultPPD,
  DefaultPrinter,
  Delegation,
  FileCommand,
  Include,
  LibraryPath,
  Medium,
  Obsolete,
  Options,
  OutputFirstLine,
  PageLabelFormat,
  PrependLibraryPath,
  Printer,
  UnknownPrinter,
  UserOption,
  Variable
};

#line 81 "confg.gperf"
struct keyword_s
{
  const char *name;
  enum keyword_e code;
  /* Number of arguments. */
  int argc;
  /* If true, the last argument composed of the rest of the line,
     otherwise separated by spaces. */
  bool line_token;
};

#define TOTAL_KEYWORDS 21
#define MIN_WORD_LENGTH 6
#define MAX_WORD_LENGTH 19
#define MIN_HASH_VALUE 6
#define MAX_HASH_VALUE 33
/* maximum key range = 28, duplicates = 0 */

#ifdef __GNUC__
__inline
#else
#ifdef __cplusplus
inline
#endif
#endif
static unsigned int
hash (register const char *str, register unsigned int len)
{
  static unsigned char asso_values[] =
    {
      34, 34, 34, 34, 34, 34, 34, 34, 34, 34,
      34, 34, 34, 34, 34, 34, 34, 34, 34, 34,
      34, 34, 34, 34, 34, 34, 34, 34, 34, 34,
      34, 34, 34, 34, 34, 34, 34, 34, 34, 34,
      34, 34, 34, 34, 34, 34, 34, 34, 34, 34,
      34, 34, 34, 34, 34, 34, 34, 34, 34, 34,
      34, 34, 34, 34, 34, 34, 34, 10, 34, 34,
      34, 34, 34, 34, 34, 34, 15, 34, 34, 15,
      34, 34, 34, 34,  5, 34, 34, 34, 34, 34,
      34, 34, 34, 34, 34, 34, 34,  0, 34, 34,
      34, 10, 34, 10, 34, 34, 34, 34, 34, 34,
      15,  5, 34, 34, 34, 34, 20,  0, 34, 34,
      34, 34, 34, 34, 34, 34, 34, 34, 34, 34,
      34, 34, 34, 34, 34, 34, 34, 34, 34, 34,
      34, 34, 34, 34, 34, 34, 34, 34, 34, 34,
      34, 34, 34, 34, 34, 34, 34, 34, 34, 34,
      34, 34, 34, 34, 34, 34, 34, 34, 34, 34,
      34, 34, 34, 34, 34, 34, 34, 34, 34, 34,
      34, 34, 34, 34, 34, 34, 34, 34, 34, 34,
      34, 34, 34, 34, 34, 34, 34, 34, 34, 34,
      34, 34, 34, 34, 34, 34, 34, 34, 34, 34,
      34, 34, 34, 34, 34, 34, 34, 34, 34, 34,
      34, 34, 34, 34, 34, 34, 34, 34, 34, 34,
      34, 34, 34, 34, 34, 34, 34, 34, 34, 34,
      34, 34, 34, 34, 34, 34, 34, 34, 34, 34,
      34, 34, 34, 34, 34, 34
    };
  return len + asso_values[(unsigned char)str[4]];
}

#ifdef __GNUC__
__inline
#ifdef __GNUC_STDC_INLINE__
__attribute__ ((__gnu_inline__))
#endif
#endif
struct keyword_s *
in_word_set (register const char *str, register unsigned int len)
{
  static struct keyword_s wordlist[] =
    {
#line 100 "confg.gperf"
      {"Media:",			Medium, 		2, true},
#line 101 "confg.gperf"
      {"Medium:",		Medium, 		2, true},
#line 97 "confg.gperf"
      {"Include:",		Include,		1, false},
#line 112 "confg.gperf"
      {"Variable:",		Variable,		2, true},
#line 93 "confg.gperf"
      {"DefaultPPD:",		DefaultPPD,		1, false},
#line 98 "confg.gperf"
      {"LibraryPath:",		LibraryPath, 		1, false},
#line 102 "confg.gperf"
      {"Options:",		Options, 		1, true},
#line 94 "confg.gperf"
      {"DefaultPrinter:",	DefaultPrinter,		1, true},
#line 103 "confg.gperf"
      {"OutputFirstLine:",	OutputFirstLine, 	1, true},
#line 105 "confg.gperf"
      {"PassThrough:",		Obsolete, 		1, true},
#line 106 "confg.gperf"
      {"Pattern:",		Obsolete, 		1, true},
#line 110 "confg.gperf"
      {"UnknownPrinter:",	UnknownPrinter,		1, true},
#line 95 "confg.gperf"
      {"Delegation:",		Delegation, 		1, true},
#line 96 "confg.gperf"
      {"FileCommand:",		FileCommand, 		1, true},
#line 99 "confg.gperf"
      {"MacroMetaSequence:",	Variable, 		2, true},
#line 109 "confg.gperf"
      {"TemporaryDirectory:",	Obsolete,		1, true},
#line 111 "confg.gperf"
      {"UserOption:",		UserOption, 		2, true},
#line 108 "confg.gperf"
      {"Printer:",		Printer, 		2, true},
#line 107 "confg.gperf"
      {"PrependLibraryPath:",	PrependLibraryPath,	1, true},
#line 104 "confg.gperf"
      {"PageLabelFormat:",	PageLabelFormat, 	1, true},
#line 92 "confg.gperf"
      {"AppendLibraryPath:",	AppendLibraryPath, 	1, true}
    };

  static short lookup[] =
    {
      -1, -1, -1, -1, -1, -1,  0,  1,  2,  3, -1,  4,  5,  6,
      -1,  7,  8,  9, 10, -1, 11, 12, 13, 14, 15, -1, 16, -1,
      17, 18, -1, 19, -1, 20
    };

  if (len <= MAX_WORD_LENGTH && len >= MIN_WORD_LENGTH)
    {
      register int key = hash (str, len);

      if (key <= MAX_HASH_VALUE && key >= 0)
        {
          register int index = lookup[key];

          if (index >= 0)
            {
              register const char *s = wordlist[index].name;

              if (*str == *s && !strcmp (str + 1, s + 1))
                return &wordlist[index];
            }
        }
    }
  return 0;
}
#line 113 "confg.gperf"

/*
 * Read the configuration file
 */
int
a2_read_config (a2ps_job * job, const char *path, const char *file)
{
/* The maximum number of args for an entry. */
#define MAX_ARGC 10
  FILE *fp;
  char * fname;
  char *buf = NULL;
  size_t bufsiz = 0;
  int firstline = 0, lastline = 0;

  fname = xpath_concat (path, file, NULL);

  fp = fopen (fname, "r");
  if (fp == NULL)
    {
      free (fname);
      return 0;
    }

  message (msg_opt | msg_file,
	   (stderr, "Reading configuration file `%s'\n", fname));

  while (getshline_numbered (&firstline, &lastline, &buf, &bufsiz, fp) != -1)
    {
      struct keyword_s *keyword;
      /* An array of the arguments.  The first argument ARGV[0] is the
	 name of the entry, eg 'Options:'. */
      int argc;
      char *argv [MAX_ARGC];

      argv[0] = strtok (buf, " \t\n");

      /* Blank line, but not empty */
      if (!argv[0])
	continue;

      keyword = in_word_set (argv[0], strlen (argv[0]));
      if (!keyword)
	error_at_line (1, 0, fname, firstline,
		       _("invalid option `%s'"), quotearg (argv[0]));

      /* Fetch the arguments */
      for (argc = 1 ; argc <= keyword->argc ; argc++)
	{
	  if (keyword->line_token && argc == keyword->argc)
	    argv [argc] = strtok (NULL, "\n");
	  else
	    argv [argc] = strtok (NULL, " \t\n");
	  if (argv [argc] == NULL)
	    error_at_line (1, 0, fname, firstline,
			   _("missing argument for `%s'"), quotearg (argv[0]));
	}
      /* Check that there is no extra argument. */
      if (strtok (NULL, "\n"))
	error_at_line (1, 0, fname, firstline,
		       "extra argument for `%s'", quotearg (argv[0]));

      /* Process the entry. */
      switch (keyword->code)
	{
	case Include:	/* At this point, read another config file. */
	  {
	    char * dir;
	    if (*argv[1] == DIRECTORY_SEPARATOR)
	      /* Path is absolute */
	      dir = NULL;
	    else
	      /* Relative.  Give its root. */
	      dir = dir_name (fname);

	    if (!a2_read_config (job, dir, argv[1]))
	      {
		char *included_file = xpath_concat (dir, argv[1], NULL);
		error_at_line (0, errno, fname, firstline,
			       _("cannot open file `%s'"),
			       quotearg (included_file));
		free (included_file);
	      }
	    XFREE (dir);
	  }
	  break;

	case Options:
	  {
	    /* Set PROGRAM_NAME so that the error messages report the
               file name and line. */
	    char *old_program_name = program_name;
	    program_name = ALLOCA (char,
				   strlen (program_name)
				   + strlen (fname)
				   + strlen ("%:%:999990"));
	    sprintf (program_name, "%s:%s:%d",
		     old_program_name, fname, firstline);
	    a2ps_handle_string_options (job, argv[1]);
	    program_name = old_program_name;
	  }
	  break;

	case DefaultPPD:      /* Default PPD file */
          a2ps_printers_default_ppdkey_set (job->printers, argv[1]);
	  break;

      /* Handling of the printers */
	case Printer:
	  if (!a2ps_printers_add (job->printers, argv[1], argv[2]))
	    error_at_line (1, 0, fname, firstline,
			   _("invalid definition for printer `%s': %s"),
			   argv[1], quotearg (argv[2]));
	  break;

	case UnknownPrinter:
	  if (!a2ps_printers_add (job->printers, _("Unknown Printer"), argv[1]))
	    error_at_line (1, 0, fname, firstline,
			   _("invalid definition for printer `%s': %s"),
			   _("Unknown Printer"), quotearg (argv[1]));
	  break;

	case DefaultPrinter:
	  if (!a2ps_printers_add (job->printers, _("Default Printer"), argv[1]))
	    error_at_line (1, 0, fname, firstline,
			   _("invalid definition for printer `%s': %s"),
			   _("Default Printer"), quotearg (argv[1]));
	  break;

	case Delegation:
	  /* This is only for a2ps the program.  Read this only if
	   there's a reader */
	  if (delegation_hook)
	    (*delegation_hook) (fname, firstline, argv[1]);
	  break;

	case UserOption:
	  user_option_add (job, argv[1], argv[2]);
	  break;

	case OutputFirstLine:
	  xustrcpy (job->status->magic_number, argv[1]);
	  break;

	case PageLabelFormat:
	  xustrcpy (job->status->page_label_format, argv[1]);
	  break;

	case Medium:
	  {
	    int w, h, llx, lly, urx, ury;

	    switch (sscanf (argv[2], "%d %d %d %d %d %d",
			    &w, &h, &llx, &lly, &urx, &ury))
	      {
	      case 6:
		/* BBox is also given */
		break;

	      case 2:
		/* A short hand has been used: use 24 points as a
		  margin all around */
		llx = lly = 24;
		urx = w - 24;
		ury = h - 24;
		break;

	      default:
		error_at_line (1, 0, fname, firstline,
			       "invalid number of arguments for `%s'",
			       quotearg (argv[0]));
	      }
	    add_medium (job, argv[1], w, h, llx, lly, urx, ury);
	  }
	  break;

	case Variable:
	  if (!macro_meta_sequence_add (job, argv[1], argv[2]))
	    error_at_line (1, 0, fname, firstline,
			   _("invalid variable identifier `%s'"),
			   quotearg (argv[1]));
	  break;

	  /* Handling of the library path */
	case LibraryPath:
	  XFREE (job->common.path);
	  job->common.path = pw_string_to_path (argv[1]);
	  break;

	case AppendLibraryPath:
	  job->common.path = pw_append_string_to_path (job->common.path,
						       argv[1]);
	  break;

	case PrependLibraryPath:
	  job->common.path = pw_prepend_string_to_path (job->common.path,
							argv[1]);
	  break;

	case FileCommand:	/* How to call file */
	  xstrcpy (job->file_command, argv[1]);
	  break;

	case Obsolete:
	  /* TRANS: The following message says that in a2ps.cfg there
	     is an entry (such as `Pattern:', or `PassThrough:') which
	     is no longer used. */
	  error_at_line (0, 0, fname, firstline,
			 _("obsolete `%s' entry.  Ignored"), argv[0]);
	  break;
	}
    }
  XFREE (fname);
  fclose (fp);
  /* BUF was allocated by getshlinenumbered. */
  free (buf);
  return 1;
}

/* Global config.
 *    This is really not an easy thing because, people may want
 *    to check the package before the installation.  The worst
 *    case is when an older a2ps is yet installed.  So we _must_
 *    have a special way to deal with the tests.  This is why
 *    I introduced an env-var: A2PS_CONFIG, which
 *    points to a2ps.cfg.
 *    Note that it also improves the robustness of `make distcheck'.
 */
void
a2_read_sys_config (a2ps_job * job)
{
  const char *config_file;

  config_file = getenv ("A2PS_CONFIG");
  if (!config_file)
    config_file = SYSCONFFILE;

  /* I see no reason to end a2ps here if the file is not found: other
     files follow.  Just say it.  */
  if (a2_read_config (job, NULL, config_file))
    return;
  error (0, errno,
	 _("cannot open file `%s'"), quotearg (config_file));
}
