/* bool.c - Main routines.
   Copyright (C) 2001 Free Software Foundation, Inc.

   This program is free software; you can redistribute it and/or
   modify it under the terms of the GNU General Public License
   as published by the Free Software Foundation; either version 2
   of the License, or (at your option) any later version.

   This program is distributed in the hope that it will be useful,
   but WITHOUT ANY WARRANTY; without even the implied warranty of
   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
   GNU General Public License for more details.

   You should have received a copy of the GNU General Public License
   along with this program; if not, write to the Free Software
   Foundation, Inc., 59 Temple Place - Suite 330, Boston, MA
   02111-1307, USA.  */

/* Written by Marc Tardif <intmktg@cam.org>.  */

/* The buffering algorithm in this program can be summarized with the
   following illustration:

   buffer       pos_beg    buf_beg         pos_end   buf_end     lim
     v             v          v               v         v         v
     +------------------------+-----------------------------------+
               bufsalloc                     BUFSIZE
        +----------+----------+       +-------+         +---------+
          previous   previous          current            current
            save     residue            save              residue

   The buffer address points to the beginning of the memory location
   where the input is manipulated.  This segment is separated in two
   sequential fragments.

   The first fragment stores the relevant data from the previous read
   operation.  The size is kept in the bufsalloc variable which can
   increase if more space is required.  The data consists of two parts:
   - A save buffer containing the context preceding the last character
     parsed in the previous input buffer.  This part starts anywhere
     after the buffer address and stops at pos_beg.  This address is
     where parsing will continue following new input.
   - A residue buffer containing the characters which were not parsed
     from the previous input.  This part starts immediately after the
     save buffer at pos_beg and stops at buf_beg.  Coincidentally, the
     latter address is also where new data is read.

   The second fragment is where the read system call copies at most
   BUFSIZE bytes from the input.  The address of the buffer end is
   copied into the lim variable after which a NUL byte is appended as
   a sentinel character.

   Manipulation of the input consists of maintaining two pointers:
   - The buf_end address points to the last character parsed.  When
     the current buffer is exhausted and buf_end is not equal to lim,
     the difference constitutes the residue for the next input.
   - When parsing text, the pos_end address is always the same as
     buf_end.  But when parsing HTML, the formatted text is copied
     over the current buffer and the pointer is stored in pos_end.
     When lim is reached, the save buffer is copied from this point
     backwards.

   Note that pos_end and pos_beg are actually the same variable in the
   source code, one becomes the other after filling the input buffer.
   Same applies to buf_end and buf_beg.
*/

#include <sys/types.h>
#include <locale.h>
#include <ctype.h>
#include <string.h>
#include <fcntl.h>
#include <unistd.h>
#include <stdlib.h>
#include <stdio.h>
#include "config.h"

#include <errno.h>
#ifndef errno
extern int errno;
#endif

#if ! defined HAVE_MEMMOVE && ! defined memmove
# define memmove(d, s, n) bcopy (s, d, n)
#endif

#include "context.h"
#include "err.h"
#include "getopt.h"
#include "options.h"

#define BUFFER 4096
#define SAVE    512

/* Base of buffer.  */
static char *buffer;

/* Allocated size of buffer storage region.  */
static size_t bufsalloc;

/* Command-line options.  */
struct options opt;
static char const short_options[] = "C:D:FHLO:PVbchilnpqs";
static struct option long_options[] =
{
  {"byte-offset", no_argument, NULL, 'b'},
  {"count", no_argument, NULL, 'c'},
  {"context", required_argument, NULL, 'C'},
  {"distance", required_argument, NULL, 'D'},
  {"files-with-matches", no_argument, NULL, 'l'},
  {"files-without-match", no_argument, NULL, 'L'},
  {"fixed-string", no_argument, NULL, 'F'},
  {"help", no_argument, (int *)&opt.show_help, 1},
  {"ignore-case", no_argument, NULL, 'i'},
  {"line-number", no_argument, NULL, 'n'},
  {"no-filename", no_argument, NULL, 'h'},
  {"no-pattern", no_argument, NULL, 'p'},
  {"occurrences", required_argument, NULL, 'O'},
  {"quiet", no_argument, NULL, 'q'},
  {"silent", no_argument, NULL, 'q'},
  {"no-messages", no_argument, NULL, 's'},
  {"version", no_argument, NULL, 'V'},
  {"with-filename", no_argument, NULL, 'H'},
  {"with-pattern", no_argument, NULL, 'P'},
  {0, 0, 0, 0}
};

static int
ck_atoi (char const *str, int *out)
{
  char const *p;

  for (p = str; *p; p++)
    if (*p < '0' || *p > '9')
      return -1;

  *out = atoi (optarg);
  return 0;
}

static void
init (char *str)
{
  buffer = malloc (BUFFER + SAVE + 1);
  if (!buffer)
    err_fatal ("memory exhausted");

  switch (context_init (str))
    {
    case 0:
      err_fatal ("invalid query");
    case 1:
      break;
    default:
      if (!opt.no_pattern)
        opt.with_pattern = 1;
      break;
    }

  bufsalloc = SAVE;
}

static enum extension
get_extension (char *file)
{
  register char *end;
  struct { char *str; int val; } ext[] =
    {
      { "c",    SOURCE },
      { "cpp",  SOURCE },
      { "h",    SOURCE },
      { "pl",   SOURCE },
      { "html", HTML   },
      { "htm",  HTML   },
      { NULL,   TEXT   }
    };

  file += 1;
  for (end = NULL; *file; file++)
    if (*file == '.')
      end = file;

  if (end)
    {
      register char *left, *right;
      int i;

      end += 1;
      for (i=0; ext[i].str; i++)
        {
          left = end;
          right = ext[i].str;
          while (tolower (*left) == *right++)
            if (*left++ == '\0')
              return ext[i].val;
        }
    }

  return TEXT;
}

static int
file_read (int fd)
{
  ssize_t bytesread;
  size_t residue, save;
  char *buf, *lim, *pos;

  residue = 0;
  buf = pos = buffer + bufsalloc;
  *(buf - 1) = '\0';

  for (;;)
    {
      size_t residue_offset, save_offset;

      while ((bytesread = read (fd, buf, BUFFER)) < 0
             && errno == EINTR)
        continue;

      if (bytesread <= 0)
        break;

      lim = buf + bytesread;
      *lim = '\0';
      save = context_find (&pos, &buf, lim);
      residue = lim - buf;

      residue_offset = buf - buffer;
      save_offset = pos - save - buffer;
      if (bufsalloc < residue + save)
        {
          while (bufsalloc < residue + save)
            bufsalloc *= 2;

          buffer = realloc (buffer, bufsalloc + BUFFER + 1);
          if (!buffer)
            err_fatal ("memory exhausted");
        }

      buf = buffer + bufsalloc;
      pos = buf - residue;
      lim = pos - save;
      *(lim - 1) = '\0';
      memmove (lim, buffer + save_offset, save);
      memmove (pos, buffer + residue_offset, residue);
    }

  lim = buf;
  *lim = '\0';
  while (!bytesread && pos < lim)
    {
      buf = lim - 1;
      context_find (&pos, &buf, lim);
    }

  return context_print ();
}

static int
file_open (char *file)
{
  enum extension type;
  int desc, status;

  if (!file)
    {
      desc = 0;
      type = TEXT;
      opt.filename = "(standard input)";
    }
  else
    {
      while ((desc = open (file, O_RDONLY)) < 0 && errno == EINTR)
        continue;

      if (desc < 0)
        {
          err_message ("open");
          return 0;
        }

      type = get_extension (file);
      opt.filename = file;
    }

  context_prep (type);
  status = file_read (desc);

  while (close (desc) != 0)
    if (errno != EINTR)
      {
        err_message ("close");
        break;
      }

  return status;
}

static void
usage (int status)
{
  if (status != 0)
    fprintf (stderr, "\
Usage: bool [options] PATTERN [FILE]...\n\
Try `bool --help' for more information.\n\
");
  else
    printf ("\
Usage: bool [OPTION]... PATTERN [FILE] ...\n\
Search for PATTERN in each FILE or standard input.\n\
\n\
Interpretation:\n\
  -F, --fixed-string        PATTERN is a string, not an expression\n\
  -i, --ignore-case         ignore case distinctions\n\
\n\
Output control:\n\
  -b, --byte-offset         print the byte offset with output lines\n\
  -n, --line-number         print line number with output lines\n\
  -H, --with-filename       print the filename for each match\n\
  -h, --no-filename         suppress the prefixing filename on output\n\
  -P, --with-pattern        print the pattern for each match\n\
  -p, --no-pattern          suppress the prefixing pattern on outptut\n\
  -q, --quiet, --silent     suppress all normal output\n\
  -L, --files-without-match only print FILE names containing no match\n\
  -l, --files-with-matches  only print FILE names containing matches\n\
  -c, --count               only print a count of matches per FILE\n\
\n\
Context control:\n\
  -C, --context=NUM         print NUM characters of output context\n\
  -D, --distance=NUM        NEAR matches are at most NUM words apart\n\
  -O, --occurrences=NUM     print NUM lines of context for each pattern\n\
\n\
Miscellaneous:\n\
  -s, --no-messages         suppress error messages\n\
  -V, --version             print version information and exit\n\
      --help                display this help and exit\n\
");

  exit (status);
}

int
main (int argc, char *argv[])
{
  extern char *optarg;
  extern int optind;
  int c, state;

  state = 0;
  opt.context = DEFAULT_CONTEXT;
  opt.distance = DEFAULT_DISTANCE;
  opt.occurrences = DEFAULT_OCCURRENCES;

  setlocale (LC_CTYPE, "");

  while ((c = getopt_long (argc, argv, short_options, long_options, NULL))
         != -1)
    switch (c)
      {
      case 'b':
        opt.out_byte = 1;
        break;
      case 'C':
        if (optarg)
          {
            if (ck_atoi (optarg, &opt.context))
              err_fatal ("invalid context argument");
          }
        break;
      case 'c':
        opt.out_quiet = 1;
        opt.count_matches = 1;
        break;
      case 'D':
        if (optarg)
          {
            if (ck_atoi (optarg, &opt.distance))
              err_fatal ("invalid distance argument");
          }
        break;
      case 'F':
        opt.fixed_string = 1;
        break;
      case 'H':
        opt.with_filename = 1;
        break;
      case 'h':
        opt.no_filename = 1;
        break;
      case 'i':
        opt.ignore_case = 1;
        break;
      case 'L':
        opt.out_quiet = 1;
        opt.list_files = 1;
        break;
      case 'l':
        opt.out_quiet = 1;
        opt.list_files = 2;
        break;
      case 'n':
        opt.out_line = 1;
        break;
      case 'O':
        if (optarg)
          {
            if (ck_atoi (optarg, &opt.occurrences))
              err_fatal ("invalid occurrence argument");
          }
        break;
      case 'P':
        opt.with_pattern = 1;
        break;
      case 'p':
        opt.no_pattern = 1;
        break;
      case 'q':
        opt.out_quiet = 1;
        break;
      case 's':
        opt.suppress_errors = 1;
        break;
      case 'V':
        printf ("bool (GNU) %s\n", VERSION);
        exit (0);
      case 0:
        /* long options */
        break;
      default:
        usage (1);
        break;
      }

  if (opt.show_help)
    usage (0);

  if (optind == argc)
    usage (1);

  init (argv[optind++]);

  if (argc - optind > 1 && !opt.no_filename)
    opt.with_filename = 1;

  if (optind < argc)
    {
      state = 0;
      do
        {
          char *file = argv[optind];
          state += file_open (strcmp (file, "-") == 0
            ? (char *) NULL
            : file);
        }
      while (++optind < argc);
    }
  else
    state = file_open ((char *) NULL);

  context_free ();
  free (buffer);
  return !state;
}

