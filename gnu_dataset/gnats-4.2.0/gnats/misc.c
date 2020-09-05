/* Miscellaneous utility routines for GNATS.
   Copyright (C) 2000, 2001, 2007 Milan Zamazal
   Copyright (C) 1993, 1994, 1995 Free Software Foundation, Inc.
   Contributed by Tim Wicinski (wicinski@barn.com)
   and Brendan Kehoe (brendan@cygnus.com).
   Further hacked by Milan Zamazal (pdm@zamazal.org).

This file is part of GNU GNATS.

GNU GNATS is free software; you can redistribute it and/or modify
it under the terms of the GNU General Public License as published by
the Free Software Foundation; either version 3, or (at your option)
any later version.

GNU GNATS is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
GNU General Public License for more details.

You should have received a copy of the GNU General Public License
along with GNU GNATS; see the file COPYING. If not, see
<http://www.gnu.org/licenses/>.
*/

#include "gnats.h"


/* Exit constants.  All GNATS C sources should use only the symbolic names
   defined here as exit codes. */
const int EXIT_OK = 0;
const int EXIT_PROGRAM_ERROR = 127;


void program_error (const char *file, int line);



/* Logging */

/* TODO: The current logging facilities are quite primitive.  They should be
   improved in the future.  However the primary goal now is to unify logging in
   all the GNATS components.  For that purpose the current logging facility is
   sufficient. */

/* Debugging level. */
static int debug_level = LOG_ERR; 

/* File to log all messages to. */
static FILE *gnats_logfile = NULL;

/* Current logging method.  */
static Logging_Methods log_method = NOLOG;

/* Set logging level to LOG_INFO.
   Currently we support only two levels of debugging, thus it makes sense. */
void
enable_debugging (void)
{
  debug_level = LOG_INFO;
}

/* Log NUM_ARG messages given in `...' with logging SEVERITY.
   Actually at most two messages in `...' are supported. */
void 
#if defined(__STDC__) || defined(_AIX)
log_msg (int severity, int num_arg, ...)
#else
log_msg (int severity, int num_arg, va_dcl va_alist)
#endif
{
  va_list args;

  VA_START (args, num_arg);

  if (debug_level >= severity)
    {
      char *message;
      char *buf;
      
      message = va_arg (args, char *);
      /* The following is incredibly stupid and broken, but I don't know how to
	 do it better in C. */
      if (num_arg > 0)
	{
	  char *message2 = va_arg (args, char *);
	  asprintf (&buf, "%s: %s %s\n", program_name, message, message2);
	}
      else
	{
	  asprintf (&buf, "%s: %s\n", program_name, message);
	}

      switch (log_method)
	{
#ifdef HAVE_SYSLOG_H
	case SYSLOG:
	  syslog (severity, "%s", buf);
	  break;
#endif
	case MAIL:
	  /* This is currently not used, because it could send a lot of mails
	     to someone. */
	  program_error (__FILE__, __LINE__);
	  break;
	case LOGFILE:
	  if (gnats_logfile != (FILE *)NULL)
	    {
	      fprintf (gnats_logfile, "%s", buf);
	      break;
	    }
	  /* No log file, log to stderr. */
	case STDERR:
	  fprintf (stderr, "%s", buf);
	  break;
	case NOLOG:
	  /* Do nothing. */
	  break;
	default:
	  program_error (__FILE__, __LINE__);
	}

      free (buf);
    }

  va_end (args);
}

/* Set log_method appropriately.
   `logfile' is the file to log to.  If it is NULL, we use another logging
   destination by guessing, considering various criteria.
   TODO: This method is very rudimentary and should be improved. */
void
init_logging (const char *logfile)
{
  if (logfile)
    {
      /* A log file explicitly requested.  Let's try to open it. */
      gnats_logfile = fopen (logfile, "at");
      if (gnats_logfile != NULL)
	{
	  log_method = LOGFILE;
	  return;
	}
    }
  
  if (isatty (fileno (stderr)))
    /* If the program is invoked from a terminal, it is good to present the
       messages directly to the user. */
    log_method = STDERR;
  else
    {
      /* Maybe this is a daemon. */
#ifdef HAVE_SYSLOG_H
      closelog ();
#ifdef LOG_DAEMON
      openlog ("gnatsd", (LOG_PID|LOG_CONS), LOG_DAEMON);
#else
      openlog ("gnatsd", LOG_PID);
#endif
      log_method = SYSLOG;
#else
      /* This should be MAIL, but I don't know how to use it well. */
      log_method = NOLOG;
#endif
    }

  if (logfile)
    /* Log file requested, but couldn't be open.  Now we can report this
       fact. */
    log_msg (LOG_ERR, 1, "Log file couldn't be open:", logfile);
}

/* Open the file (actually the pipe) to which the mail message will
   be written.  */
FILE *
open_mail_file (const DatabaseInfo database)
{
  FILE *fp = NULL;
  char *mailAgentPath = mailAgent (database);

  if (mailAgentPath != NULL)
    {
      /* Can't use log_msg here, since gnats_logfile is being set by this first
     thing.  */

      fp = popen (mailAgentPath, "w");
      free (mailAgentPath);

      if (debugMode (database))
	{
	  fprintf (fp, "From: %s (GNATS Management)\n", 
		   gnatsAdminMailAddr (database));
	  fprintf (fp, "To: %s\n", gnatsAdminMailAddr (database));
	  fprintf (fp, "Subject: mail output from %s\n", program_name);
	  fprintf (fp, "\n\n");
	}
    }

  return fp;
}

void
close_mail_file (fp)
     FILE *fp;
{

  if (fp)
    {
      fflush (fp);
      pclose (fp);
    }
}


/* Report a program error at `line' in `file' and exit with an error code. */
void
program_error (const char *filename, int line)
{
  char *message;
  asprintf (&message, "Program error at %s:%d", filename, line);
  log_msg (LOG_ERR, 0, message);
  free (message);
  exit (EXIT_PROGRAM_ERROR);
}

/* Initialize the system and load the database DATABASE_NAME.
   Return the basic database access structure.
   PROGRAM_NAME is the name of the calling program.
   If any error occurs, NULL is returned and `err' is set appropriately. */
DatabaseInfo
init_gnats (const char *program_name, const char *database_name,
	    ErrorDesc *err)
{
  init_logging (NULL);
  re_set_syntax ((RE_SYNTAX_POSIX_EXTENDED | RE_BK_PLUS_QM) & ~RE_DOT_NEWLINE);
  if (initDatabaseList (err) != 0)
    {
      return NULL;
    }
  return findOrLoadDatabase (program_name, database_name, err);
}

StringList *
new_string_list_ent (char *name, StringList *next)
{
  StringList *res = (StringList *) xmalloc (sizeof (StringList));
  res->name = name;
  res->next = next;
  return res;
}

/* Scan down LINE, returning the next token.  We can't use strtok, since
   we often deal with constant strings like "foo | bar" for the default
   values for a PR.

   Returns the returned token as a malloc()ed string, and changes
   *LINE to point to the next place to begin parsing for the next
   token, or sets it to NULL if the token being returned is the last
   one. */
char *
get_next_field (const char **line_ptr, int delim)
{
  const char *line = *line_ptr;
  const char *end_line = line;
  const char *next_token;
  char *res;

  while (*end_line != delim && *end_line != '\0')
    {
      end_line++;
    }

  next_token = end_line;

  /* skip whitespace at the end of the token */
  while (end_line > line && isspace ((int)(unsigned char) end_line[-1]))
    {
      end_line--;
    }

  res = xstrndup (line, end_line - line);
  if (*next_token == delim)
    {
      *line_ptr = next_token + 1;
    }
  else
    {
      *line_ptr = NULL;
    }

  return res;
}

/* Adds quote-marks (") around the string, and escapes any quotes that
   appear within the string with '\'. */
char *
quote_string (const char *string)
{
  const char *p = string;
  char *rp;
  char *res;
  int len = strlen (string) + 2;

  for (p = string; *p != '\0'; p++)
    {
      if (*p == '"')
	{
	  len++;
	}
    }

  res = xmalloc (len + 1);
  rp = res + 1;

  res[0] = '"';
  res[len - 1] = '"';
  res[len] = '\0';

  for (p = string; *p != '\0'; p++)
    {
      if (*p == '"')
	{
	  *(rp++) = '\\';
	}
      *(rp++) = *p;
    }
  return res;
}

#define MYMIN(a,b) ((a) < (b) ? (a) : (b))

/* Read a newline-terminated line of text from INPUT.  If MAXLENARG is
   non-NULL and has a value greater than zero, a maximum of *MAXLENARG
   characters will be read.  (Since the string is terminated with a
   '\0', *MAXLENARG + 1 chars will be allocated.)

   The returned line is allocated with malloc (), and is the property
   of the caller.  If MAXLENARG is non-NULL, the number of characters
   read is stored there.

   A NULL value is returned if no text was read.  */
char *
read_line (FILE *input, size_t *max_len_arg)
{
  size_t len = 0;
  size_t max_len = (max_len_arg != NULL ? *max_len_arg : 0);
  const size_t default_len = 512;
  char *res = xmalloc (max_len>0 ? max_len+1 : default_len);

  while (fgets (res+len, (max_len>0 ? max_len : default_len), input)
	 != NULL)
    {
      size_t slen = strlen (res + len);
      len += slen;
      if (res[len - 1] == '\n')
	break;
      
      if (max_len > 0)
	{
	  max_len -= slen;
	  if (max_len == 0)
	    break;
	}
      else
	{
	  res = xrealloc (res, len + default_len);
	}
    }
  
  if (len == 0)
    {
      free (res);
      res = NULL;
    }
  else if (max_len_arg == NULL || *max_len_arg == 0)
    {
      res = xrealloc (res, len + 1);
    }

  if (max_len_arg != NULL)
    {
      *max_len_arg = len;
    }
  return res;
}

/*

@deftypefn Supplemental char * xstrndup(const char *@var{str}, size_t @var{len})   

Convenience function to copy @var{len} bytes of @var{str} to a newly allocated
string.  Returns char pointer to new string, or returns NULL if @var{len} < 1
or @var{str} == NULL.

@end deftypefn

*/
char *
xstrndup (const char *str, size_t len)
{
  if (str == NULL)
    {
      return NULL;
    }
  else
    {
      char *res;

      if (len < 1) {
	len = 0;
      }
      res = xmalloc (len + 1);
      memcpy (res, str, len);
      res[len] = '\0';
      return res;
    }
}

/* Allocate a memory with internal error handling. */
PTR
xmalloc(size_t size)
{
  void *ptr;

  if (size == (size_t)NULL)
    size = 1;
  if ((ptr = malloc(size)) == NULL) {
    (void)fprintf(stderr, "xmalloc: unable to allocate memory");
    program_error(__FILE__, __LINE__);
  }
  return (ptr);
}

/* Safely change the size of previously allocated memory area. */
PTR
xrealloc(PTR ptr, size_t size)
{
  PTR ptr2;

  if (size == (size_t)NULL)
    size = 1;
  if ((ptr2 = realloc(ptr, size)) == NULL) {
    if (ptr != (PTR)NULL)
      free(ptr);
    (void)fprintf(stderr, "xrealloc: unable to relocate memory");
    program_error(__FILE__, __LINE__);
  }
  return(ptr2);
}

/* Save a copy of a string with internal error handling. */
char *
xstrdup(const char *str)
{
  char *strc;

  if ((strc = strdup(str)) == NULL) {
    (void)fprintf(stderr, "xstrdup: unable to duplicate a string");
    program_error(__FILE__, __LINE__);
  }
  return(strc);
}

#ifndef HAVE_MKDIR
/* mkdir adapted from GNU tar.  */

/* Make directory DPATH, with permission mode DMODE.

   Written by Robert Rother, Mariah Corporation, August 1985
   (sdcsvax!rmr or rmr@@uscd).  If you want it, it's yours.

   Severely hacked over by John Gilmore to make a 4.2BSD compatible
   subroutine.	11Mar86; hoptoad!gnu

   Modified by rmtodd@@uokmax 6-28-87 -- when making an already existing dir,
   subroutine didn't return EEXIST.  It does now.  */

int
mkdir (dpath, dmode)
     char *dpath;
     int dmode;
{
  int cpid, status;
  struct stat statbuf;

  if (stat (dpath, &statbuf) == 0)
    {
      errno = EEXIST;		/* stat worked, so it already exists.  */
      return -1;
    }

  /* If stat fails for a reason other than non-existence, return error.  */
  if (errno != ENOENT)
    return -1;

  cpid = fork ();
  switch (cpid)
    {
    case -1:			/* Cannot fork.  */
      return -1;		/* errno is set already.  */

    case 0:			/* Child process.  */
      /* Cheap hack to set mode of new directory.  Since this child
	 process is going away anyway, we zap its umask.
	 This won't suffice to set SUID, SGID, etc. on this
	 directory, so the parent process calls chmod afterward.  */
      status = umask (0);	/* Get current umask.  */
      umask (status | (0777 & ~dmode));	/* Set for mkdir.  */
      execl ("/bin/mkdir", "mkdir", dpath, (char *) 0);
      _exit (1);

    default:			/* Parent process.  */
      while (wait (&status) != cpid) /* Wait for kid to finish.  */
	/* Do nothing.  */ ;

      if (status & 0xFFFF)
	{
	  errno = EIO;		/* /bin/mkdir failed.  */
	  return -1;
	}
      return chmod (dpath, dmode);
    }
}
#endif /* HAVE_MKDIR */

/* Return open file descriptor of the file specified by `template' suitable to
   `mktemp'.  The file is open for reading and writing.
   If the stream can't be opened, a negative value is returned. */
int open_temporary_file (char *template, int mode)
{
  int fd = -1;
#ifndef HAVE_MKSTEMP
  const int NUMBER_OF_TRIALS = 3;
#endif
  
#ifdef HAVE_MKSTEMP
  fd = mkstemp (template);
  chmod (template, mode);
#else
  {
    int i;
    for (i = 0; i < NUMBER_OF_TRIALS && fd < 0; i++)
      {   
#ifdef HAVE_MKTEMP
	mktemp (template);
#else
	mkstemps (template, 0);
#endif
	fd = open (template, O_RDWR | O_CREAT | O_EXCL, mode);
      }
  }
#endif
  return fd;
}

/* Return the name of the directory to use for general temporary files. */
const char *
temporary_directory (void)
{
#ifdef P_tmpdir
  return P_tmpdir;
#else
  char *tmpdir = getenv ("TMPDIR");
  if (tmpdir == NULL)
    {
      tmpdir = "/tmp";
    }
  return tmpdir;
#endif
}

/* Return open temporary file specified by `template' suitable to `mktemp'.
   `fopen_mode' is the mode string to be given to fopen.
   If the file can't be opened, return NULL. */
FILE *
fopen_temporary_file (char *template, const char *fopen_mode, const int mode)
{
  int fd = open_temporary_file (template, mode);
  return (fd < 0 ? NULL : fdopen (fd, fopen_mode));
}

/* Auxiliary for gnats_strftime. */
static int
minutes_gmt_offset (const struct tm *local_time_pointer)
{
  const int MINUTES_PER_DAY = 24*60;
  time_t unix_time;
  struct tm local;
  struct tm gmt;
  int offset;

  /* Make local copies of the return values */
  local = *local_time_pointer;
  unix_time = mktime (&local);
  gmt = *gmtime (&unix_time);

  /* mktime() not portably reliable; calculate minutes offset ourselves */
  offset = ((local.tm_hour - gmt.tm_hour) * 60 +
	    (local.tm_min  - gmt.tm_min));

  /* Adjust backwards/forwards if the day is different */
  if      (local.tm_year < gmt.tm_year) offset -= MINUTES_PER_DAY;
  else if (local.tm_year > gmt.tm_year) offset += MINUTES_PER_DAY;
  else if (local.tm_yday < gmt.tm_yday) offset -= MINUTES_PER_DAY;
  else if (local.tm_yday > gmt.tm_yday) offset += MINUTES_PER_DAY;

  return offset;
}

/* The same as `strftime' except it handles the case when `%z' is unsupported
   by libc. */
size_t
gnats_strftime (char *s, size_t size, const char *template,
		const struct tm *brokentime)
{
  static short have_strftime_with_z = -1;  
  if (have_strftime_with_z < 0)
    {
      char buf[16];
      strftime (buf, 16, "%z", brokentime);
      /* jonm@alchemetrics.co.uk - added check for +/- at the start
      ** of the string to support SCO OpenServer.  The undocumented
      ** %z does not have a '+' on for positive offsets, so the 
      ** return from get_curr_date() cannot be parsed by get_date().
      */
      have_strftime_with_z = ((int)buf[0] == '+' || (int)buf[0] == '-') &&
	      isdigit ((int)(buf[1]));
    }
  
  if (have_strftime_with_z)
    return strftime (s, size, template, brokentime);
  else
    {
      int padding = 0;
      const char *in = template;
      char *fixed_template = 0;
      char *out = 0;
      /* Because brokentime points to static data (allocated
       * by localtime()), it cannot be passed to a subroutine
       * and then later be relied on to point to the same data. */
      struct tm btime = *brokentime;
      int result;

      /* Count number of %z so we know how much characters to add to the
       * template.  We actually count the number of additional characters.
       * As we are going to replace each "%z" by "+hhmm" (sign, hours,
       * minutes), this is 3 extra chars for each %z. */
      while (*in)
	{
	  if (*in == '%' && *(in+1) == 'z')
	    {
	      in += 2;
	      padding += 3;  /* 3 extra chars for each %z */
	    }
	  else
	    {
	      in++;
	    }
	}

      /* Now allocate enough space. */
      fixed_template = (char*)xmalloc (strlen(template)+padding+1);
      in = template;  /* Inspect it again, this time replacing all %z. */
      out = fixed_template;
     
      while (*in != '\0')
	{
	  char c = *in++;
	  if (c != '%')
	    {
	      *out++ = c;
	    }
	  else if (*in != 'z')
	    {
	      *out++ = c;  /* the '%' */
	      *out++ = *in++;
	    }
	  else
	    {
	      int offset = minutes_gmt_offset (brokentime);
	      char offset_buf[6];
	      char sign = '+';
	      unsigned int i, hours, minutes;

	      if (offset < 0)
		{
		  sign = '-';
		  offset = -offset;
		}
	      hours = offset / 60;
	      minutes = offset % 60;
	      sprintf (offset_buf, "%c%02d%02d", sign, hours, minutes);
	      for (i = 0; i < strlen (offset_buf); i++)
		*out++ = offset_buf[i];
	      in++; /* skip over 'z' */
	    }
	}
      *out = '\0';
    
      result = strftime (s, size, fixed_template, &btime);
      free (fixed_template);
      return result;
    }
}

/* Print usage information and exit with EXIT_CODE.
   `texts' contains the output strings to be concatenated and printed; its last
   element must be NULL.
   (This is not a single string, because ISO C guarantees string length only to
   about 509 bytes.) */
void
usage (const char *const texts[], int exit_code)
{
  FILE *output = (exit_code ? stderr : stdout);
  const char *const *t;
  for (t = texts; *t != NULL; t++)
    fprintf (output, "%s", *t);
  exit (exit_code);
}

/* Output the version information for PROGRAM_NAME and exit. */
void
version (const char *const program_name)
{
  printf ("%s %s\n", program_name, version_string);
  exit (EXIT_OK);
}

/* Return true iff STRING is either NULL or doesn't contain any non-whitespace
   character. */
bool
value_is_empty (const char *string)
{
  if (string == NULL)
    return TRUE;
  {
    unsigned int i;
    for (i = 0; i < strlen (string); i++)
      if (! isspace ((int)(unsigned char)(string[i])))
	return FALSE;
    return TRUE;
  }
}

/* The following functions are stolen from libiberty library (-liberty) */

#ifndef HAVE_ASPRINTF
/*
@deftypefn Extension int asprintf (char **@var{resptr}, const char *@var{format}, ...)

Like @code{sprintf}, but instead of passing a pointer to a buffer, you
pass a pointer to a pointer.  This function will compute the size of
the buffer needed, allocate memory with @code{malloc}, and store a
pointer to the allocated memory in @code{*@var{resptr}}.  The value
returned is the same as @code{sprintf} would return.  If memory could
not be allocated, minus one is returned and @code{NULL} is stored in
@code{*@var{resptr}}.

@end deftypefn
*/

int
asprintf VPARAMS ((char **buf, const char *fmt, ...))
{
  int status;
  VA_OPEN (ap, fmt);
  VA_FIXEDARG (ap, char **, buf);
  VA_FIXEDARG (ap, const char *, fmt);
  status = vasprintf (buf, fmt, ap);
  VA_CLOSE (ap);
  return status;
}
#endif /* HAVE_ASPRINTF */

#ifndef HAVE_VASPRINTF
/*
@deftypefn Extension int vasprintf (char **@var{resptr}, const char *@var{format}, va_list @var{args})

Like @code{vsprintf}, but instead of passing a pointer to a buffer,
you pass a pointer to a pointer.  This function will compute the size
of the buffer needed, allocate memory with @code{malloc}, and store a
pointer to the allocated memory in @code{*@var{resptr}}.  The value
returned is the same as @code{vsprintf} would return.  If memory could
not be allocated, minus one is returned and @code{NULL} is stored in
@code{*@var{resptr}}.

@end deftypefn
*/

/* This is a vasprintf helper */
static int int_vasprintf PARAMS ((char **, const char *, va_list));

static int
int_vasprintf (result, format, args)
     char **result;
     const char *format;
     va_list args;
{
  const char *p = format;
  /* Add one to make sure that it is never zero, which might cause malloc
     to return NULL.  */
  int total_width = strlen (format) + 1;
  va_list ap;

#ifdef va_copy
  va_copy (ap, args);
#else
  memcpy ((PTR) &ap, (PTR) &args, sizeof (va_list));
#endif

  while (*p != '\0')
    {
      if (*p++ == '%')
	{
	  while (strchr ("-+ #0", *p))
	    ++p;
	  if (*p == '*')
	    {
	      ++p;
	      total_width += abs (va_arg (ap, int));
	    }
	  else
	    total_width += strtoul (p, (char **) &p, 10);
	  if (*p == '.')
	    {
	      ++p;
	      if (*p == '*')
		{
		  ++p;
		  total_width += abs (va_arg (ap, int));
		}
	      else
	      total_width += strtoul (p, (char **) &p, 10);
	    }
	  while (strchr ("hlL", *p))
	    ++p;
	  /* Should be big enough for any format specifier except %s and floats.  */
	  total_width += 30;
	  switch (*p)
	    {
	    case 'd':
	    case 'i':
	    case 'o':
	    case 'u':
	    case 'x':
	    case 'X':
	    case 'c':
	      (void) va_arg (ap, int);
	      break;
	    case 'f':
	    case 'e':
	    case 'E':
	    case 'g':
	    case 'G':
	      (void) va_arg (ap, double);
	      /* Since an ieee double can have an exponent of 307, we'll
		 make the buffer wide enough to cover the gross case. */
	      total_width += 307;
	      break;
	    case 's':
	      total_width += strlen (va_arg (ap, char *));
	      break;
	    case 'p':
	    case 'n':
	      (void) va_arg (ap, char *);
	      break;
	    }
	  p++;
	}
    }
#ifdef va_copy
  va_end (ap);
#endif
  *result = (char *) malloc (total_width);
  if (*result != NULL)
    return vsprintf (*result, format, args);
  else
    return -1;
}

int
vasprintf (result, format, args)
     char **result;
     const char *format;
#if defined (_BSD_VA_LIST_) && defined (__FreeBSD__)
     _BSD_VA_LIST_ args;
#else
     va_list args;
#endif
{
  return int_vasprintf (result, format, args);
}
#endif /* HAVE_VASPRINTF */

#ifndef HAVE_BASENAME
/*
@deftypefn Supplemental char* basename (const char *@var{name})

Returns a pointer to the last component of pathname @var{name}.
Behavior is undefined if the pathname ends in a directory separator.

@end deftypefn
*/

#ifndef DIR_SEPARATOR
#define DIR_SEPARATOR '/'
#endif

#if defined (_WIN32) || defined (__MSDOS__) || defined (__DJGPP__) || \
  defined (__OS2__)
#define HAVE_DOS_BASED_FILE_SYSTEM
#ifndef DIR_SEPARATOR_2 
#define DIR_SEPARATOR_2 '\\'
#endif
#endif

/* Define IS_DIR_SEPARATOR.  */
#ifndef DIR_SEPARATOR_2
# define IS_DIR_SEPARATOR(ch) ((ch) == DIR_SEPARATOR)
#else /* DIR_SEPARATOR_2 */
# define IS_DIR_SEPARATOR(ch) \
	(((ch) == DIR_SEPARATOR) || ((ch) == DIR_SEPARATOR_2))
#endif /* DIR_SEPARATOR_2 */

/* Note: to be POSIX-2 compliant "name" have a "char *" type. */
char *
basename (name)
     char *name;
{
  const char *base;

#if defined (HAVE_DOS_BASED_FILE_SYSTEM)
  /* Skip over the disk name in MSDOS pathnames. */
  if (ISALPHA (name[0]) && name[1] == ':') 
    name += 2;
#endif

  for (base = name; *name; name++)
    {
      if (IS_DIR_SEPARATOR (*name))
	{
	  base = name + 1;
	}
    }
  return (char *) base;
}
#endif /* HAVE_BASENAME */

#ifndef HAVE_UNSETENV
#define __environ	environ
extern char **environ;

void
unsetenv (name)
     const char *name;
{
  const size_t len = strlen (name);
  char **ep;

  for (ep = __environ; *ep; ++ep)
    if (!strncmp (*ep, name, len) && (*ep)[len] == '=')
      {
	/* Found it.  Remove this pointer by moving later ones back.  */
	char **dp = ep;
	do
	  dp[0] = dp[1];
	while (*dp++);
	/* Continue the loop in case NAME appears again.  */
      }

}
#endif /* !HAVE_UNSETENV */
