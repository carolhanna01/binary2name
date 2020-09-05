/* Miscellaneous utility routines for GNATS.
   Copyright (C) 1993, 1994, 1995, 2001, 2007 Free Software Foundation, Inc.
   Contributed by Tim Wicinski (wicinski@barn.com)
   and Brendan Kehoe (brendan@cygnus.com).

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
#include "gnatsd.h"

/* Copy regular file SOURCE onto file DEST.
   Return 1 if an error occurred, 0 if successful. */

int
copy_file (const char *source, const char *dest)
{
  int ifd;
  int ofd;
  char buf[1024 * 8];
  int len;			/* Number of bytes read into `buf'. */
  
  if (unlink (dest) && errno != ENOENT)
    {
      log_msg (LOG_INFO, 1, "cannot remove ", dest);
      return 1;
    }

  ifd = open (source, O_RDONLY, 0);
  if (ifd < 0)
    {
      log_msg (LOG_INFO, 1, "can not open: ", source);
      return 1;
    }
  ofd = open (dest, O_WRONLY | O_CREAT | O_TRUNC, 0600);
  if (ofd < 0)
    {
      log_msg (LOG_INFO, 1, "can not open: ", dest);
      close (ifd);
      return 1;
    }

  while ((len = read (ifd, buf, sizeof (buf))) > 0)
    {
      int wrote = 0;
      char *bp = buf;
      
      do
	{
	  wrote = write (ofd, bp, len);
	  if (wrote < 0)
	    {
	      log_msg (LOG_INFO, 1, "error in writing:", dest);
	      close (ifd);
	      close (ofd);
	      unlink (dest);
	      return 1;
	    }
	  bp += wrote;
	  len -= wrote;
	} while (len > 0);
    }
  if (len < 0)
    {
      log_msg (LOG_INFO, 1, source, strerror(errno));
      close (ifd);
      close (ofd);
      unlink (dest);
      return 1;
    }

  if (close (ifd) < 0)
    {
      log_msg (LOG_INFO, 1, source, strerror(errno));
      close (ofd);
      return 1;
    }
  if (close (ofd) < 0)
    {
      log_msg (LOG_INFO, 1, dest, strerror(errno));
      return 1;
    }

  /* FIXME: This is fine for GNATS, but if there's anything we ever want
     to have special permissions, we must add the code to restore the
     permissions to what they were.  */
  chmod (dest, 0644);
  
  return 0;
}

/* Try to lock the file noted by FP.  */
int
is_gnats_locked (const DatabaseInfo database)
{
  char *path = gnats_adm_dir (database, "gnats.lock");
  int res = fileExists (path);
  free (path);
  return res;
}

int
lock_gnats (const DatabaseInfo database, ErrorDesc *err)
{
  char *path;
  int count, res;
  const int MAXWAIT = 10;
  const int GRANULARITY = 1;

  path = gnats_adm_dir (database, "gnats.lock");


  /* try repeatedly to get the lock */
  for (count = 0; count < MAXWAIT / GRANULARITY; count++)
    {
      errno = 0;
      /* use atomic create, to avoid races */
      if (open (path, O_CREAT | O_TRUNC | O_WRONLY | O_EXCL, 0) != -1)
	{
	  /* success */
	  break;
	}
      else
	{
	  if (errno != EEXIST)
	    {
	      /* something went wrong, error out */
	      break;
	    }
	}
      /* somebody else has the lock, sleep and try again */
      sleep (GRANULARITY);
    }

  if (!errno)
    {
      /* success */
      res = 0;
    }
  else if (errno == EEXIST)
    {
      setError (err, CODE_GNATS_LOCKED,
		"GNATS lock file exists %s", path);
      res = -1;
    }
  else
    {
      setError (err, CODE_FILE_ERROR,
		"GNATS cannot create lock file: %s",
		strerror (errno));
      log_msg(LOG_ERR, 1, "error in lock_gnats:",
	      getErrorMessage (*err));
      res = 1;
    }

  free (path);
  return res;
}

int
unlock_gnats (const DatabaseInfo database, ErrorDesc *err)
{
  char *path = gnats_adm_dir (database, "gnats.lock");

  if (path == NULL)
    {
      setError (err, CODE_FILE_ERROR,
		"Can't get adm directory for database");
      return 1;
    }

  if (! fileExists (path))
    {
      setError (err, CODE_GNATS_NOT_LOCKED,
		"GNATS database is already unlocked.");
      free (path);
      return 1;
    }

  if (unlink (path) != 0)
    {
      setError (err, CODE_FILE_ERROR,
		"Unable to unlock database: %s", strerror(errno));
      free (path);
      return -1;
    }

  free (path);
  return 0;
}

int
fileExists (const char *filename)
{
  struct stat s;

  if (stat (filename, &s) == 0)
    {
      return 1;
    }
  else
    {
      return 0;
    }
}


/* VARARGS */
void 
punt (const DatabaseInfo database, int die, ...)
{
  va_list args;
  char *fmt;
  FILE *fp;
  char hostname[MAXHOSTNAMELEN];

  VA_START (args, die);

  fmt = va_arg (args, char *);

  if (database != NULL)
    {
      fp = open_mail_file (database);
      if (fp == NULL)
	{
	  char *lastDitchTry;

	  asprintf (&lastDitchTry, "mail %s", gnatsAdminMailAddr (database));
	  fp = popen (lastDitchTry, "w");
	  free (lastDitchTry);
	}

      if (fp == NULL)
	{
	  fprintf (stderr, "%s: cannot open mail file for errors\n",
		   program_name);
	  client_unlock_gnats ();
	  exit (3);
	}

      if (gethostname (hostname, MAXHOSTNAMELEN) != 0)
	{
	  strcpy (hostname, "(hostname unavailable)");
	}

      fprintf (fp, "From: %s (GNATS Management)\n",
	       gnatsAdminMailAddr (database));
      fprintf (fp, "To: %s\n", gnatsAdminMailAddr (database));
      fprintf (fp, "Subject: problem with %s on host %s\n", program_name,
	       hostname);
      fprintf (fp, "\n\n");	/* Two newlines to massage buggy sendmails */

      vfprintf (fp, fmt, args);

      close_mail_file (fp);
    }
  else
  {
    /* didn't get a valid database, complain to stderr */
      fprintf (stderr, "Problem with %s\n", program_name);
      vfprintf (stderr, fmt, args);
      fprintf (stderr, "\n");
  }
  va_end (args);

  if (die)
    {
      client_unlock_gnats ();
      exit (2);
    }
}

static void
unlock_quit (int sig ATTRIBUTE_UNUSED)
{
  client_unlock_gnats ();
  unblock_signals ();
  exit (1);
}

void
block_signals (void)
{
  signal (SIGSEGV, unlock_quit);
  signal (SIGHUP, unlock_quit);
  signal (SIGTERM, unlock_quit);
  signal (SIGINT, unlock_quit);
  signal (SIGQUIT, unlock_quit);
#ifdef SIGIOT
  signal (SIGIOT, unlock_quit);
#endif
#ifdef SIGILL
  signal (SIGILL, unlock_quit);
#endif
#ifdef SIGABRT
  signal (SIGABRT, unlock_quit);
#endif
}

/* Gaaaah. Bastards.  */
#if defined (__sun__) && defined (__svr4__)
#undef SIG_DFL
#define SIG_DFL (void(*)(int))0
#endif

void
unblock_signals (void)
{
  signal (SIGSEGV, SIG_DFL);
  signal (SIGHUP, SIG_DFL);
  signal (SIGTERM, SIG_DFL);
  signal (SIGINT, SIG_DFL);
  signal (SIGQUIT, SIG_DFL);
#ifdef SIGIOT
  signal (SIGIOT, SIG_DFL);
#endif
#ifdef SIGILL
  signal (SIGILL, SIG_DFL);
#endif
#ifdef SIGABRT
  signal (SIGABRT, SIG_DFL);
#endif
}

const char*
get_prid_from_path (const char *path)
{
  if (strchr (path, '/') == NULL)
    {
      return (path);
    }
  return (strrchr (path, '/') + 1);
}

const char*
get_cat_prid_from_path (const char *path)
{
  int   i = 0;
  const char *p = path + strlen (path);
  if (strchr (path, '/') == NULL)
    return (path);
  while (i < 2 && p > path)
  {
    p--;
    if (*p == '/')
      i++;
  }
  if (i == 2)
    {
      return (p + 1);
    }
  else
    {
      return path;
    }
}

char*
get_lock_path (const DatabaseInfo database, const char *prID)
{
  char *path = NULL;

  if (prID != NULL)
    {
      char *tempname;

      asprintf (&tempname, "locks/%s.lock", prID);
      path = gnats_adm_dir (database, tempname);
      free (tempname);
    }
  return path;
}

int
isPrLocked (const DatabaseInfo database, const char *prNum)
{
  char *path = get_lock_path (database, prNum);
  int res = fileExists (path);

  free (path);
  return res;
}

char *
get_curr_date (void)
{
  time_t t = time (NULL);
  char *date = (char *) xmalloc (GNATS_TIME_LENGTH);
  gnats_strftime (date, GNATS_TIME_LENGTH, "%a, %d %b %Y %H:%M:%S %z",
		  localtime (&t));
  return date;
}

int
gnatsdbHasNetconn (const char *database_name)
{
  const char *var = getenv ("GNATSDB");
  if (database_name != NULL && database_name[0] != '\0')
    {
      return databaseSpecIsNetConn (database_name);
    }
  else if (var != NULL && var[0] != '\0')
    {
      if (strchr (var, ':') != NULL)
	{
	  return 1;
	}
      else
	{
	  return databaseSpecIsNetConn (var);
	}
    }
  else
    {
      return databaseSpecIsNetConn ("default");
    }
}

/* check_state_type - return 1 if state is of type type, otherwise 0 */
int
check_state_type (const DatabaseInfo database, 
		  const char *state, const char *type)
{
  AdmEntry *state_chain = fieldDefForIndex (STATE (database))->adm_contents;
  AdmEntry *ent = find_chain_entry_nocopy (state_chain, state);

  if (ent != NULL)
    {
      return (strcmp (ent->admFields[StateAdmType], type) == 0);
    }
  return 0;
}     

struct error_desc 
{
  int errorCode;
  BadFields badFieldList;
  char *errorMessage;
};

struct bad_fields
{
  FieldIndex field;
  char *value;
  struct bad_fields *next;
};

void
setError (ErrorDesc *errorDesc, int errorCode, ...)
{
  va_list args;
  char *format;

  VA_START (args, errorCode);

  *errorDesc = (ErrorDesc) xmalloc (sizeof (struct error_desc));

  (*errorDesc)->errorCode = errorCode;

  switch (errorCode)
    {
    case CODE_INVALID_FIELD_CONTENTS:
    case CODE_INVALID_ENUM:
    case CODE_READONLY_FIELD:
      (*errorDesc)->badFieldList = va_arg (args, BadFields);
      break;
    default:
      (*errorDesc)->badFieldList = NULL;
      break;
    }
  format = va_arg (args, char *);
  if (format != NULL)
    {
      vasprintf (&(*errorDesc)->errorMessage, format, args);
    }
  else
    {
      (*errorDesc)->errorMessage = NULL;
    }
  va_end (args);
}

BadFields
newBadFieldEntry (FieldIndex field, const char *badValue, BadFields next)
{
  BadFields res = (BadFields) xmalloc (sizeof (struct bad_fields));

  res->field = field;
  if (badValue != NULL)
    {
      res->value = xstrdup (badValue);
    }
  else
    {
      res->value = NULL;
    }
  res->next = next;
  return res;
}

void
freeBadFieldList (BadFields list)
{
  while (list != NULL)
    {
      BadFields next = list->next;
      free (list->value);
      free (list);
      list = next;
    }
}

void
freeErrorDesc (ErrorDesc desc)
{
  if (desc != NULL)
    {
      freeBadFieldList (desc->badFieldList);
      if (desc->errorMessage != NULL)
	{
	  free (desc->errorMessage);
	}
      free (desc);
    }
}

const char *
getErrorMessage (ErrorDesc desc)
{
  return desc->errorMessage;
}

int
getErrorCode (ErrorDesc desc)
{
  return desc->errorCode;
}

BadFields
getBadFieldList (ErrorDesc errorDesc)
{
  return errorDesc->badFieldList;
}

FieldIndex
badFieldIndex (BadFields ent)
{
  return ent->field;
}

const char *
badFieldValue (BadFields ent)
{
  return ent->value;
}

BadFields
nextBadField (BadFields ent)
{
  return ent->next;
}
