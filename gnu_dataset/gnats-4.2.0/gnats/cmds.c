/* Commands for the GNATS server.
   Copyright (C) 1994, 95, 95, 1997, 2001, 2002, 2007 Free Software Foundation, Inc.
   Contributed by Brendan Kehoe (brendan@cygnus.com).
   Further hacked by Milan Zamazal (pdm@zamazal.org) and other contributors.

This file is part of GNU GNATS.

GNU GNATS is free software; you can redistribute it and/or modify
it under the terms of the GNU General Public License as published by
the Free Software Foundation; either version 3, or (at your option)
any later version.

GNU GNATS is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
GNU General Public License for more detailmails.

You should have received a copy of the GNU General Public License
along with GNU GNATS; see the file COPYING.  If not,  see
<http://www.gnu.org/licenses/>.
*/

#include "gnats.h"
#include "gnatsd.h"
#include "query.h"
#include "mail.h"
#include "ds.h"

#ifdef HAVE_KERBEROS
#include <des.h>
#include <krb.h>
#ifndef HAVE_KRB_GET_ERR_TEXT
#define krb_get_err_text(status) krb_err_txt[status]
#endif
#endif

extern int require_db;
static QueryExpr query = NULL;

#ifdef HAVE_KERBEROS
extern int client_authorized;
#endif

static int chk_nargs (int, const char*);
static char *get_text_memory (ErrorDesc *err);

static QueryFormat *queryFormat = NULL;
static char *editEmailAddr = NULL;

static DatabaseInfo currentDatabase = NULL;
static char *currentUsername = NULL;
static char *currentPassword = NULL;


static int
chk_nargs (int ac, const char *what)
{
  if (ac == 1)
    {
      return 1;
    }
  else
    {
      if (ac > 1)
	{
	  printf ("%d Only one %s allowed.\r\n", CODE_CMD_ERROR, what);
	}
      else
	{
	  printf ("%d One %s required.\r\n", CODE_CMD_ERROR, what);
	}
      return 0;
    }
}

static void
print_server_errors (ErrorDesc err)
{
  switch (getErrorCode (err))
    {
    case CODE_INVALID_ENUM:
      {
	BadFields badFieldList = getBadFieldList (err);
	while (badFieldList != NULL)
	  {
	    BadFields nextField = nextBadField (badFieldList);
	    FieldIndex f = badFieldIndex (badFieldList);
	    printf ("%d%cThere is a bad value `%s' for the field `%s'.\r\n",
		    CODE_INVALID_ENUM,
		    nextField != NULL ? '-' : ' ',
		    badFieldValue (badFieldList),
		    fieldDefForIndex (f)->name);
	    badFieldList = nextField;
	  }
	break;
      }
    default:
      printf ("%d %s\r\n", getErrorCode (err), getErrorMessage (err));
      break;
    }
  freeErrorDesc (err);
}

static int
daemon_lock_gnats (bool verbose)
{
  ErrorDesc err;
  int err_code;

  if ((err_code = lock_gnats (currentDatabase, &err)) == 0)
    {
      if (verbose)
	printf ("%d GNATS database is now locked.\r\n", CODE_OK);
    }
  else
    {
      printf ("%d %s\r\n", getErrorCode (err), getErrorMessage (err));
    }
  return err_code;
}

static int
daemon_unlock_gnats (bool verbose)
{
  ErrorDesc err;
  int err_code = unlock_gnats (currentDatabase, &err);

  if (err_code == 0)
    {
      if (verbose)
	printf ("%d GNATS database is now unlocked.\r\n", CODE_OK);
      return 0;
    }
  else if (err_code > 0)
    {
      printf ("%d GNATS database is already unlocked\r\n", CODE_OK);
      return 0;
    }
  else
    {
      printf ("%d %s\r\n", getErrorCode (err), getErrorMessage (err));
      return -1;
    }
}

static void
noargs (const char *s)
{
  printf ("%d %s takes no arguments.\r\n", CODE_CMD_ERROR, s);
}

static char *
get_text (void)
{
  FILE *tf;
  char *path;
  char *buf;
  const char *tmpdir;
  MsgType r;
  int done = 0;
  int error = 0;

  tmpdir = temporary_directory ();
  asprintf (&path, "%s/gnatsXXXXXX", tmpdir);
  if ((tf = fopen_temporary_file (path, "w", 0600)) == (FILE *) NULL)
    {
      /* give error that we can't create the temp and leave. */
      /* XXX ??? !!! Where did we `give error'??  Ugh caveman verb noun!  */
      free (path);
      return NULL;
    }

  while (! done && ! error)
    {
      r = get_line (&buf, 2 * 60 * 60);

      if (r == PR_ok)
	{
	  if (buf[0] == '.' 
	      && (buf[1] == '\r' || buf[1] == '\n' || buf[1] == '\0'))
	    {
	      done = 1;
	    }
	  else
	    {
	      /* Remove escape character added by write_multiline() */
	      int i = 0;
	      if (buf[0] == '.')
		{
		  i = 1;
		}
	      fputs (buf + i, tf);
	      fputs ("\n", tf);
	      if (ferror (tf))
		{
		  error = 1;
		}
	    }
	}
      else 
	{
	  if (r == PR_timeout)
	    {
	      printf ("%d Read timeout.\r\n", CODE_TIMEOUT);
	    }
	  error = 1;
	}
      if (buf != NULL)
	{
	  free (buf);
	  buf = NULL;
	}
    }

  if (feof (stdin) || ferror (stdin))
    {
      printf ("%d error reading\n", CODE_ERROR);
      error = 1;
    }
  else if (ferror (tf))
    {
      printf ("%d error writing\n", CODE_ERROR);
      error = 1;
    }
  else if (fflush (tf) != 0)
    {
      printf ("%d error flushing read input\n", CODE_ERROR);
      error = 1;
    }

  fclose (tf);

  if (buf != NULL)
    {
      free (buf);
    }

  if (! error)
    {
      return path;
    }
  else
    {
      if (unlink (path) && errno != ENOENT)
	{
	  syslog (LOG_ERR, "can't remove %s: %m", path);
	}
      free (path);
      return NULL;
    }
}

/* Print out the entry returned from a query operation.  WHICH is the
   number of the entry (1 for the first matching entry, 2 for the
   second, etc).  PR is the PR entry for the matching entry.
   QUERY_FORMAT is the query format that was passed in when the query
   was executed.

   This function is also invoked when the queries are complete; in
   this case I will be NULL, and WHICH will represent the total number
   of matching records. */

static void
server_print_query (int which, PR *pr, QueryFormat *query_format)
{
  if (pr != NULL)
    {
      if (which == 1)
	{
	  printf ("%d PRs follow.\r\n", CODE_PR_READY);
	}
      else
	{
	  printf ("\r\n");
	}
      print_pr (stdout, pr, query_format, "\r\n");
    }
  else
    {
      if (which == 0)
	{
	  printf ("%d No PRs match.\r\n", CODE_NO_PRS_MATCHED);
	}
      else
	{
	  printf ("\r\n.\r\n");
	}
    }
}

void
GNATS_qfmt (int ac, char **av)
{
  ErrorDesc err;

  if (! chk_nargs (ac, "query format"))
    {
      return;
    }
  queryFormat = findQueryFormat (currentDatabase, av[0], &err);
  if (queryFormat != NULL)
    {
      printf ("%d Ok.\r\n", CODE_OK);
    }
  else
    {
      print_server_errors (err);
    }
}

void
GNATS_user (int ac, char **av)
{
  ErrorDesc err;

  if (ac == 0)
    {
      printf ("%d-The current user access level is:\r\n",
	      CODE_INFORMATION_FILLER);
      printf ("%d %s\r\n", CODE_INFORMATION,
	      access_level_str (user_access));
    }
  else if ((ac == 1) || (ac == 2))
    {
      if (databaseValid (currentDatabase))
	{
	  if (gnatsdChdb (databaseName (currentDatabase), av[0], ac == 2 ? av[1] : "", 0,
			  &err) != 0)
	    {
	      print_server_errors (err);
	    }
	}
      else
	{
	  if (currentUsername != NULL)
	    {
	      free (currentUsername);
	    }
	  if (currentPassword != NULL)
	    {
	      free (currentPassword);
	    }
	  currentUsername = xstrdup (av[0]);
	  if (ac == 2)
	    {
	      currentPassword = xstrdup (av[1]);
	    }
	  else
	    {
	      currentPassword = (char *)"";
	    }
	  printf ("%d Current database is not valid; use CHDB to set the database\r\n",
		  CODE_OK);
	}
    }
  else
    {
      printf ("%d Need one or two arguments, username and optionally a password\r\n", 
	      CODE_CMD_ERROR);
    }
}

static void
set_confidential_access (QueryExpr *search)
{
  if (user_access < ACCESS_VIEWCONF)
    {
      QueryExpr newQ
	= parseQueryExpression (currentDatabase, 
				"builtinfield:Confidential~\"no\"",
				NULL);
      *search = booleanQuery (QueryAnd, *search, newQ);
    }
}

static void
do_server_query (int ac, char **av)
{
  ErrorDesc err;
  if (queryFormat == NULL)
    {
      printf ("%d No query format specified\r\n", CODE_INVALID_QUERY_FORMAT);
      return;
    }
  set_confidential_access (&query);
  if (db_query (currentDatabase, ac, av, query,
		server_print_query, queryFormat, &err) < 0)
    {
      print_server_errors (err);
    }
}

void
GNATS_quit (int ac ATTRIBUTE_UNUSED, char **av ATTRIBUTE_UNUSED)
{
  printf ("Hi, I'M GNATS_quit.\r\n");
}

void
GNATS_quer (int ac, char **av)
{
  do_server_query (ac, av);
}

void
GNATS_lock (int ac, char **av)
{
  char *l = NULL;
  int i, len;
  ErrorDesc err;
  int result;

  if (ac < 2)
    {
      printf ("%d Must lock with PR number, username, and process locking it.\r\n",
	      CODE_CMD_ERROR);
      return;
    }

  if (! pr_exists (currentDatabase, av[0], &err))
    {
      print_server_errors (err);
      return;
    }

  if (ac > 2)
    {
      /* XXX FIXME -- we need a cleaner approach to this.  */
      /* XXX FIXME -- we need to rewrite this "program" from scratch.  */
      for (i = 2, len = 0; i < ac; i++)
	{
	  len += strlen (av[i]);
	}
      l = (char *) xmalloc (sizeof (char) * len + ac - 2);
      strcpy (l, av[2]);
      for (i = 3; i < ac; i++)
	{
	  strcat (l, " ");
	  strcat (l, av[i]);
	}
    }

  if ((result = lock_pr (currentDatabase, av[0], av[1], l, &err)) != 0)
    {
      PR *pr = pr_load_by_id (currentDatabase, av[0], 0, &err);
      
      if (pr != NULL)
	{
	  printf ("%d PRs follow.\r\n", CODE_PR_READY);
	  print_named_format_pr (stdout, pr, "full", "\r\n", &err);
	  printf (".\r\n");
	  free_pr (pr);
	}
      else
	{
	  result = 0;
	  if (unlock_pr (currentDatabase, av[0], &err) != 0)
	    {
	      /* ??? XXX !!! */
	      setError (&err, CODE_NONEXISTENT_PR, "Invalid PR %s.", av[0]);
	    }
	}
    }

  if (result == 0)
    {
      print_server_errors (err);
    }

  if (l != NULL)
    {
      free (l);
    }
}


void
GNATS_subm (int ac, char **av ATTRIBUTE_UNUSED)
{
  char *tempfile;
  FILE *fp;
  ErrorDesc err;
  int new_pr_num;

  if (ac != 0)
    {
      noargs ("SUBM");
      return;
    }

  if (is_gnats_locked (currentDatabase))
    {
      printf ("%d GNATS is currently locked; try again later.\r\n",
	      CODE_GNATS_LOCKED);
      return;
    }

  printf ("%d Ok.\r\n", CODE_SEND_PR); 
  fflush (stdout);

  tempfile = get_text ();
  if (tempfile == NULL)
    {
      return;
    }

  fp = fopen (tempfile, "r");

  if (fp != NULL)
    {
      if (daemon_lock_gnats (FALSE) == 0)
	{
	  if ((new_pr_num = submit_pr (currentDatabase, fp, &err)) != 0)
	    {
	      printf ("%d-The added PR number is:\r\n",
		      CODE_INFORMATION_FILLER);
	      printf ("%d %d\r\n", CODE_INFORMATION, new_pr_num);
	      fflush (stdout);
	    }
	  else
	    {
	      print_server_errors (err);
	    }
	  daemon_unlock_gnats (FALSE);
	}
    }
  else
    {
      printf ("%d Failed reading saved text.\r\n", CODE_FILE_ERROR);
    }

  unlink (tempfile);
  free (tempfile);
}

void
GNATS_unlk (int ac, char **av)
{
  ErrorDesc err;

  if (! chk_nargs (ac, "PR to be unlocked"))
    return;

  if (! pr_exists (currentDatabase, av[0], &err))
    {
      print_server_errors (err);
      return;
    }
  
  if (unlock_pr (currentDatabase, av[0], &err) != 0)
    {
      printf ("%d PR %s unlocked.\r\n", CODE_OK, av[0]);
    }
  else
    {
      print_server_errors (err);
    }
}

void
GNATS_lkdb (int ac, char **av ATTRIBUTE_UNUSED)
{
  if (ac != 0)
    {
      noargs ("LKDB");
      return;
    }

  daemon_lock_gnats (TRUE);
}

void
GNATS_undb (int ac, char **av ATTRIBUTE_UNUSED)
{
  if (ac != 0)
    {
      noargs ("UNDB");
      return;
    }

  daemon_unlock_gnats (TRUE);
}

/* Change to the specified DATABASE. USERANME and PASSWORD are used to
   authenticate to the database.  If QUIET is non-zero, we do not print
   an acknowledgement that the change succeeded.  

   A non-zero value is returned if we could not change to the requested
   database, and ERR will then describe the error.  */

int
gnatsdChdb (const char *nameOfDb, const char *username, const char *passwd,
	    int quiet,
	    ErrorDesc *err)
{
  int dbValid = 0;
  DatabaseInfo new_db = init_gnats ("gnatsd", nameOfDb, err);

  if (username != NULL)
    {
      if (currentUsername != NULL)
	{
	  free (currentUsername);
	}
      currentUsername = xstrdup (username);
      
      if (currentPassword != NULL)
	{
	  free (currentPassword);
	}
      if (passwd != NULL)
	{
	  currentPassword = xstrdup (passwd);
	}
      else
	{
	  currentPassword = NULL;
	}
    }

  if (currentUsername == NULL)
    {
      currentUsername = xstrdup ("");
    }

  if (currentPassword == NULL)
    {
      currentPassword = xstrdup ("");
    }

  if (new_db != NULL)
    {
      Access_Level access;

      /* make sure that a host that is allowed to access one database does
	 not switch to a database where permission is not granted */
      if (! verifyHostAndUser (new_db, current_host, current_addr, 
			       currentUsername, currentPassword, &access))
	{
	  const char *database_name = databaseName(new_db);
	  /* Hmmm.  Should we still do this here? ??? XXX !!! */
	  syslog (LOG_ERR, "host %s (%s) not allowed access for %s",
		  current_host, current_addr, database_name);
	  setError (err, CODE_NO_ACCESS, 
		    "You are not permitted to access database %s.\r\n",
		    database_name);
	  freeDatabaseInfo (new_db);
	}
      else
	{
	  /* Clear the gnatsd --require-db flag and set user access to host
	     default access for new db */
	  require_db  = 0;
	  user_access = access;
	  /* Don't free the current database if it's the same as the
	     new one. */
	  if (currentDatabase != new_db)
	    {
	      freeDatabaseInfo (currentDatabase);
	      queryFormat = NULL;
	    }
	  currentDatabase = new_db;
	  /* Default an editing users email address */
	  editEmailAddr = get_responsible_addr (currentDatabase, 0, 0,
						username);

	  if (! quiet)
	    {
	      printf ("%d-Now accessing GNATS database '%s'\r\n", CODE_OK,
		      databaseName (new_db));
	      printf ("%d User access level set to '%s'\r\n", CODE_OK,
		      access_level_str (access));
	    }
	  dbValid = 1;
	}
    }

  return ! dbValid;
}

/* change database command */
void
GNATS_chdb (int ac, char **av)
{
  ErrorDesc err;
  const char *user = NULL;
  const char *passwd = NULL;

  if (ac != 1 && ac != 2 && ac != 3)
    {
      printf ("%d One, two, or three arguments required.\r\n", CODE_CMD_ERROR); 
      return;
    }

  if (ac == 3)
    {
      user = av[1];
      passwd = av[2];
    }
  else if (ac == 2)
    {
      user = av[1];
    }

  if (gnatsdChdb (av[0], user, passwd, 0, &err) != 0)
    {
      print_server_errors (err);
    }
}

void
GNATS_chek (int ac, char **av ATTRIBUTE_UNUSED)
{
  char *given;
  FILE *fp;
  ErrorDesc err;
  int initial = 0;

  if (ac > 1)
    {
      printf ("%d CHEK takes either 0 or 1 argument.\r\n", CODE_CMD_ERROR);
      return;
    }

  if (ac == 1)
    {
      /* XXX ??? !!! Should look at the argument. */
      initial = 1;
    }

  printf ("%d Ok.\r\n", CODE_SEND_PR); 
  fflush (stdout);
  given = get_text ();

  if (given == NULL)
    {
      return; /* XXX */
    }

  fp = fopen (given, "r");
  if (fp != NULL)
    {
      if (! check_pr_file (currentDatabase, fp, &err, initial))
	{
	  print_server_errors (err);
	  printf ("%d Errors found checking PR text.\r\n", 
		  CODE_INVALID_PR_CONTENTS);
	}
      else
	{
	  printf ("%d PR text checked OK.\r\n", CODE_OK);
	}

      fclose (fp);

      if (unlink (given) && errno != ENOENT)
	{
	  syslog (LOG_ERR, "can't remove %s: %m", given);
	}
    }
  else 
    {
      printf ("%d Failed reading saved text.\r\n", CODE_FILE_ERROR);
    }

  free (given);
  return;
}

void
GNATS_edit (int ac, char **av)
{
  char *given;
  FILE *fp;
  ErrorDesc err;

  if (! chk_nargs (ac, "PR to be edited"))
    return;

  if (! pr_exists (currentDatabase, av[0], &err))
    {
      print_server_errors (err);
      return;
    }

  if (! isPrLocked (currentDatabase, av[0]))
    {
      setError (&err, CODE_PR_NOT_LOCKED, "You must lock PR %s and get its current content first.", av[0]);
      print_server_errors (err);
      return;
    }

  if (is_gnats_locked (currentDatabase))
    {
      printf ("%d GNATS is currently locked; try again later.\r\n",
	      CODE_GNATS_LOCKED);
      return;
    }

  printf ("%d Ok.\r\n", CODE_SEND_PR); 
  fflush (stdout);
  given = get_text ();

  if (given == NULL)
    {
      return; /* XXX */
    }

  fp = fopen (given, "r");
  if (fp == NULL)
    {
      printf ("%d Failed reading saved text.\r\n", CODE_FILE_ERROR);
    }
  else
    {
      if (daemon_lock_gnats (FALSE) == 0)
	{
	  if (! replace_pr (currentDatabase, fp, editEmailAddr, &err))
	    {
	      print_server_errors (err);
	      printf ("%d Failed writing out PR.\r\n", CODE_WRITE_PR_FAILED);
	    }
	  else
	    {
	      printf ("%d PR %s filed.\r\n", CODE_OK, av[0]);
	    }
	  fclose (fp);

	  daemon_unlock_gnats (FALSE);

	  if (unlink (given) && errno != ENOENT)
	    {
	      syslog (LOG_ERR, "can't remove %s: %m", given);
	    }
	}
    }
  free (given);
}

static void
GNATS_appnOrRepl (int ac, char **av, int mode)
{
  char *text;
  ErrorDesc err;
  FieldIndex fieldIndex;

  if (ac != 2)
    {
      printf ("%d Need two arguments, PR number and field\r\n", 
	      CODE_CMD_ERROR);
      return;
    }
  fieldIndex = find_field_index (currentDatabase, av[1]);
  if (fieldIndex == InvalidFieldIndex)
    {
      printf ("%d Invalid field name `%s'\r\n",
	      CODE_INVALID_FIELD_NAME, av[1]);
    }
  else
    {
      char *changeReason = NULL;

      printf ("%d Ok.\r\n", CODE_SEND_TEXT);
      fflush (stdout);
      text = get_text_memory (&err);

      if (text != NULL)
	{
	  if (requiresChangeReason (fieldDefForIndex (fieldIndex)))
	    {
	      printf ("%d Ok, now send the field change reason.\r\n",
		      CODE_SEND_CHANGE_REASON);
	      fflush (stdout);
	      changeReason = get_text_memory (&err);
	      if (changeReason == NULL)
		{
		  free (text);
		  text = NULL;
		}
	    }
	}

      if (text != NULL)
	{
	  /* Remove trailing newline. */
	  text[strlen(text) - 1] = '\0';

	  if (changeReason != NULL)
	    {
	      changeReason[strlen (changeReason) - 1] = '\0';
	    }

	  if (daemon_lock_gnats (FALSE) == 0)
	    {
	      if (edit_field (currentDatabase, av[0], fieldIndex, mode, text,
			      changeReason, editEmailAddr, &err) == 0)
		{
		  print_server_errors (err);
		}
	      else
		{
		  printf ("%d Ok.\r\n", CODE_OK);
		}
	      daemon_unlock_gnats (FALSE);
	    }
	}
      else
	{
	  print_server_errors (err);
	}
    }
}

void
GNATS_appn (int ac, char **av)
{
  GNATS_appnOrRepl (ac, av, 1);
}

void
GNATS_repl (int ac, char **av)
{
  GNATS_appnOrRepl (ac, av, 0);
}

void
GNATS_editaddr (int ac, char **av)
{
  if (! chk_nargs (ac, "email address"))
    {
      return;
    }
  if (editEmailAddr != NULL)
    {
      free (editEmailAddr);
    }
  editEmailAddr = xstrdup (av[0]);
  printf ("%d Ok.\r\n", CODE_OK);
}

void
GNATS_delete (int ac, char **av)
{
  if (chk_nargs (ac, "PR number"))
    {
      ErrorDesc err;
      int result = deletePR (currentDatabase, av[0], editEmailAddr, &err);

      if (result == 0)
	{
	  printf ("%d PR %s deleted.\r\n", CODE_OK, av[0]);
	}
      else
	{
	  print_server_errors (err);
	}
    }
}



void
GNATS_admv (int ac, char **av)
{
  if (ac == 2 || ac == 3)
    {
      ComplexFieldIndex ci = newComplexFieldIndex (currentDatabase, av[0]);
      if (parseComplexFieldIndex (ci) != 0
	  || simpleFieldIndexValue (ci) == InvalidFieldIndex)
	{
	  printf ("%d Invalid field name `%s'\r\n",
		  CODE_INVALID_FIELD_NAME, av[0]);
	}
      else
	{
	  FieldIndex i =simpleFieldIndexValue (ci);
	  AdmEntry *ent = get_adm_record (i, av[1]);
	  if (ent == NULL)
	    {
	      printf ("%d No entry in field `%s' for `%s'.\r\n",
		      CODE_NO_ADM_ENTRY, av[0], av[1]);
	    }
	  else
	    {
	      printf ("%d ", CODE_INFORMATION);
	      printAdmSubfield (stdout, "\r\n", i, ent,
				(ac > 2) ? av[2] : NULL);
	    }
	}
    }
  else
    {
      printf ("%d Need two or three arguments: fieldname, key, and optional subfield\n",
	      CODE_CMD_ERROR);
    }
}

void
GNATS_vfld (int ac, char **av)
{
  FieldIndex field;
  char *newt;
  ErrorDesc err;
  size_t len;

  if (! chk_nargs (ac, "field name"))
    return;

  field = find_field_index (currentDatabase, av[0]);
  if (field == InvalidFieldIndex)
    {
      printf ("%d No such field as `%s'\r\n", CODE_INVALID_FIELD_NAME,
	      av[0]);
      return;
    }

  printf ("%d Ok, send field text now.\r\n", CODE_SEND_TEXT);
  fflush (stdout);
  newt = get_text_memory (&err);
  if (newt != NULL)
    {
      len = strlen (newt);
      if (fieldDefForIndex (field)->datatype != MultiText
	  && newt[len - 1] == '\n')
	{
	  newt[len - 1] = '\0';
	}
      if (! validateFieldValue (field, newt, &err, 0))
	{
	  print_server_errors (err);
	}
      else
	{
	  printf ("%d Ok.\r\n", CODE_OK);
	}
      free (newt);
    }
  else
    {
      print_server_errors (err);
    }
}

void
GNATS_fieldflags (int ac, char **av)
{
  int x;
  
  if (ac < 1)
    {
      printf ("%d At least one field name is required.\r\n", CODE_CMD_ERROR);
      return;
    }
  for (x = 0; x < ac; x++)
    {
      char contchar = ((x < (ac - 1)) ? '-' : ' ');
      FieldIndex fnum = find_field_index (currentDatabase, av[x]);

      if (fnum != InvalidFieldIndex)
	{
	  char *flags = getFieldFlags (fnum);

	  printf ("%d%c%s\r\n", CODE_INFORMATION, contchar, flags);
	  free (flags);
	}
      else
	{
	  printf ("%d%cNo such field as `%s'\r\n",
		  CODE_INVALID_FIELD_NAME, contchar, av[x]);
	}
    }
}

void
GNATS_expr (int ac, char **av)
{
  QueryExpr q2;

  if (! chk_nargs (ac, "expression"))
    {
      return;
    }
  q2 = parseQueryExpression (currentDatabase, 
			     av[0], av[0] + strlen (av[0]) - 1);
  if (q2 == NULL)
    {
      printf ("%d Invalid expression.\r\n", CODE_INVALID_EXPR);
      return;
    }
  else
    {
      query = booleanQuery (QueryAnd, query, q2);
    }
  printf ("%d Ok.\r\n", CODE_OK);
}

void
GNATS_rset (int ac, char **av ATTRIBUTE_UNUSED)
{
  ErrorDesc err = NULL;

  if (ac != 0)
    {
      printf ("%d No arguments expected for RSET.\r\n", CODE_CMD_ERROR);
      return;
    }

  freeQueryExpr (query);
  query = NULL;

  db_reset (currentDatabase, &err);
  if (err != NULL)
    {
      print_server_errors (err);
      freeDatabaseInfo (currentDatabase);
      currentDatabase = NULL;
      return;
    }
  printf ("%d Reset state.\r\n", CODE_OK);
}

static char *
get_text_memory (ErrorDesc *err)
{
  char *buf = NULL;
  char *res = NULL;
  size_t reslen = 0;
  MsgType r;
  int done = 0;
  int error = 0;

  while (! done && ! error)
    {
      r = get_line (&buf, 2 * 60 * 60);
      if (r == PR_ok)
	{
	  char *bp = buf;
	  size_t buflen;

	  if (buf[0] == '.' 
	      && (buf[1] == '\r' || buf[1] == '\n' || buf[1] == '\0'))
	    {
	      done = 1;
	    }
	  else
	    {
	      if (bp[0] == '.')
		{
		  bp++;
		}
	      buflen = strlen (bp);
	      while (bp[buflen - 1] == '\r' || bp[buflen - 1] == '\n')
		{
		  buflen--;
		}
	      bp[buflen++] = '\n';
	      res = xrealloc (res, reslen + buflen + 1);
	      memcpy (res + reslen, bp, buflen);
	      reslen += buflen;
	      res[reslen] = '\0';
	    }
	}
      else 
	{
	  if (r == PR_timeout)
	    {
	      setError (err, CODE_TIMEOUT, "Read timeout");
	    }
	  else if (r == PR_eof)
	    {
	      setError (err, CODE_ERROR, "Read EOF");
	    }
	  error = 1;
	}
      if (buf != NULL)
	{
	  free (buf);
	  buf = NULL;
	}
    }

  if (error)
    {
      if (res != NULL)
	{
	  free (res);
	}
      res = NULL;
    }
  else
    {
      if (res == NULL)
	{
	  res = xstrdup ("");
	}
    }

  return res;
}

#ifdef HAVE_KERBEROS
/* Get an input line, or die.  */
static void
simple_get_line (char **buf, int timo)
{
  switch (get_line (buf, timo))
    {
    case PR_timeout:
    case PR_eof:
      /* for these two, assume the client simply died */
      exit (1);
      break;
    case PR_ok:
      break;
    default:
      abort ();
    }
}

void
tohex (ptr, buf, sz)
     char *ptr, *buf;
     int sz;
{
  static const char hex[16] = "0123456789abcdef";
  int c;
  while (sz--)
    {
      c = *ptr++ & 0xff;
      *buf++ = hex[c >> 4];
      *buf++ = hex[c & 15];
    }
  *buf++ = 0;
}

int
hexdigit (c)
     int c;
{
  if (c >= '0' && c <= '9')
    return c - '0';
  if (c >= 'a' && c <= 'f')
    return c - 'a' + 10;
  if (c >= 'A' && c <= 'F')
    return c - 'A' + 10;
  return -1;
}
void
fromhex (ptr, buf, sz)
     char *ptr, *buf;
     int sz;
{
  int val;
  while (sz--)
    {
      val = hexdigit (*buf++);
      val <<= 4;
      val |= hexdigit (*buf++);
      *ptr++ = val;
    }
}
#endif

void
GNATS_auth (int ac, char **av)
{
  if (! chk_nargs(ac, "authentication type name"))
    return;
#ifdef HAVE_KERBEROS
  /* This isn't the right way to do this ... we ought to exchange data
     in printable ascii form or something like that.  This'll do for
     now.  */
  else if (!strcmp (av[0], "krb4-simple"))
    {
      extern char *getusershell ();
      char *keyfile;
      char *desired_user;
      char *k_buf;
      struct sockaddr_in peer, laddr;
      int len = sizeof (peer);
      int status;
      char instance[INST_SZ+1], version[KRB_SENDAUTH_VLEN+1];
      char *p;
      struct passwd *pwd;
      /* Must be exactly 4 bytes.  */
      int cksum;
      /* Bundle these together to make it easy to erase them later.  */
      struct {
	AUTH_DAT auth;
	Key_schedule sched;
	KTEXT_ST ticket;
	char buf[MAX_KTXT_LEN * 3 + 10];
      } k;

      if (getpeername (0, (struct sockaddr *) &peer, &len) < 0)
	{
	  syslog (LOG_ERR, "can't get peername: %m");
	  goto addr_error;
	}
      if (getsockname (0, (struct sockaddr *) &laddr, &len) < 0)
	{
	  syslog (LOG_ERR, "can't get sockname: %m");
	addr_error:
	  printf ("%d Server system error checking addresses: %s\r\n",
		  CODE_ERROR, strerror (errno));
	  return;
	}

      keyfile =  gnats_adm_dir ("srvtab");

      /* Sanity-check installation.  */
      {
	if (! fileExists (keyfile))
	  {
	    syslog (LOG_ERR, "can't find Kerberos key file %s: %m", keyfile);
	  bad_key_file:
	    printf ("%d Server installation error, cannot access key file\r\n",
		    CODE_ERROR);
	    free (keyfile);
	    return;
	  }
	else if (statbuf.st_mode & 0044)
	  {
	    syslog (LOG_ERR,
		    "Kerberos key file %s is readable to others; recommend changing it",
		    keyfile);
	    goto bad_key_file;
	  }
	else if (! fileExists (keyfile))
	  {
	    syslog (LOG_ERR, "can't read Kerberos key file %s: %m", keyfile);
	    goto bad_key_file;
	  }
      }

      printf ("%d Go for it!\r\n", CODE_OK);
      fflush (stdout);

      /* Do authentication step.  */

      /* This is ugly, because krb_sendauth/krb_recvauth don't
	 maintain a sensible connection state if authentication fails.
	 This is unacceptable here.  So I've broken out most of the
	 interesting bits of those routines here.  I've also added a
	 hex encoding of the data exchanged instead of the standard
	 authenticator format, to maintain the text-only protocol used
	 in GNATS.  */

      /* Kerberos authenticator timeout is five minutes; give the client
	 nearly that long.  */
      simple_get_line (&desired_user, 4 * 60);
      simple_get_line (&k_buf, 4 * 60);
      memcpy (k.buf, k_buf, sizeof (k.buf) - 1);
      if (k.buf[0] != 'x')
	{
	  printf ("%d Bad hex encoding.\r\n", CODE_ERROR);
	  return;
	}
      p = k.buf + 1;
      fromhex (version, p, KRB_SENDAUTH_VLEN * 2);
      p += KRB_SENDAUTH_VLEN * 2;
      version[KRB_SENDAUTH_VLEN] = 0;
      if (memcmp ("AUTHV0.1", version, KRB_SENDAUTH_VLEN))
	{
	  printf ("%d Unrecognized `sendauth' version `%s'.\r\n",
		  CODE_ERROR, version);
	  return;
	}
      fromhex (version, p, KRB_SENDAUTH_VLEN * 2);
      p += KRB_SENDAUTH_VLEN * 2;
      if (memcmp (GNATS_KRB4_VERSIONID, version, KRB_SENDAUTH_VLEN))
	{
	  printf ("%d Unrecognized authentication protocol version `%s'\r\n",
		  CODE_ERROR, version);
	  return;
	}
      p += 8;
      k.ticket.length = strlen (p) / 2;
      fromhex (k.ticket.dat, p, k.ticket.length);
      strcpy (instance, "*");
      status = krb_rd_req (&k.ticket, GNATS_KRB4_PRINCIPAL_NAME, instance,
			   peer.sin_addr.s_addr, &k.auth, keyfile);
      free (keyfile);
      if (status != KSUCCESS)
	{
	  syslog (LOG_ERR, "auth failed for %s: %s",
		  current_host, krb_get_err_text (status));
	  printf ("%d Authentication failed: %s.\r\n",
		  CODE_ERROR, krb_get_err_text (status));
	  return;
	}
      if (sizeof (cksum) != 4)
	abort ();
      cksum = ntohl (k.auth.checksum + 1);
      key_sched (k.auth.session, k.sched);
      len = krb_mk_priv ((unsigned char *) &cksum, k.buf, 4, k.sched,
			 &k.auth.session, &laddr, &peer);
      if (len < 0)
	{
	  printf ("%d Kerberos error making private message.\r\n",
		  CODE_ERROR);
	  return;
	}
      if (len + 5 > sizeof (k.buf) / 3)
	{
	  syslog (LOG_ERR, "Need more internal buffer space for krb_mk_priv.");
	  printf ("%d Server bug: krb_mk_priv overruns buffer.\r\n",
		  CODE_ERROR);
	  return;
	}
      p = k.buf + len + 1;
      tohex (k.buf, p, len);
      printf ("x%s\r\n", p);

      /* Okay, authentication step is done.  Now check authorization.  */
      if (kuserok (&k.auth, desired_user))
	{
	  printf ("%d You are not allowed access as user `%s'.\r\n",
		  CODE_NO_ACCESS, desired_user);
	  return;
	}
      pwd = getpwnam (desired_user);
      if (!pwd)
	{
	  printf ("%d Cannot find a password file entry for `%s'.\r\n",
		  CODE_NO_ACCESS, desired_user);
	  return;
	}
      for (setusershell (); p; p = getusershell ())
	if (!strcmp (pwd->pw_shell, p))
	  break;
      endusershell ();
      if (p == 0)
	{
	  printf ("%d Access denied for user `%s'.\r\n",
		  CODE_NO_ACCESS, desired_user);
	  return;
	}

      printf ("%d Okie-dokey.\r\n", CODE_OK);
      client_authorized = 1;
      user_access = ACCESS_EDIT;
      return;
    }
#endif
  else
    {
      printf ("%d Authentication type `%s' not supported.\r\n",
	      CODE_AUTH_TYPE_UNSUP, av[0]);
      return;
    }
}

void
GNATS_list (int ac, char **av)
{
  ListTypes whichList;
  if (! chk_nargs (ac, "list type"))
    {
      return;
    }
  whichList = stringToListType (av[0]);

  if (whichList != InvalidListType)
    {
      printf ("%d List follows.\r\n", CODE_TEXT_READY);
      /* XXX ??? !!! Need to print an error message, hmmm?  */
      if (getGnatsFile (currentDatabase, whichList, NULL, "\r\n") == 0)
	{
	  printf (".\r\n");
	}
    }
  else
    {
      printf ("%d No such list as %s\r\n", CODE_INVALID_LIST, av[0]);
    }
}

void
GNATS_ftyp (int ac, char **av)
{
  int x;

  if (ac < 1)
    {
      /* ohhh lazy */
      chk_nargs (ac, "field name");
      return;
    }
  for (x = 0; x < ac; x++)
    {
      char contchar =  (x < (ac - 1)) ? '-' : ' ';
      FieldIndex fnum = find_field_index (currentDatabase, av[x]);

      if (fnum != InvalidFieldIndex)
	{
	  printf ("%d%c%s\n", CODE_INFORMATION, contchar, 
		  fieldTypeAsString (fieldDefForIndex (fnum)->datatype));
	}
      else
	{
	  printf ("%d%cNo such field as `%s'\r\n",
		  CODE_INVALID_FIELD_NAME, contchar, av[x]);
	}
    }
}

void
GNATS_ftypinfo (int ac, char **av)
{
  if (ac == 2)
    {
      FieldIndex fnum = find_field_index (currentDatabase, av[0]);
      
      if (fnum == InvalidFieldIndex)
        {
          printf ("%d No such field as `%s'.\r\n", CODE_INVALID_FIELD_NAME,
                  av[0]);
          return;
        }
      
      if (strcmp(av[1], "separators") == 0) {
        if (fieldDefForIndex (fnum)->datatype == MultiEnum)
          {
            const char *sep = fieldDefForIndex (fnum)->multiEnumSeparator;
            if (sep == NULL)
	      sep = DEFAULT_MULTIENUM_SEPARATOR;
            printf ("%d '%s'\r\n", CODE_INFORMATION, sep);
          }
        else
          {
            printf ("%d Property `%s' not defined for field type `%s'.\r\n",
                    CODE_INVALID_FTYPE_PROPERTY, av[1],
                    fieldTypeAsString (fieldDefForIndex (fnum)->datatype));
          }
      }
      
      /* Unknown property: */
      else
        {
          printf ("%d No such property as `%s'.\r\n",
                  CODE_INVALID_FTYPE_PROPERTY, av[1]);
        }
    }
  
  else
    {
      printf ("%d Need two arguments, fieldname and property\r\n",
              CODE_CMD_ERROR);
    }
}

void
GNATS_fdsc (int ac, char **av)
{
  int x;

  if (ac < 1)
    {
      chk_nargs (ac, "field name");
      return;
    }
  for (x = 0; x < ac; x++)
    {
      char contchar = (x < (ac - 1)) ? '-' : ' ';
      FieldIndex fnum = find_field_index (currentDatabase, av[x]);
      if (fnum != InvalidFieldIndex)
	{
	  const char *desc = fieldDefForIndex (fnum)->description;

	  if (desc == NULL)
	    {
	      desc = "";
	    }

	  printf ("%d%c%s\n", CODE_INFORMATION, contchar, desc);
	}
      else
	{
	  printf ("%d%cNo such field as `%s'\r\n",
		  CODE_INVALID_FIELD_NAME, contchar, av[x]);
	}
    }
}

void
GNATS_fvld (int ac, char **av)
{
  FieldIndex fnum;

  if (! chk_nargs (ac, "field name"))
    {
      return;
    }
  fnum = find_field_index (currentDatabase, av[0]);
  if (fnum == InvalidFieldIndex)
    {
      printf ("%d No such field as `%s'\r\n", CODE_INVALID_FIELD_NAME,
	      av[0]);
    }
  else
    {
      printf ("%d Valid values follow\r\n", CODE_TEXT_READY);
      printValidValues (stdout, fnum, "\r\n");
      printf (".\r\n");
    }
}

void
GNATS_inputdefault (int ac, char **av)
{
  int x;

  if (ac < 1)
    {
      chk_nargs (ac, "field name");
      return;
    }
  for (x = 0; x < ac; x++)
    {
      char contchar = (x < (ac - 1)) ? '-' : ' ';
      FieldIndex fnum = find_field_index (currentDatabase, av[x]);
      if (fnum != InvalidFieldIndex)
	{
	  const char *value = fieldDefForIndex (fnum)->input_default_value;

	  if (value == NULL)
	    {
	      value = fieldDefForIndex (fnum)->default_value;
	      if (value == NULL)
		{
		  value = "";
		}
	    }

	  printf ("%d%c", CODE_INFORMATION, contchar);
	  while (*value != '\0')
	    {
	      if (*value == '\n')
		{
		  printf ("\\n");
		}
	      else
		{
		  putchar (*value);
		}
	      value++;
	    }
	  printf ("\r\n");
	  
	}
      else
	{
	  printf ("%d%cNo such field as `%s'\r\n",
		  CODE_INVALID_FIELD_NAME, contchar, av[x]);
	}
    }
}

void
GNATS_dbls (int ac, char **av ATTRIBUTE_UNUSED)
{
  AdmEntry *dbl;
  ErrorDesc err;

  if (ac != 0)
    {
      noargs ("DBLS");
      return;
    }
  
  dbl = getDatabaseList (&err);
  if (dbl == NULL)
    {
      print_server_errors (err);
    }
  else
    {
      printf ("%d List follows.\r\n", CODE_TEXT_READY);
      while (dbl != NULL)
	{
	  printf ("%s\r\n", dbl->admFields[DatabaseListKey]);
	  dbl = dbl->next;
	}
      printf (".\r\n");
    }
}

void
GNATS_dbdesc (int ac, char **av)
{
  AdmEntry *dbl;
  AdmEntry *ent;
  ErrorDesc err;

  if (! chk_nargs (ac, "database name"))
    {
      return;
    }
  dbl = getDatabaseList (&err);
  if (dbl == NULL)
    {
      print_server_errors (err);
    }
  else
    {
      ent = find_chain_entry_nocopy (dbl, av[0]);
 
      if (ent != NULL)
	{
	  printf ("%d %s\r\n", CODE_INFORMATION, 
		  ent->admFields[DatabaseListDesc]);
	}
      else
	{
	  printf ("%d No such database as `%s'.\r\n", CODE_INVALID_DATABASE,
		  av[0]);
	}
    }
}

void
GNATS_help (int ac ATTRIBUTE_UNUSED, char **av ATTRIBUTE_UNUSED)
{
  printf ("%d-The GNATS daemon accepts the following commands:\r\n",
	  CODE_INFORMATION);
  printf ("%d-   QUIT                    close the connection\r\n",
	  CODE_INFORMATION);
  printf ("%d-   LIST <list type>        list info of the specified type\r\n",
	  CODE_INFORMATION);
  printf ("%d-   FTYP <field>            return the datatype of <field>\r\n",
	  CODE_INFORMATION);
  printf ("%d-   FTYPINFO <field> <property>\r\n",
      CODE_INFORMATION);
  printf ("%d-                           return info about a property of the\r\n",
      CODE_INFORMATION);
  printf ("%d-                           specified field\r\n",
      CODE_INFORMATION);
  printf ("%d-   FDSC <field>            return the description of <field>\r\n",
	  CODE_INFORMATION);
  printf ("%d-   FVLD <field>            return a string or regexp describing the\r\n",
	  CODE_INFORMATION);
  printf ("%d-                           legal values for <field>\r\n",
	  CODE_INFORMATION);
  printf ("%d-   VFLD <field>            validate that the provided text is valid\r\n",
	  CODE_INFORMATION);
  printf ("%d-                           for the specified field\r\n",
	  CODE_INFORMATION);
  printf ("%d-   INPUTDEFAULT <field> [<field> ...]\r\n",
	  CODE_INFORMATION);
  printf ("%d-                           List the suggested default input values\r\n",
	  CODE_INFORMATION);
  printf ("%d-                           for the specified field(s)\r\n",
	  CODE_INFORMATION);
  printf ("%d-   FIELDFLAGS <field> [<field> ...]\r\n",
	  CODE_INFORMATION);
  printf ("%d-                           List the field flags for the specified\r\n",
	  CODE_INFORMATION);
  printf ("%d-                           fields\r\n",
	  CODE_INFORMATION);
  printf ("%d-   ADMV <field> <key> [<subfield>]\r\n",
	  CODE_INFORMATION);
  printf ("%d-                           Retrieve a record from a field's admin db\r\n",
	  CODE_INFORMATION);
  printf ("%d-   QFMT <query format>     Use the specified query format to format\r\n",
	  CODE_INFORMATION);
  printf ("%d-                           the output of the query\r\n",
	  CODE_INFORMATION);
  printf ("%d-   RSET                    reset internal settings to initial startup\r\n",
	  CODE_INFORMATION);
  printf ("%d-   LKDB                    lock the main GNATS database\r\n",
	  CODE_INFORMATION);
  printf ("%d-   UNDB                    unlock the main GNATS database\r\n",
	  CODE_INFORMATION);
  printf ("%d-   LOCK <PR> <user> <pid>  lock <PR> for <user> and optional <pid>\r\n",
	  CODE_INFORMATION);
  printf ("%d-                           and return PR text\r\n",
	  CODE_INFORMATION);
  printf ("%d-   UNLK <PR>               unlock <PR>\r\n",
	  CODE_INFORMATION);
  printf ("%d-   DELETE <PR>             Delete the specified <PR>\r\n",
	  CODE_INFORMATION);
  printf ("%d-   CHEK [initial]          check PR text for errors\r\n",
	  CODE_INFORMATION);
  printf ("%d-   EDIT <PR>               check in edited <PR>\r\n",
	  CODE_INFORMATION);
  printf ("%d-   EDITADDR <address>      set e-mail address to <address>\r\n",
	  CODE_INFORMATION);
  printf ("%d-   APPN <PR> <field>       append to contents of <field> in <PR>\r\n",
	  CODE_INFORMATION);
  printf ("%d-   REPL <PR> <field>       replace contents of <field> in <PR>\r\n",
 	  CODE_INFORMATION);
  printf ("%d-   SUBM                    submit a new PR\r\n",
	  CODE_INFORMATION);
  printf ("%d-   CHDB <database> [<user> [<passwd>]]\r\n",
	  CODE_INFORMATION);
  printf ("%d-                           change GNATS ROOT to <database>\r\n",
	  CODE_INFORMATION);
  printf ("%d-   USER <name> [<passwd>]  Sets the current user\r\n",
	  CODE_INFORMATION);
  printf ("%d-   USER                    Report current access level\r\n",
	  CODE_INFORMATION);
  printf ("%d-   DBLS                    list databases known to server\r\n",
 	  CODE_INFORMATION);
  printf ("%d-   DBDESC <database>       return description of <database>\r\n",
	  CODE_INFORMATION);
  printf ("%d-   EXPR <expression>       restrict search to PRs that match the\r\n",
	  CODE_INFORMATION);
  printf ("%d-                           query expression\r\n",
	  CODE_INFORMATION);
#ifdef HAVE_KERBEROS
  printf ("%d-   AUTH <kerberos>	 Kerberos authentication\r\n",
	  CODE_INFORMATION);
#endif
  printf ("%d-  The search itself is performed by QUER.  It may be invoked\r\n",
	  CODE_INFORMATION);
  printf ("%d-  with no arguments,  which will search the entire database given\r\n",
	  CODE_INFORMATION);
  printf ("%d-  the constraints above, or with one or more PRs in its command,\r\n",
	  CODE_INFORMATION);
  printf ("%d-  which will search those PRs specifically.\r\n",
	  CODE_INFORMATION);
  printf ("%d End of HELP info for GNATS daemon.\r\n", CODE_INFORMATION);
}
