/* Entry for the GNATS system.
   Copyright (C) 2001 Milan Zamazal
   Copyright (C) 1993, 1994, 1995, 2007 Free Software Foundation, Inc.
   Originally written by Tim Wicinski (wicinski@barn.com).
   Further work by Brendan Kehoe (brendan@cygnus.com).
   Further work by Milan Zamazal (pdm@zamazal.org).

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
#include "pcodes.h"
#include "ds.h"

/* Name of this program.  */
const char *program_name;

enum {
  NONE,
  LOCK,
  UNLOCK,
  LOCKDB,
  UNLOCKDB,
  CHECK,
  CHECK_INITIAL,
  MODIFY,
  SUBMIT,
  APPEND,
  REPLACE,
  DELETE
} edit_options;

#define DELETE_PR_OPT 256
#define SHOW_PRNUM_OPT 257

struct option long_options[] =
{
  {"lock", 1, NULL, 'l'},
  {"unlock", 0, NULL, 'u'},
  {"lockdb", 0, NULL, 'L'},
  {"unlockdb", 0, NULL, 'U'},
  {"check", 0, NULL, 'c'},
  {"check-initial", 0, NULL, 'C'},
  {"submit", 0, NULL, 's'},
  {"append", 1, NULL, 'a'},
  {"replace", 1, NULL, 'r'},
  {"reason", 1, NULL, 'R'},
  {"process", 1, NULL, 'p'},
  {"database", 1, NULL, 'd'},
  {"filename", 1, NULL, 'f'},
  {"version", 0, NULL, 'V'},
  {"delete-pr", 0, NULL, DELETE_PR_OPT},
  {"show-prnum", 0, NULL, SHOW_PRNUM_OPT},
  {"help", 0, NULL, 'h'},
  {"user", 1, NULL, 'v'},
  {"passwd", 1, NULL, 'w'},
  {"host", 1, NULL, 'H'},
  {"port", 1, NULL, 'P'},
  {"debug", 0, NULL, 'D'},
  {"email-addr", 1, NULL, 'e'},
  {"global-databases-file",1,NULL,'g'},
  {NULL, 0, NULL, 0}
};

#define PROGRAM_NAME "pr-edit"

static const char *const USAGE[] = {
  "Usage: " PROGRAM_NAME " [OPTION]... [PR]\n\
Modify database.\n\
\n",
"Actions:\n\
  -l --lock=USER           lock the given PR\n\
  -u --unlock              unlock the given PR\n\
  -L --lockdb              lock the whole database\n\
  -U --unlockdb            unlock the database\n\
  -c --check               check input for editting, don't change PR\n\
  -C --check-initial       check input for submission, don't submit new PR\n\
  -s --submit              submit new PR\n",
  "\
     --show-prnum          display the newly created PR number (for --submit)\n\
  -a --append=FIELDNAME    append input to FIELDNAME\n\
  -r --replace=FIELDNAME   replace FIELDNAME with input\n\
     --delete-pr           delete PR from the database completely\n\
  -R --reason=REASON       change describing text required by some fields\n\
\n",
"Connection options:\n\
  -d --database=DATABASE   use DATABASE\n\
  -H --host=SERVER         connect to SERVER\n\
  -P --port=PORT           connect to the port PORT\n\
  -v --user=NETID          use NETID as the username on the remote server\n\
  -w --passwd=PASSWORD     user's password\n\
  -e --email-addr=ADDRESS  send contingent e-mail reports to ADDRESS\n\
  -p --process=PID         PID of the connecting process\n\
  -f --filename=FILENAME   read input from FILENAME instead of stdin\n\
\n",
"Other options:\n\
  -h --help                display this help and exit\n\
  -V --version             output version information and exit\n\
  -D --debug               enable some debugging\n\
  -g --global-databases-file=PATH\n\
                           PATH to databases file\n",
  NULL
};   


/* Read the entire contents of the file referenced by INP into memory, and
   return it as a char* string allocated with malloc ().  */

static char *
read_file (FILE *inp)
{
  char *res = xmalloc (512);  /* 512 is just a number.  It's not a limit of
				 any sort.  */
  size_t reslen = 512;
  size_t totlen = 0, inplen;

  while ((inplen = fread (res + totlen, 1, reslen - totlen, inp)) != 0)
    {
      totlen += inplen;
      if (totlen == reslen)
	{
	  reslen += 512;
	  res = xrealloc (res, reslen);
	}
    }
  if (totlen == reslen)
    {
      reslen++;
      res = xrealloc (res, reslen);
    }
  if (inp != stdin)
    {
      fclose (inp);
    }
  res[totlen] = '\0';
  return res;
}

static void
handleNetworkEdit (int edit_options, FILE *fpin, char *prnum, char *username,
		   char *editEmailAddr, char *processid, char *fieldname,
		   char *reason, int show_prnum)
{
  int exitcode = 0;

  switch (edit_options)
    {
    case LOCK:
      netLockPR (prnum, username, processid);
      break;
    case UNLOCK:
      netUnlockPR (prnum);
      break;
    case APPEND:
      netEditField (fpin, prnum, fieldname, editEmailAddr, 1, reason);
      break;
    case REPLACE:
      netEditField (fpin, prnum, fieldname, editEmailAddr, 0, reason);
      break;
    case SUBMIT:
      {
	netSubmitNewPR (fpin, show_prnum);
	break;
      }
    case CHECK:
    case CHECK_INITIAL:
      {
	netCheckPR (fpin, edit_options == CHECK_INITIAL ? 1 : 0);
	break;
      }
    case LOCKDB:
      netLockDB ();
      break;
    case UNLOCKDB:
      netUnlockDB ();
      break;
    case DELETE:
      netDeletePR (prnum, editEmailAddr);
      break;
    default:
      {
	netModifyPR (fpin, prnum, editEmailAddr);
	break;
      }
    }
  client_exit ();
  exit (exitcode);
}

extern int debug;

int
main (int argc, char **argv)
{
  DatabaseInfo database = NULL;
  FILE *fp = stdin;
  int option;
  char *prnum = NULL;
  char *username = (char *)NULL, *processid = (char *)NULL;
  /* Default to 1, a passing value.  */
  int result = 1;
  ErrorDesc err;
  int db_locked = 0;
  char *fieldname = NULL;
  char *nameOfDatabase = NULL;
  char *netid = NULL;
  char *passwd = NULL;
  char *host = NULL;
  int port = -1;
  int networkmode = 0;
  char *editUserEmailAddr = NULL;
  char *reason = NULL;
  int show_prnum = 0;

  program_name = basename (argv[0]);
  edit_options = MODIFY;

  while ((option = getopt_long (argc, argv, "l:uLUDhcp:d:g:f:VFv:w:H:P:e:",
				long_options, (int *)0)) != EOF)
    {
      switch (option)
	{
	case 'd':
	  nameOfDatabase = optarg;
	  break;

	case 'g':
	  if (optarg[0] == '\0')
	    {
              fprintf(stderr, "%s: filename must be non-null\n", program_name);
              exit(1);
	    }
	  global_db_list_file = optarg;
	  break;

	case 'D':
	  debug = 1;
	  break;

	case SHOW_PRNUM_OPT:
	  show_prnum = 1;
	  break;

	case 'f':
	  fp = fopen (optarg, "r");
	  /* If they gave a bogus argument, then exit right away; we don't
	     want to send mail to gnats-admin every time someone tries to
	     edit a bogus PR number.  */
	  if (fp == (FILE *)NULL)
            {
              fprintf (stderr, "%s: Bad PR file %s.\n", program_name, optarg);
              exit (2);
            }
	  break;

	case 'p':
	  processid = optarg;
	  break;

	case 'l': 
	  edit_options = LOCK;
	  username = optarg;
	  break;

	case 'u':
	  edit_options = UNLOCK;
	  break;

	case 'L':
	  edit_options = LOCKDB;
	  break;

	case 'U':
	  edit_options = UNLOCKDB;
	  break;

	case 'c':
	  edit_options = CHECK;
	  break;

	case 'C':
	  edit_options = CHECK_INITIAL;
	  break;

	case 's':
	  edit_options = SUBMIT;
	  break;

	case 'a':
	  edit_options = APPEND;
	  fieldname = optarg;
	  break;

	case 'r':
	  edit_options = REPLACE;
	  fieldname = optarg;
	  break;

	case 'R':
	  reason = optarg;
	  break;
	  
	case DELETE_PR_OPT:
	  edit_options = DELETE;
	  break;

	case 'V':
	  version (PROGRAM_NAME);
	  exit (0);
	  break;
	  
	case 'h':
	  usage (USAGE, 0);
	  break;

	case 'H':
	  host = optarg;
	  networkmode = 1;
	  break;

	case 'P':
	  port = atoi (optarg);
	  networkmode = 1;
	  break;

        case 'v':
          netid = optarg;
	  networkmode = 1;
          break;

        case 'w':
          passwd = optarg;
	  networkmode = 1;
          break;

	case 'e':
	  editUserEmailAddr = xstrdup (optarg);
	  break;

	default:
	  usage (USAGE, 3);
	  break;
	}
    }

  /* if there is another arg, then we take it to be a file name. */
  if (optind < argc)
    {
      prnum = argv[optind];
    }

  if (prnum == NULL
      && (edit_options == MODIFY
	  || edit_options == APPEND
	  || edit_options == REPLACE))
    {
      fprintf (stderr, "%s: Must supply the PR number when modifying a PR\n",
	       program_name);
      exit (1);
    }

  if (editUserEmailAddr == NULL)
    {
      struct passwd *myent = getpwuid (getuid ());
      if (myent != NULL)
	{
	  char hostname[256];

	  gethostname (hostname, sizeof (hostname));
	  asprintf (&editUserEmailAddr, "%s@%s", myent->pw_name, hostname);
	}
      else
	{
	  fprintf (stderr, "%s: no username for uid %d\n", program_name,
		   (int) getuid ());
	  exit (1);
	}
    }

  if ((edit_options == LOCK) && (prnum == NULL || ! username))
    {
      fprintf (stderr,
	       "%s: Must lock with PR number, username, and process locking it.\n",
               program_name);
      exit (2);
    }

  if ((edit_options == UNLOCK) && (prnum == NULL))
    {
      fprintf (stderr, "%s: Must unlock with PR number.\n",
               program_name);
      exit (2);
    }

  if (! networkmode)
    {
      networkmode = gnatsdbHasNetconn (nameOfDatabase);
    }

  if (networkmode)
    {
      if (client_init_gnats (&err, netid, passwd, host, port,
			     nameOfDatabase) != 0)
	{
	  client_print_errors (database, err);
	  exit (3);
	}
      handleNetworkEdit (edit_options, fp, prnum, username, editUserEmailAddr,
			 processid, fieldname, reason, show_prnum);
    }

  database = init_gnats (program_name, nameOfDatabase, &err);
  if (database == NULL)
    {
      result = 0;
    }
  else
    {
      umask (022);

      block_signals ();

      if (edit_options == LOCKDB
	  || edit_options == APPEND
	  || edit_options == REPLACE
	  || edit_options == MODIFY
	  || edit_options == SUBMIT)
	{
	  result = (client_lock_gnats (database, &err) == 0);
	  if (result)
	    {
	      db_locked = 1;
	    }
	}
    }
  
  if (result)
    {
      switch (edit_options) 
	{
	case LOCK:
	  if (pr_exists (database, prnum, &err))
	    {
	      result = lock_pr (database, prnum, username, processid, &err);
	      if (result != 0)
		{
		  PR *pr = pr_load_by_id (database, prnum, 0, &err);
		  if (pr != NULL)
		    {
		      print_named_format_pr (stdout, pr, "edit", "\n", &err);
		      free_pr (pr);
		    }
		  else
		    {
		      if (unlock_pr (database, prnum, &err) != 0)
			{
			  /* ??? XXX !!! */
			  setError (&err, CODE_NONEXISTENT_PR,
				    "Invalid PR %s.", prnum);
			}
		      result = 0;
		    }
		}
	    }
	  else
	    {
	      result = 0;
	    }
	  break;

	case UNLOCK:
	  result = unlock_pr (database, prnum, &err);
	  break;

	case SUBMIT:
	  {
	    result = submit_pr (database, fp, &err);
            if (show_prnum && result)
              {
                fprintf (stdout, "%d\n", result);
              }
	    break;
	  }
	case APPEND:
	  {
	    char *contents = read_file (fp);
	    FieldIndex field = find_field_index (database, fieldname);
	    if (field == InvalidFieldIndex)
	      {
		setError (&err, CODE_INVALID_FIELD_NAME,
			  "No such field as %s", fieldname);
		result = 0;
	      }
	    else
	      {
		result = edit_field (database, prnum, field, 1,
				     contents, reason,
				     editUserEmailAddr, &err);
	      }
	    break;
	  }
	case REPLACE:
	  {
	    char *contents = read_file (fp);

	    FieldIndex field = find_field_index (database, fieldname);
	    if (field == InvalidFieldIndex)
	      {
		setError (&err, CODE_INVALID_FIELD_NAME,
			  "No such field as %s", fieldname);
		result = 0;
	      }
	    else
	      {
		result = edit_field (database, prnum, field, 0, 
				     contents, reason,
				     editUserEmailAddr, &err);
	      }
	    break;
	  }
	case DELETE:
	  {
	    struct passwd *pwent = getpwnam (GNATS_USER);

	    if (pwent == NULL)
	      {
		setError (&err, CODE_ERROR,
			  "No such user as %s\n", GNATS_USER);
		result = 0;
	      }
	    else if ((int) pwent->pw_uid != (int) getuid ())
	      {
		setError (&err, CODE_FILE_ERROR,
			  "Must be user %s to delete a PR\n", GNATS_USER);
		result = 0;
	      }
	    else
	      {
		result = ! deletePR (database, prnum, editUserEmailAddr, &err);
	      }
	    break;
	  }
	case CHECK:
	case CHECK_INITIAL:
	  result = check_pr_file (database, fp, &err, 
				  edit_options == CHECK_INITIAL);
	  break;
	case LOCKDB:
	case UNLOCKDB:
	  result = 1; /* so that the program exits with 0 status */
	  break;

	default: 
	  result = replace_pr (database, fp, editUserEmailAddr, &err);
	  break;
	}
    }

  if (db_locked || edit_options == UNLOCKDB)
    {
      client_unlock_gnats ();
    }

  if (! result)
    {
      client_print_errors (database, err);
    }

  unblock_signals ();

  if (editUserEmailAddr != NULL)
    {
      free (editUserEmailAddr);
      editUserEmailAddr = NULL;
    }

  exit (result ? 0 : 2);
}
