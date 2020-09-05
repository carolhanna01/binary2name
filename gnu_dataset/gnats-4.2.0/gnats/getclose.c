/* List PRs that were closed between certain dates.
   Copyright (C) 2001 Milan Zamazal
   Copyright (C) 1996, 1997 Free Software Foundation, Inc.
   Contributed by Brendan Kehoe (brendan@cygnus.com).
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

/* Initial version.  To use this to find the PRs fixed in a particular
   release, run it as
      getclose migration freeze lastfreeze
   where the options are
      migration   -- date of the migration for the current release
      freeze      -- date of the freeze for the current release
      lastfreeze  -- date of the freeze for the *previous* release
   The results will be in the form
      FIXED:foo:1234:the synopsis
      MAYBE:bar:1236:the synopsis
   Those marked as FIXED were closed before the migration into the new release.
   Those marked as MAYBE were closed after migration, but before freeze; thus,
   the fix may or may not have made it into the current release.  These need
   to be researched to find out if the changes, if any, were included. */

#include "gnats.h"
#include "query.h"
#include "ds.h"

/* The name this program was run with.  */
const char *program_name;

/* If 1, print the name of each PR that we look at.  */
int verbose;

/* When we migrated, froze the migrated code, and finally, when we
   froze the last release.  */
time_t migration, freeze, lastfreeze;

struct option long_options[] =
{
  {"database", 1, NULL, 'd'},
  {"global-databases-file", 1, NULL, 'g'},
  {"verbose", 0, NULL, 'v'},
  {"version", 0, NULL, 'V'},
  {"host", 1, NULL, 'H'},
  {"port", 1, NULL, 'P'},
  {"user", 1, NULL, 'u'},
  {"passwd", 1, NULL, 'w'},
  {NULL, 0, NULL, 0}
};

#define PROGRAM_NAME "getclose"

static const char *const USAGE[] = {
  "Usage: " PROGRAM_NAME " [OPTION]... MIGRATION FREEZE LASTFREEZE\n",
  "Find PRs fixed in a particular release.\n\
  MIGRATION  -- date of the migration for the current release\n\
  FREEZE     -- date of the freeze for the current release\n\
  LASTFREEZE -- date of the freeze for the previous release\n\
\n",
  "The results will be in the form\n\
   FIXED:foo:1234:the synopsis\n\
   MAYBE:bar:1236:the synopsis\n\
Those marked as FIXED were closed before the migration into the current\n\
release.\n\
Those marked as MAYBE were closed after migration, but before freeze; thus,\n\
the fix may or may not have made it into the current release.  These need\n\
to be researched to find out if the changes, if any, were included.\n\
\n",
  "Connection options:\n\
  -d --database=DATABASE   use DATABASE\n\
  -H --host=SERVER         connect to SERVER\n\
  -P --port=PORT           connect to the port PORT\n\
  -v --user=NETID          use NETID as the username on the remote server\n\
  -w --passwd=PASSWORD     user's password\n\
\n",
  "Other options:\n\
  -g --global-databases-file=PATH\n\
                           PATH to databases file\n\
  -v --verbose             print the name of each PR as it is checked\n\
  -h --help                display this help and exit\n\
  -V --version             output version information and exit\n",
  NULL};


static time_t
got_closed (const char *p)
{
  if (p != NULL)
    {
      const char *final1, *final2;
      char *c;
      /* Skip ahead to the last time it was closed.  */
      final1 = p;
      do
	{
	  final2 = final1;
	  final1 = strstr (final2, "-closed\n");
	  if (final1)
	    final1 += 8;
	} while (final1);
      if (! final2)
	return (time_t)0;

      /* Grab the date, and return the time it represents.  */
      final1 = strstr (final2, "When: ");
      if (! final1)
	return (time_t)0;
      c = strchr (final1 + 6, '\n');
      if (c)
	*c = '\0';

      return get_date ((char *)(final1 + 6), NULL);
    }
  else
    {
      return 0;
    }
}

static void
do_stat (PR *pr)
{
  /* XXX ??? Check the final argument */
  time_t closed = got_closed (field_value (pr, AUDIT_TRAIL (pr->database)));
      
  if (verbose)
    {
      fprintf (stderr, "Checking PR %s\n", 
	       field_value (pr, NUMBER (pr->database)));
    }
  /* We don't care about ones that had no closed status in its
     audit trail.  */
  if (!closed)
    return;
  
  if (verbose)
    {
      fprintf (stderr, "PR closed on date %d\n", (int)closed);
    }
  /* fixed in this release */
  if ((closed > lastfreeze) && (closed < migration))
    {
      printf ("FIXED:%s:%s:%s\n", field_value (pr, CATEGORY (pr->database)),
	      field_value (pr, NUMBER (pr->database)), 
	      field_value (pr, SYNOPSIS (pr->database)));
    }
  /* maybe, maybe not */
  else if ((closed >= migration) && (closed <= freeze))
    {
      printf ("MAYBE:%s:%s:%s\n", field_value (pr, CATEGORY (pr->database)),
	      field_value (pr, NUMBER (pr->database)),
	      field_value (pr, SYNOPSIS (pr->database)));
    }
}

static void
process_pr (int num ATTRIBUTE_UNUSED, PR *pr,
	    QueryFormat *data ATTRIBUTE_UNUSED)
{
  ErrorDesc err;

  if (pr == NULL)
    {
      return;
    }

  if (pr_load (pr, &err) == 0)
    {
      do_stat (pr);
      free_pr_contents (pr);
    }
  else
    {
      client_print_errors (pr->database, err);
      fprintf (stderr, "Unable to read PR %s\n",
	       field_value (pr, NUMBER (pr->database)));
    }
}

static void
do_prlist (const DatabaseInfo database)
{
  QueryExpr expr;
  ErrorDesc err;
  const char *query_expr = "(builtinfield:State[type] ~ \"closed\") & (builtinfield:Confidential ~ \"no\")";

  expr = parseQueryExpression (database, query_expr, NULL);
  if (db_query (database, 0, NULL, expr, process_pr, NULL, &err) < 0)
    {
      client_print_errors (database, err);
    }
  freeQueryExpr (expr);
}

static void
do_netprlist (void)
{
  StringList *prs;
  const char *expr = "(! builtinfield:State[type] ~ \"closed\") & ([builtinfield:Confidential ~ \"no\")";

  for (prs = clientGetPRList (expr); prs != NULL ; prs = prs->next)
    {
      PR *pr = clientReadPR (prs->name);

      if (pr != NULL)
	{
	  do_stat (pr);
	  free_pr (pr);
	}
      else
	{
	  fprintf (stderr, "Unable to read PR %s\n", prs->name);
	}
    }
}

int failed_dates = 0;
static time_t
procdate (char *d)
{
  time_t t;

  t = get_date (d, NULL);
  if (!t || t == -1)
    {
      fprintf (stderr, "%s: invalid date `%s'\n",
	       program_name, d);
      failed_dates = 1;
    }

  return t;
}

int
main (int argc, char **argv)
{
  int optc;
  char *database_name = NULL;
  int networkmode = 0;
  char *hostname = NULL;
  int port = -1;
  char *user = NULL;
  char *passwd = NULL;

  program_name = basename (argv[0]);
  verbose = 0;

  while ((optc = getopt_long (argc, argv, "hvVd:g:H:P:u:w:",
			      long_options, (int *) 0)) != EOF)
    {
      switch (optc)
	{
	case 'd':
	  database_name = optarg;
	  break;

	case 'g':
	  if (optarg[0] == '\0')
	    {
              fprintf(stderr, "%s: filename must be non-null\n", program_name);
              exit(1);
	    }
	  global_db_list_file = optarg;
	  break;

	case 'V':
	  version (PROGRAM_NAME);
	  exit (0);
	  break;

	case 'v':
	  verbose = 1;
	  break;

	case 'h':
	  usage (USAGE, 0);
	  break;

	case 'H':
	  hostname = optarg;
	  networkmode = 1;
	  break;

	case 'P':
	  port = atoi (optarg);
	  networkmode = 1;
	  break;

        case 'u':
          user = optarg;
	  networkmode = 1;
          break;

        case 'w':
          passwd = optarg;
	  networkmode = 1;
          break;

	default:
	  usage (USAGE, 1);
	}
    }

  if (optind == argc || optind > 3)
    usage (USAGE, 1);

  migration = procdate (argv[optind++]);
  freeze = procdate (argv[optind++]);
  lastfreeze = procdate (argv[optind++]);

  if (failed_dates)
    exit (1);

  if (verbose)
    {
      fprintf (stderr, "Migration date: %d\n", (int) migration);
      fprintf (stderr, "Freeze date:    %d\n", (int) freeze);
      fprintf (stderr, "Last freeze:    %d\n", (int) lastfreeze);
    }

  if (! networkmode)
    {
      networkmode = gnatsdbHasNetconn (database_name);
    }

  if (! networkmode)
    {
      ErrorDesc err;
      DatabaseInfo database;

      database = init_gnats (program_name, database_name, &err);
      if (database == NULL)
	{
	  client_print_errors (database, err);
	  exit (1);
	}
  
      do_prlist (database);
    }
  else
    {
      ErrorDesc err;

      if (client_init_gnats (&err, user, passwd, hostname, port, 
			     database_name) != 0)
	{
	  client_print_errors (NULL, err);
	  exit (1);
	}
      do_netprlist ();
    }

  exit (0);
}
