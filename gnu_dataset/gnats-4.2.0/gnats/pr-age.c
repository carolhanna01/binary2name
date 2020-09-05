/* Give the age of a PR.
   Copyright (C) 2001 Milan Zamazal
   Copyright (C) 1994, 95, 96, 1997, 2007 Free Software Foundation, Inc.
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

#include "gnats.h"
#include "query.h"
#include "ds.h"

/* The name this program was run with.  */
const char *program_name;

typedef struct _times
{
  int days;
  int hours;
} Times;

struct option long_options[] =
{
  {"database", 1, NULL, 'd'},
  {"global-databases-file",1,NULL,'g'},
  {"host", 1, NULL, 'H'},
  {"port", 1, NULL, 'P'},
  {"user", 1, NULL, 'v'},
  {"passwd", 1, NULL, 'w'},
  {"version", 0, NULL, 'V'},
  {"help", 0, NULL, 'h'},
  {NULL, 0, NULL, 0}
};

#define PROGRAM_NAME "pr-age"

static const char *const USAGE[] = {
  "Usage: " PROGRAM_NAME " [OPTION]... PR\n\
Print days and hours passed since arrival of PR.\n\
\n",
  "Connection options:\n\
  -d --database=DATABASE   use DATABASE\n\
  -g --global-databases-file=PATH\n\
                           use PATH to global databases file\n\
  -H --host=SERVER         connect to SERVER\n\
  -P --port=PORT           connect to the port PORT\n\
  -v --user=NETID          use NETID as the username on the remote server\n\
  -w --passwd=PASSWORD     user's password\n\
\n",
  "Other options:\n\
  -h --help                display this help and exit\n\
  -V --version             output version information and exit\n",
  NULL};


static Times *
time_diff (time_t then)
{
  Times *t = (Times *) xmalloc (sizeof (Times));
  time_t seconds = time (0);
  time_t diff = seconds - then;
  time_t days, hours;

  days = (diff / 3600) / 24;
  diff -= (days * 60 * 60 * 24);
  hours = diff / 3600;

  t->days = days;
  t->hours = hours;
  return t;
}

static void
report_age (PR *pr)
{
  const char *dateStr = field_value (pr, ARRIVAL_DATE (pr->database));
  time_t then;
  Times *t;

  if (dateStr == NULL)
    {
      dateStr = "";
    }
  then = get_date ((char *) dateStr, NULL);
  if (then == 0)
    {
      fprintf (stderr, "%s: don't know the time for %s\n",
	       program_name, field_value (pr, NUMBER (pr->database)));
      exit (1);
    }
  t = time_diff (then);
  printf ("%3d %2d\n", t->days, t->hours);
  free ((char*)t);
}

int
main (int argc, char **argv)
{
  int optc;
  char *nameOfDatabase = NULL;
  char *user = NULL;
  char *passwd = NULL;
  char *hostname = NULL;
  int port = -1;
  int networkmode = 0;
  PR *pr;
  ErrorDesc err;

  program_name = basename (argv[0]);

  while ((optc = getopt_long (argc, argv, "hVd:g:H:P:v:w:",
			      long_options, (int *) 0)) != EOF)
    {
      switch (optc)
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

        case 'v':
          user = optarg;
	  networkmode = 1;
          break;

	case 'V':
	  version (PROGRAM_NAME);
	  exit (0);
	  break;

        case 'w':
          passwd = optarg;
	  networkmode = 1;
          break;

	default:
	  usage (USAGE, 1);
	}
    }

  if (optind == argc || optind >= argc)
    usage (USAGE, 1);

  if (! networkmode)
    {
      networkmode = gnatsdbHasNetconn (nameOfDatabase);
    }

  if (! networkmode)
    {
      DatabaseInfo database = init_gnats (program_name, nameOfDatabase, &err);
      if (database == NULL)
	{
	  client_print_errors (NULL, err);
	  exit (1);
	}
      pr = pr_load_by_id (database, argv[optind], 0, &err);
      if (pr == NULL)
	{
	  client_print_errors (database, err);
	  exit (1);
	}
    }
  else
    {
      if (client_init_gnats (&err, user, passwd, hostname, port, 
			     nameOfDatabase) != 0)
	{
	  client_print_errors (NULL, err);
	  exit (1);
	}
      pr = clientReadPR (argv[optind]);
    }
  if (pr != NULL)
    {
      report_age (pr);
    }
  exit (0);
}
