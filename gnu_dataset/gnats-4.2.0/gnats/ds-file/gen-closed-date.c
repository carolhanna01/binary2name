/* Generate the Closed-Date field for an existing GNATS database.
   Copyright (C) 1993, 1995, 2001, 2002, 2007 Free Software Foundation, Inc.
   Contributed by Rick Macdonald (rickm@vsl.com).
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
along with GNU GNATS; see the file COPYING.  If not, see
<http://www.gnu.org/licenses/>.
*/

#include "gnats.h"
#include "gnats-dirs.h"

#include <utime.h>

/* The name this program was run with.  */
const char *program_name;

/* If 1, we're running in test mode and PR files are _not_ actually updated.  */
int test_mode = 0;

struct option long_options[] =
{
  {"database", 1, NULL, 'd'},
  {"global-databases-file",1,NULL,'g'},
  {"outfile", 1, NULL, 'o'},
  {"help", 0, NULL, 'h'},
  {"version", 0, NULL, 'V'},
  {"test", 0, NULL, 't'},
  {NULL, 0, NULL, 0}
};

#define PROGRAM_NAME "gen-closed-date"

static const char *const USAGE[] = {
  "Usage: " PROGRAM_NAME " [OPTION]...\n\
Generate the Closed-Date field for an existing GNATS database.\n\
\n"
  "\
  -d --database=DATABASE  use DATABASE\n\
  -g --global-databases-file=PATH\n\
                          PATH to databases file\n\
  -o --outfile=FILENAME   output the date to FILENAME\n\
  -t --test               report changes to be made, but don't change the files\n\
\n\
  -h --help               display this help and exit\n\
  -V --version            output version information and exit\n",
  NULL};
	   
typedef struct entry
{
  unsigned int number;
  char *string;
} Entry;

static int
get_closed (PR *pr, ErrorDesc *err)
{
  const char *final1, *final2, *from_start, *to_start;
  char from[32], to[32];
  const char *p, *c, *when_start;
  char when[133];
  char *new_audit;
  const char *copy_ptr;
  int  len, from_len, to_len, closed_date_set = 0, changed_separator;

  p = field_value (pr, AUDIT_TRAIL (pr->database));

  if (p == NULL || strlen (p) == 0) 
    {
      /* No Audit-Trail at all; initialize Closed-Date */
      set_field (pr, CLOSED_DATE (pr->database), "", err);
      return 1;
    }

  new_audit = xmalloc (strlen (p) * 2);
  new_audit[0] = '\0';
  copy_ptr = p;

  /* Skip ahead to the last time it was closed.  */
  final2 = p;
  do
    {
      final1 = strstr (final2, "State-Changed-From-To:");
      if (final1 != NULL)
	{
	  final1 += 22;
	  while (final1[0] != '\0' && isspace ((int)(unsigned char) final1[0]))
	    {
	      final1++;
	    }
	  from_start = final1;
	  while (final1[0] != '\0' && final1[0] != '-')
	    {
	      final1++;
	    }
	  from_len = final1 - from_start;
	  changed_separator = 0;
	  if (final1[0] != '\0' && final1[1] == '>') 
	    {
	      final1 += 2;
	    } 
	  else 
	    {
	      /* Change - to -> here */
	      final1++;
	      strncat (new_audit, copy_ptr, final1 - copy_ptr);
	      strcat  (new_audit, ">");
	      copy_ptr = final1;
	      changed_separator = 1;
	    }
	  to_start = final1;
	  while (final1[0] != '\0' && !isspace ((int)(unsigned char) final1[0]))
	    {
	      final1++;
	    }
	  to_len = final1 - to_start;
	  strncpy (from, from_start, from_len);
	  from[from_len] = '\0';
	  strncpy (to, to_start, to_len);
	  to[to_len] = '\0';
	  final2 = final1;

	  if (test_mode && changed_separator) 
	    {
	      printf ("\t%s->%s\n", from, to);
	    }

	  if (check_state_type (pr->database, from, "closed") !=
	      check_state_type (pr->database, to, "closed"))
	    {
	      if (check_state_type (pr->database, from, "closed"))
		{
		  /* No longer closed; clear the Closed-Date field */
		  set_field (pr, CLOSED_DATE (pr->database), "", err);
		}
	      else 
		{
		  /* Now closed; set the Closed-Date field */
		  /* Grab the When date str */
		  final1 = strstr (final2, "State-Changed-When:");
		  if (final1 != NULL) 
		    {
		      final1 += 19;
		      while (final1[0] != '\0' && isspace ((int)(unsigned char) final1[0]))
			{
			  final1++;
			}
		      when_start = final1;
		      c = strchr (final1, '\n');
		      final2 = c;
		      while (c >= final1 && isspace ((int)(unsigned char) c[0]))
			{
			  c--;
			}
		      c++;
		      len = c - when_start;
		      (void) strncpy (when, when_start, len);
		      when[len] = '\0';
		      set_field (pr, CLOSED_DATE (pr->database), when, err);
		    }
		  final1 = final2; /* Just to keep the while loop going */
		}
	      if (test_mode) 
		{
		  printf("\t%s/%s Closed-Date: %s\n", 
			 field_value (pr, NUMBER (pr->database)),
			 field_value (pr, CATEGORY (pr->database)),
			 field_value (pr, CLOSED_DATE (pr->database)));
		}
	      closed_date_set = 1;
	    }
	}
    } while (final1 != NULL);

  strcat (new_audit, copy_ptr);
  set_field (pr, AUDIT_TRAIL (pr->database), new_audit, err);
  free (new_audit);

  if (! closed_date_set) 
    {
      /* No state changes at all; initialize Closed-Date */
      set_field (pr, CLOSED_DATE (pr->database), "", err);
    }

  return 1;
}

/* For a given category C, go into its directory and either emit an
   index entry for each file in it, or swallow its index entry so we
   can sort it later.  */
static void
do_category (DatabaseInfo database, const char *c)
{
  DIR *d;
  struct dirent *next;
  struct stat    stbuf;
  struct utimbuf utbuf;
  FILE *fp;
  size_t len = strlen (databaseDir (database)) + 1 + strlen (c) + 2;
  /* Allocate for a PR with 20 digits.  */
  char *path = (char *) xmalloc (len + 21);
  char *p;
  ErrorDesc err;

  errno = 0;
  if (chdir (databaseDir (database)) < 0)
    {
      fprintf (stderr, "%s: can't read the database directory %s: %s\n",
	       program_name, databaseDir (database), strerror (errno));
      exit (3);
    }

  if (c == NULL || *c == '\0')
    {
      free (path);
      return;
    }

  errno = 0;
  d = opendir (c);
  if (d == 0)
    {
      fprintf (stderr, "%s: can't open the category directory %s/%s: %s\n",
	       program_name, databaseDir (database), c, strerror (errno));
      free (path);
      return;
    }

  sprintf (path, "%s/%s/", databaseDir (database), c);

  /* Process each file in the directory; ignore files that have periods
     in their names; either they're the . and .. dirs, or they're a
     lock file (1234.lock).  */
  while ((next = readdir (d)))
    {
      if (strchr (next->d_name, '.') == NULL)
	{
	  PR *pr;

	  p = path + len - 1;
	  strcat (p, next->d_name);

	  fp = fopen (path, "r");
	  if (fp == (FILE *) NULL)
	    {
	      fprintf (stderr, "%s: couldn't read pr %s: %s\n",
		       program_name, path, strerror (errno));

	      *p = '\0';
	      continue;
	    }

	  pr = allocPR (database);

	  if (read_header (pr, fp) < 0)
	    {
	      fprintf (stderr, "%s: error reading pr %s\n", program_name,
		       path);
	      *p = '\0';
	      free_pr (pr);
	      continue;
	    }

	  read_pr (pr, fp, 0);
	  fclose (fp);

	  get_closed (pr, &err);

	  printf("%s/%s Closed-Date: %s\n",
		 field_value (pr, NUMBER (pr->database)),
		 field_value (pr, CATEGORY (pr->database)),
		 field_value (pr, CLOSED_DATE (pr->database)));

	  if (!test_mode)
	    {
	      /* Reset the file mtime after writing it out */
	      stat (path, &stbuf);
	      if (createPrFile (pr, path, 1, &err) != 0)
		{
		  client_print_errors (pr->database, err);
		}
	      utbuf.actime = utbuf.modtime = stbuf.st_mtime;
	      utime (path, &utbuf);
	    }

	  *p = '\0';
	  free_pr (pr);
	}
    }

  closedir (d);
  free (path);
}
  
int
main (int argc, char **argv)
{
  int optc;
  AdmEntry *clist, *c;
  FILE *output = NULL; /* ??? */
  char *database_name = NULL;
  ErrorDesc err;
  DatabaseInfo database = NULL;

  program_name = (char *) basename (argv[0]);

  while ((optc = getopt_long (argc, argv, "d:g:ho:tV",
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

	case 'h':
	  usage (USAGE, 0);
	  break;

	case 'o':
	  output = fopen (optarg, "w+");
	  if (output == (FILE *) NULL)
	    {
	      fprintf (stderr, "%s: can't write to %s: %s\n", program_name,
		       optarg, strerror (errno));
	      exit (3);
	    }
	  break;

	case 't':
	  test_mode = 1;
	  break;

	case 'V':
	  version (PROGRAM_NAME);
	  break;

	default:
	  usage (USAGE, 1);
	}
    }

  database = init_gnats (program_name, database_name, &err);
  if (! databaseValid (database))
    {
      client_print_errors (database, err);
      exit (1);
    }

  umask (022);

  /* lock the whole thing. */
  if (client_lock_gnats (database, &err) != 0)
    {
      client_print_errors (database, err);
      exit (1);
    }

  clist = build_chain (CATEGORY (database));

  for (c = clist ; c ; c = c->next)
    {
      do_category (database, c->admFields[CategoryAdmKey]);
    }

  if (output != NULL)
    {
      fclose (output);
    }
  
  /* unlock the whole thing. */
  client_unlock_gnats ();

  exit (0);
}
