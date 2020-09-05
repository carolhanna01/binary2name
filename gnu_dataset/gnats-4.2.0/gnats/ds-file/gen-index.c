/* Generate an index file for an existing GNATS database.
   Copyright (C) 1993, 1995, 1999, 2001, 2002 Free Software Foundation, Inc.
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
#include "gnats-dirs.h"
#include "ds-file.h"

/* The name this program was run with.  */
const char *program_name;

/* Where the index file should be output. */
FILE *output = NULL;

/* If TRUE, we should sort the index list by number.  */
int numeric_sorting = FALSE;

#define PROGRAM_NAME "gen-index"

static const char *const USAGE[] = {
  "Usage: " PROGRAM_NAME " [OPTION]...\n\
Generate new database index.\n\
\n",
  "\
  -d --database=DATABASE    use DATABASE\n\
  -g --global-databases-file=PATH   PATH to databases file\n\
  -o --outfile=FILENAME     output the index to FILENAME\n\
  -n --numerical            perform numerical sort\n\
  -i --import               read in the existing index instead of PRs\n\
  -e --export               force outputting the index in the text form\n\
\n\
  -h --help                 display this help and exit\n\
  -V --version              output version information and exit\n",
  NULL};

typedef struct entry
{
  unsigned int number;
  char *string;
  size_t length;
} Entry;

#define ENTRYNUMINCREMENT 1024
Entry *entries = NULL;
int entryArraySize = 0;
int num_entries = 0;


static int
entry_cmp (const void *e1a, const void *e2a)
{
  const Entry *e1 = e1a;
  const Entry *e2 = e2a;

  return e1->number - e2->number;
}

static void
add_pr (const DatabaseInfo database, PR *pr)
{
  char *line = NULL;
  size_t lineLen;
  
  if (indexIsBinary (database))
    {
      line = createIndexEntryBinary (pr, &lineLen);
    }
  else
    {
      line = createIndexEntry (pr, &lineLen);
    }
  if (line != NULL)
    {
      if (numeric_sorting)
	{
	  if (num_entries >= entryArraySize)
	    {
	      entryArraySize += ENTRYNUMINCREMENT;
	      entries
		= (Entry *) xrealloc ((char *) entries,
				      entryArraySize * sizeof (Entry));
	    }

	  entries[num_entries].number
	    = atoi (field_value (pr, NUMBER (pr->database)));
	  entries[num_entries].string = line;
	  entries[num_entries].length = lineLen;
	  num_entries++;
	}
      else
	{
	  fwrite (line, 1, lineLen, output);
	  free (line);
	}
    }
}

/* For a given category C, go into its directory and either emit an
   index entry for each file in it, or swallow its index entry so we
   can sort it later.  */
static void
do_category (DatabaseInfo database, const char *c)
{
  DIR *d;
  struct dirent *next;
  FILE *fp;
  int len = strlen (databaseDir (database)) + 1 + strlen (c) + 2;
  /* Allocate for a PR with 20 digits.  */
  char *path = (char *) xmalloc (len + 21);

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
  if (d == NULL)
    {
      if (! createCategoryDirs (database))
	{
	  fprintf (stderr, "%s: can't open the category directory %s/%s: %s\n",
		   program_name, databaseDir (database), c, strerror (errno));
	}
      free (path);
      return;
    }

  sprintf (path, "%s/%s/", databaseDir (database), c);

  /* Process each file in the directory; ignore files that have periods
     in their names; either they're the . and .. dirs, or they're a
     lock file (1234.lock).  */
  while ((next = readdir (d)))
    if (strchr (next->d_name, '.') == NULL && strlen (next->d_name) < 20)
      {
	PR *pr;
	char *p = path + len - 1;

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
	    fprintf (stderr, "%s: error reading pr %s\n", program_name, path);
	    *p = '\0';
	    free_pr (pr);
	    continue;
	  }

	read_pr (pr, fp, 1);
	fclose (fp);

	{
	  const char *n = field_value (pr, NUMBER (pr->database));
	  if (n == NULL || n[0] == '-')
	    {
	      fprintf (stderr, "%s: defective pr %s\n", program_name, path);
	      *p = '\0';
	      free_pr (pr);
	      continue;
	    }
	}

	*p = '\0';

	add_pr (database, pr);
	
	free_pr (pr);
      }

  closedir (d);
  free (path);
}

static void
do_prs (const DatabaseInfo database, PR *pr)
{
  while (pr)
    {
      add_pr (database, pr);
      pr = getNextPR (pr);
    }
}

int
main (int argc, char **argv)
{
  int optc, i;
  AdmEntry *clist, *c;
  const char *database_name = NULL;
  const char *file_name = NULL;
  ErrorDesc err = NULL;
  DatabaseInfo database;
  static int importp = FALSE;
  static int exportp = FALSE;
  PR *pr = NULL;  
  struct option long_options[] =
  {
    {"database",  required_argument, NULL, 'd'},
    {"global-databases-file", required_argument, NULL, 'g'},
    {"outfile",   required_argument, NULL, 'o'},
    {"numerical", no_argument, 	     NULL, 'n'},
    {"import",    no_argument, 	     NULL, 'i'},
    {"export",    no_argument, 	     NULL, 'e'},
    {"help",      no_argument, 	     NULL, 'h'},
    {"version",   no_argument, 	     NULL, 'V'},
    {NULL, 0, NULL, 0}
  };
  
  program_name = basename (argv[0]);
  output = stdout;

  while ((optc = getopt_long (argc, argv, "o:g:hd:nVie",
			      long_options, (int *) 0)) != EOF)
    {
      switch (optc)
	{
	case 0:			/* automatically handled flag */
	  break;
	  
	case 'd':
	  database_name = optarg;
	  break;

	case 'g':
	  if (optarg[0] == '\0')
	    {
	      fprintf (stderr, "%s: filename must be non-null\n", program_name);
	      exit (1);
	    }
	  global_db_list_file = optarg;
	  break;

	case 'o':
	  file_name = optarg;
	  break;

	case 'n':
	  numeric_sorting = TRUE;
	  break;

	case 'i':
	  importp = TRUE;
	  break;
      
	case 'e':
	  exportp = TRUE;
	  break;
      
	case 'V':
	  version (PROGRAM_NAME);
	  break;

	case 'h':
	  usage (USAGE, 0);
	  break;

	default:
	  usage (USAGE, 1);
	}
    }
  
  database = init_gnats (program_name, database_name, &err);
  if (database == NULL)
    {
      client_print_errors (database, err);
      exit (1);
    }

  umask (022);

  clist = fieldDefForIndex (CATEGORY (database))->adm_contents;

  if (importp)
    {
      pr = getFirstPR (database, &err);
      if (err)
	{
	  client_print_errors (database, err);
	  exit (1);
	}
    }

  if (exportp && indexIsBinary (database))
    {
      IndexDesc desc = getIndexDesc (database);
      setIndexDescBinary (desc, FALSE);
    }

  if (file_name)
    output = fopen (file_name, "w+");
  if (output == (FILE *) NULL)
    {
      fprintf (stderr, "%s: can't write to %s: %s\n", program_name,
	       optarg, strerror (errno));
      exit (3);
    }

  if (indexIsBinary (database))
    {
      char numFields = indexFieldCount (database);
      fwrite (&numFields, 1, 1, output);
    }
  
  if (importp)
    do_prs (database, pr);
  else
    for (c = clist ; c ; c = c->next)
      do_category (database, c->admFields[CategoryAdmKey]);

  if (numeric_sorting && num_entries > 0)
    {
      qsort (entries, num_entries, sizeof (Entry), entry_cmp);
      for (i = 0; i < num_entries; i++)
	{
	  fwrite (entries[i].string, 1, entries[i].length, output);
	}
    }

  fclose (output);
  freeDatabaseInfo (database);
  database = NULL;
  exit (0);
}
