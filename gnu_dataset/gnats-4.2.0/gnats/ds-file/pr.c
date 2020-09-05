/* Flat-file datastore API - functions operating on a single PR.
   Copyright (C) 2005,07 Free Software Foundation, Inc.
   Contributed by Mel Hatzis <hatzis@wattes.org>.

This file is part of GNU GNATS.

GNU GNATS is free software; you can redistribute it and/or modify
it under the terms of the GNU General Public License as published by
the Free Software Foundation; either version 2 of the License, or
(at your option) any later version.

GNU GNATS is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
GNU General Public License for more details.

You should have received a copy of the GNU General Public License
along with GNU GNATS; see the file COPYING. If not, see
<http://www.gnu.org/licenses/>.
*/

#include "ds.h"
#include "pcodes.h"
#include "ds-file.h"

/* Get the PR at PATH.  PR is the PR entry to be filled in.
   If PRUNE is non-zero, don't read any multitext fields.  */
static int
get_pr (PR *pr, const char *path, int prune)
{
  FILE *fp = fopen (path, "r");

  if (fp == (FILE *)NULL)
    {
      return 0;
    }

  if (read_header (pr, fp) < 0)
    {
      return 0;
    }

  read_pr (pr, fp, prune);

  fclose (fp);

  return 1;
}

static PR *
get_pr_from_index (const DatabaseInfo database, const char *prnum,
		   ErrorDesc *err)
{
  PR *pr = getFirstPR (database, err);

  /* If they gave it to us with the category, remove it. */
  if (( strrchr (prnum, '/')) != NULL)
    {
      prnum = strrchr (prnum, '/') + 1;
    }

  while (pr != NULL && strcmp (prnum, field_value (pr, NUMBER (database))) != 0)
    {
      pr = getNextPR (pr);
    }

  if (pr == NULL)
    {
      setError (err, CODE_NONEXISTENT_PR,
	        "No PR %s listed in the index.", prnum);
      return NULL;
    }

  return pr;
}

static bool
pr_file_readable (const char *path, ErrorDesc *err)
{
  FILE *fp;

  if (path == NULL)
    {
      return FALSE;
    }

  if ((fp = fopen (path, "r")) == NULL)
    {
      setError (err, CODE_FILE_ERROR, "Can't open file `%s'", path);
      return FALSE;
    }

  fclose (fp);
  return TRUE;
}

static char *
get_pr_path (const DatabaseInfo database, PR *pr, const char *prnum)
{
  char *path = NULL;
  const char *category = NULL;

  if (pr != NULL)
    {
      category = field_value (pr, CATEGORY (database));
    }

  if (category != NULL)
    {
      asprintf (&path, "%s/%s/%s", databaseDir (database), category, prnum);
    }

  return path;
}

/* Return the next available unique GNATS id.  */
static int
getBugNumber (const DatabaseInfo database, ErrorDesc *err)
{
  char *sbuf;
  int bug_number;
  FILE *bug_file;

  /* First try to find and lock the gnats lock file.  We need this since
     they want every bug to have a unique number.  If lock doesn't exist,
     make it, if possible.  */
  sbuf = gnats_adm_dir (database, "current");

  block_signals ();

  bug_file = fopen (sbuf, "r+");
  if (bug_file == (FILE *) NULL)
    {
      log_msg (LOG_INFO, 1, "file 'current' not found, creating", sbuf);
      bug_file = fopen (sbuf, "w+");
      if (bug_file != (FILE *) NULL)
	{
	  bug_number = 1;
	}
      else
	{
	  setError (err, CODE_FILE_ERROR,
		    "Can't create the GNATS 'current' file (%s).",
		    sbuf);
	  return -1;
	}
    }
  else
    {
      if (fscanf (bug_file, "%d", &bug_number) != 1)
	{
	  setError (err, CODE_FILE_ERROR,
		    "Can't read from the global PR number counter (%s).",
		    sbuf);
	  return -1;
	}

      bug_number++;      
      rewind (bug_file);
    }

  fprintf (bug_file, "%d", bug_number);

  fclose (bug_file);
  unblock_signals ();
  free (sbuf);

  return bug_number;
}

void
pr_init(PR *pr)
{
  FilePRInfo pi = (FilePRInfo) xmalloc (sizeof (struct file_pr_info));
  pr->ds_private = pi;
  allocIndex (pi);
}

void
pr_destroy(PR *pr)
{

  FilePRInfo pi = (FilePRInfo) pr->ds_private;
  if (pi != NULL)
    {
      if (pi->index != NULL)
        {
          freePRIndex (pr->database, pi);
          free (pi->index);
        }
      free (pi);
      pr->ds_private = NULL;
    }
}

/* Load a PR containing only indexed field data with it's entire data set. */
int
pr_load (PR *pr, ErrorDesc *err)
{
  char *path = gen_pr_path (pr);
  const char *num = field_value (pr, NUMBER (pr->database));
  int val;

  if (num == NULL || path == NULL)
    {
      setError (err, CODE_ERROR, "Invalid PR in pr_load()");
      if (path != NULL)
	{
	  free (path);
	}
      return -1;
    }
  else
    {
      val = get_pr (pr, path, 0);

      free (path);
      if (val == 0)
	{
	  setError (err, CODE_FILE_ERROR, "Unable to read PR %s", num);
	  return -1;
	}
      else
	{
	  return 0;
	}
    }
}

/* Read a file into a new PR data structure. */
PR *
pr_load_by_id (const DatabaseInfo database, const char *prnum,
	       int prune, ErrorDesc *err)
{
  PR *pr = NULL;
  PR *index_pr = get_pr_from_index (database, prnum, err);
  char *path = get_pr_path (database, index_pr, prnum);

  if (path != NULL)
    {
      if (pr_file_readable (path, err))
        {
	  pr = allocPR (database);
	  setPrevPR (pr, getPrevPR (index_pr));
	  setNextPR (pr, getNextPR (index_pr));
	  if (get_pr (pr, path, prune) == 0)
	    {
	      free_pr (pr);
	      pr = NULL;
	    }
        }
      free (path);
    }
  return pr;
}

int
pr_load_fields (PR *pr, FieldList flds)
{
  int do_open_pr = 0;

  if (! PR_IS_FULL (pr))
    {
      if (flds == NULL)
        {
          do_open_pr = 1;
        }
      else
        {
          while (flds != NULL)
            {
              if (! isIndexedField (flds->ent))
                {
                  do_open_pr = 1;
                  break;
                }
              flds = flds->next;
            }
        }
    }

  if (do_open_pr)
    {
      ErrorDesc err;

      if (pr_load (pr, &err))
        {
          return -1;
        }
    }
  return 1;
}

int
pr_exists (const DatabaseInfo database, const char *prnum, ErrorDesc *err)
{
  PR *pr = get_pr_from_index (database, prnum, err);
  char *path = get_pr_path (database, pr, prnum);
  int res = 0;
  if (path != NULL)
    {
      res = fileExists (path);
      free (path);
      if (res == 0)
	{
	  setError (err, CODE_FILE_ERROR, "Can't open file `%s'", path);
	}
    }
  return res;
}

/* Write a file out to FILENAME with PR in it. */
int
pr_create (PR *pr, ErrorDesc *err)
{
  int errnum = 0;
  int bug_number = 0;
  char *path = NULL;
  char *prPath = NULL;
  char *bug_name = NULL;
  char number[14]; /* No more than 13 digits in a PR #, ha ha.  */
  time_t seconds  = time (NULL);
  char arrival_time[GNATS_TIME_LENGTH];
  const DatabaseInfo database = pr->database;
  const char *category = field_value (pr, CATEGORY (database));
  int flag_autocreate = createCategoryDirs (database);

  /* Set arrival date and time of response.  */
  gnats_strftime (arrival_time, GNATS_TIME_LENGTH, "%a %b %d %H:%M:%S %z %Y",
		  localtime (&seconds));
  set_field (pr, ARRIVAL_DATE (database), arrival_time, err);

  /* Put together the path to where the bug will be stored.  If the dir
     is not there, and the category is the default, auto-create that one,
     if we want to.  If not, make the bug pending, and store in there.  */

  asprintf (&path, "%s/%s", databaseDir (database), category);

  errnum = fileExists (path) ? 0 : -1;

  if (errnum != 0 && ! flag_autocreate)
    {
      /* XXX ??? !!! Should append a message to the email being sent out. */
      category = defaultCategory(database);
      set_field (pr, CATEGORY (database), defaultCategory(database), err);
      log_msg (LOG_INFO, 1, "directory does not exist, changing to default:",
	       path);
      free (path);
      asprintf (&path, "%s/%s", databaseDir (database), category);
      errnum = fileExists (path) ? 0 : -1;
    }

  /* Check ERR again, to see if default category was there.  */
  if (errnum != 0)
    {
      mode_t mode = categoryDirPerms (database);

      if (strcmp (category, defaultCategory(database)) == 0)
	{
	  log_msg (LOG_INFO, 1, defaultCategory(database),
		   " does not exist, creating...");
	  if (mkdir (path, mode) != 0)
	    {
	      setError (err, CODE_FILE_ERROR,
			"Can't create `%s' directory under %s.",
			defaultCategory(database), databaseDir (database));
	      free (path);
	      return -1;
	    }
	}
      else if (flag_autocreate)
	{
	  if (mkdir (path, mode) != 0)
	    {
	      setError (err, CODE_FILE_ERROR,
			"Cannot create group: %s", path);
	      free (path);
	      return -1;
	    }
	  else
	    {
	      log_msg (LOG_INFO, 1, "creating directory:", path);
	    }
	}
    }

  free (path);

  bug_number = getBugNumber (database, err);
  if (bug_number > 0)
    {
      sprintf (number, "%d", bug_number);
      set_field (pr, NUMBER (database), number, err);

      /* Write the file out.  */
      asprintf (&bug_name, "%s/%d", category, bug_number);
      asprintf (&prPath, "%s/%s", databaseDir (database), bug_name);
      if (createPrFile (pr, prPath, 1, err) != 0)
	{
	  bug_number = -1;
	}
      else
	{
	  log_msg (LOG_INFO, 1, "PR written out:", prPath);

	  /* Add a line into the index for use by query-pr.  */
	  /* This should be done regardless of whether the bug is default or
	     not. */
	  if (addToIndex (pr, err) != 0)
	    {
	      bug_number = -1;
	    }
	}
    }
  if (prPath != NULL)
    {
      free (prPath);
    }
  if (bug_name != NULL)
    {
      free (bug_name);
    }

  return bug_number;
}

int
pr_update (PR *curr_pr, PR *new_pr, ErrorDesc *err)
{
  char *new_path =  NULL;
  char *old_path =  NULL;
  char *bkup_path = NULL;
  FILE *prfile;
  const DatabaseInfo database = new_pr->database;

  /* check to see if the category changes, and if so, make sure
   * the new category dir exists */
  if (strcmp (field_value (curr_pr, CATEGORY (database)),
	      field_value (new_pr, CATEGORY (database))) != 0)
    {
      char *dirPath;

      asprintf (&dirPath, "%s/%s", databaseDir (database), 
		field_value (new_pr, CATEGORY (database)));
      if (! fileExists (dirPath))
	{
	  if (createCategoryDirs (database))
	    {
	      mode_t mode = categoryDirPerms (database);

	      if (mkdir (dirPath, mode) != 0)
		{
		  setError (err, CODE_FILE_ERROR, 
			    "Error creating directory for category %s: %s",
			    field_value (new_pr, CATEGORY (database)),
			    strerror (errno));
                  free (dirPath);
                  return 0;
		}
	    }
	  else
	    {
	      setError (err, CODE_FILE_ERROR, 
			"Directory for category %s does not exist",
			field_value (new_pr, CATEGORY (database)));
              free (dirPath);
              return 0;
	    }
	}
    }

  /* backup the current PR file */
  old_path = gen_pr_path (curr_pr);
  asprintf (&bkup_path, "%s.old", old_path);
  if (rename (old_path, bkup_path) < 0)
    {
      if (errno != EXDEV)
	{
	  setError (err, CODE_FILE_ERROR, "Could not rename %s: %s",
		    old_path, strerror (errno));
          free (bkup_path);
          free (old_path);
          return 0;
	}
      if (copy_file (old_path, bkup_path) != 0)
	{
	  setError (err, CODE_FILE_ERROR, "Could not copy %s to %s: %s",
		    old_path, bkup_path, strerror (errno));
          free (bkup_path);
          free (old_path);
          return 0;
	}

      /* Don't complain if this fails, since trying to write to it will give
	 us the diagnostic if it's really serious.  */
      unlink (old_path);
    }

  /* Now build the new PR file.  */
  new_path = gen_pr_path (new_pr);
  prfile = fopen (new_path, "w+");
  if (prfile == (FILE *) NULL)
    {
      setError (err, CODE_FILE_ERROR, "Cannot write the PR to %s: %s",
		new_path, strerror (errno));
      free (bkup_path);
      free (old_path);
      free (new_path);
      return 0;
    }

  /* We had to wait until now because we wanted to wait and see
     about changing the closed_date. XXX ??? !!! FIXME */

  buildIndexEntry (new_pr);

  write_entire_header (prfile, new_pr, "\n");
  fprintf (prfile, "\n");
  write_entire_pr (prfile, new_pr, "\n");

  if (fclose (prfile) == EOF)
    {
      setError (err, CODE_FILE_ERROR, "Error writing out PR %s: %s",
		new_path, strerror (errno));
      free (bkup_path);
      free (old_path);
      free (new_path);
      return 0;
    }

  unlink (bkup_path);

  /* unlink the old file, if it is in another category dir. */
  if (strcmp (field_value (curr_pr, CATEGORY (database)),
	      field_value (new_pr, CATEGORY (database))) != 0)
    {
      unlink (old_path);
    }

  free (bkup_path);
  free (old_path);
  free (new_path);

  /* write out the new index.  */
  replaceCurrentPRInIndex (curr_pr, new_pr, err);
  if (writeIndex (database, err) != 0)
    {
      return 0;
    }

  return 1;
}

int
pr_delete (const DatabaseInfo database, const char *prnum, ErrorDesc *err)
{
  PR *pr;
  char *path = NULL;

  pr = get_pr_from_index (database, prnum, err);
  path = get_pr_path (database, pr, prnum);

  if (path == NULL || !pr_file_readable (path, err))
    {
      if (path != NULL)
	{
	  free (path);
	}
      return -1;
    }

  if (removePRFromIndex (database, prnum, err))
    {
      free (path);
      return -5;
    }
  
  if (unlink (path))
    {
      setError (err, CODE_FILE_ERROR, "Unable to unlink file %s\n", path);
      free (path);
      return -6;
    }

  free (path);
  return 1;
}

