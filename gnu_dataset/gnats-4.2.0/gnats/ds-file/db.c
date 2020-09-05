/* Flat-file datastore API - functions operating on a single database instance.
   Copyright (C) 2005,07 Free Software Foundation, Inc.
   Contributed by Mel Hatzis <hatzis@wattes.org>.

This file is part of GNU GNATS.

GNU GNATS is free software; you can redistribute it and/or modify
it under the terms of the GNU General Public License as published by
the Free Software Foundation; either version 3 of the License, or
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
#include "gnatsd.h"
#include "ds-file.h"

/* load the contents of the PR which are required to test the PR against
 * the passed in query expression - ie. read's the PR from disk if the
 * query expression refers to fields which are not in the index.
 * Returns -1 on error, 0 if the PR wasn't loaded and 1 if it was loaded. */
static int
get_pr_contents(PR *pr, QueryTree qexp)
{
  int pr_loaded = 0;

  if (qexp == NULL)
    {
      return 1;
    }

  switch (qexp->op)
    {
    case QueryAnd:
    case QueryOr:
      {
	pr_loaded = get_pr_contents (pr, qexp->left);
        if (pr_loaded == 0)
          {
            pr_loaded = get_pr_contents (pr, qexp->right);
          }
	break;
      }
    case QueryNot:
      {
	pr_loaded = get_pr_contents (pr, qexp->left);
	break;
      }
    case QueryMatch:
      {
        SearchItem *query_element = &(qexp->ent);
        ComplexFieldIndex lhs_field = query_element->lhs->fieldIndex;
        if ((! PR_IS_FULL (pr)) && (! isIndexedField (lhs_field)))
          {
            ErrorDesc err;
            if (pr_load (pr, &err) != 0)
              {
                return -1;
              }
            pr_loaded = 1;
          }
	break;
      }
    default:
      {
	abort ();
	break;
      }
    }

  return pr_loaded;
}

int
db_init (DatabaseInfo database, ErrorDesc *err)
{
  int retval = 0;
  char *path;
  FileDBInfo dbi = (FileDBInfo) xmalloc (sizeof (struct file_db_info));
  setDatastorePrivate (database, (void *) dbi);
  dbi->indexDesc = NULL;
  path = gnats_adm_dir (database, "dbconfig");
  if (fconfigParse (database, path, NULL, err))
    {
      retval = -1;
    }
  else
    {
      initIndex (database);
    }
  free (path);
  return retval;
}

void
db_destroy (DatabaseInfo database)
{
  FileDBInfo dbi = (FileDBInfo) getDatastorePrivate(database);
  clearPRChain (database);
  if (dbi != NULL)
    {
      if (dbi->indexDesc != NULL)
        {
          freeIndexDesc (dbi->indexDesc);
        }
      free (dbi);
      setDatastorePrivate (database, (void *)NULL);
    }
}

/* Iterate through a set of PRs.  For those PRs that match the
   expression in EXP, invoke FUNC with the count of PRs that have
   matched so far, the PR* entry for the PR, and the supplied
   QUERY_FORMAT.  

   FUNC is invoked once more after all of the queries have been
   searched.  The PR* pointer is NULL, and the count corresponds to
   the total # of PRs that matched.

   If AC is 0 or AV is NULL, we iterate through all of the PRs in the
   index for the current database.  Otherwise, only those PRs that
   match the PRIDs in AV[0], AV[1]. AV[2]...AV[AC-1] are tested.  */

int
db_query (const DatabaseInfo database, int ac, char **av, QueryExpr exp,
             QueryFunc func, QueryFormat *query_format, ErrorDesc *err)
{
  int found = 0;
  QueryTree tree = NULL;

  if (exp != NULL)
    {
      if (exp->database != database)
	{
	  abort ();
	}
      tree = exp->tree;
    }

  *err = NULL;
  if (ac == 0 || av == NULL)
    {
      PR *pr = getFirstPR (database, err);

      if (*err != NULL)
	{
	  return -1;
	}
      /* We weren't given a list of PRs to check, so we do the
	 whole shooting match.  */
      while (pr != NULL)
	{
	  if (get_pr_contents (pr, tree) != -1 &&
              pr_matches_tree (pr, NULL, tree, NULL))
	    {
	      found++;
	      func (found, pr, query_format);
	    }
	  free_pr_header (pr);
	  free_pr_contents (pr);
	  pr = getNextPR (pr);
	}
    }
  else
    {
      int cpr;

      for (cpr = 0; cpr < ac; cpr++)
	{
	  char *pat, *n;
	  char *p = av[cpr];
	  int plen;
	  PR *pr;
	  struct re_pattern_buffer buf;

	  memset (&buf, 0, sizeof (buf));

	  /* Remove the category */
	  if ((n = (char *) strrchr (p, '/')) != NULL) 
	    {
	      p = n + 1;
	    }

	  plen = strlen (p);
	  pat = (char *) xmalloc (plen + 3);
	  strcpy (pat, p);
	  strcpy (pat + plen, "\\'");

	  *err = NULL;
	  pr = getFirstPR (database, err);
	  if (*err != NULL)
	    {
	      return -1;
	    }

	  while (pr != NULL)
	    {
	      if (gnats_regcmp (pat, field_value (pr, NUMBER (pr->database)),
				&buf) == 0)
		{
		  if (get_pr_contents (pr, tree) != -1 &&
		      pr_matches_expr (pr, NULL, exp, NULL))
		    {
		      found++;
		      func (found, pr, query_format);
		    }
		  /* Only one PR will match this PR number, because it's
		     not really a regexp */
		  break;
		}
	      free_pr_header (pr);
	      free_pr_contents (pr);
	      pr = getNextPR (pr);
	    }
	  buf.translate = NULL;
	  regfree (&buf);
	  free (pat);
	}
    }

  func (found, NULL, query_format);

  return found;
}

void
db_reset (const DatabaseInfo database, ErrorDesc *err)
{
  checkPRChain (database, err);
}

