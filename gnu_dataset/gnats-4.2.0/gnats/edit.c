/* Tools for editing a PR.
   Copyright (C) 1994, 1995, 2001, 2002 Free Software Foundation, Inc.
   Contributed by Brendan Kehoe (brendan@cygnus.com).

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
#include "gnatsd.h"
#include "regex.h"
#include "query.h"
#include "mail.h"
#include "ds.h"

/* Euuugh.  There's gotta be a better way to keep this around.  */
static char *newAuditTrailEntries = NULL;

static void
sendAuditMail (PR *pr, PR *oldPR, const char *editUserEmailAddr, 
	       const char *newAuditTrailEntries, ErrorDesc *err)
{
  FormatNamedParameter *parms = NULL;
  const DatabaseInfo database = pr->database;

  parms = allocateNamedParameter ("$EditUserEmailAddr", editUserEmailAddr,
				  parms);
  parms = allocateNamedParameter ("$NewAuditTrail", newAuditTrailEntries,
				  parms);

  if (strcmp (field_value (pr, RESPONSIBLE (database)),
	      field_value (oldPR, RESPONSIBLE (database))) != 0)
    {
      parms
	= allocateNamedParameter ("$OldResponsible",
				  field_value (oldPR, RESPONSIBLE (database)),
				  parms);
    }

  composeMailMessage (pr, oldPR, "audit-mail", parms, NULL, err);
  freeFormatParameterList (parms);
}

/* Add an entry to the AUDIT_TRAIL field. PARAMS are the format
   parameters used when composing the entry.  FMT is the format of the
   audit-trail entry to add.  */

static int
addAuditTrailEnt (PR *pr, QueryFormat *fmt, 
		  FormatNamedParameter *params, ErrorDesc *err)
{
  char *newAuditString = NULL;
  char *finalAuditString;
  const char *t;
  char *currDate = get_curr_date ();

  if (fmt == NULL)
    {
      fmt = getAuditTrailFormat (pr->database);
    }

  /* Format the new audit entry. */
  process_format (NULL, &newAuditString, pr, NULL, fmt, "\n", params);

  /* Squirrel it away, because we'll need to mail it later.  */
  append_string (&newAuditTrailEntries, newAuditString);

  /* Now append the formatted string to the audit-trail field.  */
  t = field_value (pr, AUDIT_TRAIL (pr->database));
  if (t == NULL)
    {
      t = "";
    }
  finalAuditString = xstrdup (t);
  append_string (&finalAuditString, newAuditString);
  set_field (pr, AUDIT_TRAIL (pr->database), finalAuditString, err);

  free (finalAuditString);
  free (newAuditString);
  free (currDate);

  return 0;
}

static int
applyChangeAction (ChangeActions action, PR *pr, PR *oldPR, FieldIndex field,
		   ErrorDesc *err, FormatNamedParameter *params)
{
  FieldEdit *fieldEdits = action->edits;
  FieldList fields = action->requiredFields;

  while (fields != NULL)
    {
      const char *fldval = get_field_value (pr, oldPR, fields->ent, NULL, NULL);
      if (value_is_empty (fldval))
	{
	  setError (err, CODE_INVALID_PR_CONTENTS,
		    "Required field %s missing from PR %s.",
		    complexFieldIndexToString (fields->ent),
		    field_value (pr, NUMBER (pr->database)));
	  return 1;
	}
      fields = fields->next;
    }

  if (action->requireChangeReason && field_change_reason (pr, field) == NULL)
    {
      setError (err, CODE_INVALID_PR_CONTENTS,
		"Edit of field %s requires a change reason.",
		fieldDefForIndex (field)->name);
      return 1;
    }

  while (fieldEdits != NULL)
    {
      if (applyFieldEdit (pr, fieldEdits, err, params) != 0)
	{
	  return 1;
	}
      fieldEdits = fieldEdits->next;
    }
  return 0;
}

static int
applyChangeActions (PR *pr, PR *oldPR, FieldIndex field,
		    ChangeActions actions, ErrorDesc *err,
		    FormatNamedParameter *params)
{
  ChangeActions actionList = actions;

  while (actionList != NULL)
    {
      if (actionList->expr == NULL
	    || pr_matches_expr (pr, oldPR, actionList->expr, params))
	{
	  if (applyChangeAction (actionList, pr, oldPR, field, err, params))
	    {
	      return 1;
	    }

          if (field != InvalidFieldIndex && actionList->addAuditTrail)
	    {
	      addAuditTrailEnt (pr, actionList->auditTrailFormat, params, err);
	    }
	}
      actionList = actionList->next;
    }

  if (field != InvalidFieldIndex)
    {
      if (fieldDefForIndex (field)->datatype != MultiText)
	{
	  ChangeActions globalActions = globalChangeActions (pr->database);

	  while (globalActions != NULL)
	    {
	      if (globalActions->addAuditTrail)
		{
		  addAuditTrailEnt (pr, globalActions->auditTrailFormat,
				    params, err);
		}
	      globalActions = globalActions->next;
	    }
	}
    }
  return 0;
}

static int
processFieldChange (PR *pr, PR *oldPR, FieldIndex field,
		    ErrorDesc *err, const char *editUserEmailAddr,
		    const char *oldValue, const char *newValue)
{
  ChangeActions actions;
  FormatNamedParameter *params = NULL;
  int res;

  if (oldValue == NULL)
    {
      oldValue = "";
    }
  if (newValue == NULL)
    {
      newValue = "";
    }

  if (fieldDefForIndex (field)->readonly)
    {
      setError (err, CODE_READONLY_FIELD, 
		newBadFieldEntry (field, NULL, NULL),
		"Field %s is read-only: `%s'->`%s'",
		fieldDefForIndex (field)->name,
		oldValue,
		newValue);
      return 1;
    }

  {
    char *currDate = get_curr_date ();
    params = allocateNamedParameter ("$CurrentDate", currDate, params);
    free (currDate);
  }

  params = allocateNamedParameter ("$FieldName", 
				   fieldDefForIndex (field)->name, params);
  params = allocateNamedParameter ("$OldValue", oldValue, params);
  params = allocateNamedParameter ("$NewValue", newValue, params);
  params = allocateNamedParameter ("$EditUserEmailAddr", editUserEmailAddr,
				   params);
  {
    const char *reason = field_change_reason (pr, field);
    if (reason == NULL)
      {
	reason = "";
      }
    params = allocateNamedParameter ("$ChangeReason", reason , params);
  }

  actions = fieldDefForIndex (field)->changeActions;
  res = applyChangeActions (pr, oldPR, field, actions, err, params);
  freeFormatParameterList (params);
  return res;
}

static int
processPRChanges (const char *editUserEmailAddr, PR *old_pr, PR *new_pr,
		  ErrorDesc *err)
{
  DatabaseInfo database = old_pr->database;
  int num_fields = get_num_fields (old_pr->database);
  int x;
  ChangeActions globalActions = globalChangeActions (database);
  int *fieldsChanged;

  fieldsChanged = (int *) xmalloc (sizeof (int) * num_fields);

  for (x = 0; x < num_fields; x++)
    {
      FieldIndex field = getNthField (database, x);
      const char *old_value = field_value (old_pr, field);
      const char *new_value = field_value (new_pr, field);

      if (fieldDefForIndex (field)->readonly
	  && (new_value == NULL || new_value[0] == '\0')
	  && old_value != NULL)
	{
	  if (set_field (new_pr, field, old_value, err) == 0)
	    {
	      free (fieldsChanged);
	      return 1;
	    }
	  new_value = field_value (new_pr, field);
	}

      if (old_value == NULL && new_value != NULL && new_value[0] == '\0')
	{
	  /* Ignore the new empty field.  */
	  unsetField (new_pr, field);
	  new_value = NULL;
	}

      if (new_value == NULL && old_value != NULL && 
	  ! prFieldHasValue (old_pr, field))
	{
	  /* Sometimes empty entries show up in the index, but they
             aren't "really in the PR".  */
	  old_value = NULL;
	}

      /* This code uses strcmp instead of intFieldCompare () because
	 we really care if the field value was changed in *any* manner,
	 not just if they happen to compare equal according to
	 intFieldCompare ().  */
      if ((old_value == NULL && new_value != NULL)
	  || (new_value == NULL && old_value != NULL)
	  || (new_value != old_value && strcmp (old_value, new_value) != 0))
	{
	  /* Ignore changes to readonly fields.  */
	  if (fieldDefForIndex (field)->readonly)
	    {
	      fieldsChanged[x] = 0;
	      if (old_value == NULL)
	        {
		  unsetField (new_pr, field);
	        }
	      else
	        {
		  set_field (new_pr, field, old_value, err);
	        }
	    }
	  else
	    {
	      fieldsChanged[x] = 1;
	    }
	}
      else
	{
	  fieldsChanged[x] = 0;
	}
    }

  for (x = 0; x < num_fields; x++)
    {
      if (fieldsChanged[x])
	{
	  FieldIndex field = getNthField (database, x);
	  const char *old_value = field_value (old_pr, field);
	  const char *new_value = field_value (new_pr, field);

	  if (processFieldChange (new_pr, old_pr, field, err, 
				  editUserEmailAddr,
				  old_value, new_value) != 0)
	    {
	      free (fieldsChanged);
	      return 1;
	    }
	}
    }

  free (fieldsChanged);

  if (globalActions != NULL)
    {
      FormatNamedParameter *params = NULL;
      char *currDate = get_curr_date ();
      int res;

      params = allocateNamedParameter ("$CurrentDate", currDate, params);
      free (currDate);
      params = allocateNamedParameter ("$EditUserEmailAddr", editUserEmailAddr,
				       params);

      res = applyChangeActions (new_pr, old_pr, InvalidFieldIndex, 
				globalActions, err, params);
      freeFormatParameterList (params);
      if (res != 0)
	{
	  return 1;
	}
    }

  return 0;
}

/* Replace an existing PR with the same PR number as NEW_PR.
   EDITUSEREMAILADDR is the email address of the user that is performing the
   edit.  */
static int
rewrite_pr (PR *pr, PR *new_pr, const char *editUserEmailAddr, ErrorDesc *err)
{
  const DatabaseInfo database = new_pr->database;

  newAuditTrailEntries = NULL;

  if (field_value (new_pr, CATEGORY (database)) == NULL)
    {
      setError (err, CODE_INVALID_PR_CONTENTS,
		"Missing category from new PR contents");
      return 0;
    }

  if (! isPrLocked (new_pr->database, field_value (new_pr, NUMBER (database))))
    {
      setError (err, CODE_PR_NOT_LOCKED, "PR %s is not locked", 
		field_value (new_pr, NUMBER (database)));
      return 0;
    }

  if (processPRChanges (editUserEmailAddr, pr, new_pr, err) != 0)
    {
      return 0;
    }

  if (!pr_update (pr, new_pr, err))
    {
      return 0;
    }

  /* Send mail about the edit.  */
  if (newAuditTrailEntries != NULL)
    {
      sendAuditMail (new_pr, pr, editUserEmailAddr, newAuditTrailEntries,
		     err);
      free (newAuditTrailEntries);
      newAuditTrailEntries = NULL;
    }

  return 1;
}


int
replace_pr (const DatabaseInfo database, FILE *fp,
	    const char *editUserEmailAddr, ErrorDesc *err)
{
  int res;
  const char *prnum = NULL;
  PR *curr_pr;
  PR *new_pr = allocPR (database);

  /* Read the message header in.  */
  if (read_header (new_pr, fp) < 0)
    {
      setError (err, CODE_EOF_PR, "Couldn't read PR header");
      return 0;
    }

  read_pr (new_pr, fp, 0);

  prnum = field_value (new_pr, NUMBER (database));
  if (prnum == NULL)
    {
      setError (err, CODE_INVALID_PR_CONTENTS,
	        "Missing PR number from new PR contents");
      return 0;
    }

  if (! pr_exists (database, prnum, err))
    {
      return 0;
    }

  curr_pr = pr_load_by_id (database, prnum, 0, err);
  if (curr_pr == NULL)
    {
      setError (err, CODE_NO_INDEX,
	        "Unable to locate existing PR %s", prnum);
      return 0;
    }

  if (! check_pr (new_pr, err, 0))
    {
      return 0;
    }

  res = rewrite_pr (curr_pr, new_pr, editUserEmailAddr, err);

  free_pr (curr_pr);
  free_pr (new_pr);

  return res;
}

/* Verify that TEXT is a valid value for field FIELD.  If not, an appropriate
   error will be stored in ERR, and a zero value is returned.  If TEXT
   is valid, a non-zero value will be returned.

   If DONTCHECKENUMS is non-zero, we don't verify that enum fields contain
   a valid enumerated value. */

int
validateFieldValue (FieldIndex field, const char *text, ErrorDesc *err,
		    int dontCheckEnums)
{
  if (field == NUMBER (fieldDefForIndex (field)->database))
    {
      if (text[0] == '-')
	{
	  setError (err, CODE_INVALID_FIELD_CONTENTS,
		    newBadFieldEntry (NUMBER (field->database), text, NULL),
		    "Number field has invalid value");
	  return 0;
	}
    }
  switch (fieldDefForIndex (field)->datatype)
    {
    case Date:
      {
	/* We'll allow dates to be an empty string. */
	if (text != NULL && text[0] != '\0')
	  {
	    time_t t = get_date ((char *)text, NULL);
	    if (t < 0)
	      {
		setError (err, CODE_INVALID_DATE, 
			  "Couldn't parse the %s date: %s.",
			  fieldDefForIndex (field)->name, text);
		return 0;
	      }
	  }
      }
      break;
    case Integer:
      {
	if (text != NULL)
	  {
	    const char *tptr = text;
	    if (tptr[0] == '-' || tptr[0] == '+')
	      {
		tptr++;
	      }
	    while (tptr[0] != '\0')
	      {
		if (! isdigit ((int) tptr[0]))
		  {
		    setError (err, CODE_INVALID_FIELD_CONTENTS,
			      newBadFieldEntry (field, text, NULL),
			      "Integer field %s had non-integer contents",
			      fieldDefForIndex (field)->name);
		    return 0;
		  }
		tptr++;
	      }
	  }
      }
      break;
    case PRListType:
      {
	const char *tptr = text;
	unsigned int prcount = 0;
	unsigned int maxprs = fieldDefForIndex (field)->maxPrsPerLine;
	while (tptr != NULL)
	  {
	    char *prnum = get_next_field (&tptr, ' ');
	    if (prnum[0] != '\0')
	      {
		if (! pr_exists (fieldDefForIndex (field)->database, prnum,
				err))
		  {
		    setError (err, CODE_INVALID_FIELD_CONTENTS,
			      newBadFieldEntry (field, prnum, NULL),
			      "PR list field %s had invalid PR `%s'",
			      fieldDefForIndex (field)->name,
			      prnum);
		    free (prnum);
		    return 0;
		  }
		prcount++;
		if (maxprs > 0 && prcount > maxprs)
		  {
		    setError (err, CODE_INVALID_FIELD_CONTENTS,
			      newBadFieldEntry (field, prnum, NULL),
			      "PR list field %s has too many entries",
			      fieldDefForIndex (field)->name);
		    free (prnum);
		    return 0;
		  }
	      }
	    free (prnum);
	  }
	break;
      }
    default:
      {
	break;
      }
    }

  if (fieldDefForIndex (field)->regex != NULL)
    {
      const char *rtextToCheck = text;
      StringList *rl;

      if (rtextToCheck == NULL)
	{
	  rtextToCheck = "";
	}
      for (rl = fieldDefForIndex (field)->regex; rl != NULL; rl = rl->next)
	{
	  if (gnats_regcmp (rl->name, rtextToCheck, NULL) == 0)
	    {
	      break;
	    }
	}
      if (rl == NULL)
	{
	  setError (err, CODE_INVALID_FIELD_CONTENTS, 
		    newBadFieldEntry (field, text, NULL),
		    "Field %s with contents `%s' didn't match any of the required regexps",
		    fieldDefForIndex (field)->name, rtextToCheck);
	  return 0;
	}
    }
  if (! dontCheckEnums)
    {
      if (fieldDefForIndex (field)->datatype == Enum 
	  || fieldDefForIndex (field)->datatype == MultiEnum)
	{
	  if (! verify_enum (field, text))
	    {
	      setError (err, CODE_INVALID_ENUM,
			newBadFieldEntry (field, text, NULL),
		       "Invalid value `%s' for enumerated field `%s'",
		       text, fieldDefForIndex (field)->name);
	      return 0;
	    }
	}
    }
  return 1;
}


/* check_pr_file - reads the PR in from FP, verifies its contents, and
   returns the bogus info in ERR.  If INITIAL is true, this is a new
   PR rather than an edit of an existing one.  */

int
check_pr_file (DatabaseInfo database, FILE *fp, ErrorDesc *err, int initial)
{
  PR *pr;
  int res;

  pr = allocPR (database);
  /* Read the message header in.  */
  if (read_header (pr, fp) < 0)
    {
      setError (err, CODE_EOF_PR, "Couldn't read PR header");
      return 0;
    }

  read_pr (pr, fp, 0);

  res = check_pr (pr, err, initial);

  free_pr (pr);
  return res;
}

/* Check PR for invalid values; the invalid fields are set in ERR.  If
   INITIAL_ENTRY is true, PR is a new PR instead of an edit of an
   existing one. */
int
check_pr (PR *pr, ErrorDesc *err, int initial_entry)
{
  const DatabaseInfo database = pr->database;
  BadFields enums;
  int x;
  int num_fields = get_num_fields (database);

  for (x = 0; x < num_fields; x++)
    {
      FieldIndex field = getNthField (database, x);
      if (initial_entry && field == NUMBER (database))
	{
	  continue;
	}
      if (! validateFieldValue (field, field_value (pr, field), err, 1))
	{
	  return 0;
	}
    }

  enums = checkEnumTypes (pr, NULL, 0);
  if (enums != NULL)
    {
      setError (err, CODE_INVALID_ENUM, enums, NULL);
      return 0;
    }
  else
    {
      return 1;
    }
}

int
lock_pr (const DatabaseInfo database, const char *prID,
	 const char *user, const char *processid, ErrorDesc *err)
{
  char *lock_path;
  int count;
  int fdes = -1;
  
  if (prID == NULL)
    {
      return 0;
    }

  lock_path = get_lock_path (database, prID);

#define MAXWAIT 2
#define GRANULARITY 1

  /* try repeatedly (well, twice...) to get the lock */
  for (count = 0; count < MAXWAIT / GRANULARITY; count++)
    {
      errno = 0;
      /* use atomic create, to avoid races */
      fdes = open (lock_path, O_CREAT | O_TRUNC | O_WRONLY | O_EXCL, 0644);
      if (fdes != -1)
	{
	  /* success */
	  break;
	}
      else
	{
	  if (errno == EEXIST || errno == EINTR)
	    {
	      /* somebody else has the lock, or we were rudely
                 interrupted, sleep and try again */
	      sleep (GRANULARITY);
	    }
	  else
	    {
	      /* something went wrong, error out */
	      break;
	    }
	}
    }
  
  if (!errno)
    {
      /* success */
      FILE *ifp = fdopen (fdes, "w");
      if (ifp == NULL)
	{
	  setError (err, CODE_FILE_ERROR, "Cannot write to lock file %s",
		    lock_path);
	  log_msg (LOG_ERR, 1, "error in lock_pr:",
		   getErrorMessage (*err));
	  close (fdes);
	  /* try to get rid of the lock file */
	  unlink (lock_path);
	  /* XXX??? we should really go the extra mile and figure out
	     if the lock is still there, and punt to the administrator
	     if we can't delete it.  but that's a lot of trouble for
	     an edge case... */
	  return 0;
	}

      if (processid != NULL)
	{
	  fprintf (ifp, "%s %s\n", user, processid);
	}
      else
	{
	  fprintf (ifp, "%s\n", user);
	}

      fclose (ifp);
      free (lock_path);
      return 1;
    }
  else if (errno == EEXIST)
    {
      /* somebody else has the lock */
      struct stat sb;
      char *date = (char *) xmalloc (GNATS_TIME_LENGTH);
      char buf[1024];  /* This size doesn't matter here */
      char *s;

      FILE *fp = fopen (lock_path, "r");

      /* If we couldn't open it for reading or get the lock file contents,
	 something else is hosed, so just bail now. */
      if (fp == (FILE *) NULL || fgets (buf, sizeof (buf) - 1, fp) == NULL)
	{
	  setError (err, CODE_FILE_ERROR, "Cannot read lock file %s",
		    lock_path);
	  log_msg (LOG_ERR, 1, "error in lock_pr:",
		   getErrorMessage (*err));
	  free (lock_path);
	  return 0;
	}

      s = strchr (buf, '\n');
      if (s != NULL)
	{
	  s[0] = '\0';
	}

      /* report time of lock as well  */
      stat (lock_path, &sb);
      strftime (date, GNATS_TIME_LENGTH, "%Y-%m-%d:%H:%M:%SZ",
		gmtime(&sb.st_mtime));

      setError (err, CODE_LOCKED_PR, "PR %s is locked by '%s', since %s",
		prID, buf, date);

      fclose (fp);
      free (lock_path);
      return 0;
    }
  else
    {
      setError (err, CODE_FILE_ERROR, "Cannot create lock file %s: %s",
		lock_path, strerror(errno));
      log_msg (LOG_ERR, 1, "error in lock_gnats:",
	       getErrorMessage (*err));
      free (lock_path);
      return 0;
    }
}

int
unlock_pr (const DatabaseInfo database, const char *prID, ErrorDesc *err)
{
  char *lock_path;

  if (prID == NULL)
    return 0;

  lock_path = get_lock_path (database, prID);

  if (! fileExists (lock_path))
    {
      setError (err, CODE_PR_NOT_LOCKED, "PR %s is not locked.", prID);
      free (lock_path);
      return 0;
    }

  if (unlink (lock_path) != 0)
    {
      setError (err, CODE_FILE_ERROR, "Cannot delete lock file %s: %s.",
		lock_path, strerror (errno));
      free (lock_path);
      return 0;
    }

  free (lock_path);
  return 1;
}

int
edit_field (const DatabaseInfo database, const char *prnum, 
	    FieldIndex fieldIndex, int append,
	    char *newcontents, char *changeReason, 
	    const char *editUserEmailAddr, ErrorDesc *err)
{
  char pid[32];
  const char *oldfield;
  int res;
  PR *pr, *new_pr;

  if (! pr_exists (database, prnum, err))
    {
      free (newcontents);
      if (changeReason != NULL)
        {
	  free (changeReason);
        }
      return 0;
    }
  if (fieldIndex == InvalidFieldIndex)
    {
      setError (err, CODE_INVALID_FIELD_NAME, "Invalid field name");
      free (newcontents);
      if (changeReason != NULL)
        {
	  free (changeReason);
        }
      return 0;
    }
  if (requiresChangeReason (fieldDefForIndex (fieldIndex))
      && changeReason == NULL)
    {
      setError (err, CODE_INVALID_FIELD_EDIT,
		"Edit of field %s requires a change reason.", 
		fieldDefForIndex (fieldIndex)->name);
      free (newcontents);
      return 0;
    }

  sprintf (pid, "%d", (int) getpid ());
  if (lock_pr (database, prnum, "edit_field", pid, err) == 0)
    {
      free (newcontents);
      if (changeReason != NULL)
        {
	  free (changeReason);
        }
      return 0;
    }

  pr = pr_load_by_id (database, prnum, 0, err);
  if (pr == NULL)
    {
      unlock_pr (database, prnum, err);
      setError (err, CODE_UNREADABLE_PR, "Error reading PR %s.", prnum);
      free (newcontents);
      if (changeReason != NULL)
	{
	  free (changeReason);
	}
      return 0;
    }

  oldfield = field_value (pr, fieldIndex);

  if (append)
    {
      char *totalVal;
      if (oldfield != NULL)
	{
	  totalVal = xstrdup (oldfield);
	}
      else
	{
	  totalVal = NULL;
	}
      append_string (&totalVal, newcontents);
      free (newcontents);
      newcontents = totalVal;
    }

  /* create a copy of the currently active pr so that we can modify it and
     do subsequent validation allowing for us to bail out cleanly */
  new_pr = allocPR (database);
  set_field (new_pr, NUMBER (database), prnum, err);
  set_field (new_pr, CATEGORY (database), field_value (pr, CATEGORY (database)),
	     err);
  pr_load (new_pr, err);

  /* set_field () verifies that the value is valid before doing the
     set.  */
  /* set_field returns a boolean!!! */
  if (set_field (new_pr, fieldIndex, newcontents, err) == 0)
    {
      res = 0;
    }
  else
    {
      if (changeReason != NULL)
	{
	  setFieldChangeReason (new_pr, fieldIndex, changeReason);
	  free (changeReason);
	  changeReason = NULL;
	}

      res = rewrite_pr (pr, new_pr, editUserEmailAddr, err);
    }

  free_pr (pr);
  free_pr (new_pr);
  free (newcontents);
  if (changeReason != NULL)
    {
      free (changeReason);
      changeReason = NULL;
    }

  if (unlock_pr (database, prnum, err) == 0)
    {
      res = 0;
    }

  return res;
}

int
applyFieldEdit (PR *pr, FieldEdit *edit, ErrorDesc *err,
		FormatNamedParameter *params)
{
  const DatabaseInfo database = pr->database;
  char *buffer = NULL;
  char *fmt = edit->textFormat;
  FieldList currField = edit->fieldsForFormat;
  char *currDate = get_curr_date ();
  int res;

  if (edit->fieldToEditName != NULL)
    {
      edit->fieldToEdit = find_field_index (database, edit->fieldToEditName);
      if (edit->fieldToEdit == InvalidFieldIndex)
	{
	  setError (err, CODE_INVALID_FIELD_NAME, "Invalid field name %s.\n",
		    edit->fieldToEditName);
	  return -1;
	}
      free (edit->fieldToEditName);
      edit->fieldToEditName = NULL;
    }

  if (edit->append)
    {
      append_string (&buffer, field_value (pr, edit->fieldToEdit));
    }
  while (fmt != NULL && *fmt != '\0')
    {
      if (*fmt == '\\' && *(fmt + 1) == 'n')
	{
	  append_string (&buffer, "\n");
	}
      else if (*fmt == '%')
	{
	  fmt++;
	  if (fmt[0] == '%')
	    {
	      append_string (&buffer, "%");
	      fmt++;
	    }
	  else
	    {
	      char *vptr;
	      char *value;
	      char *vend;
	      
	      while (!isalpha ((int) *fmt))
		{
		  fmt++;
		}

	      {
		int mustBeFreed = 0;

		const char *vtemp = get_field_value (pr, pr, currField->ent,
						     params, &mustBeFreed);
		if (vtemp == NULL)
		  {
		    vtemp = "";
		  }
		if (mustBeFreed)
		  {
		    value = (char *) vtemp;
		  }
		else
		  {
		    value = xstrdup (vtemp);
		  }
	      }

	      vend = value + strlen (value) - 1;

	      vptr = value;

	      while (*value != '\0' && isspace ((int)(unsigned char) *value))
		{
		  value++;
		}

	      while (vend >= value && isspace ((int)(unsigned char) *vend))
		{
		  *(vend--) = '\0';
		}

	      append_string (&buffer, value);
	      free (vptr);
	      fmt++;
	      currField = currField->next;
	    }
	}
      else
	{
	  char buf[2];

	  buf[0] = *fmt;
	  buf[1] = '\0';
	  append_string (&buffer, buf);
	  fmt++;
	}
    }

  if (buffer == NULL)
    {
      buffer = xstrdup ("");
    }

  res = ! set_field (pr, edit->fieldToEdit, (buffer != NULL ? buffer : ""),
		     err);

  if (buffer != NULL)
    {
      free (buffer);
    }

  free (currDate);
  return res;
}

/* Delete PR PRNUM.  */
int
deletePR (const DatabaseInfo database, const char *prnum, 
	  const char *editUserEmailAddr, ErrorDesc *err)
{
  char pid[32];
  FormatNamedParameter *parms = NULL;
  short delete_status;

  if (client_lock_gnats (database, err))
    {
      return -4;
    }

  if (lock_pr (database, prnum, GNATS_USER, pid, err) == 0)
    {
      client_unlock_gnats ();
      return -4;
    }

  if ((delete_status = pr_delete (database, prnum, err)) < 0)
    {
      unlock_pr (database, prnum, err);
      client_unlock_gnats ();
      return delete_status;
    }

  if (unlock_pr (database, prnum, err) == 0)
    {
      client_unlock_gnats ();
      return -7;
    }
  client_unlock_gnats ();

  parms = allocateNamedParameter ("$EditUserEmailAddr", editUserEmailAddr,
				  parms);
  parms = allocateNamedParameter ("$PRNum", prnum, parms);
  
  {
    PR *pr = allocPR (database);
    pr->read_in = TRUE;		/* TODO: hack, read_in is private */
    composeMailMessage (pr, NULL, "deleted-pr-mail", parms, NULL, err);
  }
  
  return 0;
}

ChangeActions
newChangeAction (const DatabaseInfo database, const char *optExpr)
{
  ChangeActions res = (ChangeActions) xmalloc (sizeof (struct change_actions));
  if (optExpr != NULL)
    {
      res->expr = parseQueryExpression (database, 
					optExpr,
					optExpr + strlen (optExpr) - 1);
    }
  else
    {
      res->expr = NULL;
    }
  res->requiredFields = NULL;
  res->edits = NULL;
  res->addAuditTrail = 0;
  res->auditTrailFormat = NULL;
  res->requireChangeReason = 0;
  res->next = NULL;
  return res;
}

void
freeChangeActions (ChangeActions actions)
{
  while (actions != NULL)
    {
      ChangeActions next = actions->next;

      freeQueryExpr (actions->expr);
      freeFieldEdit (actions->edits);
      freeFieldList (actions->requiredFields);
      freeQueryFormat (actions->auditTrailFormat);
      free (actions);
      actions = next;
    }
}

int
requiresChangeReason (FieldDef field)
{
  ChangeActions acts = field->changeActions;

  while (acts != NULL)
    {
      if (acts->requireChangeReason)
	{
	  return 1;
	}
      acts = acts->next;
    }
  return 0;
}

