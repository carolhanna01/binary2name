/* Main handling routines for GNATS.
   Copyright (C) 2001 Milan Zamazal
   Copyright (C) 1993, 1994, 1995, 1999, 2000, 2007 Free Software Foundation, Inc.
   Contributed by Tim Wicinski (wicinski@barn.com) and
   by Brendan Kehoe (brendan@cygnus.com).
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
#include "query.h"
#include "pcodes.h"
#include "mail.h"
#include "ds.h"

static int needs_analysis (PR *pr);

static int run_atpr(PR *pr, AdmEntry *submitter, int submitter_rtime,
		    struct tm *expired, int bug_number, ErrorDesc *err);

static char* derive_submitter  (PR *pr);

static int missing_required_fields (const DatabaseInfo database, PR *pr,
                                    ErrorDesc *err);


/* Creates a PR file using the contents of PR.  If an error occurs, a
   negative value will be returned an an appropriate error code will
   be set in ERR.  The new PR number will be returned on success.
   FLAG_AUTOCREATE indicates that a PR with a non-existent category
   should cause the category to be automatically created.  */

static int
createNewPR (PR *pr, ErrorDesc *err)
{
  const DatabaseInfo database = pr->database;
  int bug_number = 0;
  AdmEntry *submitter = NULL;
  AdmEntry *category = NULL;
  AdmEntry *responsible = NULL;
  struct tm *expired = NULL;
  int submitter_rtime;

  const char *site, *bug_group;
  BadFields bad_fields = NULL;
  char *def_subm = NULL;

  if (! databaseValid (database))
    {
      setError (err, CODE_FILE_ERROR,
	       "Invalid database %s.", databaseName (database));
      return -1;
    }

  if (missing_required_fields (database, pr, err) > 0)
    {
      return -1;
    }

  /*  If we don't have a valid submitter ID, try to get it from the "From:"
      header.  If that doesn't work, use the default from the config file.  */

  site = field_value (pr, SUBMITTER (database));
  if (site != NULL)
    {
      submitter = get_adm_record (SUBMITTER (database), site);
    }
  else
    {
      submitter = NULL;
    }
  def_subm = defaultSubmitter (database);
  if (submitter == NULL || (strcmp (site, def_subm) == 0))
    {
      site = derive_submitter (pr); 
      set_field (pr, SUBMITTER (database), site, err);
      submitter = get_adm_record (SUBMITTER (database), site);
    }

  if (submitter == NULL)
    {
      log_msg (LOG_INFO, 1, "resetting to default submitter from:", site);
      site = def_subm;

      /* If it's still an error, punt because this should not happen.  
         Actually, it can't happen unless the submitter file is empty.  */
      submitter = get_adm_record (SUBMITTER (database), site);
      if (submitter == NULL)
	{
	  setError (err, CODE_INVALID_ENUM,
		    newBadFieldEntry (SUBMITTER (database), def_subm, NULL),
		    "Can't find default submitter %s", def_subm);
	  free (def_subm);
	  return -1;
	}
      else
	{
	  set_field (pr, SUBMITTER (database), def_subm, err);
	}
    }

  if (submitter->admFields[SubmitterAdmRtime] == NULL ||
      submitter->admFields[SubmitterAdmRtime][0] == '\0')
    {
      submitter_rtime = -1;
    }
  else
    {
      submitter_rtime = atoi (submitter->admFields[SubmitterAdmRtime]);
    }

  /* If originator wasn't set, try to get the value from the From mail
     header.  This is what users usually expect. */
  {
    FieldIndex idx_origin = ORIGINATOR (database);
    const char *value_origin = (idx_origin == InvalidFieldIndex
				? NULL
				: field_value (pr, idx_origin));
    if (value_is_empty (value_origin))
      {
	const char *value_from;
	ErrorDesc err;
	value_from = header_value (pr, REPLY_TO);
	if (value_is_empty (value_from))
	  value_from = header_value (pr, FROM);
	if (! value_is_empty (value_from))
	  set_field (pr, idx_origin, xstrdup (value_from), &err);
      }
  }

  bug_group = field_value (pr, CATEGORY (database));
  if (bug_group != NULL)
    {
      category = get_adm_record (CATEGORY (database), bug_group);
    }
  else
    {
      category = NULL;
    }

  if (category == NULL)
    {
      bad_fields = newBadFieldEntry (CATEGORY (database), bug_group,
				     bad_fields);

      {
	char *message;
	asprintf (&message, "%s from: %s\n", defaultCategory(database),
		  bug_group);
	log_msg (LOG_INFO, 1, "resetting bug category to ", message);
	free (message);
      }
      bug_group = defaultCategory(database);
      set_field (pr, CATEGORY (database), defaultCategory(database), err);

      /* Again, but with default now.  If it's not there, kick this to
	 the GNATS_ADMIN.  */
      category = get_adm_record (CATEGORY (database), bug_group);
      if (category == NULL)
	{
	  setError (err, CODE_FILE_ERROR, 
		    "No `%s' directory under %s.",
		    defaultCategory(database), databaseDir (database));
	  return -1;
	}
    }

  /* Set responsible field, adding full name as well (if found).  If it's
     coming in with a Responsible: field already, don't wipe it out.  */
  {
    const char *p = field_value (pr, RESPONSIBLE (database));
    if (p != NULL && p[0] != '\0')
      {
	const char *person, *tmp;
	char *pcpy = NULL;

	if ((tmp = strchr (p, ' ')) != NULL)
	  {
	    pcpy = xstrdup (p);
	    pcpy[tmp - p] = '\0';
	    person = pcpy;
	  }
	else
	  {
	    person = p;
	  }
	
	responsible = get_responsible_address (database, person);
	if (pcpy != NULL)
	  {
	    free (pcpy);
	  }
      }
  }

  if (responsible == NULL)
    {
      responsible
	= get_responsible_address (database,
				   category->admFields[CategoryAdmPerson]);

      if (responsible != NULL)
	{
	  set_field (pr, RESPONSIBLE (database), 
		     responsible->admFields[ResponsibleAdmKey], err);

	  log_msg (LOG_INFO, 1, "responsible person is:",
		   field_value (pr, RESPONSIBLE (database)));
	}
    }

  {
    /* Check the subject and the synopsis lines.  If both of these are null,
     then forget it.  Currently it seems that values are set to '\0' when
     there is nothing there. */

    const char *synopsis = field_value (pr, SYNOPSIS (database));
    const char *subject = header_value (pr, SUBJECT);
    if ((subject == NULL || *subject == '\0') 
	&& (synopsis != NULL && *synopsis != '\0'))
      {
	set_header (pr, SUBJECT, xstrdup (synopsis));
      }

    if ((synopsis == NULL || *synopsis == '\0')
	&& (subject != NULL && *subject != '\0'))
      {
	set_field (pr, SYNOPSIS (database), subject, err);
      }
  }

  /* If the submitter isn't supposed to have a response time, don't bother
     trying to get it.  */
  if (submitter_rtime >= 0)
    {
      expired = get_response_time (database, submitter_rtime);
    }

  {
    /* Ensure the PR has a valid STATE.  We set the default state
       on any of these cases:
       
         1. field_value() returns NULL.
	 
	 2. if field_value returns the default value.  (field_value in
	    4.0beta1 returns the default, even if it is not really set.)

	 3. if set_field with the existing value fails.  If something
	    used set_field() previously, it was already validated,
	    but we call set_field to validate again to be sure.
    */
    char *val = (char *)field_value (pr, STATE (database));
    char *default_val = fieldDefForIndex (STATE (database))->default_value;
    
    if (val != NULL)
      {
	val = xstrdup (val);
      }
    if (val == NULL
	|| ! strcmp (val, default_val)
	|| ! set_field (pr, STATE (database), val, err))
      {
	set_field (pr, STATE (database), default_val, err);
      }
    if (val != NULL)
      {
	free (val);
      }
  }

  bug_number = pr_create (pr, err);

  if (bug_number > 0)
    {
      bad_fields = checkEnumTypes (pr, bad_fields, 1);
 
      /* If it isn't in the default category [pending], send all of
	 the mail for it, create an index entry, and run at_pr.  Otherwise,
	 just notify gnats-admin.  */
      if (strcmp (category->admFields[CategoryAdmKey],
		  defaultCategory (database)) != 0)
	{
	  /* Acknowledge the person's problem report.  */
	  if (submitterAck (database))
	    {
	      composeMailMessage (pr, NULL, "initial-response-to-submitter",
				  NULL, bad_fields, err);
	    }

	  composeMailMessage (pr, NULL, "initial-pr-notification", NULL, 
			      bad_fields, err);

	  if (submitter_rtime != -1 && notifyExpire (database)
	      && needs_analysis (pr))
	    {
	      if (run_atpr (pr, submitter, submitter_rtime, expired, bug_number,
			    err) != 0)
		{
		  bug_number = -1;
		}
	    }
	}
      else
	{
	  composeMailMessage (pr, NULL, "initial-pr-notification-pending",
			      NULL, bad_fields, err);
	}
    }

  free_adm_entry (category);
  free_adm_entry (submitter);
  free_adm_entry (responsible);
  if (def_subm != NULL)
    {
      free (def_subm);
    }

  return bug_number;
}

static int
missing_required_fields (const DatabaseInfo database, PR *pr, ErrorDesc *err)
{
  int cnt = 0;
  FieldList fields = getRequiredInputFields(database);

  while (fields != NULL)
    {
      const char *fldval = get_field_value (pr, NULL, fields->ent, NULL, NULL);
      if (value_is_empty (fldval))
	{
          setError (err, CODE_INVALID_PR_CONTENTS,
                    "Required field %s missing from new PR - cannot submit.",
                    complexFieldIndexToString (fields->ent),
                    field_value (pr, NUMBER (pr->database)));
	  cnt++;
	}
      fields = fields->next;
    }
  return cnt;
}

/* XXX ??? !!! FIXME Shouldn't be hardcoded.   */
static int
needs_analysis (PR *pr)
{
  const char *severity = field_value (pr, SEVERITY (pr->database));
  const char *priority = field_value (pr, PRIORITY (pr->database));
  if (severity != NULL && priority != NULL)
    {
      return strcmp (severity, "critical") == 0
	|| (strcmp (severity, "serious") == 0
	    && strcmp (priority, "high") == 0);
    }
  else
    {
      return 0;
    }
}

static char *
arg_quote (const char *s)
{
  char *buf;
  char *p;
  size_t finalLen = 0;
  const char *argPtr;

  if (s == NULL)
    {
      return xstrdup ("''");
    }

  for (argPtr = s; *argPtr != '\0'; argPtr++)
    {
      if (*argPtr == '\'')
	{
	  finalLen += 4;
	}
      else
	{
	  finalLen++;
	}
    }

  buf = xmalloc (finalLen + 2);

  p = buf;
  for (; *s != '\0'; s++)
    {
      if (*s == '\'')
	{
	  *p++ = '\'';
	  *p++ = '\\';
	  *p++ = '\'';
	  *p++ = '\'';
	}
      else
	{
	  *p++ = *s;
	}
    }

  if (p == buf)
    {
      *p++ = ' ';
    }
  
  *p = '\0';

  return buf;
}

static int
run_atpr (PR *pr, AdmEntry *submitter, int submitter_rtime, 
	  struct tm *expired, int bug_number, ErrorDesc *err)
{
  char *at_pr;
  char *command = NULL;
  char buf[GNATS_TIME_LENGTH];
  FILE *p;
  int i;
  static const char *ats[] = { "/usr/bin/at", "/bin/at", NULL };

  asprintf (&at_pr, "%s/at-pr", binDir (pr->database));

  strftime (buf, GNATS_TIME_LENGTH, "%H:%M %b %d", expired);

  for (i = 0; ats[i] != NULL; i++)
    {
      if (access (ats[i], X_OK) == 0)
	{
	  asprintf (&command, "%s %s  &> /dev/null", ats[i], buf);
	  break;
	}
    }

  if (command != NULL)
    {
      p = popen (command, "w");
      if (p == (FILE *)NULL)
	{
	  setError (err, CODE_FILE_ERROR,
		    "Couldn't open the pipe for at-pr.");
	  log_msg (LOG_INFO, 0, "popen failed:");
	  return 1;
	}
      else
	{
	  char *orig = arg_quote (field_value (pr, ORIGINATOR (pr->database)));
	  char *cont = arg_quote (submitter->admFields[SubmitterAdmContact]);
	  char *pa = arg_quote (gnatsAdminMailAddr (pr->database));
      
	  fprintf (p, "%s --database %s %d %d %s '%s' '%s' '%s'\n", at_pr,
		   databaseName (pr->database),
		   submitter_rtime, bug_number,
		   submitter->admFields[SubmitterAdmKey],
		   orig, cont, pa);

	  pclose (p);
	  free (orig);
	  free (cont);
	  free (pa);
	  return 0;
	}
    }
  else
    {
      setError (err, CODE_FILE_ERROR, "couldn't find `at'");
      return 1;
    }
}

/* Check to see if PR is a reply to an existing PR.  If so, return the
   PR ID of the PR.  */

static char *
checkIfReply (PR *pr, ErrorDesc *err)
{
  char *prID;
  char *prCat;
  AdmEntry *cat;
  const char *headerValue;
  struct re_pattern_buffer regex;
  struct re_registers regs;
  int i, len;
  reg_syntax_t old_syntax;

  headerValue = header_value (pr, SUBJECT);

  if (headerValue == NULL || *headerValue == '\0')
    {
      return NULL;
    }

  old_syntax = re_set_syntax (RE_NO_BK_PARENS | RE_NO_BK_VBAR
			      | RE_CHAR_CLASSES);
  memset ((void *) &regex, 0, sizeof (regex));

  {
    const char *const PAT = "\\<(PR[ \t#/]?|([-[:alnum:]_+.]+)/)([0-9]+)";
    re_compile_pattern (PAT, strlen (PAT), &regex);
  }
  i = re_search (&regex, headerValue, strlen (headerValue), 0,
		 strlen (headerValue), &regs);
  regfree (&regex);
  re_set_syntax (old_syntax);

  if (i < 0)
    {
      return NULL;
    }

  /* Check the category if there is one. */
  if (regs.start[2] > -1)
    {
      len = regs.end[2] - regs.start[2];
      prCat = xmalloc (len + 1);
      memcpy (prCat, headerValue + regs.start[2], len);
      prCat[len] = '\0';

      /* See if the category exists: */
      cat = get_adm_record (CATEGORY (pr->database), prCat);
      free_adm_entry(cat);
      free (prCat);
      if (cat == NULL)
	{
          free (regs.start);
          free (regs.end);
	  return NULL;
	}
    }

  /* Check the PR number. */
  len = regs.end[3] - regs.start[3];
  prID = xmalloc (len + 1);
  memcpy (prID, headerValue + regs.start[3], len);
  prID[len] = '\0';

  free (regs.start);
  free (regs.end);

  if (! pr_exists (pr->database, prID, err))
    {
      free (prID);
      prID = NULL;
    }

  return prID;
}

/*  This function compares the e-mail address in the "From:" header of a new
    PR with entries in the "addresses" file.  If there is a match, we return
    the submitter ID from "addresses", otherwise we return the default
    submitter.   */

static char *
derive_submitter (PR *pr)
{
  const char *from_address;
  char *from_string;
  char *from_copy = NULL;
  char *line;
  char *res = NULL;

  char *s;

  FILE *fp;

  from_address = header_value (pr, FROM);

  if (from_address == NULL || *from_address == '\0')
    {
      return defaultSubmitter (pr->database);
    }

  from_copy = xstrdup (from_address);
  s = from_copy + strlen(from_copy) - 1;
  
  /* Grab the address from out between the <>. */
  if (*s == '>')
    {
      *s = '\0';
      from_string = strrchr (from_copy, '<');
      if (from_string != NULL)
	{
	  from_string++;
	}
      else
	{
	  from_string = from_copy;
	}
    }
  else
    {
      char *t = strchr (from_copy, ' ');

      if (t != NULL)
	{
	  *t = '\0';
	}
      from_string = from_copy;
    }
  {
    char *path = gnats_adm_dir (pr->database, "addresses");
    fp = fopen (path, "r");
    free (path);
  }
  if (fp != NULL)
    {
      while (res == NULL && (line = read_line (fp, NULL)) != NULL)
	{
	  if (*line != '#')
	    {
	      AdmEntry *ent = build_adm_entry (line, InvalidFieldIndex);
	      if (ent->fieldcount == 2)
		{
		  if (strcasecmp (ent->admFields[1], from_string) == 0)
		    {
		      res = xstrdup (ent->admFields[0]);
		    }
		}
	      free_adm_entry (ent);
	    }
	  free (line);
	}

      fclose (fp);
    }

  free (from_copy);
  if (res != NULL)
    {
      return res;
    }
  else
    {
      return defaultSubmitter (pr->database);
    }
}



/* Append an email reply in NEW_PR to an existing PR with PR number NUMBER.
   The headers of the email mesage are included in NEW_PR, while the
   contents of the PR have not yet been read from INFILE.

   The entire body of the message is appended, while just the
   To:, From:, Date:, and Subject: lines from the header are included.  */
static int
append_report (FILE *infile, PR *new_pr, const char *number,
	       ErrorDesc *err)
{
  char line[1024]; /* This size doesn't matter here */
  char *buf;
  size_t buf_size, len;
  const char *from, *to, *subject, *date, *cc;
  PR *pr;

  from = header_value (new_pr, FROM);
  to = header_value (new_pr, TO);
  cc = header_value (new_pr, CC);
  subject = header_value (new_pr, SUBJECT);
  date = header_value (new_pr, DATE);

  if (*date == '\0') 
    {
      date = get_curr_date ();
    }

  if (! pr_exists (new_pr->database, number, err))
    {
      return 1;
    }

  /* Now, add some header information before we get the rest
     of their message.  */
  /* XXX ??? !!! The format of this should be in the config file... the only
     issue is handling the indentation of the message text.  */
  asprintf (&buf, "\
From: %s\n\
To: %s\n\
Cc: %s\n\
Subject: %s\n\
Date: %s\n\
\n",
	    from, to, cc, subject, date);

  /* Copy the message from infile, indenting each line by one character */
  len = buf_size = strlen (buf);
  while ((fgets (line, sizeof (line) - 1, infile)) != NULL)
    {
      const size_t line_len = strlen (line);
      const size_t req_len = len + line_len + 2;

      if (buf_size < req_len)
	{
	  buf_size = ((2*buf_size <= req_len) ? req_len : 2*buf_size);
	  buf = xrealloc (buf, buf_size);
	}
      if (buf[len-1] == '\n')
	{
	  buf[len++] = ' ';
	}
      memcpy (buf + len, line, line_len + 1);
      len += line_len;
    }

  fclose (infile);

  pr = pr_load_by_id (new_pr->database, number, 0, err);
  if (pr == NULL)
    {
      return 1;
    }
  else
    {
      if (edit_field (pr->database, number, AUDIT_TRAIL (pr->database), 1,
		      xstrdup (buf), NULL, header_value (pr, FROM), err)
	  == 0)
	{
	  return 1;
	}
    }

  /*
   * This could be a silent update to the PR, in which case, don't tell
   * the world about it (e.g. automatic build/source control messages).
   */
  if (raw_header_value (new_pr, X_GNATS_NONOTIFY) == NULL)
    {
      FormatNamedParameter *params = NULL;

      params = allocateNamedParameter ("$MailFrom", from, params);
      params = allocateNamedParameter ("$MailTo", to, params);
      params = allocateNamedParameter ("$MailSubject", subject, params);
      params = allocateNamedParameter ("$MailCC", cc, params);
      params = allocateNamedParameter ("$NewAuditTrail", buf, params);
      composeMailMessage (pr, NULL, "appended-email-response", params, NULL,
			  err);
      freeFormatParameterList (params);
    }
  free (buf);

  unblock_signals ();
  return 0;
}

/* Submit the PR whose contents are referred to by FP.
 * Return the new PR number if it's a new PR, or return the PR number of
 * the PR which was appended to if it's an existing PR. */
int
submit_pr (const DatabaseInfo database, FILE *fp, ErrorDesc *err)
{
  int result, retval;
  PR *pr = allocPR (database);

  result = (read_header (pr, fp) >= 0);

  if (result != 0)
    {
      /* Check if the message is a reply to an existing PR; if it is, then
	 just append this message to the end of that PR.  Otherwise, process
	 it normally.  */
      char *prID = checkIfReply (pr, err);
      if (prID != NULL)
	{
	  /* Before inserting the reply, see if someone's got the PR
	     locked; if they do, then try it at the next run of the queue.  */
	  if (isPrLocked (database, prID))
	    {
	      result = 0;
	      setError (err, CODE_LOCKED_PR, "PR already locked");
	    }
	  else
	    {
	      retval = append_report (fp, pr, prID, err);
	      result = (retval == 0) ? atoi (prID) : 0;
	      free_pr (pr);
	    }
	  free (prID);
	}
      else
	{
	  read_pr (pr, fp, 0);
	  retval = createNewPR (pr, err);
	  result = (retval < 0) ? 0 : retval;
	}
    }
  else
    {
      setError (err, CODE_UNREADABLE_PR, "Failure reading header");
    }
  return result;
}
