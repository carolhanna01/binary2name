/* Functions for manipulating a PR.
   Copyright (C) 2001 Milan Zamazal
   Copyright (C) 1993, 1994, 1995, 1999, 2000, 2007 Free Software Foundation, Inc.
   Originally contributed by Tim Wicinski (wicinski@barn.com)
   and Brendan Kehoe (brendan@cygnus.com).
   Completely rewritten by Bob Manson (manson@juniper.net).
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
#include "pcodes.h"
#include "ds.h"

/* These will eventually be properly configurable.  */
#define PR_FIELD_TAG_START		'>'
#define PR_FIELD_TAG_END		':'

static FieldIndex find_field_num (const char *header, size_t len, PR *pr);

struct PRDataString
{
  char *buffer;
  char *string;
};

static void
freePRDataString (struct PRDataString *ptr)
{
  if (ptr->buffer != NULL)
    {
      free (ptr->buffer);
      ptr->buffer = NULL;
      ptr->string = NULL;
    }
  else if (ptr->string != NULL)
    {
      free (ptr->string);
      ptr->string = NULL;
    }
}

/* Used when reading in a PR.  */
struct PR_private 
{
  /* Buffer used for current field contents. */
  char *buffer;

  /* Values of the fields.  */
  struct PR_private_value
  {
    /* The actual line contents.  */
    struct PRDataString data;
    /* Reason for change, if one is needed.  */
    struct PRDataString reason;
  } *value;

  /* Values of the headers. */
  char **header;

  /* The total size of the buffer.  */
  size_t bufferSize;
  /* Length of the current contents. */
  size_t bufferLen;
  /* Buffer used for the Unformatted: field.  */
  char *unformatted;
  /* Total size of the unformatted buffer.  */
  size_t unformattedSize;
  /* Length of the current contents. */
  size_t unformattedLen;
  /* Current read state.  */
  enum ReadState {
    NormalState, MultiTextState
  } readState;
  /* The index of the multitext field being read in, if any.  */
  FieldIndex multiFieldIndex;
  /* Flag indicating that the multitext field being filled in is the
     -Changed-Why: field. */
  int multiFieldIsReason;

  /* Non-zero if this is the field indicating the reason for the change. */
  int isReason;

  /* Set if any data has actually been stored in the PR.  */
  int non_zero;
};

static const char *headerNames[NUM_HEADER_ITEMS] = 
{
  "From",
  "Return-Path:",
  "Received:",
  "Message-Id:",
  "Date:",
  "From:",
  "Sender:",
  "Reply-To:",
  "To:",
  "Apparently-To:",
  "Cc:",
  "In-Reply-To:",
  "Subject:",
  "References:",
  "X-Send-Pr-Version:",
  "X-GNATS-Notify:",
  "X-GNATS-NoNotify:"
};

/* Allocate a PR struct.  */
PR *
allocPR (const DatabaseInfo database)
{
  PR *res = (PR *) xmalloc (sizeof (PR));
  int field_num = get_num_fields (database);

  res->database = database;
  pr_init (res);  /* initialize the private ds info */
  res->private = (struct PR_private *) xmalloc (sizeof (struct PR_private));
  if (field_num == -1)
    {
      res->private->value = NULL;
    }
  else
  {
      res->private->value
	= (struct PR_private_value *) xmalloc (sizeof (struct PR_private_value)
					       * field_num);
      memset (res->private->value, 0,
	      sizeof (struct PR_private_value) * field_num);
  }
  res->private->header 
    = (char **) xmalloc (sizeof (char *) * NUM_HEADER_ITEMS);
  memset (res->private->header, 0, sizeof (char *) * NUM_HEADER_ITEMS);

  res->private->buffer = NULL;
  res->private->bufferLen = 0;
  res->private->unformatted = NULL;
  res->private->unformattedSize = 0;
  res->private->unformattedLen = 0;
  res->private->multiFieldIndex = 0;
  res->private->isReason = 0;
  res->private->multiFieldIsReason = 0;
  res->private->non_zero = 0;
  res->prune = 0;
  res->read_in = 0;

  return res;
}

/* Initializes PR for reading.  Each line of the PR should be passed
   in via addLineToPR (). */

void
initReadPR (PR *pr)
{
  free_pr_contents (pr);
  pr->private->multiFieldIndex = InvalidFieldIndex;
  pr->private->readState = NormalState;
  pr->private->bufferSize = BUFSIZ;
  pr->private->unformattedSize = BUFSIZ;
  pr->private->buffer = xmalloc (pr->private->bufferSize);
  pr->private->unformatted = xmalloc (pr->private->unformattedSize);
  pr->private->bufferLen = 0;
  pr->private->unformattedLen = 0;
}

/* Read the file pointed to by FP, and look for matching field names, trying
   to attach values to those names.  If PRUNE is non-zero, multitext
   fields are not read in.  */

void
read_pr (PR *pr, FILE *fp, int prune)
{
  if (fp != NULL && pr != NULL)
    {
      size_t linelen = 0;
      char *line;

      initReadPR (pr);
      pr->prune = prune;

      while ((line = read_line (fp, &linelen)) != NULL)
	{
	  addLineToPR (pr, line, line, linelen, prune);
	  linelen = 0;
	}

      finishReadPR (pr, prune);
    }
}

/* Grab a field header name.  The name is returned as a pointer into
   the line (it's always the passed-in value of *LINEPTR).  The length
   of the header is returned in HEADERLEN. *LINEPTR is adjusted to
   point to the rest of the line (any whitespace between the name and
   the rest of the line is skipped). */

static const char *
getFieldHeader (char **linePtr, size_t *headerLen, size_t linelen)
{
  char *line = *linePtr;
  char *lineEnd = line + linelen;
  char *res = *linePtr;

  /* Grab the first word ending in space or : */
  while (line < lineEnd && (! isspace ((int)(unsigned char) *line)) && *line != ':')
    {
      line++;
    }

  /* We want to include the : in the result */
  if (line < lineEnd && *line == ':')
    {
      line++;
    }

  *headerLen = line - res;

  while (line < lineEnd && isspace ((int)(unsigned char) *line))
    {
      line++;
    }

  *linePtr = line;
  return res;
}

/* Add line LINE of length LINELEN to PR.  If PRUNE is non-zero,
   multitext fields are not added. */

void
addLineToPR (PR *pr, char *buffer, char *line, size_t linelen, int prune)
{
  FieldIndex fieldIndex = InvalidFieldIndex;
  struct PR_private *prv = pr->private;

  if (linelen > 1 && line[0] == PR_FIELD_TAG_START)
    {
      char *temp = line;
      size_t headerLen;
      const char *header = getFieldHeader (&temp, &headerLen, linelen);
      if (header != NULL)
	{
	  if (header[headerLen - 1] == ':')
	    {
	      FieldIndex newFieldIndex 
		= find_field_num (header, headerLen, pr);

	      if (newFieldIndex != InvalidFieldIndex)
		{
		  fieldIndex = newFieldIndex;
		  pr->private->readState = NormalState;
		  linelen -= (temp - line);
		  line = temp;
		}
	    }
	}
    }

  /* Heuristic for finding included PR fields */
  if (prv->readState == MultiTextState)
    {
      /* Glluuuugh.  XXX ??? !!!  So we're saying "if the field index
	 is the same as the current multitext field being read, or if
	 the field was already set, then we don't have a valid field
	 number?  MOMMYYYY!!!! I'm tellin... "heuristic" my butt.  */
      if (fieldIndex != InvalidFieldIndex
	  && (fieldIndex == prv->multiFieldIndex
	      || prv->value[fieldNumber (fieldIndex)].data.string != NULL))
	{
	  fieldIndex = InvalidFieldIndex;
	}
    }

  /* If we can't find the name and we are not in multi_text line mode,
     it goes into unformatted.  */
  if (fieldIndex != InvalidFieldIndex)
    {
      if (fieldDefForIndex (fieldIndex)->datatype == MultiText
	  || prv->isReason)
	{
	  prv->readState = MultiTextState;

	  if (! prune)
	    {
	      if (prv->multiFieldIndex != InvalidFieldIndex)
		{
		  struct PRDataString *dest;

		  if (prv->multiFieldIsReason)
		    {
		      dest = &(prv->value[fieldNumber (prv->multiFieldIndex)].reason);
		    }
		  else
		    {
		      dest = &(prv->value[fieldNumber (prv->multiFieldIndex)].data);
		    }
		  prv->buffer[prv->bufferLen++] = '\0';
		  if (dest->string != NULL)
		    {
		      freePRDataString (dest);
		    }
		  /* Doing two memcpys here isn't so great.  Could just shove
		     the old buffer into the field, then allocate a new
		     one.  */
		  dest->string = xmalloc (prv->bufferLen);
		  memcpy (dest->string, prv->buffer, prv->bufferLen);
		  prv->non_zero = 1;
		}
	      memcpy (prv->buffer, line, linelen);
	      prv->bufferLen = linelen;
	    }
	  prv->multiFieldIndex = fieldIndex;
	  prv->multiFieldIsReason = prv->isReason;
	}
      else 
	{
	  struct PRDataString *dest = &(prv->value[fieldNumber (fieldIndex)].data);

	  /* Skip leading spaces; I don't think this is really needed
	     anyway, since we've already done it in getFieldHeader ().  */
	  while (linelen > 0 && isspace ((int)(unsigned char) *line))
	    {
	      line++;
	      linelen--;
	    }

	  /* Fields that aren't MultiText shouldn't have spaces
	     at the end.  */
	  while (linelen > 0 && isspace ((int)(unsigned char) line[linelen - 1]))
	    {
	      linelen--;
	    }

	  line[linelen] = '\0';
	  if (dest->string != NULL)
	    {
	      freePRDataString (dest);
	    }
	  dest->string = line;
	  dest->buffer = buffer;
	  prv->non_zero = 1;
	  prv->readState = NormalState;
	  buffer = NULL;
	}
    }
  else if (prv->readState == MultiTextState) 
    {
      if (! prune)
	{
	  if ((linelen + prv->bufferLen) >= prv->bufferSize)
	    {
	      while ((linelen + prv->bufferLen + 2)
		     >= prv->bufferSize)
		{
		  prv->bufferSize = (prv->bufferSize * 2) + 2;
		}
	      prv->buffer = (char *) xrealloc (prv->buffer,
					       prv->bufferSize);
	    }
	  /* Insert a space if there isn't one already.  */
	  if (prv->multiFieldIndex == UNFORMATTED (pr->database) 
	      && line[0] != ' ')
	    {
	      prv->buffer[prv->bufferLen] = ' ';
	      prv->bufferLen++;
	    }
	  memcpy (prv->buffer + prv->bufferLen, line, linelen);
	  prv->bufferLen += linelen;
	}
    }
  else
    {
      /* It must be unformatted.  This is done separately since an
	 unformatted field can show up anywhere.  */
      prv->readState = NormalState;

      /* We skip unformatted text if we're pruning.  */
      if (! prune)
	{
	  if ((linelen + prv->unformattedLen + 1) 
	      >= prv->unformattedSize)
	    {
	      while ((linelen + prv->unformattedLen + 1) 
		     >= prv->unformattedSize)
		{
		  prv->unformattedSize 
		    = (prv->unformattedSize * 2) + 1;
		}
	      prv->unformatted =
		(char *) xrealloc (prv->unformatted,
				   prv->unformattedSize);
	    }

	  /* Put a space in front of each unformatted line, showing it as
	     "quoted text" */

	  prv->unformatted[prv->unformattedLen] = ' ';
	  prv->unformattedLen++;
	  memcpy (prv->unformatted + prv->unformattedLen,
		  line, linelen);
	  prv->unformattedLen += linelen;
	}
    }
  if (buffer != NULL)
    {
      free (buffer);
    }
}

/* Finish reading PR; if PRUNE is non-zero, we're skipping multitext
   fields.  */

void
finishReadPR (PR *pr, int prune)
{
  struct PR_private *prv = pr->private;

  /* Store the last multitext field, if it wasn't stored.  */
  if (prv->multiFieldIndex != InvalidFieldIndex && ! prune)
    {
      struct PRDataString *dest;

      if (prv->multiFieldIsReason)
	{
	  dest = &(prv->value[fieldNumber (prv->multiFieldIndex)].reason);
	}
      else
	{
	  dest = &(prv->value[fieldNumber (prv->multiFieldIndex)].data);
	}
      if (dest->string != NULL)
	{
	  freePRDataString (dest);
	}
      prv->buffer[prv->bufferLen] = '\0';
      dest->buffer = prv->buffer;
      dest->string = prv->buffer;
      prv->non_zero = 1;
      prv->multiFieldIndex = InvalidFieldIndex;
    }
  else
    {
      free (prv->buffer);
    }

  /* Check to see if the unformatted field was used. */
  if (prv->unformattedLen != 0 && ! prune)
    {
      prv->unformatted[prv->unformattedLen] = '\0';
      if (prv->value[fieldNumber (UNFORMATTED (pr->database))].data.string 
	  != NULL)
	{
	  int len 
	    = strlen (prv->value[fieldNumber (UNFORMATTED (pr->database))].data.string) + 1;
	  if ((prv->unformattedLen + len) 
	      > prv->unformattedSize)
	    {
	      prv->unformattedSize = prv->unformattedLen + len;
	      prv->unformatted 
		= (char *) xrealloc (prv->unformatted,
				     prv->unformattedSize);
	    }

	  memcpy (prv->unformatted + prv->unformattedLen,
		  prv->value[fieldNumber (UNFORMATTED (pr->database))].data.string,
		  len);
	  unsetField (pr, UNFORMATTED (pr->database));
	}

      /* ??? XXX !!! WTF is this shit?  */
      if (prv->value[fieldNumber (DESCRIPTION (pr->database))].data.string 
	  != NULL)
	{
	  prv->value[fieldNumber (UNFORMATTED (pr->database))].data.string 
	    = prv->unformatted;
	  prv->value[fieldNumber (UNFORMATTED (pr->database))].data.buffer 
	    = NULL;
	}
      else
	{
	  prv->value[fieldNumber (DESCRIPTION (pr->database))].data.string
	    = prv->unformatted;
	  prv->value[fieldNumber (DESCRIPTION (pr->database))].data.buffer
	    = NULL;
	}
      prv->non_zero = 1;
    }
  else
    {
      free (prv->unformatted);
    }
  prv->buffer = NULL;
  prv->unformatted = NULL;
  pr->read_in = 1;
}

/* Escape leading dots and end each line with \r\n */
void
write_multitext (fp, str, eolTerminator)
     FILE *fp;
     const char *str;
     const char *eolTerminator;
{
  const char *s, *e, *end = str + strlen (str);

  if (eolTerminator == NULL
      || (eolTerminator[0] == '\n' && eolTerminator[1] == '\0'))
    {
      fputs (str, fp);
      return;
    }
  
  s = str;
  while (s != NULL && s < end)
    {
      if (*s == '.')
	{
	  fputc ('.', fp);
	}
      e = strchr (s, '\n');
      if (e != NULL)
        {
	  fwrite (s, 1, e - s, fp);
	  fputs (eolTerminator, fp);
          s = e + 1;
        }
      else
        {
	  fputs (s, fp);
	  s = NULL;
        }
    }
}

void
write_pr_field (FILE *fp, FieldIndex field, const char *fieldText,
		const char *eolTerminator)
{
  if (! fieldDefForIndex (field)->editonly && fieldText != NULL)
    {
      static const char *spaceList = "                ";
      int flen = strlen (fieldDefForIndex (field)->name) + 2;

      if (flen > 15)
	{
	  flen = 15;
	}
      switch (fieldDefForIndex (field)->datatype)
	{
	case MultiText:
	  {
	    size_t len;

	    fputc (PR_FIELD_TAG_START, fp);
	    fputs (fieldDefForIndex (field)->name, fp);
	    fputc (PR_FIELD_TAG_END, fp);
	    fputs (eolTerminator, fp);
	    write_multitext (fp, fieldText, eolTerminator);
	    len = strlen (fieldText);
	    if (len > 0 && fieldText[len - 1] != '\n')
	      {
		fputs (eolTerminator, fp);
	      }
	    break;
	  }
	case Date:
	  {
	    char t[GNATS_TIME_LENGTH];
	    time_t timeVal = get_any_date (fieldText);
	    if (timeVal == 0 || timeVal == -1)
	      t[0] = '\0';
	    else
	      gnats_strftime (t, GNATS_TIME_LENGTH, "%a %b %d %H:%M:%S %z %Y",
			      localtime (&timeVal));
	    fprintf (fp, "%c%s%c%s %s%s", PR_FIELD_TAG_START,
		     fieldDefForIndex (field)->name,
		     PR_FIELD_TAG_END, spaceList + flen, t, eolTerminator);
	    break;
	  }
	default:
	  {
	    fputc (PR_FIELD_TAG_START, fp);
	    fputs (fieldDefForIndex (field)->name, fp);
	    fputc (PR_FIELD_TAG_END, fp);
	    fputs (spaceList + flen, fp);
	    fputc (' ', fp);
	    fputs (fieldText, fp);
	    fputs (eolTerminator, fp);
	    break;
	  }
	}
    }
}

/* Write the entire PR into the file passed in via FP.  */
void 
write_entire_pr (FILE *fp, PR *pr, const char *eolTerminator)
{
  int i;
  int num_fields = get_num_fields (pr->database);

  for (i = 0; i < num_fields; i++)
    {
      write_pr_field (fp, getNthField (pr->database, i),
		      field_value (pr, getNthField (pr->database, i)),
		      eolTerminator);
    }
}


int
verify_enum (FieldIndex i, const char *v)
{
  StringList *p;

  if (fieldDefForIndex (i)->datatype == MultiEnum)
    {
      return verifyMultiEnum (i, v);
    }

  if (fieldDefForIndex (i)->datatype != Enum)
    {
      return -1;
    }

  if (fieldDefForIndex (i)->allow_any_value)
    {
      return 1;
    }

  if (v != NULL)
    {
      for (p = fieldDefForIndex (i)->enumValues; p != NULL; p = p->next)
	{
	  if (strcasecmp (v, p->name) == 0)
	    {
	      return 1;
	    }
	}
    }
  else if (fieldDefForIndex (i)->default_value == NULL)
    {
      return 1;
    }

  return 0;
}

int
verifyMultiEnum (FieldIndex i, const char *value)
{
  const char *curp = value;
  const char *separators;

  if (fieldDefForIndex (i)->datatype != MultiEnum)
    {
      return -1;
    }

  if (value == NULL)
    {
      if (fieldDefForIndex (i)->default_value == NULL)
	{
	  return 1;
	}
      else
	{
	  return 0;
	}
    }

  separators = fieldDefForIndex (i)->multiEnumSeparator;

  while (1)
    {
      char *nextp = strpbrk (curp, separators);
      size_t len;
      StringList *p;

      if (nextp == NULL)
	{
	  len = strlen (curp);
	}
      else
	{
	  len = nextp - curp;
	}

      for (p = fieldDefForIndex (i)->enumValues; p != NULL; p = p->next)
	{
	  if (p->name[len] == '\0' && strncasecmp (curp, p->name, len) == 0)
	    {
	      break;
	    }
	}
      if (p == NULL)
	{
	  return 0;
	}
      if (nextp == NULL)
	{
	  return 1;
	}
      curp = nextp + 1;
    }
}

/* Check all the enumerated typed fields that are declared, and check
   to make sure they're all good values.  Reset any null or invalid
   entries.  */
BadFields
checkEnumTypes (PR *pr, BadFields currBadFields, int isInitialPR)
{
  int i;
  int pr_fields = get_num_fields (pr->database);
  BadFields badFields = currBadFields;

  for (i = 0; i < pr_fields; i++)
    {
      FieldIndex field = getNthField (pr->database, i);
      FieldType type = fieldDefForIndex (field)->datatype;
      if ((type == Enum || type == MultiEnum)
	  && ! fieldDefForIndex (field)->allow_any_value)
	{
	  if (! verify_enum (field, pr->private->value[i].data.string))
	    {
	      if ((! isInitialPR)
		  || (pr->private->value[i].data.string != NULL))
		{
		  badFields 
		    = newBadFieldEntry (field,
					pr->private->value[i].data.string,
					badFields);
		  unsetField (pr, field);
		}
	      if (fieldDefForIndex (field)->default_value != NULL)
		{
		  pr->private->value[i].data.string
		    = xstrdup (fieldDefForIndex (field)->default_value);
		  pr->private->non_zero = 1;
		  pr->private->value[i].data.buffer = NULL;
		}
	    }
	}
    }

  return badFields;
}

/* Find the field number for STRING. It's assumed to have a PR_FIELD_TAG_END
   at string[len - 1] and begins with a PR_FIELD_TAG_START.  */

static FieldIndex
find_field_num (const char *string, size_t len, PR *pr)
{
  FieldIndex i = InvalidFieldIndex;

  if (string == NULL || string[0] != PR_FIELD_TAG_START 
      || string[len - 1] != PR_FIELD_TAG_END)
    {
      return InvalidFieldIndex;
    }

  string++;
  len--;

  if (len > 13
      && (strncasecmp (string + len - 13, "-Changed-Why:", 13) == 0))
    {
      len -= 12;
      pr->private->isReason = 1;
    }
  else
    {
      pr->private->isReason = 0;
    }

  i = find_field_index_with_len (pr->database, string, len - 1);

  return i;
}

const char *
field_change_reason (PR *pr, FieldIndex field)
{
  if (field == InvalidFieldIndex)
    {
      abort ();
    }

  if (pr->prune || pr->private->header == NULL 
      || pr->private->value == NULL 
      || pr->private->value[fieldNumber (field)].data.string == NULL)
    {
      return NULL;
    }
  else
    {
      return pr->private->value[fieldNumber (field)].reason.string;
    }
}

void
setFieldChangeReason (PR *pr, FieldIndex field,
		      const char *text)
{
  if (field == InvalidFieldIndex)
    {
      abort ();
    }
  freePRDataString (&pr->private->value[fieldNumber (field)].reason);
  pr->private->value[fieldNumber (field)].reason.string = xstrdup (text);
  pr->private->value[fieldNumber (field)].reason.buffer = NULL;
}
  

/* Returns the value of field FIELD from PR.  If the PR does not directly
   have a value, check the field cache; if that's also empty, we return
   the field's default value.  

   The policy about returning a default value is a bit broken--we
   should only do that if the PR's actually been read in from a file.  */

const char *
field_value (PR *pr, FieldIndex field)
{
  char *result = NULL;

  if (field == InvalidFieldIndex)
    {
      abort ();
    }

  if (pr->private != NULL && pr->private->value != NULL)
    {
      result = pr->private->value[field->number].data.string;
    }
  
  if (result == NULL)
    {
      result = field_cache_value (pr, field);
      if (result != NULL
	  && fieldDefForIndex (field)->datatype == Date
	  && strcmp (result, "0") == 0)
	{
	  result = NULL;
	}
    }
  
  /* Doing this here doesn't *seem* right.  */
  if (result == NULL)
    {
      if (fieldDefForIndex (field)->default_value != NULL)
	{
	  pr->private->value[fieldNumber (field)].data.string
	    = xstrdup (fieldDefForIndex (field)->default_value);
	  pr->private->value[fieldNumber (field)].data.buffer = NULL;
	  result = pr->private->value[fieldNumber (field)].data.string;
	  pr->private->non_zero = 1;
	}
    }

  return result;
}

int
prFieldHasValue (PR *pr, FieldIndex field)
{
  if (field == InvalidFieldIndex)
    {
      abort ();
    }

  return (pr->private != NULL && pr->private->value != NULL
	  && pr->private->value[fieldNumber (field)].data.string != NULL);
  
}

/* Set the value of the PR field INDEX to STRING.  Returns TRUE if the
   operation was successful, FALSE if not (e.g., if STRING was a bad
   value for an enumerated field).  */
bool 
set_field (PR *pr, FieldIndex field, const char *string,
	   ErrorDesc *err)
{
  bool valid;

  if (field == InvalidFieldIndex)
    {
      abort ();
    }

  valid = validateFieldValue (field, string, err, 0);

  if (valid)
    {
      unsetField (pr, field);
      pr->private->value[fieldNumber (field)].data.string 
	= (char *) xstrdup (string);
      pr->private->value[fieldNumber (field)].data.buffer = NULL; 
      pr->private->non_zero = 1;
    }

  return valid;
}

void
unsetField (PR *pr, FieldIndex field)
{
  if (field == InvalidFieldIndex)
    {
      abort ();
    }
  freePRDataString (&pr->private->value[fieldNumber (field)].data);
  freePRDataString (&pr->private->value[fieldNumber (field)].reason);
}



void
freeStringList (StringList *s)
{
  StringList *n;

  while (s != NULL)
    {
      n = s->next;
      free (s->name);
      free (s);
      s = n;
    }
}


/* Look to see if STRING is a mail header we know about.  */
static short 
lookup_header (const char *string, size_t len)
{
  Header_Name i;

  for (i = (Header_Name) 0; i < NUM_HEADER_ITEMS; i++)
    if (headerNames[i] != NULL
	&& (strncasecmp (headerNames[i], string, len) == 0))
      {
	return i;
      }

  return InvalidHeaderName;
}

/* If there's more than one instance of a given header, keep
   the first one and ignore any future ones.  There's one
   exception to this rule: the `Received:' header.  We are likely
   to get many of them, so just drop them down into the "value"
   of the first received header. */
static void
set_continued_header (PR *pr, Header_Name i, char *buf)
{
  char *b;

  if (pr->private->header[i] != NULL)
    {
      if (keepReceivedHeaders (pr->database) && i == RECEIVED)
	{
	  if (*buf == ' ')
	    {
	      asprintf (&b, "%s\nReceived:%s", pr->private->header[i], buf);
	    }
	  else
	    {
	      asprintf (&b, "%s\nReceived: %s", pr->private->header[i], buf);
	    }
	  free (pr->private->header[i]);
	  free (buf);
	  pr->private->header[i] = b;
	}
    }
  else
    {
      pr->private->header[i] = buf;
    }
}

int
read_header (PR *pr, FILE *fp)
{
  Header_Name currHeader = -1;
  bool processing = FALSE;
  bool headers_found = FALSE;
  char *buf = NULL;
  int c;

#define PEEK(FP) (c = getc (FP), ungetc (c, FP))
  
  while (PEEK (fp) != EOF)
    {
      if ((c == '\t' || c == ' ') && processing)
	{
	  /* RFC-822 multi-line header.  */
	  char *line = read_line (fp, NULL);
	  append_string (&buf, line);
	  free (line);
	  line = NULL;
	}
      else
	{
	  if (processing)
	    {
	      size_t buflen = strlen (buf);

	      if (buflen > 0 && buf[buflen - 1] == '\n')
		{
		  /* Strip that blasted newline off.  */
		  buf [buflen - 1] = '\0';
		}
	      set_continued_header (pr, currHeader, buf);
	      buf = NULL;
	      processing = FALSE;
	    }

	  /* If there's a blank line or a PR field, get out quickly.
	     We used to also jump out when we saw a `>', but some
	     versions of Unix, including USL Unix, place a `>From:'
	     in the header for remote mail. */
	  /* But not handling `>' at all is bad too!  It rejects PRs that
	     contain no mail headers and no newline. */
	  if (c == '\n')
	    {
	      while (c == '\n')
		{
		  c = getc (fp);
		}

	      ungetc (c, fp);
	      break;
	    }
	  else
	    {
	      size_t lineLen = 0;
	      char *line = read_line (fp, &lineLen);
	      char *temp = line;
	      size_t headerLen;
	      const char delimiter = '>';
	      const char *header = getFieldHeader (&temp, &headerLen, lineLen);

	      if (header != NULL)
		{
		  currHeader = lookup_header (header, headerLen);
		  if (currHeader != InvalidHeaderName)
		    {
		      headers_found = TRUE;
		      processing = TRUE;
		      buf = xstrdup (temp);
		    }
		  else
		    {
		      if (c == delimiter
			  && strncasecmp (">From:", header, headerLen))
			break;
		    }
		}
	      else
		{
		  if (c == delimiter)
		    break;
		  currHeader = InvalidHeaderName;
		}
	      free (line);
	      line = NULL;
	    }
	}
    }

  if (feof (fp))
    {
      return -1;
    }

  /* If no headers found, then back up.  
     XXX ??? !!! Very bad.  What if we're on a stream we can't rewind?  */
  if (!headers_found)
    {
      rewind (fp);
    }

  return 0;
}

static void
write_header_field (FILE *fp, PR *pr, Header_Name field,
		    const char *eolTerminator)
{
  fputs (headerNames[field], fp);
  if (pr->private->header[field][0] != '\0')
    {
      if (pr->private->header[field][0] != ' ')
	{
	  fputc (' ', fp);
	}
      fputs (pr->private->header[field], fp);
    }
  fputs (eolTerminator, fp);
}

void 
write_entire_header (FILE *fp, PR *pr, const char *eolTerminator)
{
  Header_Name i;

  for (i = (Header_Name) 0; i < NUM_HEADER_ITEMS; i++)
    {
      if (pr->private->header[i] != NULL)
	{
	  write_header_field (fp, pr, i, eolTerminator);
	}
    }
}


const char *
header_name (Header_Name name)
{
  if (name >= (int) NUM_HEADER_ITEMS)
    return NULL;
  else
    return headerNames[name];
}

Header_Name
find_header_index (const char *name)
{
  return lookup_header (name, strlen (name));
}

const char *
raw_header_value (PR *pr, Header_Name name)
{
  if (name >= NUM_HEADER_ITEMS || name < 0)
    {
      return NULL;
    }

  return pr->private->header[name];
}

const char *
header_value (PR *pr, Header_Name name)
{
  const char *s = raw_header_value (pr, name);

  if (s == NULL)
    {
      pr->private->header[name] = (char *) xstrdup ("");
      s = pr->private->header[name];
    }

  return s;
}

void
set_header (PR *pr, Header_Name name, const char *string)
{
  if (name >= NUM_HEADER_ITEMS || string == NULL)
    return;

  if (pr->private->header[name] != NULL)
    {
      free (pr->private->header[name]);
    }
  pr->private->header[name] = (char *) xstrdup (string);
}

void
free_pr_header (PR *pr)
{
  int i;

  for (i = 0; i < NUM_HEADER_ITEMS; i++)
    {
      if (pr->private->header[i] != NULL)
	{
	  free (pr->private->header[i]);
	  pr->private->header[i] = NULL;
	}
    }
}

char *
gen_pr_path (PR *pr)
{
  char *buf;
  asprintf (&buf, "%s/%s/%s", databaseDir (pr->database),
	    field_value (pr, CATEGORY (pr->database)),
	    field_value (pr, NUMBER (pr->database)));
  return buf;
}

void
free_pr_contents (PR *pr)
{
  if (pr->private != NULL && pr->private->value != NULL)
    {
      int i;
      int pr_fields = get_num_fields (pr->database);

      if (pr->private->non_zero)
	{
	  for (i = 0; i < pr_fields; i++)
	    {
	      if (pr->private->value[i].data.string != NULL)
		{
		  freePRDataString (&pr->private->value[i].data);
		}
	      if (pr->private->value[i].reason.string != NULL)
		{
		  freePRDataString (&pr->private->value[i].reason);
		}
	    }
	  pr->private->non_zero = 0;
	}
      if (pr->private->buffer != NULL)
	{
	  free (pr->private->buffer);
	  pr->private->buffer = NULL;
	}
    }
  pr->read_in = 0;
}

/* Write a file out to FILENAME with PR in it.  If FORCE is false,
   then make sure the file doesn't already exist.  */

int
createPrFile (PR *pr, const char *filename, int force, ErrorDesc *err)
{
  FILE *output;

  /* Quick check - if file already exists, this is not a good sign.  */
  if (force == 0 && fileExists (filename))
    {
      setError (err, CODE_FILE_ERROR,
		"File for PR %s already exists, filename is %s", 
		field_value (pr, NUMBER (pr->database)), filename);
      return -1;
    }

  block_signals ();

  /* Now build the file.  */
  output = fopen (filename, "w+");
  if (output == NULL)
    {
      setError (err, CODE_FILE_ERROR,
		"Error in createPrFile () creating file %s", 
		filename);
      unblock_signals ();
      return -1;
    }

  write_entire_header (output, pr, "\n");
  fputc ('\n', output);
  write_entire_pr (output, pr, "\n");

  fclose (output);
  unblock_signals ();
  return 0;
}

void
free_pr (PR *pr)
{
  free_pr_header (pr);
  free_pr_contents (pr);
  pr_destroy (pr); /* free up the private ds info */
  free (pr->private->header);
  free (pr->private->value);
  free (pr->private);
  free (pr);
}

void
freeInputTemplate (InputTemplate *template)
{
  while (template != NULL)
    {
      InputTemplate *n = template->next;
      free (template);
      template = n;
    }
}

void
printValidValues (FILE *outfile, FieldIndex i, const char *eol)
{
  switch (fieldDefForIndex (i)->datatype)
    {
    case Enum:
    case MultiEnum:
      {
	StringList *s = fieldDefForIndex (i)->enumValues;
	while (s != NULL)
	  {
	    write_multitext (outfile, s->name, eol);
	    fputs (eol, outfile);
	    s = s->next;
	  }
	break;
      }
    case TextWithRegex:
      {
	StringList *s = fieldDefForIndex (i)->regex;
	while (s != NULL)
	  {
	    write_multitext (outfile, s->name, eol);
	    fputs (eol, outfile);
	    s = s->next;
	  }
	break;
      }
    default:
      {
	write_multitext (outfile, ".*\n", eol);
	break;
      }
    }
}
