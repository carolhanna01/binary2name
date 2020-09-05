/* Query handling code.
   Copyright (C) 1994, 95, 96, 97, 99, 2000, 07 Free Software Foundation, Inc.
   Contributed by Brendan Kehoe (brendan@cygnus.com).
   Massively revised by Bob Manson (manson@juniper.net).

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
#include "pcodes.h"
#include "ds.h"

/* One item to search for. */

static QueryItem freeQueryItemEnt = NULL;

static QueryItem
newQueryItem (ComplexFieldIndex index)
{
  QueryItem res;

  if (freeQueryItemEnt != NULL)
    {
      res = freeQueryItemEnt;
      freeQueryItemEnt = NULL;
    }
  else
    {
      res = (QueryItem) xmalloc (sizeof (struct query_item));
    }
  res->fieldIndex = index;
  if (isConstantFieldValue (res->fieldIndex))
    {
      res->compRegexp = 
	(struct re_pattern_buffer *) 
	xmalloc (sizeof (struct re_pattern_buffer));
      memset (res->compRegexp, 0, sizeof (struct re_pattern_buffer));
    }
  else
    {
      res->compRegexp = NULL;
    }
  return res;
}

static void
freeQueryItem (QueryItem i)
{
  if (i != NULL)
    {
      freeComplexFieldIndex (i->fieldIndex);
      if (i->compRegexp != NULL)
	{
	  /* XXX ??? !!! Hack 'cause the translate buffer is a static array */
	  i->compRegexp->translate = NULL;
	  regfree (i->compRegexp);
	  free (i->compRegexp);
	}
      if (freeQueryItemEnt == NULL)
	{
	  freeQueryItemEnt = i;
	}
      else
	{
	  free (i);
	}
    }
}

struct SearchTypes {
  const char *name;
  const char *operator;
  SearchType searchType;
} searchTypes[] = {
  /* Order here does matter--we need to have == before =. (Bleah, yeah). */
  { "LessThan",		 "<",  LessThan },
  { "GreaterThan",	 ">",  GreaterThan },
  { "Equals",		 "==", Equals },
  { "NotEquals",	 "!=", NotEquals },
  { "RegCmp",		 "=",  RegCmp },
  { "RegFind",		 "~",  RegFind },
  { "DefaultSearchType", "^",  DefaultSearchType },
  { NULL, 		 NULL, InvalidSearchType }
};

/* Adds query format Q to the list of query formats. */
void
addQueryFormat (DatabaseInfo database, QueryFormat *q)
{
  QueryFormat *list = getQueryFormatList (database);

  q->next = list;
  setQueryFormatList (database, q);
}

static QueryFormat *
parseQueryFormat (const DatabaseInfo database, const char *expr, 
		  ErrorDesc *err)
{
  const char *sstart;
  char *fstring = NULL;
  QueryFormat *res;
  FieldList currEnt = NULL;

  while (expr[0] != '\0' && isspace ((int)(unsigned char) expr[0]))
    {
      expr++;
    }
  if (expr[0] == '\0')
    {
      return NULL;
    }
  if (expr[0] == '"')
    {
      expr++;
      sstart = expr;

      while (expr[0] != '\0' && expr[0] != '\"')
	{
	  if (expr[0] == '\\' && expr[1] != '\0')
	    {
	      expr++;
	    }
	  expr++;
	}

      fstring = xmalloc (expr - sstart + 1);
      memcpy (fstring, sstart, expr - sstart);
      fstring[expr - sstart] = '\0';
      if (expr[0] == '"')
	{
	  expr++;
	}
    }
  res = (QueryFormat *) xmalloc (sizeof (QueryFormat));
  res->name = NULL;
  res->printf = fstring;
  res->separator = xstrdup ("\n");
  res->fields = NULL;
  while (expr[0] != '\0')
    {
      const char *nstart = expr;
      FieldList newEnt;

      if (isspace ((int)(unsigned char) expr[0]))
	{
	  expr++;
	  continue;
	}
      while ((! isspace ((int)(unsigned char) expr[0])) && expr[0] != '\0')
	{
	  expr++;
	}
      fstring = xmalloc (expr - nstart + 1);
      memcpy (fstring, nstart, expr - nstart);
      fstring[expr - nstart] = '\0';
      newEnt = newFieldListEnt (database, fstring, NULL);
      if (currEnt == NULL)
	{
	  res->fields = newEnt;
	}
      else
	{
	  currEnt->next = newEnt;
	}
      currEnt = newEnt;
      if (parseComplexFieldIndex (currEnt->ent) != 0)
	{
	  setError (err, CODE_INVALID_FIELD_NAME,
		    "Invalid field name or query format %s", fstring);
	  freeQueryFormat (res);
	  res = NULL;
	  break;
	}
    }
  return res;
}

/* Find the first query format with name NAME; returns the query format,
   or NULL if one was not found. */
QueryFormat *
findQueryFormat (const DatabaseInfo database, const char *name, ErrorDesc *err)
{
  QueryFormat *q = getQueryFormatList (database);

  while (q != NULL)
    {
      if (strcmp (q->name, name) == 0)
	{
	  return q;
	}
      q = q->next;
    }
  /* It might be a format expression.  */
  return parseQueryFormat (database, name, err);
}

/* Return a non-zero value if P is composed entirely of digits.  */
static int
numeric (const char *p)
{
  while (*p != '\0') 
    {
      if (!isdigit ((int) *p))
	return 0;
      p++;
    }
  return 1;
}


/* Convert STRING into a time_t.  It may either be a Unix time value
   (seconds since Jan 1, 1970) or one of the other standard date
   formats accepted by GNATS. */
time_t
get_any_date (const char *string)
{
  if (string == NULL)
    {
      return -1;
    }
  else if (numeric (string))
    {
      return atoi (string);
    }
  else
    {
      return get_date ((char *) string, NULL);
    }
}

/* Return the numeric equivalent of this state; 0 if not available. */
int
enum_numeric (const char *text, FieldIndex field)
{
  if ((text != NULL) && (text[0] != '\0'))
    {
      StringList *values;
      int count = 1;

      for (values = fieldDefForIndex (field)->enumValues;
	   values != NULL;
	   values = values->next)
	{
	  if (strcasecmp (values->name, text) == 0)
	    {
	      return count; 
	    }
	  count++;
	}
    }

  return 0;
}

static char *
sql_time (const char *s)
{
  time_t t;
  struct tm *ar_time;
  /* Note: we deliberately use 21 here, since that is what the width of
     this time string will end up looking like.  In the case where we
     use things relative to timezone and such, it'd grow---but not here.  */
  char *buf = (char *) xmalloc (21);

  t = get_any_date (s);
  ar_time = (struct tm *) localtime (&t);
  strftime (buf, 21, "%Y-%m-%d %H:%M:%S", ar_time);

  return buf;
}

static char case_fold[256];

/* Return 0 if VALUE matched the regexp in PAT, 1 otherwise.  */
int
gnats_regcmp (const char *pat, const char *value,
	      struct re_pattern_buffer *barg)
{
  struct re_pattern_buffer buf;
  struct re_pattern_buffer *bufPtr;
  union {
    const char *c;
    int i;
  } r;

  if (barg == NULL)
    {
      bufPtr = &buf;
      memset ((void *) &buf, 0, sizeof (buf));
    }
  else
    {
      bufPtr = barg;
    }

  if (case_fold[1] == 0)
    {
      int i;
      for (i = 0; i < 256; i++)
	{
	  case_fold[i] = tolower (i);
	}
    }
  if (bufPtr->translate == NULL)
    {
      bufPtr->translate = case_fold;
      bufPtr->fastmap = xmalloc (256);
#ifdef USE_RX
      bufPtr->syntax_parens = (char *)1;
#endif
  
      r.c = re_compile_pattern (pat, strlen (pat), bufPtr);
      if (r.c)
	{
	  fprintf (stderr, "%s: re_compile_pattern: %s\n", program_name, r.c);
	  /* XXX ??? !!! Wow.  This makes real sense.  :-( */
	  exit (2);
	}
    }
  r.i = strlen (value);
  r.i = re_match (bufPtr, value, strlen (value), 0, 0);
  if (barg == NULL)
    {
      buf.translate = NULL;
      regfree (&buf);
    }
  switch (r.i)
    {
    case -2:
      fprintf (stderr,
	       "%s: warning: re_match died with pattern %s and string %s\n",
	       program_name, pat, value);
      /*FALLTHRU*/
    case -1:
      return 1;
    default:
      return 0;
    }
}
      
/* Return 0 if any portion of VALUE matches the regexp in PAT, 1 otherwise. */
int
regfind (const char *pat, const char *value, struct re_pattern_buffer *barg)
{
  struct re_pattern_buffer buf;
  struct re_pattern_buffer *bufPtr;
  union {
    const char *c;
    int i;
  } r;

  if (barg == NULL)
    {
      bufPtr = &buf;
      memset ((void *) &buf, 0, sizeof (buf));
    }
  else
    {
      bufPtr = barg;
    }

  if (case_fold[1] == 0)
    {
      int i;
      for (i = 0; i < 256; i++)
	{
	  case_fold[i] = tolower (i);
	}
    }
  if (bufPtr->translate == NULL)
    {
      bufPtr->translate = case_fold;
      bufPtr->fastmap = xmalloc (256);
#ifdef USE_RX
      bufPtr->syntax_parens = (char *)1;
#endif
  
      r.c = re_compile_pattern (pat, strlen (pat), bufPtr);
      if (r.c)
	{
	  fprintf (stderr, "%s: re_compile_pattern: %s\n", program_name, r.c);
	  /* XXX ??? !!! Wow.  This makes real sense.  :-( */
	  exit (2);
	}
    }
  r.i = strlen (value);
  r.i = re_search (bufPtr, value, r.i, 0, r.i, 0);
  if (barg == NULL)
    {
      buf.translate = NULL;
      regfree (&buf);
    }
  switch (r.i)
    {
    case -2:
      fprintf (stderr,
	       "%s: warning: re_search died with pattern %s and string %s\n",
	       program_name, pat, value);
      /*FALLTHRU*/
    case -1:
      return 1;
    default:
      return 0;
    }
}

static int
intFieldCompare (QueryItem *fields, const char *lhs, const char *rhs)
{
  FieldType fieldType;
  FieldIndex fieldIndex = simpleFieldIndexValue (fields[0]->fieldIndex);

  if (fieldIndex != InvalidFieldIndex)
    {
      fieldType = fieldDefForIndex (fieldIndex)->datatype;
    }
  else
    {
      fieldType = Text;
    }

  switch (fieldType)
    {
    case Date:
      {
	time_t lv = (lhs[0] == '\0' ? 0 : get_any_date (lhs));
	time_t rv = (rhs[0] == '\0' ? 0 : get_any_date (rhs));

	if (lv == -1 || rv == -1)
	  {
	    return strcmp (lhs, rhs);
	  }
	else if (lv < rv)
	  {
	    return -1;
	  }
	else if (lv == rv)
	  {
	    return 0;
	  }
	else
	  {
	    return 1;
	  }
      }
      break;
    case Integer:
      {
	int lv = atoi (lhs);
	int rv = atoi (rhs);
	if (lv < rv)
	  {
	    return -1;
	  }
	else if (lv == rv)
	  {
	    return 0;
	  }
	else
	  {
	    return 1;
	  }
      }
      break;
    case Enum:
      {
	int lv = enum_numeric (lhs, fieldIndex);
	int rv = enum_numeric (rhs, fieldIndex);

	if (lv == 0 || rv == 0)
	  {
	    return strcmp (lhs, rhs);
	  }
	if (lv < rv)
	  {
	    return -1;
	  }
	else if (lv == rv)
	  {
	    return 0;
	  }
	else
	  {
	    return 1;
	  }
      }
      break;
    default:
      {
	return strcmp (lhs, rhs);
      }
      break;
    }
}

static int
fieldCompare (PR *pr, PR *oldPR, QueryItem *fields,
	      SearchType searchType, FormatNamedParameter *params)
{
  struct localFieldInfo
  {
    const char *value;
    FieldIndex index;
    int mustBeFreed;
  } vals[2];
  int i;
  int res = 0;
  int x;

  for (i = 0; i < 2; i++)
    {
      /* Not sure what to do with this mess.  XXX ??? !!! FIXME */
      if (parseComplexFieldIndex (fields[i]->fieldIndex) != 0)
	{
	  return 0;
	}
    }

  for (i = 0; i < 2; i++)
    {
      if (complexFieldType (fields[i]->fieldIndex) != InvalidFieldType)
	{
	  int num_fields = get_num_fields (pr->database);
	  FieldType fieldType = complexFieldType (fields[i]->fieldIndex);

	  for (x = 0; x < num_fields; x++)
	    {
	      FieldIndex field = getNthField (pr->database, x);
	      if ((fieldType == Text && fieldDefForIndex (field)->textsearch)
		  || (fieldType != Text 
		      && fieldDefForIndex (field)->datatype == fieldType))
		{
		  ComplexFieldIndex ent = simpleComplexFieldIndex (field);
		  QueryItem newFields[2];
		  int res;

		  newFields[i] = newQueryItem (ent);
		  newFields[1 - i] = fields [1 - i];

		  res = fieldCompare (pr, oldPR, newFields, searchType,
				      params);

		  freeQueryItem (newFields[i]);
		  if (res)
		    {
		      return 1;
		    }
		}
	    }
	  return 0;
	}
    }

  for (x = 0; x < 2; x++)
    {
      vals[x].index = simpleFieldIndexValue (fields[x]->fieldIndex);
    }

  if (searchType == DefaultSearchType)
    {
      if (vals[0].index != InvalidFieldIndex)
	{
	  searchType = fieldDefForIndex (vals[0].index)->defaultSearchType;
	}
      else
	{
	  searchType = RegCmp;
	}
    }

  for (x = 0; x < 2; x++)
    {
      vals[x].value = get_field_value (pr, oldPR, fields[x]->fieldIndex,
				       params, &(vals[x].mustBeFreed));
    }

  if (vals[0].value == NULL || vals[1].value == NULL)
    {
      for (x = 0; x < 2; x++)
	{
	  if (vals[x].mustBeFreed && vals[x].value != NULL)
	    {
	      free ((char *)vals[x].value);
	    }
	}
      return 0;
    }

  switch (searchType)
    {
    case DefaultSearchType:
      /* Shouldn't get here */
      abort ();
      break;

    case RegCmp:
      res = (gnats_regcmp (vals[1].value, vals[0].value, fields[1]->compRegexp)
	     == 0);
      break;

    case RegFind:
      res = (regfind (vals[1].value, vals[0].value, fields[1]->compRegexp) == 0);
      break;

    case LessThan:
      res = (intFieldCompare (fields, vals[0].value, vals[1].value) < 0);
      break;

    case GreaterThan:
      res = (intFieldCompare (fields, vals[0].value, vals[1].value) > 0);
      break;

    case Equals:
      res = (intFieldCompare (fields, vals[0].value, vals[1].value) == 0);
      break;

    case NotEquals:
      res = (intFieldCompare (fields, vals[0].value, vals[1].value) != 0);
      break;

    case StringMatch:
      res = (strcmp (vals[0].value, vals[1].value) == 0);
      break;

    case NilSearch:
      /* ??? XXX !!! Hmmmm.  Return 1?  */
      res = 0;
      break;

    case InvalidSearchType:
      abort ();
      break;
    }
  for (x = 0; x < 2; x++)
    {
      if (vals[x].mustBeFreed)
	{
	  free ((char *) vals[x].value);
	}
    }
  return res;
}

static int
pr_match_field (PR *pr, PR *oldPR, SearchItem *item,
		FormatNamedParameter *params)
{
  QueryItem fields[2];

  if (pr == NULL)
    {
      return 0;
    }

  fields[0] = item->lhs;
  fields[1] = item->rhs;

  return fieldCompare (pr, oldPR, fields, item->searchType, params);
}

/* Returns a non-zero value if the PR referenced by I matches the expression
   tree in QEXP.  */

int
pr_matches_tree (PR *pr, PR *oldPR, QueryTree qexp,
		 FormatNamedParameter *params)
{
  if (qexp == NULL)
    {
      return 1;
    }

  switch (qexp->op)
    {
    case QueryMatch:
      return pr_match_field (pr, oldPR, &qexp->ent, params);
      break;
    case QueryNot:
      return ! pr_matches_tree (pr, oldPR, qexp->left, params);
      break;
    case QueryAnd:
      return pr_matches_tree (pr, oldPR, qexp->left, params)
	&& pr_matches_tree (pr, oldPR, qexp->right, params);
      break;
    case QueryOr:
      return pr_matches_tree (pr, oldPR, qexp->left, params)
	|| pr_matches_tree (pr, oldPR, qexp->right, params);
      break;
    default:
      abort ();
    }
  return 0;
}

int
pr_matches_expr (PR *pr, PR *oldPR, QueryExpr qexp,
		 FormatNamedParameter *params)
{
  QueryTree tree = NULL;

  if (qexp != NULL)
    {
      tree = qexp->tree;
    }
  return pr_matches_tree (pr, oldPR, tree, params);
}

void
append_string (char **res, const char *string)
{
  if (*res != NULL)
    {
      size_t oldlen = strlen (*res);
      size_t newlen = strlen (string);
      *res = xrealloc (*res, oldlen + newlen + 1);
      memcpy (*res + oldlen, string, newlen + 1);
    }
  else
    {
      *res = xstrdup (string);
    }
}

/* mmmm, cheezy. */
static void
append_char (char **res, char chr)
{
  char buf[2];

  buf[0] = chr;
  buf[1] = '\0';
  append_string (res, buf);
}

/* Prints the string CONTENTS using the printf format FORMAT to either
   FP (if it is non-NULL), or appended to RES via append_string ().  */

static void
do_print (FILE *fp, char **res, const char *format, const char *contents)
{
  size_t flen = strlen (format);
  if (format[flen - 1] == 'd')
    {
      int val = atoi (contents);
      if (fp != NULL)
	{
	  fprintf (fp, format, val);
	}
      else
	{
	  char *new;

	  asprintf (&new, format, val);
	  append_string (res, new);
	  free (new);
	}
    }
  else
    {
      if (fp != NULL)
	{
	  fprintf (fp, format, contents);
	}
      else
	{
	  char *new;

	  asprintf (&new, format, contents);
	  append_string (res, new);
	  free (new);
	}
    }
}

static void
writeFullPR (FILE *fp, PR *pr, int rawQuery, const char *eolTerminator)
{
  int i;
  int num_fields = get_num_fields (pr->database);

  for (i = 0; i < num_fields; i++)
    {
      int mustBeFreed = 0;
      const char *contents;
      FieldIndex field = getNthField (pr->database, i);

      if (! rawQuery)
	{
	  ComplexFieldIndex fieldIndex = simpleComplexFieldIndex (field);
	  contents = get_field_value (pr, NULL, fieldIndex, NULL,
				      &mustBeFreed);
	  freeComplexFieldIndex (fieldIndex);
	}
      else
	{
	  contents = field_value (pr, field);
	}

      write_pr_field (fp, field, contents, eolTerminator);
      if (mustBeFreed)
	{
	  free ((char *) contents);
	}
    }
}

static void
format_pr_field (FILE *fp, char **res, PR *pr, PR *oldPR, FieldList *fieldPtr,
		 const char *format, const char *eolTerminator,
		 FormatNamedParameter *parameters, int rawQuery)
{
  int flen = strlen (format);
  char *fdup = xstrdup (format);
  const char *p;

  ComplexFieldIndex field = (*fieldPtr)->ent;

  switch (format[flen - 1])
    {
    case 's':
      {
	int mustBeFreed = 0;
	const char *contents =
	  get_field_value (pr, oldPR, field, parameters, &mustBeFreed);

	if (contents == NULL)
	  {
	    contents = "";
	  }

	do_print (fp, res, fdup, contents);
	if (mustBeFreed)
	  {
	    free ((char *) contents);
	  }
	*fieldPtr = (*fieldPtr)->next;
      }
      break;

    case 'S':
      {
	int mustBeFreed = 0;
	const char *contents = 
	  get_field_value (pr, oldPR, field, parameters, &mustBeFreed);
	char *temp;

	if (contents == NULL)
	  {
	    contents = "";
	  }

	for (p = contents; *p && *p != ' '; p++)
	  {
	    /* Empty */
	  }
	fdup[flen - 1] = 's';
	if (*p == ' ')
	  {
	    temp = xmalloc (p - contents + 1);
	    memcpy (temp, contents, p - contents);
	    temp[p - contents] = '\0';
	    fprintf (fp, fdup, temp);
	    free (temp);
	  }
	else
	  {
	    fprintf (fp, fdup, contents);
	  }
	if (mustBeFreed)
	  {
	    free ((char *) contents);
	  }
	*fieldPtr = (*fieldPtr)->next;
      }
      break;

    case 'F':
      {
	int mustBeFreed = 0;
	const char *contents
	  = get_field_value (pr, oldPR, field, parameters, &mustBeFreed);

	write_pr_field (fp, simpleFieldIndexValue (field), contents,
			eolTerminator);
	if (mustBeFreed)
	  {
	    free ((char *) contents);
	  }
	*fieldPtr = (*fieldPtr)->next;
	break;
      }

    case 'd':
      {
	char buffer[21];
	int mustBeFreed = 0;
	const char *contents
	  = get_field_value (pr, oldPR, field, parameters, &mustBeFreed);
	FieldIndex fieldIndex = simpleFieldIndexValue (field);

	if (contents == NULL)
	  {
	    contents = "";
	  }
	switch (fieldDefForIndex (fieldIndex)->datatype)
	  {
	  case Enum:
	    sprintf (buffer, "%d", enum_numeric (contents, fieldIndex));
	    do_print (fp, res, format, buffer);
	    break;
	  case Date:
	    sprintf (buffer, "%d", (int) get_any_date (contents));
	    do_print (fp, res, format, buffer);
	    break;
	  case Integer:
	    sprintf (buffer, "%d", atoi (contents));
	    do_print (fp, res, format, buffer);
	    break;
	  default:
	    do_print (fp, res, format, "0");
	    break;
	  }
	if (mustBeFreed)
	  {
	    free ((char *) contents);
	  }
	*fieldPtr = (*fieldPtr)->next;
      }
      break;

    case 'D':
      {
	FieldIndex fieldIndex = simpleFieldIndexValue (field);

	if (fieldIndex != InvalidFieldIndex
	    && fieldDefForIndex (fieldIndex)->datatype == Date)
	  {
	    int mustBeFreed = 0;
	    const char *strftimeFormat = "%a %b %d %H:%M:%S %z %Y";
	    char *fend = NULL;
	    const char *contents 
	      = get_field_value (pr, oldPR, field, parameters, &mustBeFreed);
	    char *t = NULL;
	    time_t time;

	    if (contents == NULL)
	      {
		contents = "";
	      }
	    time = get_any_date (contents);

	    if (fdup[1] == '{')
	      {
		fend = strchr (fdup + 2, '}');
		if (fend != NULL)
		  {
		    strftimeFormat = fdup + 2;
		    *fend = '\0';
		  }
	      }
 
	    if (time == 0 || time == -1)
	      {
		/* Silly, but I'm lazy.  */
		t = xstrdup ("");
	      }
	    else
	      {
		size_t resLen;
		size_t tlen = 0;

		/* It isn't clear what strftime returns if there is a format
		   error.  So we're paranoid and limit the amount of data
		   it can allocate.  */
		do 
		  {
		    tlen += 512;
		    t = xrealloc (t, tlen);
		    t[0] = '\0';
		    resLen = gnats_strftime (t, tlen, strftimeFormat,
					     localtime (&time));
		  } while (resLen == 0 && tlen < 4096);
	      }
	    fdup[flen - 1] = 's';
	    if (fend != NULL)
	      {
		*fend = '%';
	      }
	    do_print (fp, res, fend != NULL ? fend : fdup, t);
	    free (t);
	    if (mustBeFreed)
	      {
		free ((char *) contents);
	      }
	  }
	*fieldPtr = (*fieldPtr)->next;
      }
      break;

    case 'Q':
      {
	FieldIndex fieldIndex = simpleFieldIndexValue (field);

	if (fieldIndex != InvalidFieldIndex 
	    && fieldDefForIndex (fieldIndex)->datatype == Date)
	  {
	    int mustBeFreed = 0;
	    const char *contents 
	      = get_field_value (pr, oldPR, field, parameters, &mustBeFreed);
	    long i;

	    if (contents == NULL)
	      {
		contents = "";
	      }

	    i = get_any_date (contents);

	    fdup[flen - 1] = 's';
	    if (i != 0 && i != -1)
	      {
		char *qd = sql_time (contents);
		do_print (fp, res, fdup, qd);
		free (qd);
	      }
	    else
	      {
		do_print (fp, res, fdup, "");
	      }
	    if (mustBeFreed)
	      {
		free ((char *) contents);
	      }
	  }
	*fieldPtr = (*fieldPtr)->next;
      }
      break;

    case 'P':
      {
	writeFullPR (fp, pr, rawQuery, eolTerminator);
	break;
      }
    }
  free (fdup);
}

static int
process_printf_format (FILE *fp, char **res, PR *pr, PR *oldPR, 
		       const char *format, FieldList fields,
		       const char *eolTerminator, 
		       FormatNamedParameter *parameters,
		       int rawQuery)
{
  static char fcopy[1024];

  while (*format != '\0')
    {
      if (format[0] == '\\' && format[1] == 't')
	{
	  if (fp != NULL)
	    {
	      fputc ('\t', fp);
	    }
	  else
	    {
	      append_char (res, '\t');
	    }
	  format++;
	}
      else if (format[0] == '\\' && format[1] == 'n')
	{
	  if (fp != NULL)
	    {
	      fputs (eolTerminator, fp);
	    }
	  else
	    {
	      append_string (res, eolTerminator);
	    }
	  format++;
	}
      else if (format[0] == '%')
	{
	  char *fptr = fcopy + 1;

	  fcopy[0] = *(format++);
	  if (*format == '{')
	    {
	      while (*format != '}' && (fptr - fcopy) < 1022)
		{
		  *(fptr++) = *format;
		  format++;
		}
	      if (*format == '}')
		{
		  *(fptr++) = *format;
		  format++;
		}
	    }
	  while ((isdigit ((int) *format) || *format == '-' || *format == '+'
		  || *format == '.')
		 && ((fptr - fcopy) < 1022))
	    {
	      *(fptr++) = *format;
	      format++;
	    }
	  *(fptr++) = *format;
	  *fptr = '\0';
	  if (*format == '%')
	    {
	      fputs (format, fp);
	    }
	  else
	    {
	      if (fields == NULL)
		{
		  return 0;
		}

	      format_pr_field (fp, res, pr, oldPR, &fields, fcopy,
			       eolTerminator, parameters, rawQuery);
	    }
	}
      else
	{
	  if (fp != NULL)
	    {
	      fputc (*format, fp);
	    }
	  else
	    {
	      append_char (res, *format);
	    }
	}
      format++;
    }
  return 1;
}

int
process_format (FILE *fp, char **res, PR *pr,  PR *oldPR, QueryFormat *fmt, 
		const char *eolTerminator, FormatNamedParameter *parameters)
{
  if (pr != NULL)
    {
      if (pr_load_fields (pr, fmt->fields) == -1)
        {
	  return 0;
        }
    }

  if (fmt->printf != NULL)
    {
      return process_printf_format (fp, res, pr, oldPR, fmt->printf,
				    fmt->fields, eolTerminator, parameters,
				    fmt->rawQuery);
    }
  else if (pr != NULL)
    {
      if (fmt->fields == NULL)
	{
	  write_entire_header (fp, pr, eolTerminator);
	  fprintf (fp, "%s", eolTerminator);
	  writeFullPR (fp, pr, fmt->rawQuery, eolTerminator);
	}
      else
	{
	  FieldList flist = fmt->fields;

	  while (flist != NULL)
	    {
	      if (fmt->separator == NULL)
		{
		  const char *contents;
		  int mustBeFreed = 0;
		  FieldIndex fieldIndex = simpleFieldIndexValue (flist->ent);

		  if (fmt->rawQuery)
		    {
		      contents = field_value (pr, fieldIndex);
		    }
		  else
		    {
		      contents = get_field_value (pr, oldPR, flist->ent,
						  parameters, &mustBeFreed);
		    }
		  write_pr_field (fp, fieldIndex, contents, eolTerminator);
		  if (mustBeFreed)
		    {
		      free ((char *) contents);
		    }
		  flist = flist->next;
		}
	      else
		{
		  format_pr_field (fp, res, pr, oldPR, &flist, "%s", 
				   eolTerminator, parameters, fmt->rawQuery);
		}
	    }
	}
      return 1;
    }
  else
    {
      return 0;
    }
}

int
print_named_format_pr (FILE *fp, PR *pr, const char *fmtName,
		       const char *eolTerminator,
		       ErrorDesc *err)
{
  QueryFormat *fmt = findQueryFormat (pr->database, fmtName, err);

  if (fmt == NULL)
    {
      return 0;
    }
  else
    {
      return process_format (fp, NULL, pr, NULL, fmt, eolTerminator, NULL);
    }
}

int
print_pr (FILE *dest, PR *pr, QueryFormat *query_format,
	  const char *eolTerminator)
{
  return process_format (dest, NULL, pr, NULL, query_format, eolTerminator,
			 NULL);
}


static QueryTree
newQueryTree (void)
{
  QueryTree ent = (QueryTree) xmalloc (sizeof (struct queryTree));
  ent->left = NULL;
  ent->right = NULL;
  ent->ent.searchType = InvalidSearchType;
  ent->ent.lhs = NULL;
  ent->ent.rhs = NULL;
  ent->op = InvalidQueryOp;
  return ent;
}

static QueryExpr
newQueryExpr (const DatabaseInfo database)
{
  QueryExpr ent = (QueryExpr) xmalloc (sizeof (struct queryExpr));
  ent->database = database;
  ent->tree = newQueryTree ();
  return ent;
}

static QueryExpr
queryField (const DatabaseInfo database,
	    ComplexFieldIndex lhs, SearchType stype, ComplexFieldIndex rhs)
{
  QueryExpr ent = newQueryExpr (database);
  ent->tree->op = QueryMatch;
  ent->tree->ent.searchType = stype;
  ent->tree->ent.lhs = newQueryItem (lhs);
  ent->tree->ent.rhs = newQueryItem (rhs);
  return ent;
}

static void
freeQueryTree (QueryTree tree)
{
  if (tree != NULL)
    {
      freeQueryTree (tree->left);
      freeQueryTree (tree->right);
      freeQueryItem (tree->ent.lhs);
      freeQueryItem (tree->ent.rhs);
      free (tree);
    }
}

void
freeQueryExpr (QueryExpr query)
{
  if (query != NULL)
    {
      freeQueryTree (query->tree);
      free (query);
    }
}

/* Return the SearchType value for OPERATOR, which is a query
   expression operator. */
static SearchType
getSearchTypeForOperator (const char *operator, size_t *len)
{
  int x;

  for (x = 0; searchTypes[x].name != NULL; x++)
    {
      if (searchTypes[x].operator[0] == operator[0])
	{
	  *len = strlen (searchTypes[x].operator);
	  if (*len == 1
	      || strncmp (searchTypes[x].operator, operator, *len) == 0)
	    {
	      break;
	    }
	}
    }
  return searchTypes[x].searchType;
}

/* Return the search operator for SearchType TYPE. */
const char *
getSearchOperatorForType (SearchType type)
{
  int x;
  for (x = 0; searchTypes[x].name != NULL; x++)
    {
      if (searchTypes[x].searchType == type)
	{
	  return searchTypes[x].operator;
	}
    }
  return NULL;
}



/* Find the matching parenthesis for the parenthesized expression between
   EXPR and EXPEND.  Returns either a pointer to the matching parenthesis,
   or NULL on failure. */

static const char *
findMatchingParen (const char *expr, const char *expend)
{
  int pcnt = 0;

  if (*expr != '(')
    {
      abort ();
    }

  while (expr <= expend)
    {
      if (*expr == '(')
	{
	  pcnt++;
	}
      else if (*expr == ')')
	{
	  pcnt--;
	}
      if (pcnt == 0)
	{
	  break;
	}
      expr++;
    }

  if (pcnt > 0)
    {
      /* Invalid expression. */
      return NULL;
    }
  else
    {
      return expr;
    }
}

/* Find the ending quote for the quoted string between EXPR and
   EXPREND inclusive; EXPR must point to the leading quote.  Returns a
   pointer to the ending quote on success, or NULL on failure.  */

static const char *
findMatchingQuote (const char *expr, const char *exprend)
{
  if (*expr != '"')
    {
      abort ();
    }

  expr++;

  while (expr <= exprend)
    {
      if (*expr == '\\')
	{
	  expr++;
	}
      else if (*expr == '"')
	{
	  return expr;
	}
      expr++;
    }

    if (expr <= exprend)
      {
	return expr;
      }
    else
      {
	return NULL;
      }
}

/* Remove any leading and trailing white space in the string between
   *EXPR and *EXPEND inclusive. */
static void
stripWhiteSpace (const char **expr, const char **expend)
{
  while (*expr <= *expend && isspace ((int) **expr))
    {
      (*expr)++;
    }
  while (*expr <= *expend && isspace ((int) **expend))
    {
      (*expend)--;
    }
}

/* Parse the possibly-quoted string argument between EXPR and EXPEND
   inclusive.  Returns a malloc()ed copy of the string, with all
   leading and trailing whitespace removed if the string was unquoted.  */

static char *
parseStringArgument (const char *expr, const char *expend)
{
  char *res;

  stripWhiteSpace (&expr, &expend);
  if (*expr == '"' && *expend == '"')
    {
      expr++;
      expend--;
    }
  res = xmalloc (expend - expr + 2);
  memcpy (res, expr, expend - expr + 1);
  res[expend - expr + 1] = '\0';
  return res;
}

static ComplexFieldIndex
parseQueryArgument (const DatabaseInfo database,
		    const char *start, const char *end)
{
  stripWhiteSpace (&start, &end);

  if (start[0] == '"')
    {
      char *string = parseStringArgument (start, end);
      ComplexFieldIndex field = newComplexFieldLiteralString (string);
      free (string);
      return field;
    }
  else
    {
      char *name = parseStringArgument (start, end);
      ComplexFieldIndex field = newComplexFieldIndex (database, name);
      free (name);
      return field;
    }
}

/* Parse a "simple" query expression between EXPR and EXPEND inclusive. */
static QueryExpr
parseSimpleQueryExpression (const DatabaseInfo database, const char *expr,
			    const char *expend)
{
  const char *exprstart;

  stripWhiteSpace (&expr, &expend);
  exprstart = expr;

  while (expr <= expend)
    {
      if (*expr == '\\')
	{
	  expr++;
	}
      else if (*expr == '"')
	{
	  expr = findMatchingQuote (expr, expend);
	  if (expr == NULL)
	    {
	      return NULL;
	    }
	}
      else if (*expr == '(')
	{
	  const char *pend = findMatchingParen (expr, expend);
	  if (pend == NULL || pend != expend)
	    {
	      return NULL;
	    }
	  else
	    {
	      return parseQueryExpression (database, expr + 1, expend - 1);
	    }
	}
      else 
	{
	  size_t len = 0;
	  SearchType searchType = getSearchTypeForOperator (expr, &len);

	  if (searchType != InvalidSearchType)
	    {
	      ComplexFieldIndex lhs, rhs;

	      lhs = parseQueryArgument (database, exprstart, expr - 1);
	      rhs = parseQueryArgument (database, expr + len, expend);

	      return queryField (database, lhs, searchType, rhs);
	    }
	}
      expr++;
    }
  return NULL;
}

/* Try to parse the string between EXPR and EXPREND inclusive as a 
   query expression.  The equivalent QueryExpr tree is returned on
   success, or a NULL value if the expression could not be parsed.

   We may not be handling
		a & b | c
   quite right--this gets parsed as
   		a & (b | c)
   instead of
		(a & b) | c
   Also,
   		! a & b & c
   is parsed as
		! (a & b & c)
   which may or may not be what is desired.

   Depending on order without using parenthesis to make things clear
   is just asking for trouble. */

QueryExpr
parseQueryExpression (const DatabaseInfo database, 
		      const char *expr, const char *exprend)
{
  const char *exprstart = expr;

  if (exprend == NULL)
    {
      exprend = expr + strlen (expr) - 1;
    }

  while (expr <= exprend)
    {
      if (*expr == '\\')
	{
	  expr++;
	}
      else if (*expr == '"')
	{
	  expr = findMatchingQuote (expr, exprend);
	  if (expr == NULL)
	    {
	      return NULL;
	    }
	}
      else if (*expr == '(')
	{
	  const char *pend = findMatchingParen (expr, exprend);
	  if (pend == NULL)
	    {
	      return NULL;
	    }
	  if (pend == exprend)
	    {
	      return parseQueryExpression (database, expr + 1, exprend - 1);
	    }
	  expr = pend;
	}
      else if (*expr == '!' && *(expr + 1) != '=')
	{
	  QueryExpr exp;
	  exp = parseQueryExpression (database, expr + 1, exprend);
	  return booleanQuery (QueryNot, exp, NULL);
	}
      else if (*expr == '&' || *expr == '|')
	{
	  QueryExpr left, right;
	  QueryOp op = (*expr == '&' ? QueryAnd : QueryOr);

	  left = parseQueryExpression (database, exprstart, expr - 1);
	  if (left == NULL)
	    {
	      return NULL;
	    }
	  right = parseQueryExpression (database, expr + 1, exprend);
	  if (right == NULL)
	    {
	      return NULL;
	    }
	  return booleanQuery (op, left, right);
	}
      expr++;
    }
  /* Didn't find any ANDs or ORs, so maybe it's a simple expression. */
  return parseSimpleQueryExpression (database, exprstart, exprend);
}

/* Appends an expression searching for SearchItem ITEM to the string
   in STRING, and returns either a pointer to the new string, or NULL
   on failure. */

static char *
appendSearchItem (char *string, SearchItem *item)
{
  int oldlen = (string == NULL) ? 0 : strlen (string);
  SearchType searchType = item->searchType;
  size_t addlen;
  char *lhs = complexFieldIndexToString (item->lhs->fieldIndex);
  char *rhs = complexFieldIndexToString (item->rhs->fieldIndex);
  const char *oper;

  oper = getSearchOperatorForType (searchType);
  if (oper == NULL)
    {
      return NULL;
    }
  addlen = strlen (lhs) + strlen (oper) + strlen (rhs);
  string = xrealloc (string, oldlen + addlen + 1);
  strcpy (string + oldlen, lhs);
  strcat (string + oldlen, oper);
  strcat (string + oldlen, rhs);
  free (lhs);
  free (rhs);
  return string;
}

/* Converts the QueryExpr EXPR to a string, and appends it to STRING;
   STRING is reallocated dynamically with xrealloc () as needed.

   Returns either the newly-appended string, or NULL on failure. */

static char *
queryTreeToString (char *string, QueryTree expr)
{
  /* A little macro to append CH to the string STRING.  Yes, this is
     sorta slow.  No, I don't care.  */
#define addCh(CH) {\
     size_t len = (string == NULL) ? 0 : strlen (string);\
     string = xrealloc (string, len + 2);\
     string[len] = (CH);\
     string[len + 1] = '\0';\
  }

  addCh ('(');

  switch (expr->op)
    {
    case QueryAnd:
    case QueryOr:
      {
	string = queryTreeToString (string, expr->left);
	if (string != NULL)
	  {
	    addCh (expr->op == QueryAnd ? '&' : '|');
	    string = queryTreeToString (string, expr->right);
	  }
	break;
      }
    case QueryNot:
      {
	/* A little tricky.  We replace the '(' we added above with a
	   '!', then add a '('.  */
	size_t len = strlen (string);
	string[len - 1] = '!';
	addCh ('(');
	string = queryTreeToString (string, expr->left);
	break;
      }
    case QueryMatch:
      {
	string = appendSearchItem (string, &(expr->ent));
	break;
      }
    default:
      {
	abort ();
	break;
      }
    }
  if (string != NULL)
    {
      addCh (')');
    }
  return string;
#undef addChar
}

char *
queryExprToString (char *string, QueryExpr expr)
{
  QueryTree tree = NULL;

  if (expr != NULL)
    {
      tree = expr->tree;
    }
  return queryTreeToString (string, tree);
}

QueryExpr
booleanQuery (QueryOp op, QueryExpr left, QueryExpr right)
{
  QueryExpr res;

  if (left == NULL && right == NULL)
    {
      return NULL;
    }
  if (op != QueryNot)
    {
      if (left == NULL)
	{
	  return right;
	}
      else if (right == NULL)
	{
	  return left;
	}
    }

  res = newQueryExpr (left->database);
  res->tree->op = op;
  switch (op)
    {
    case QueryOr:
    case QueryAnd:
      res->tree->left = left->tree;
      res->tree->right = right->tree;
      left->tree = NULL;
      right->tree = NULL;
      freeQueryExpr (left);
      freeQueryExpr (right);
      break;
    case QueryNot:
      res->tree->left = left->tree;
      res->tree->right = NULL;
      left->tree = NULL;
      freeQueryExpr (left);
      break;
    default:
      abort ();
      break;
    }
  return res;
}

void
freeQueryFormat (QueryFormat *q)
{
  if (q != NULL)
    {
      if (q->name != NULL)
	{
	  free (q->name);
	}
      if (q->printf != NULL)
	{
	  free (q->printf);
	}
      if (q->separator != NULL)
	{
	  free (q->separator);
	}
      if (q->fields != NULL)
	{
	  freeFieldList (q->fields);
	}
      free (q);
    }
}

void
freeQueryFormatList (QueryFormat *q)
{
  while (q != NULL)
    {
      QueryFormat *next = q->next;
      freeQueryFormat (q);
      q = next;
    }
}
