/* pr-edit.c - creates, edits, or deletes PR's
 
Copyright (C) 2007  Free Software Foundation, Inc.

This program is free software: you can redistribute it and/or modify
it under the terms of the GNU General Public License as published by
the Free Software Foundation, either version 3 of the License, or (at
your option) any later version.

This program is distributed in the hope that it will be useful, but
WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
General Public License for more details.

You should have received a copy of the GNU General Public License
along with GNU GNATS; see the file COPYING. If not, see
<http://www.gnu.org/licenses/>.
*/

#include "gnats.h"
#include "builtin-fields.h"
#include "pcodes.h"

/* The set of builtin fields.  

   If you change these, you must change builtin-fields.h to match.  */

struct pr_builtin_field 
{
  /* The header string of the field. */
  const char *builtin_name;
};

struct pr_builtin_field builtinFields[NUM_BUILTIN_FIELDS] = 
{
  { "Number" },
  { "Category" },
  { "Synopsis" },
  { "Confidential" },
  { "Severity" },
  { "Priority" },
  { "Responsible" },
  { "State" },
  { "Submitter-Id" },
  { "Arrival-Date" },
  { "Closed-Date" },
  { "Last-Modified" },
  { "Originator" },
  { "Description" },
  { "Audit-Trail" },
  { "Unformatted" }
};

/* Init all the fields in the PR.  */

static FILE *inpfile = NULL;
int (*yyinpfunc)(char *buf, int max) = NULL;

static int
getFconfigLine (char *buf, int max)
{
  if (fgets (buf, max, inpfile) == NULL)
    {
      return 0;
    }
  return (strlen (buf));
}

static int badConfig;
static ErrorDesc *errForDbParse;
static const char *fconfigFile;
DatabaseInfo databaseBeingDefined;

/* Read in the field configuration file, and configure DATABASE
   appropriately.

   If FILENAME is non-NULL, use it as the file to read from.  If FUNC
   is specified, FUNC will be used to read the actual data from the
   file; this is used by the network client to read the configuration
   from the server, rather than from a local file.

   FUNC is passed in a buffer in which to store the data read from the
   configuration file, and the maximum amount of data that can be
   written to BUF.

   FUNC should return the number of characters read, or 0 on EOF or
   error.  */

int
fconfigParse (DatabaseInfo database, const char *filename,
	      int (*func)(char *buf, int max), ErrorDesc *err)
{
  extern int fconfig_lineno;
  extern void fconfparse (void);
  extern void cleanLexer (void);

  databaseBeingDefined = database;

  badConfig = 0;

  if (filename != NULL)
    {
      inpfile = fopen (filename, "r");
      if (inpfile == NULL)
	{
	  setError (err, CODE_INVALID_DATABASE,
		    "Unable to read config file %s", filename);
	  return 1;
	}
      fconfigFile = filename;
    }
  else
    {
      fconfigFile = "server-supplied configuration";
    }
  if (func != NULL)
    {
      yyinpfunc = func;
    }
  else
    {
      yyinpfunc = getFconfigLine;
    }
  errForDbParse = err;
  fconfig_lineno = 1;
  fconfparse ();
  if (filename != NULL)
    {
      fclose (inpfile);
    }
  cleanLexer ();
  return badConfig;
}

/* Not terribly helpful.  XXX ??? !!! */
void
fconferror (const char *message)
{
  extern int fconfig_lineno;

  if (! badConfig)
    {
      setError (errForDbParse, CODE_INVALID_DATABASE,
		"Error at line %d of %s:%s",
		fconfig_lineno, fconfigFile, message);
    }
  badConfig = 1;
}

int
fconflex (void)
{
  extern int yylex (void);
  return yylex ();
}

int
setBuiltinField (FieldIndex field, const char *name)
{
  int x;

  for (x = 0; x < NUM_BUILTIN_FIELDS; x++)
    {
      if (strcasecmp (name, builtinFields[x].builtin_name) == 0)
	{
	  setBuiltinDBField (field->database, x, field);
	  break;
	}
    }

  if (x == NUM_BUILTIN_FIELDS)
    {
      return -1;
    }
  else
    {
      return 0;
    }
}

FieldIndex
findBuiltinField (const DatabaseInfo database, const char *name)
{
  int x;

  for (x = 0; x < NUM_BUILTIN_FIELDS; x++)
    {
      if (strcasecmp (name, builtinFields[x].builtin_name) == 0)
	{
	  return getBuiltinField (database, x);
	}
    }
  return find_field_index (database, name);
}
