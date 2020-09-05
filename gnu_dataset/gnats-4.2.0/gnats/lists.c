/* Routines to provide lists of the GNATS low-level files.
   Copyright (C) 1995, 1999, 2000, 2007  Free Software Foundation, Inc.
   Contributed by Brendan Kehoe (brendan@cygnus.com).
   Majorly reworked by Bob Manson (manson@juniper.net).

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

/* The maximum length of a file suffix listed below.  */
#define MAXSUFFIX 4
struct list_type_list {
  const char *name;
  ListTypes which;
  const char *fileSuffix;
  const char *builtinFieldName;
} listTypeList[] = {
  { "Categories",         ListCategories,         ".cat", "category" },
  { "Submitters",         ListSubmitters,         ".sub", "submitter-id" },
  { "Responsible",        ListResponsible,        ".res", "responsible" },
  { "States",             ListStates,             ".sta", "state" },
  { "FieldNames",         ListFieldNames,         ".fns", NULL },
  { "InitialInputFields", ListInitialInputFields, ".lfn", NULL },
  { "InitialRequiredFields", ListInitialRequiredFields, ".lrn", NULL },
  { "Databases",          ListDatabases,          ".dbl", NULL },
  { NULL,		  InvalidListType,	  NULL,   NULL }
};

ListTypes
stringToListType (const char *string)
{
  int x;

  for (x = 0; listTypeList[x].name != NULL; x++)
    {
      if (strcasecmp (string, listTypeList[x].name) == 0)
	{
	  return listTypeList[x].which;
	}
    }
  return -1;
}

const char *
listTypeToString (ListTypes which)
{
  int x;

  for (x = 0;
       listTypeList[x].which != which && listTypeList[x].name != NULL;
       x++)
    {
      /* Empty */
    }
  return listTypeList[x].name;
}

int
getGnatsFile (const DatabaseInfo database, ListTypes whichList, 
	      const char *file, const char *eolTerminator)
{
  char *outf = NULL;
  FILE *fpout = NULL;
  struct list_type_list *list_desc;
  int x;
  AdmEntry *chain = NULL;
  int fieldCount = 0;
  ErrorDesc err;

  for (x = 0;
       listTypeList[x].which != whichList 
	 && listTypeList[x].name != NULL; x++)
    {
      /* Empty */
    }

  if (listTypeList[x].name == NULL)
    {
      return -1;
    }
  else
    {
      list_desc = &(listTypeList[x]);
    }

  if (file != NULL)
    {
      outf = (char *) xmalloc (strlen (file) + MAXSUFFIX + 1);
      strcpy (outf, file);
      eolTerminator = "\n";
    }
  else
    {
      fpout = stdout;
    }

  if (outf != NULL)
    {
      strcat (outf, list_desc->fileSuffix);
    }

  if (outf != NULL)
    {
      fpout = fopen (outf, "w");
      if (fpout == (FILE *)NULL)
        return -1; /* XXX */
    }

  switch (list_desc->which)
    {
    case ListFieldNames:
      {
	int x;
	FieldIndex f;

	for (x = 0; (f = getNthField (database, x)) != NULL; x++)
	  {
	    fprintf (fpout, "%s\n", fieldDefForIndex (f)->name);
	  }
	break;
      }
    case ListInitialInputFields:
      {
	InputTemplate *t;

	for (t = getInputTemplate (database); t != NULL; t = t->next)
	  {
	    FieldDef field = fieldDefForIndex (t->index);
	    fprintf (fpout, "%s\n", field->name);
	  }
	break;
      }
    case ListInitialRequiredFields:
      {
	FieldList fields = getRequiredInputFields(database);
	while (fields != NULL)
	  {
	    fprintf (fpout, "%s\n", complexFieldIndexToString (fields->ent));
	    fields = fields->next;
	  }
	break;
      }
    case ListDatabases:
      chain = getDatabaseList (&err);
      fieldCount = 3;
      break;
    case ListStates:
    case ListCategories:
    case ListSubmitters:
    case ListResponsible:
      {
	FieldIndex field = findBuiltinField (database, 
					    list_desc->builtinFieldName);
	if (field != NULL)
	  {
	    chain = fieldDefForIndex (field)->adm_contents;
	    fieldCount = fieldDefForIndex (field)->adm_db_fields;
	  }
	else
	  {
	    chain = NULL;
	    fieldCount = 0;
	  }
      }
      break;

    case InvalidListType:
      abort ();
      break;
    }

  while (chain != NULL)
    {
      int x;

      for (x = 0; x < fieldCount; x++)
	{
	  fprintf (fpout, (x > 0) ? ":%s" : "%s", chain->admFields[x]);
	}
      fprintf (fpout, "%s", eolTerminator);
      chain = chain->next;
    }

  if (outf)
    {
      fclose (fpout);
    }

  return 0;
}
