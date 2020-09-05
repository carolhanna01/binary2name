/* Interface to generic administrative file handling.
   Copyright (C) 1999, 2000, 2007 Free Software Foundation, Inc.
   Written by Bob Manson (manson@juniper.net).

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
along with this program.  If not, see <http://www.gnu.org/licenses/>.
*/

#include "gnats.h"

/* Allocate a new AdmEntry containing FIELDS number of fields. */

AdmEntry *
alloc_adm_entry (int fields)
{
  AdmEntry *res = (AdmEntry *) xmalloc (sizeof (AdmEntry));
  int x;

  res->fieldcount = fields;
  res->admFields = (char **) xmalloc (sizeof (char *) * fields);
  res->next = NULL;
  for (x = 0; x < fields; x++)
    {
      res->admFields[x] = NULL;
    }
  return res;
}

/* Returns a copy of ENT. */
static AdmEntry *
copy_adm_entry (AdmEntry *ent)
{
  int x;
  AdmEntry *res = alloc_adm_entry (ent->fieldcount);
  res->field = ent->field;
  for (x = 0; x < ent->fieldcount; x++)
    {
      res->admFields[x] = (ent->admFields[x] == NULL)
			    ? NULL
			    : xstrdup (ent->admFields[x]);
    }
  return res;
}

/* Frees all of the data in ENT, and then the entry itself.  The entry may not
   be part of a chain.  */
void
free_adm_entry (AdmEntry *ent)
{
  int x;

  if (ent != NULL)
    {
      if (ent->next != NULL)
	{
	  abort ();
	}
      for (x = 0; x < ent->fieldcount; x++)
	{
	  free (ent->admFields[x]);
	}
      free (ent->admFields);
      free (ent);
    }
}

/* Parse LINE into a new AdmEntry node; FIELD is the PR field that the entry
   belongs to. */

AdmEntry *
build_adm_entry (const char *line, FieldIndex field)
{
  AdmEntry *res;
  /* First figure out how many fields are in it. */
  int fieldcnt = 1;
  const char *currlineptr = line;
  char **ptr;

  while ((currlineptr = strchr (currlineptr, ':')) != NULL)
    {
      fieldcnt++;
      currlineptr++;
    }

  res = alloc_adm_entry (fieldcnt);
  res->field = field;
  ptr = res->admFields;
  currlineptr = line;
  while (currlineptr != NULL)
    {
      *ptr = get_next_field (&currlineptr, ':');
      ptr++;
    }
  return res;
}

/* Read through the adm files, looking for the first line that matches
    KEY.  Return the matching record, or NULL if one was not found.  */

AdmEntry *
get_adm_record (FieldIndex field, const char *key)
{
  FILE *fp;
  char *filename;
  char *path;
  char *line;
  size_t keylen;
  AdmEntry *res = NULL;
  int notdone = 1;

  filename = fieldDefForIndex (field)->adm_db_name;
  if (filename == NULL)
    {
      return NULL;
    }

  path = gnats_adm_dir (fieldDefForIndex (field)->database,
			filename);

  /* append a delimiting ':' to the end of string to make sure the match
     is fully correct.  */

  keylen = strlen (key);

  fp = fopen (path, "r");
  free (path);

  if (fp == NULL)
    {
      return NULL;
    }

  while (notdone && (line = read_line (fp, NULL)) != NULL)
    {
      /* Not sure we need to explicitly ignore comments here, but it doesn't
	 hurt either.  */
      if (line[0] != '#' && line[keylen] == ':'
	  && strncasecmp(line, key, keylen) == 0)
	{
	  res = build_adm_entry (line, field);
	  if (res->fieldcount != fieldDefForIndex (field)->adm_db_fields)
	    {
	      /* ??? XXX What else to do here?  Wrong number of fields... */
	      free_adm_entry (res);
	      res = NULL;
	    }
	  notdone = 0;
	}
      free (line);
    }
  fclose (fp);

  return res;
}

/* Construct a chain of AdmEntry entries for the administrative
   database associated with field FIELD.  Returns NULL if there was an
   error, the chain of entries otherwise. */

static AdmEntry *
build_chain_for_field (FieldDef field, FieldIndex fieldIndex)
{
  FILE *fp;
  char *path;
  char *line;
  AdmEntry *res = NULL, **cur = &res;

  path = gnats_adm_dir (field->database, field->adm_db_name);
  fp = fopen (path, "r");
  free (path);

  if (fp != NULL)
    {
      while ((line = read_line (fp, NULL)) != NULL)
	{
	  if (line[0] != '#' && line[0] != ' ' && line[0] != '\n')
	    {
	      AdmEntry *newEnt = build_adm_entry (line, fieldIndex);
	      /* Ignore records with too few fields */
	      if (newEnt != NULL && newEnt->fieldcount == field->adm_db_fields)
		{
		  *cur = newEnt;
		  cur = &((*cur)->next);
		}
	      else
		{
		  free_adm_entry (newEnt);
		}
	    }
	  free (line);
	}
      fclose (fp);
    }

  return res;
}

/* Constructs a chain of AdmEntries from the administrative database
   associated with field FIELD.  Returns either the chain of entries,
   or a NULL pointer on error.  */
  
AdmEntry *
build_chain (FieldIndex field)
{
  return build_chain_for_field (fieldDefForIndex (field), field);
}

/* Search the chain of AdmEntry nodes in CHAIN for KEY.  Returns a copy
   of the entry if found, NULL otherwise. */

AdmEntry *
find_chain_entry (AdmEntry *chain, const char *key)
{
  while (chain != NULL)
    {
      if (strcasecmp (chain->admFields[0], key) == 0)
	{
	  return copy_adm_entry (chain);
	}
      chain = chain->next;
    }
  return NULL;
}

/* Search the chain of AdmEntry nodes in CHAIN for KEY.  Returns the actual
   entry if found, NULL otherwise. */

AdmEntry *
find_chain_entry_nocopy (AdmEntry *chain, const char *key)
{
  while (chain != NULL)
    {
      if (strcasecmp (chain->admFields[0], key) == 0)
	{
	  return chain;
	}
      chain = chain->next;
    }
  return NULL;
}

/* Returns a non-zero value if CHAIN contains an entry that matches KEY. */
int
has_chain_entry (AdmEntry *chain, const char *key)
{
  while (chain != NULL)
    {
      if (strcasecmp (chain->admFields[0], key) == 0)
	{
	  return 1;
	}
      chain = chain->next;
    }
  return 0;
}

static StringList *
buildEnumFieldList (FieldDef field, AdmEntry *chain)
{
  AdmEntry *c = chain;
  StringList *res = NULL, **ptr = &res;
  for (c = chain; c != NULL; c = c->next)
    {
      *ptr = new_string_list_ent (xstrdup (c->admFields[field->key_field]),
				  NULL);
      ptr = &((*ptr)->next);
    }
  return res;
}

/* Initialize the administrative database for field FIELD.  */
void
initAdmField (FieldDef field)
{
  if (field->adm_contents == NULL)
    {
      field->adm_contents = build_chain_for_field (field, InvalidFieldIndex);
      if (field->adm_contents != NULL)
	{
	  field->enumValues = buildEnumFieldList (field,
						  field->adm_contents);
	  if (! field->allow_any_value && field->default_value == NULL)
	    {
	      field->default_value 
		= xstrdup (field->adm_contents->admFields[0]);
	    }
	}
    }
}

void
freeAdmFieldDesc (AdmFieldDesc *desc)
{
  freeStringList ((StringList *) desc);
}

void
freeAdmEntryChain (AdmEntry *ent)
{
  while (ent != NULL)
    {
      AdmEntry *n = ent->next;
      ent->next = NULL;
      free_adm_entry (ent);
      ent = n;
    }
}

/* Grab the value of field I From PR, then find the corresponding entry
   in the adm file associated with field I.  If one is found, return
   the subfield value SUBFIELDNAME. Otherwise, a NULL value is returned. */

const char *
getAdmSubfieldValue (PR *pr, FieldIndex i, const char *subfieldName)
{
  AdmFieldDesc *desc;
  const char *prFieldValue;
  AdmEntry *ent;
  int x;

  if (i == NULL)
    return NULL;
  
  desc = fieldDefForIndex (i)->adm_field_des;
  prFieldValue = field_value (pr, i);
  if (prFieldValue != NULL && desc != NULL)
    {
      ent = find_chain_entry_nocopy (fieldDefForIndex (i)->adm_contents,
				     prFieldValue);

      if (ent != NULL)
	{
	  for (x = 0; 
	       x < ent->fieldcount && desc != NULL; 
	       x++, desc = desc->next)
	    {
	      if (strcasecmp (desc->name, subfieldName) == 0)
		{
		  return ent->admFields[x];
		}
	    }
	}
    }
  return NULL;
}

int
printAdmSubfield (FILE *outfile, const char *eolTerminator, FieldIndex i,
		  AdmEntry *ent, const char *optSubfieldName)
{
  int x;
  AdmFieldDesc *desc = fieldDefForIndex (i)->adm_field_des;

  if (desc == NULL && optSubfieldName != NULL)
    {
      return -1;
    }
  for (x = 0;
       x < ent->fieldcount && (optSubfieldName == NULL || desc != NULL);
       x++)
    {
      if (optSubfieldName == NULL)
	{
	  fprintf (outfile, (x == 0) ? "%s" : ":%s", ent->admFields[x]);
	}
      else if (strcmp (desc->name, optSubfieldName) == 0)
	{
	  fprintf (outfile, "%s", ent->admFields[x]);
	  break;
	}
      if (desc != NULL)
	{
	  desc = desc->next;
	}
    }
  fprintf (outfile, "%s", eolTerminator);
  return 0;
}
