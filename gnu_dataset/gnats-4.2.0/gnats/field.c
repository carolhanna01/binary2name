/* Routines for dealing with field definitions and contents.
   Copyright (C) 2000,2007 Free Software Foundation, Inc.
   Contributed by Bob Manson (manson@juniper.net).

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
#include "field.h"

enum complex_field_index_datatype {
  InvalidComplexFieldIndexDatatype, HeaderNameValue, FieldIndexValue,
  FieldTypeValue, StringValue
};

struct complex_field_index
{
  DatabaseInfo database;
  char *name;
  enum complex_field_index_datatype dtype;
  union field_list_u {
    Header_Name headerIndex;
    FieldIndex index;
    FieldType fieldType;
  } datum;
  char *admSubfield;
  int isReason;
  int isOldPr;
  int isRaw;
};

struct databaseFieldInfo
{
  FieldDef *fieldList;
  int fieldListSize;
  int lastFieldEnt;
  int *namePosition;
};

static void
insertNewFieldDef (DatabaseInfo database, FieldDef field)
{
  DatabaseFieldInfo *dinfoPtr = getDatabaseFieldInfoWrite (database);
  DatabaseFieldInfo dinfo = *dinfoPtr;

  field->database = database;
  if (dinfoPtr == NULL)
    {
      abort ();
    }
  if (dinfo == NULL)
    {
      dinfo = (*dinfoPtr = 
	       (DatabaseFieldInfo) 
	       xmalloc (sizeof (struct databaseFieldInfo)));
      dinfo->fieldList = (FieldDef *) xmalloc (sizeof (FieldDef) * 10);
      dinfo->namePosition = (int *) xmalloc (sizeof (int) * 10);
      dinfo->fieldListSize = 10;
      dinfo->lastFieldEnt = 0;
    }
  else if (dinfo->lastFieldEnt >= dinfo->fieldListSize)
    {
      dinfo->fieldListSize += 10;
      dinfo->fieldList = 
	(FieldDef *) xrealloc (dinfo->fieldList,
			       sizeof (FieldDef) * dinfo->fieldListSize);
      dinfo->namePosition =
	(int *) xrealloc (dinfo->namePosition,
			  sizeof (int) * dinfo->fieldListSize);
    }
  field->number = dinfo->lastFieldEnt;
  dinfo->fieldList[dinfo->lastFieldEnt] = field;

  {
    int x, y;

    for (x = 0; x < dinfo->lastFieldEnt; x++)
      {
	char *name = dinfo->fieldList[dinfo->namePosition[x]]->name;
	if (strcasecmp (name, field->name) > 0)
	  {
	    break;
	  }
      }
    for (y = dinfo->lastFieldEnt - 1; y >= x; y--)
      {
	dinfo->namePosition[y + 1] = dinfo->namePosition[y];
      }
    dinfo->namePosition[x] = dinfo->lastFieldEnt;
    dinfo->lastFieldEnt++;
  }
}

FieldDef
newFieldDef (DatabaseInfo database, char *name)
{
  if (find_field_index (database, name) == InvalidFieldIndex)
    {
      FieldDef res =  (FieldDef) xmalloc (sizeof (struct field_def));
      memset (res, 0, sizeof (struct field_def));
      res->name = name;
      insertNewFieldDef (database, res);
      return res;
    }
  else
    {
      return NULL;
    }
}

static void
freeFieldDef (FieldDef def)
{
  if (def->name != NULL)
    {
      free (def->name);
    }
  if (def->description != NULL)
    {
      free (def->description);
    }
  if (def->default_value != NULL)
    {
      free (def->default_value);
    }
  freeStringList (def->enumValues);
  if (def->adm_db_name != NULL)
    {
      free (def->adm_db_name);
    }
  if (def->multiEnumSeparator != NULL)
    {
      free (def->multiEnumSeparator);
    }

  if (def->auxFlags != NULL)
    {
      free (def->auxFlags);
    }

  if (def->input_default_value != NULL)
    {
      free (def->input_default_value);
    }

  freeAdmFieldDesc (def->adm_field_des);
  freeAdmEntryChain (def->adm_contents);
  freeStringList (def->regex);
  freeChangeActions (def->changeActions);
  freeQueryFormat (def->virtualFormat);
  free (def);
}

FieldIndex
getNthField (const DatabaseInfo database, int n)
{
  DatabaseFieldInfo dinfo = getDatabaseFieldInfo (database);

  if (dinfo == NULL)
    {
      return InvalidFieldIndex;
    }
  else if (n < 0 || n >= dinfo->lastFieldEnt)
    {
      return InvalidFieldIndex;
    }
  else
    {
      return dinfo->fieldList[n];
    }
}

char *
getFieldFlags (const FieldIndex field)
{
  FieldDef def = fieldDefForIndex (field);
  char *res = xstrdup ("");

  if (def == NULL)
    {
      return NULL;
    }
  if (def->textsearch)
    {
      append_string (&res, "textsearch ");
    }
  if (def->allow_any_value)
    {
      append_string (&res, "allowAnyValue ");
    }
  if (requiresChangeReason (def))
    {
      append_string (&res, "requireChangeReason ");
    }
  if (def->readonly)
    {
      append_string (&res, "readonly ");
    }
  if (def->auxFlags != NULL)
    {
      StringList *l = def->auxFlags;
      while (l != NULL)
	{
	  append_string (&res, l->name);
	  l = l->next;
	}
    }
  return res;
}

int
get_num_fields (const DatabaseInfo database)
{
  DatabaseFieldInfo dinfo = getDatabaseFieldInfo (database);

  if (dinfo == NULL)
    {
      return -1;
    }
  return dinfo->lastFieldEnt;
}

static ComplexFieldIndex emptyEnt = NULL;

void
freeComplexFieldIndex (ComplexFieldIndex i)
{
  if (i != NULL)
    {
      if (i->name != NULL)
	{
	  free (i->name);
	}
      if (i->admSubfield != NULL)
	{
	  free (i->admSubfield);
	}
      if (emptyEnt == NULL)
	{
	  emptyEnt = i;
	}
      else
	{
	  free (i);
	}
    }
}

static ComplexFieldIndex
allocComplexFieldIndex (void)
{
  ComplexFieldIndex res;

  if (emptyEnt != NULL)
    {
      res = emptyEnt;
      emptyEnt = NULL;
    }
  else
    {
      res = (ComplexFieldIndex) xmalloc (sizeof (struct complex_field_index));
    }
  return res;
}


void
freeFieldList (FieldList list)
{
  FieldList n;
  while (list != NULL)
    {
      n = list->next;
      freeComplexFieldIndex (list->ent);
      free (list);
      list = n;
    }
}

void
freeFieldEdit (FieldEdit *edit)
{
  FieldEdit *n;
  while (edit != NULL)
    {
      n = edit->next;
      if (edit->fieldToEditName != NULL)
	{
	  free (edit->fieldToEditName);
	}
      if (edit->textFormat != NULL)
	{
	  free (edit->textFormat);
	}
      freeFieldList (edit->fieldsForFormat);
      free (edit);
      edit = n;
    }
}

void
clearFieldList (DatabaseInfo database)
{
  DatabaseFieldInfo *dinfoPtr = getDatabaseFieldInfoWrite (database);
  if (dinfoPtr != NULL && *dinfoPtr != NULL)
    {
      DatabaseFieldInfo dinfo = *dinfoPtr;
      int x;

      for (x = 0; x < dinfo->lastFieldEnt; x++)
	{
	  freeFieldDef (dinfo->fieldList[x]);
	}
      free (dinfo->fieldList);
      dinfo->fieldList = NULL;
      dinfo->fieldListSize = 0;
      dinfo->lastFieldEnt = 0;
      if (dinfo->namePosition != NULL)
	{
	  free (dinfo->namePosition);
	  dinfo->namePosition = NULL;
	}
      free (dinfo);
      *dinfoPtr = NULL;
    }
}

FieldList
newFieldListEnt (const DatabaseInfo database, const char *name, FieldList next)
{
  FieldList res = (FieldList) xmalloc (sizeof (struct field_list));
  res->ent = newComplexFieldIndex (database, name);
  res->next = next;
  return res;
}

ComplexFieldIndex
newComplexFieldIndex (const DatabaseInfo database, const char *name)
{
  ComplexFieldIndex res = allocComplexFieldIndex ();

  res->database = database;
  res->name = xstrdup (name);
  res->dtype = InvalidComplexFieldIndexDatatype;
  res->admSubfield = NULL;
  res->isReason = 0;
  res->isOldPr = 0;
  res->isRaw = 0;
  return res;
}

ComplexFieldIndex
newComplexFieldLiteralString (const char *string)
{
  ComplexFieldIndex res = allocComplexFieldIndex ();

  res->database = NULL;
  res->name = xstrdup (string);
  res->dtype = StringValue;
  res->admSubfield = NULL;
  res->isReason = 0;
  res->isOldPr = 0;
  res->isRaw = 0;
  return res;
}

ComplexFieldIndex
simpleComplexFieldIndex (FieldIndex index)
{
  ComplexFieldIndex res = allocComplexFieldIndex ();

  res->database = index->database;
  res->name = NULL;
  res->datum.index = index;
  res->dtype = FieldIndexValue;
  res->admSubfield = NULL;
  res->isReason = 0;
  res->isOldPr = 0;
  res->isRaw = 0;
  return res;
}

/* Fill in the missing fields of ENT.  */

int
parseComplexFieldIndex (ComplexFieldIndex ent)
{
  if (ent->dtype == StringValue)
    {
      return 0;
    }
  else if (ent->name != NULL)
    {
      size_t nlen = strlen (ent->name);
      char *name = ent->name;

      if (name[0] == '$')
	{
	  return 0;
	}

      ent->admSubfield = NULL;
      ent->dtype = InvalidComplexFieldIndexDatatype;
      ent->isReason = 0;
      ent->isRaw = 0;
      ent->isOldPr = 0;

      if (name[nlen - 1] == ']')
	{
	  char *ptr = strchr (name, '[');

	  if (ptr != NULL && name[nlen - 1])
	    {
	      name[nlen - 1] = '\0';
	      ent->admSubfield = xstrdup (ptr + 1);
	      ptr[0] = '\0';
	      nlen = ptr - name;
	    }
	}
      if (nlen > 12 && strcmp (name + nlen - 12, "-Changed-Why") == 0)
	{
	  name[nlen - 12] = '\0';
	  ent->isReason = 1;
	}
      else
	{
	  ent->isReason = 0;
	}

      if (strncasecmp (name, "raw:", 4) == 0)
	{
	  ent->isRaw = 1;
	  name += 4;
	}

      if (strncasecmp (name, "oldpr:", 6) == 0)
	{
	  ent->isOldPr = 1;
	  name += 6;
	}

      if (strncasecmp (name, "fieldtype:", 10) == 0)
	{
	  ent->datum.fieldType = stringToFieldType (name + 10);
	  if (ent->datum.fieldType != InvalidFieldType)
	    {
	      ent->dtype = FieldTypeValue;
	    }
	}
      else if (strncasecmp (name, "builtinfield:", 13) == 0)
	{
	  ent->dtype = FieldIndexValue;
	  ent->datum.index = findBuiltinField (ent->database, name + 13);
	}
      else
	{
	  ent->datum.index = find_field_index (ent->database, name);
	  if (ent->datum.index == InvalidFieldIndex && ! ent->isReason)
	    {
	      ent->datum.headerIndex = find_header_index (name);
	      if (ent->datum.headerIndex != InvalidHeaderName)
		{
		  ent->dtype = HeaderNameValue;
		}
	    }
	  else
	    {
	      ent->dtype = FieldIndexValue;
	    }
	}
      free (ent->name);
      ent->name = NULL;
    }
  if (ent->dtype == InvalidComplexFieldIndexDatatype)
    {
      return -1;
    }
  else
    {
      return 0;
    }
}

const char *
get_field_value (PR *newPR, PR *oldPR, ComplexFieldIndex ent,
		 FormatNamedParameter *params, int *mustBeFreed)
{
  PR *pr = newPR;

  if (mustBeFreed != NULL)
    {
      *mustBeFreed = 0;
    }

  if (ent->isOldPr && oldPR != NULL)
    {
      pr = oldPR;
    }

  if (ent->dtype == StringValue)
    {
      return ent->name;
    }

  if (ent->name != NULL && ent->name[0] == '$')
    {
      return getNamedParameterValue (params, ent->name);
    }

  if (pr == NULL)
    {
      return "";
    }

  if (ent->name == NULL || parseComplexFieldIndex (ent) == 0)
    {
      const char *value;

      if (ent->dtype == HeaderNameValue)
	{
	  value = header_value (pr, ent->datum.headerIndex);
	}
      else if (ent->dtype == FieldTypeValue)
	{
	  value = xstrdup ("invalid");
	}
      else if (ent->admSubfield != NULL)
	{
	  value = getAdmSubfieldValue (pr, ent->datum.index, ent->admSubfield);
	}
      else if (ent->isReason)
	{
	  value = field_change_reason (pr, ent->datum.index);
	}
      else if (ent->datum.index != InvalidFieldIndex)
	{
	  if (ent->isRaw || (mustBeFreed == NULL)
	      || (fieldDefForIndex (ent->datum.index)->virtualFormat == NULL))
	    {
	      value = field_value (pr, ent->datum.index);
	    }
	  else
	    {
	      char *res = NULL;
	      FieldIndex indx = ent->datum.index;

	      process_format (NULL, &res, pr, oldPR,
			      fieldDefForIndex (indx)->virtualFormat,
			      "\n", params);
	      value = res;
	      *mustBeFreed = 1;
	    }
	}
      else
	{
	  value = NULL;
	}
      return value;
    }
  else
    {
      return NULL;
    }
}

int
isConstantFieldValue (ComplexFieldIndex ent)
{
  if (ent->dtype == StringValue)
    {
      return 1;
    }
  return 0;
}

FieldIndex
simpleFieldIndexValue (ComplexFieldIndex field)
{
  if (field->name != NULL)
    {
      parseComplexFieldIndex (field);
    }
  if (field->dtype != FieldIndexValue)
    {
      return InvalidFieldIndex;
    }
  else
    {
      return field->datum.index;
    }
}


/* Return the string name equivalent of FIELD.  */
char *
complexFieldIndexToString (ComplexFieldIndex field)
{
  switch (field->dtype)
    {
    case FieldTypeValue:
      {
	const char *name = fieldTypeAsString (field->datum.fieldType);
	char *res;
	asprintf (&res, "fieldtype:%s", name);
	return res;
      }
      break;
    case StringValue:
      {
	char *res;

	asprintf (&res, "\"%s\"", field->name);
	return res;
      }
      break;
    case HeaderNameValue:
      {
	return xstrdup (header_name (field->datum.headerIndex));
      }

    case FieldIndexValue:
      {
	char *res;

	if (field->datum.index == InvalidFieldIndex)
	  {
	    res = xstrdup ("invalid");
	  }
	else
	  {
	    const char *changedWhy = (field->isReason ? "-Changed-Why" : "");

	    if (field->admSubfield != NULL)
	      {
		asprintf (&res, "%s%s[%s]",
			  fieldDefForIndex (field->datum.index)->name,
			  changedWhy,
			  field->admSubfield);
	      }
	    else
	      {
		asprintf (&res, "%s%s",
			  fieldDefForIndex (field->datum.index)->name,
			  changedWhy);
	      }
	  }
	return res;
      }
      break;

    default:
      {
	if (field->name != NULL)
	  {
	    return xstrdup (field->name);
	  }
	else
	  {
	    return xstrdup ("invalid");
	  }
      }
      break;
    }
}

FieldIndex
find_field_index_with_len (const DatabaseInfo database, const char *name,
			   size_t len)
{
  int pr_fields = get_num_fields (database);
  int start = 0; 
  int end = pr_fields - 1;
  DatabaseFieldInfo dinfo = getDatabaseFieldInfo (database);

  if (dinfo == NULL)
    {
      return InvalidFieldIndex;
    }

  while (start <= end)
    {
      int pos = (start + end) / 2;
      int val = strncasecmp (dinfo->fieldList[dinfo->namePosition[pos]]->name,
			     name, len);
      if (val == 0 && dinfo->fieldList[dinfo->namePosition[pos]]->name[len] 
	  != '\0')
	{
	  val = 1;
	}
      if (val == 0)
	{
	  return getNthField (database, dinfo->namePosition[pos]);
	}
      else if (val < 0)
	{
	  start = pos + 1;
	}
      else
	{
	  end = pos - 1;
	}
    }
  return InvalidFieldIndex;
}

FieldIndex
find_field_index (const DatabaseInfo database, const char *name)
{
  return find_field_index_with_len (database, name, strlen (name));
}

FieldType
complexFieldType (ComplexFieldIndex field)
{
  if (field->dtype == FieldTypeValue)
    {
      return field->datum.fieldType;
    }
  else
    {
      return InvalidFieldType;
    }
}

struct FieldTypeInfo {
  const char *name;
  FieldType type;
} fieldTypeInfo[] = 
{
  { "Text",          Text },
  { "MultiText",     MultiText },
  { "Enum",          Enum },
  { "MultiEnum",     MultiEnum },
  { "Integer",       Integer },
  { "Date",          Date },
  { "TextWithRegex", TextWithRegex } ,
  { NULL,            InvalidFieldType }
};

FieldType
stringToFieldType (const char *string)
{
  int x;

  for (x = 0; fieldTypeInfo[x].name != NULL; x++)
    {
      if (strcasecmp (fieldTypeInfo[x].name, string) == 0)
	{
	  return fieldTypeInfo[x].type;
	}
    }
  return InvalidFieldType;
}

const char *
fieldTypeAsString (FieldType type)
{
  int x;

  for (x = 0; fieldTypeInfo[x].name != NULL; x++)
    {
      if (fieldTypeInfo[x].type == type)
	{
	  return fieldTypeInfo[x].name;
	}
    }
  return "InvalidFieldType";
}
