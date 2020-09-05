/* Interface to describing fields.
   Copyright (C) 1999,2000,07 Free Software Foundation, Inc.
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
along with GNU GNATS; see the file COPYING. If not, see
<http://www.gnu.org/licenses/>.
*/

#ifndef FIELD_H
#define FIELD_H

typedef const struct field_def *FieldIndex;
#define InvalidFieldIndex NULL

typedef struct field_def * FieldDef;
typedef struct complex_field_index *ComplexFieldIndex;
typedef struct field_list *FieldList;
typedef struct field_edit FieldEdit;
typedef struct input_template_fields InputTemplate;
typedef struct change_actions *ChangeActions;
typedef struct databaseFieldInfo *DatabaseFieldInfo;
typedef enum
{
  InvalidFieldType = -1, Text = 0, MultiText, Enum, MultiEnum, Integer, Date,
  TextWithRegex, PRListType
} FieldType;

typedef enum
{
  InvalidSearchType = -1,
  NilSearch = 0, RegCmp, RegFind, LessThan, GreaterThan, StringMatch,
  Equals, NotEquals, DefaultSearchType
} SearchType;

#include "pr.h"
#include "adm.h"
#include "query.h"
#include "database.h"

struct change_actions
{
  QueryExpr expr;
  FieldList requiredFields;
  FieldEdit *edits;
  int addAuditTrail;
  QueryFormat *auditTrailFormat;
  int requireChangeReason;
  struct change_actions *next;
};

struct field_edit
{
  QueryExpr expr;
  char *fieldToEditName;
  FieldIndex fieldToEdit;
  int append;
  char *textFormat;
  FieldList fieldsForFormat;
  struct field_edit *next;
};

struct input_template_fields
{
  FieldIndex index;
  struct input_template_fields *next;
};

struct field_def
{
  /* The database that this entry is associated with.  */
  DatabaseInfo database;

  /* This field's index number. */
  int number;

  /* The name of this field. */
  char *name;

  /* One-line description of this field in human-readable form. */
  char *description;

  /* Default value for the field, if no value is supplied. */
  char *default_value;

  /* The suggested value when a PR is initially input.  */
  char *input_default_value;

  /* If variant is enum, this is the set of possible values. */
  StringList *enumValues;

  /* Type of data that will be accepted in this field. */
  FieldType datatype;

  /* List of characters that can be separators between enums in a
     MultiEnum field.  */
  char *multiEnumSeparator;

  /* The default type of search to do on this field. */
  SearchType defaultSearchType;

  /* If non-zero, this field should be searched when doing text searches. */
  int textsearch;

  /* A non-zero value if this is a restricted field. */
  int restricted;

  /* Spaces are not permitted to appear in the index file for this field. */
  int nospaces;

  /* This field has an administrative database associated with it; this is
     the pathanme to it. */
  char *adm_db_name;

  /* The descriptor for the administrative database, if the field has one. */
  AdmFieldDesc *adm_field_des;

  /* Chain of contents of the administrative database.  */
  AdmEntry *adm_contents;

  /* Number of fields in each record of the ADM database. */
  int adm_db_fields;

  /* Which adm field is the key.  */
  int key_field;

  /* If the field value is required to match a regexp, this is it. */
  StringList *regex;

  /* If this is an enum field and if allow_any_value is non-zero, we don't
     require the value to match one of the listed enum values.  (This is
     mainly used for the Responsible field.)  */
  int allow_any_value;

  /* Set to a non-zero value if this field may not be edited. */
  int readonly;

  /* The actions to perform when this field is edited. */
  ChangeActions changeActions;

  /* Auxiliary undefined flags.  */
  StringList *auxFlags;

  /* Maximum number of PRs permitted per field entry.  0 means "any
     number".  Usually 0 or 1.. */
  unsigned int maxPrsPerLine;

  /* Set to 1 if the field should not be displayed.  */
  int editonly;

  QueryFormat *virtualFormat;
};

struct field_list
{
  ComplexFieldIndex ent;
  struct field_list *next;
};

extern void freeFieldList (FieldList list);
extern void freeStringList (StringList *list);
extern void freeAdmFieldDesc (AdmFieldDesc *desc);
extern void freeFieldEdit (FieldEdit *edit);
extern void clearFieldList (DatabaseInfo database);

extern FieldList newFieldListEnt (const DatabaseInfo database,
				  const char *name, FieldList next);

extern FieldIndex getNthField (const DatabaseInfo database, int n);

extern ComplexFieldIndex newComplexFieldIndex (const DatabaseInfo database,
					       const char *name);
extern ComplexFieldIndex simpleComplexFieldIndex (FieldIndex index);
extern ComplexFieldIndex newComplexFieldLiteralString (const char *string);
extern void freeComplexFieldIndex (ComplexFieldIndex ent);
extern int parseComplexFieldIndex (ComplexFieldIndex ent);
extern FieldIndex simpleFieldIndexValue (ComplexFieldIndex field);
extern const char * get_field_value (PR *pr, PR *oldPR, ComplexFieldIndex ent,
				     FormatNamedParameter *params,
				     int *mustBeFreed);
int isConstantFieldValue (ComplexFieldIndex field);
extern char *complexFieldIndexToString (ComplexFieldIndex field);
extern char *getFieldFlags (const FieldIndex field);

/* A bit cheezy, but it makes sense now.  */
#define fieldDefForIndex(FIELD) ((FieldDef)FIELD)

#define fieldNumber(FIELD) (fieldDefForIndex ((FIELD))->number)

extern FieldDef newFieldDef (DatabaseInfo database, char *name);
extern FieldIndex find_field_index (const DatabaseInfo database,
				    const char *name);
extern FieldIndex find_field_index_with_len (DatabaseInfo database, 
					     const char *name, size_t len);

/* If the ComplexFieldIndex represents a field type, return it; otherwise,
   InvalidFieldType is returned. */
extern FieldType complexFieldType (ComplexFieldIndex field);

extern const char *fieldTypeAsString (FieldType field);

extern FieldType stringToFieldType (const char *string);

extern ChangeActions newChangeAction (const DatabaseInfo database,
				      const char *optExpr);

extern void freeChangeActions (ChangeActions actions);

extern int requiresChangeReason (FieldDef field);

#endif
