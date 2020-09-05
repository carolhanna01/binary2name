/* Interface to the administrative databases.
   Copyright (C) 1999, 2007 Free Software Foundation, Inc.
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

#ifndef _ADM_H_
#define _ADM_H_

#include "gnats.h"

typedef struct admEntry 
{
  /* Which PR field this entry belongs to. */
  FieldIndex field;

  /* The number of fields in this entry. */
  int fieldcount;

  /* The data. */
  char **admFields;

  /* Optional next pointer, if we have a list of these. */
  struct admEntry *next;
} AdmEntry;

typedef StringList AdmFieldDesc;

/* Enums describing each of the hardcoded ADM databases we know about. 
   The number corresponds to which field in the AdmEntry it belongs
   to. */

enum ClassAdmFields {
  ClassAdmKey = 0,
  ClassAdmType = 1,
  ClassAdmDescription = 2
};

enum StateAdmFields {
  StateAdmKey = 0,
  StateAdmType = 1,
  StateAdmDescription = 2
};

enum CategoryAdmFields {
  CategoryAdmKey = 0,
  CategoryAdmFullname = 1,
  CategoryAdmPerson = 2,
  CategoryAdmNotify = 3
};

enum ResponsibleAdmFields {
  ResponsibleAdmKey = 0,
  ResponsibleAdmFullname = 1,
  ResponsibleAdmAlias = 2
};

enum SubmitterAdmFields {
  SubmitterAdmKey = 0,
  SubmitterAdmFullname = 1,
  SubmitterAdmType = 2,
  SubmitterAdmRtime = 3,
  SubmitterAdmContact = 4,
  SubmitterAdmNotify = 5
};

enum DatabaseListFields {
  DatabaseListKey = 0,
  DatabaseListDesc = 1,
  DatabaseListPath = 2,
  DatabaseServer = 3, /* optional */
  DatabasePort = 4 /* optional */
};

enum HostListFields {
  HostListKey = 0,
  HostListAccessLevel = 1,
  HostListUnused = 2
};

/* Return the adm record matching KEY from field FIELD; if one is
   not found, return NULL. 

   This function gets the entry by reading through each entry of
   the file sequentially, so it is rather slow.  

   The returned entry should be freed with free_adm_entry () when it is
   no longer needed. */
extern AdmEntry *get_adm_record (FieldIndex field, const char *key);

/* Frees an adm record entry. */
extern void free_adm_entry (AdmEntry *ent);

/* Construct an AdmEntry item using the contents of LINE. FIELD is
   the field value placed into the entry; it is legal for it to be
   InvalidFieldIndex.  
   Fields within an entry are separated with a ':'.  */
extern AdmEntry *build_adm_entry (const char *line, FieldIndex field);

/* Build a chain of AdmEntry entries containing the contents of the adm
   file associated with FIELD.  */
extern AdmEntry *build_chain (FieldIndex field);

/* Search CHAIN for an entry containing KEY as its key. If found, a copy
   of the entry is returned; otherwise, NULL is returned. 

   The entry should be freed with free_adm_entry() when it is no longer
   needed.  */
extern AdmEntry *find_chain_entry (AdmEntry *chain, const char *key);

/* Search CHAIN for an entry containing KEY as its key. If found the entry
   in the chain is returned, otherwise NULL is returned. */
extern AdmEntry *find_chain_entry_nocopy (AdmEntry *chain, const char *key);

/* Return a non-zero value if CHAIN contains an entry with KEY as its
   key. */
extern int has_chain_entry (AdmEntry *chain, const char *key);

/* Allocate an AdmEntry with FIELDS number of fields.  */
extern AdmEntry *alloc_adm_entry (int fields);

/* Free the AdmEntry chain pointed to by ENT. */
extern void freeAdmEntryChain (AdmEntry *ent);

/* Free the AdmFieldDesc entry DESC.  */
extern void freeAdmFieldDesc (AdmFieldDesc *desc);

extern void initAdmField (FieldDef fieldDef);

/* Return the adm entry's subfield SUBFIELDNAME for the adm entry
   matching the value of FIELD in PR. */
extern const char *getAdmSubfieldValue (PR *pr, FieldIndex field, 
					const char *subfieldName);

extern int printAdmSubfield (FILE *outfile, const char *eolTerminator,
			     FieldIndex i, AdmEntry *ent,
			     const char *optSubfieldName);

#endif
