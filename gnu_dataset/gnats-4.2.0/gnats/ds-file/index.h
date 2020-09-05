/*
Copyright (C) 2007  Free Software Foundation, Inc.
Contributed by Mel Hatzis <hatzis@wattes.org>.

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

#ifndef _INDEX_H_
#define _INDEX_H_

typedef struct indexEntry Index;
typedef struct index_desc *IndexDesc;

#include "gnats.h"
#include "database.h"
#include "field.h"
#include "pr.h"
#include "ds-file.h"

extern void initIndex (DatabaseInfo database);

extern IndexDesc newIndexDesc (const DatabaseInfo database);
extern void addFieldToIndex (IndexDesc desc, FieldList ent);
extern void setIndexDescPath (IndexDesc desc, const char *path);
extern void setIndexDescSeparator (IndexDesc desc, const char *separator);
extern void setIndexDescBinary (IndexDesc desc, int flag);

extern void allocIndex (FilePRInfo info);

extern void clearPRChain (const DatabaseInfo database);
extern int checkPRChain (const DatabaseInfo database, ErrorDesc *err);

extern PR *getFirstPR (const DatabaseInfo database, ErrorDesc *err);

extern PR *getPrevPR (PR *pr);
extern PR *getNextPR (PR *pr);
extern void setPrevPR (PR *pr, PR *prev_pr);
extern void setNextPR (PR *pr, PR *next_pr);

extern void freePRIndex (DatabaseInfo database, FilePRInfo info);

/* Construct an index entry for PR.  */
extern void buildIndexEntry (PR *pr);

/* Return a non-zero value if FIELD is part of the index for its database. */
extern int isIndexedFieldIndex (FieldIndex field);
extern int isIndexedField (ComplexFieldIndex field);

/* Write out the database's PR index from the copy in memory.  Returns 0 on
   success, a non-zero value otherwise.  */
extern int writeIndex (const DatabaseInfo database, ErrorDesc *err);
/* Add the PR to the GNATS index file for its database.  Returns
   0 on success, a non-zero value otherwise.  */
extern int addToIndex (PR *pr, ErrorDesc *err);

/* Create a new string representing the index entry for PR in DEST.  */
extern char *createIndexEntry (PR *pr, size_t *entLen);
extern char *createIndexEntryBinary (PR *pr, size_t *entLen);

extern int replaceCurrentPRInIndex (PR *curr_pr, PR *newPR, ErrorDesc *err);

extern char *indexValue (PR *pr, FieldIndex field);

extern int removePRFromIndex (const DatabaseInfo database, const char *prNum,
			      ErrorDesc *err);

extern void finishIndexDesc (const DatabaseInfo database, IndexDesc new);

extern void setIndexDesc (DatabaseInfo database, const IndexDesc desc);
extern IndexDesc getIndexDesc (const DatabaseInfo database);
extern void freeIndexDesc (IndexDesc desc);

extern int indexIsBinary (const DatabaseInfo database);

extern int indexFieldCount (const DatabaseInfo database);

extern int verifyPRExists (const DatabaseInfo database, const char *prID);
#endif

