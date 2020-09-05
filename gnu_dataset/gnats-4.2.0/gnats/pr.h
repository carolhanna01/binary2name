/* Pieces of the actual PR.
   Copyright (C) 1993,2007 Free Software Foundation, Inc.
   Contributed by Tim Wicinski (wicinski@barn.com).

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

#ifndef _pr_h_
#define _pr_h_

struct PR_struct;

typedef struct PR_struct PR;

/* These better be in the same order as they appear in the headerNames
   declaration in pr.c.  */
typedef enum {
    InvalidHeaderName = -1,
    SM_FROM = 0,	/* Sendmail 'From', must come before 'From:' */
    RETURN_PATH, 
    RECEIVED,		/* received, GNATS keeps only the first rcvd header */
    MSG_ID,		/* Message-Id */
    DATE,
    FROM,		/* the "From:" field, as opposed to unix 'From' */
    SENDER,
    REPLY_TO,
    TO,
    APPAR_TO,		/* Apparently-To */
    CC,
    IN_REP_TO,		/* In-Reply-To */
    SUBJECT,
    REFERENCES,
    X_SEND_PR,
    X_GNATS_NOTIFY,
    X_GNATS_NONOTIFY,
    NUM_HEADER_ITEMS	/* Last entry, don't put anything in after this.  */
} Header_Name;

#include <stdio.h>
#include "gnats.h"
#include "field.h"
#include "database.h"

/* Describes one entire PR.  */
struct PR_struct {
  /* The database this PR is associated with.  */
  DatabaseInfo database;

  /* all the datastore specific stuff */
  void *ds_private;

  /* Non-zero if the PR contents are pruned.  */
  int prune;

  /* Non-zero if the PR has been read in.  */
  int read_in;

  /* Private data used by pr.c. */
  struct PR_private *private;
};

/* This *might* be correct.  */
#define PR_IS_FULL(PR) ((PR)->prune == 0 && (PR)->read_in != 0)

extern void write_entire_pr (FILE *outfile, PR *pr, const char *eolTerminator);

extern void write_entire_header	(FILE *outputle, PR *pr, 
				 const char *eolTerminator);

extern void write_pr_field (FILE *outfile, FieldIndex field,
			    const char *fieldText, const char *eolTerminator);

extern int get_num_fields (const DatabaseInfo database);
extern int setBuiltinField (FieldIndex field, const char *builtinName);
extern FieldIndex findBuiltinField (const DatabaseInfo database,
				    const char *name);

extern void initReadPR (PR *pr);
extern void addLineToPR (PR *pr, char *buffer, char *line, size_t linelen,
			 int prune);
extern void finishReadPR (PR *pr, int prune);

extern void write_multitext (FILE *fp, const char *str,
			     const char *eolTerminator);

extern const char *header_name (Header_Name header);
extern const char *header_value (PR *pr, Header_Name header);
extern const char *raw_header_value (PR *PR, Header_Name header);
extern void set_header (PR *pr, Header_Name header, const char *newValue);
extern int read_header (PR *pr, FILE *infile);
extern void write_header (PR *pr, FILE *outfile, Header_Name headerField);

extern PR *allocPR (const DatabaseInfo database);
extern void free_pr (PR *pr);
extern void free_pr_header (PR *pr);
extern void free_pr_contents (PR *pr);
extern char *gen_pr_path (PR *pr);

extern void read_pr (PR *pr, FILE *infile, int prune);
extern int initDbInfo (ErrorDesc *err);
extern void freeInputTemplate (InputTemplate *template);
extern const char *field_value (PR *pr, FieldIndex field);
extern int prFieldHasValue (PR *pr, FieldIndex field);
extern const char *field_change_reason (PR *pr, FieldIndex field);
extern int verifyMultiEnum (FieldIndex field, const char *value);
extern int verify_enum (FieldIndex field, const char *value);
extern bool set_field (PR *pr, FieldIndex field, const char *value, 
		       ErrorDesc *err);
extern void setFieldChangeReason (PR *pr, FieldIndex field, const char *value);
extern void unsetField (PR *pr, FieldIndex field);
extern BadFields checkEnumTypes (PR *pr, BadFields currBadFields, 
				 int isInitialPR);

extern int fconfigParse (DatabaseInfo database, const char *filename,
			 int (*func)(char *, int), ErrorDesc *err);

extern int createPrFile (PR *pr, const char *filename, int force,
			 ErrorDesc *err);

extern void printValidValues (FILE *outfile, FieldIndex i, const char *eol);

/* Given the name of a header NAME, return its Header_Name value, or
   InvalidHeaderName if NAME is not a valid header name.  */
extern Header_Name find_header_index (const char *name);

#endif /* _pr_h_ */
