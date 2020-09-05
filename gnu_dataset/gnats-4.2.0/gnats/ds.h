/* The GNATS datastore API.
   Copyright (C) 2005,07 Free Software Foundation, Inc.
   Contributed by Mel Hatzis <hatzis@wattes.org>.

This file is part of GNU GNATS.

GNU GNATS is free software; you can redistribute it and/or modify
it under the terms of the GNU General Public License as published by
the Free Software Foundation; either version 3 of the License, or
(at your option) any later version.

GNU GNATS is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
GNU General Public License for more details.

You should have received a copy of the GNU General Public License
along with GNU GNATS; see the file COPYING. If not, see
<http://www.gnu.org/licenses/>.
*/

#ifndef _DS_H
#define _DS_H

#include "gnats.h"
#include "database.h"
#include "query.h"
#include "pr.h"

/* --------- ds-[db]/db.c ------------- */

/* The function that is invoked from db_query () is of this type.
   First argument is the result # (1 for the first PR matched, 2 for
   the second, etc). The second argument is the PR entry for the
   PR, and the final argument is the QueryFormat supplied to db_query.

   The function is invoked with a NULL PR entry when we run out of PRs
   to match; the result # is the total # of PRs that were matched. */
typedef void (*QueryFunc)(int, PR *, QueryFormat *);

extern int db_init (DatabaseInfo db, ErrorDesc *err);
extern void db_destroy (DatabaseInfo db);
extern int db_query (const DatabaseInfo database, int ac, char **av,
		     QueryExpr exp, QueryFunc func, QueryFormat *query_format,
		     ErrorDesc *err);
extern void db_reset (const DatabaseInfo database, ErrorDesc *err);

/* --------- ds-[db]/pr.c ------------- */

extern void pr_init (PR *pr);
extern void pr_destroy (PR *pr);
extern int pr_create (PR *pr, ErrorDesc *err);
extern int pr_update (PR *curr_pr, PR *new_pr, ErrorDesc *err);
extern int pr_delete (const DatabaseInfo database, const char *prnum,
		      ErrorDesc *err);
extern int pr_load (PR *pr, ErrorDesc *err);
extern int pr_load_fields (PR *pr, FieldList flds);
extern PR *pr_load_by_id (const DatabaseInfo database, const char *prnum,
			  int prune, ErrorDesc *err);
extern int pr_load_fields (PR *pr, FieldList flds);
extern int pr_exists (const DatabaseInfo database, const char *prnum,
		      ErrorDesc *err);

/* --------- ds-[db]/fld.c ------------- */
                          
extern char *field_cache_value (PR *pr, FieldIndex field);

#endif /* !_DS_H */
