/* All things that the query tools have in common.
   Copyright (C) 1994,95,96,1997,2007 Free Software Foundation, Inc.
   Contributed by Brendan Kehoe (brendan@cygnus.com).

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

#ifndef _QUERY_H_
#define _QUERY_H_

typedef struct queryFormat QueryFormat;
typedef struct queryExpr *QueryExpr;
struct re_pattern_buffer;

typedef enum e_lists {
  InvalidListType = -1,
  ListCategories = 0, ListSubmitters, ListResponsible, ListStates,
  ListFieldNames,  ListInitialInputFields, ListInitialRequiredFields,
  ListDatabases
} ListTypes;

/* The four operations to perform. */
typedef enum queryOp {
  InvalidQueryOp = -1, QueryMatch = 0, QueryAnd, QueryOr, QueryNot
} QueryOp;

#include "builtin-fields.h"
#include "field.h"
#include "mail.h"

typedef struct query_item
{
  ComplexFieldIndex fieldIndex;
  struct re_pattern_buffer *compRegexp;
} *QueryItem;

typedef struct search_item
{
  /* The type of search to perform, of course.  */
  SearchType searchType;

  /* The items on the left and right-hand-sides. */
  QueryItem lhs;
  QueryItem rhs;
} SearchItem;

typedef struct queryTree
{
  /* The operation to perform on this node. */
  QueryOp op;

  /* The left and right sides of the expression; if this is a unary op,
     the left side contains the expression. */
  struct queryTree *left, *right;

  /* If this is a QueryMatch expression, the actual query to perform. */
  SearchItem ent;
} *QueryTree;

struct queryExpr
{
  /* Database that this query is associated with.  */
  DatabaseInfo database;
  /* The actual query tree.  */
  QueryTree tree;
};

struct queryFormat
{
  /* The name of this query format. */
  char *name;

  /* A printf-style format.  If this is null, we just write the fields out
     as-is. */
  char *printf;

  /* The separator to place between each field.  If this is NULL, we print
     out the field header followed by the contents, and an EOL between each
     field. */
  char *separator;

  /* The list of fields. */
  FieldList fields;

  /* If 1, the query is "raw", and should not process virtual fields.  */
  int rawQuery;

  /* The next format in the list.  */
  struct queryFormat *next;
};

/* Return the numeric equivalent of enum entry TEXT for field FIELD; entries
   are numbered starting from 1.  0 is returned if TEXT is not a valid
   entry for FIELD.  */
extern int enum_numeric (const char *text, FieldIndex field);

extern int gnats_regcmp (const char *regexp, const char *string,
			 struct re_pattern_buffer *compRegexp);
extern int regfind (const char *regexp, const char *string,
		    struct re_pattern_buffer *compRegexp);

extern QueryExpr booleanQuery (QueryOp op, QueryExpr left,
			       QueryExpr right);

extern QueryExpr queryFieldType (FieldType fieldType,
				 SearchType stype,
				 ComplexFieldIndex rhs);

extern QueryExpr queryBuiltinField (PR_BuiltinField builtinField,
				     SearchType stype,
				     const char *regex);

extern QueryExpr *insertBooleanQuery (QueryExpr *lhs, QueryOp op);

extern void freeQueryExpr (QueryExpr);

extern int print_pr (FILE *, PR *pr, QueryFormat *query_format,
		     const char *eolTerminator);

extern int print_named_format_pr (FILE *, PR *pr, const char *format_name,
				  const char *eolTerminator, ErrorDesc *err);

extern int process_format (FILE *fp, char **res, PR *pr, PR *oldPR, 
			   QueryFormat *fmt,
			   const char *eolTerminator,
			   FormatNamedParameter *parameters);

extern int pr_matches_tree (PR *pr, PR *oldPR, QueryTree qexp,
			    FormatNamedParameter *params);
extern int pr_matches_expr (PR *pr, PR *oldPR, QueryExpr expr,
			    FormatNamedParameter *params);
extern int set_query_opt (QueryExpr *, int, const char *, int);

extern void addQueryFormat (DatabaseInfo database, QueryFormat *format);
extern QueryFormat *findQueryFormat (const DatabaseInfo database,
				     const char *name, ErrorDesc *err);

extern void insert_not_closed (QueryExpr *);
extern void insert_closed (QueryExpr *);

extern SearchType getSearchTypeForName (const char *);
extern const char *getSearchOperatorForType (SearchType type);
extern QueryExpr parseQueryExpression (const DatabaseInfo database,
				       const char *expr, const char *exprend);
extern char *queryExprToString (char *string, QueryExpr expr);
extern time_t get_any_date (const char *dateString);

extern void append_string (char **res, const char *string);

extern void freeQueryFormat (QueryFormat *format);
extern void freeQueryFormatList (QueryFormat *q);
#endif
