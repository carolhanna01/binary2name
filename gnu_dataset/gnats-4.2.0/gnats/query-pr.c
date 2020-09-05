/* Perform queries on the contents of a GNATS database.
   Copyright (C) 2001, 2002 Milan Zamazal
   Copyright (C) 1993, 94, 95, 96, 1997, 2000, 07 Free Software Foundation, Inc.
   Contributed by Brendan Kehoe (brendan@cygnus.com).
   Further hacked by Milan Zamazal (pdm@zamazal.org).

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
#include "mail.h"
#include "ds.h"

/* The name this program was run with.  */
const char *program_name;

#define PORT_OPTION 257
#define EXPR_OPTION 258
#define LISTFIELDNAMES_OPTION 259
#define FIELDTYPE_OPTION 260
#define VALIDVALUES_OPTION 261
#define FIELDDESC_OPTION 262
#define LISTINPUTFIELD_OPTION 263
#define LIST_DATABASES_OPTION 264
#define PRINT_DIRECTORY_OPTION 265
#define PRINTSHVARS_OPTION 266
#define ADMSUBFIELD_OPTION 267
#define ADMFIELD_OPTION 268
#define ADMKEYVALUE_OPTION 269
#define RESPONSIBLEADDRESS_OPTION 270
#define FIELDFLAGS_OPTION 271
#define PRINTSERVERADDR_OPTION 272

extern int debug;

struct option long_options[] =
{
  {"category", 1, NULL, 'c'},
  {"synopsis", 1, NULL, 'y'},
  {"confidential", 1, NULL, 'C'},
  {"debug", 0, NULL, 'D'},
  {"database", 1, NULL, 'd'},
  {"format", 1, NULL, 'f'},
  {"full", 0, NULL, 'F'},
  {"global-databases-file",1,NULL,'g'},
  {"help", 0, NULL, 'h'},
  {"host", 1, NULL, 'H'},
  {"port", 1, NULL, PORT_OPTION},
  {"user", 1, NULL, 'v'},
  {"passwd", 1, NULL, 'w'},
  {"multitext", 1, NULL, 'm'},
  {"originator", 1, NULL, 'O'},
  {"release", 1, NULL, 'A'},
  {"class", 1, NULL, 'L'},
  {"cases", 1, NULL, 'E'},
  {"quarter", 1, NULL, 'Q'},
  {"keywords", 1, NULL, 'K'},
  {"output", 1, NULL, 'o'},
  {"priority", 1, NULL, 'p'},
  {"responsible", 1, NULL, 'r'},
  {"restricted", 0, NULL, 'R'},
  {"severity", 1, NULL, 'e'},
  {"skip-closed", 0, NULL, 'x'},
  {"sql", 0, NULL, 'i'},
  {"sql2", 0, NULL, 'I'},
  {"state", 1, NULL, 's'},
  {"summary", 0, NULL, 'q'},
  {"submitter", 1, NULL, 'S'},
  {"text", 1, NULL, 't'},
  {"required-before", 1, NULL, 'u'},
  {"required-after", 1, NULL, 'U'},
  {"arrived-before", 1, NULL, 'b'},
  {"arrived-after", 1, NULL, 'a'},
  {"modified-before", 1, NULL, 'B'},
  {"modified-after", 1, NULL, 'M'},
  {"closed-before", 1, NULL, 'z'},
  {"closed-after", 1, NULL, 'Z'},
  {"list-categories", 0, NULL, 'j'},
  {"list-responsible", 0, NULL, 'k'},
  {"list-submitters", 0, NULL, 'l'},
  {"list-states", 0, NULL, 'T'},
  {"list-databases", 0, NULL, LIST_DATABASES_OPTION},
  {"print-directory-for-database", 0, NULL, PRINT_DIRECTORY_OPTION},
  {"version", 0, NULL, 'V'},

  /* Expression-related options */
  {"and", 0, NULL, '&'},
  {"or", 0, NULL, '|'},
  {"expr", 1, NULL, EXPR_OPTION},

  /* New field info options */
  {"list-fields", 0, NULL, LISTFIELDNAMES_OPTION },
  {"list-input-fields", 0, NULL, LISTINPUTFIELD_OPTION },
  {"field-type", 1, NULL, FIELDTYPE_OPTION },
  {"field-description", 1, NULL, FIELDDESC_OPTION },
  {"field-flags", 1, NULL, FIELDFLAGS_OPTION },
  {"valid-values", 1, NULL, VALIDVALUES_OPTION },
  {"adm-field", 1, NULL, ADMFIELD_OPTION },
  {"adm-subfield", 1, NULL, ADMSUBFIELD_OPTION },
  {"adm-key", 1, NULL, ADMKEYVALUE_OPTION },
  {"responsible-address", 1, NULL, RESPONSIBLEADDRESS_OPTION },

  /* New misc options */
  {"print-sh-vars", 0, NULL, PRINTSHVARS_OPTION},
  {"print-server-addr", 0, NULL, PRINTSERVERADDR_OPTION},
  {NULL, 0, NULL, 0}
};

#define PROGRAM_NAME "query-pr"

static const char *const USAGE[] = {
  "Usage: " PROGRAM_NAME " [OPTION]... [PR]...\n\
Query database.\n\
\n",
  "Options:\n\
          [--output file | -o file]\n\
          [--database database | -d database]\n\
          [--global-databases-file PATH | -g PATH ]\n\
          [--responsible-address address]\n\
          [--list-databases] [--list-fields] [--list-input-fields]\n\
          [--field-type type] [--field-description description]\n\
          [--field-flags field]\n\
          [--adm-field field] [--adm-subfield subfield]\n\
          [--adm-key key]\n",
"          [--valid-values values]\n\
          [--format format | -f format] [--full | -F] [--summary | -q]\n\
          [--and | -&] [--or | -|] [--expr expr]\n\
          [--help | -h] [--version | -V] [--debug | -D]\n\
\n",
  "Non-network-mode options:\n\
          [--print-sh-vars] [--print-directory-for-database]\n\
\n",
  "Network-mode-only options:\n\
          [--host host | -H host] [--port port]\n\
          [--user user | -v user] [--passwd passwd | -w passwd]\n\
          [--print-server-addr]\n\
\n",
  "Deprecated options:\n\
          [--list-categories | -j] [--list-states | -T]\n\
          [--list-responsible | -k] [--list-submitters | -l]\n\
          [--category category | -c category]\n\
          [--synopsis synopsis | -y synopsis]\n",
"          [--confidential confidential | -C confidential]\n\
          [--multitext multitext | -m multitext]\n\
          [--originator originator | -O originator]\n\
          [--release release | -A release]\n",
"          [--class class | -L class] [--cases cases | -E cases]\n\
          [--quarter quarter | -Q quarter]\n\
          [--keywords keywords | -K keywords]\n\
          [--priority priority | -p priority]\n\
          [--responsible responsible | -r responsible]\n\
          [--restricted | -R] [--severity severity | -e severity]\n",
"          [--skip-closed | -x] [--sql | -i] [--sql2 | -I]\n\
          [--state state | -s state]\n\
          [--submitter submitter | -S submitter]\n\
          [--text text | -t text]\n\
          [--required-before date | -u date]\n",
"          [--required-after date | -U date]\n\
          [--arrived-before date | -b date]\n\
          [--arrived-after date | -a date]\n\
          [--modified-before date | -B date]\n\
          [--modified-after date | -M date]\n\
          [--closed-before date | -z date]\n\
          [--closed-after date | -Z date]\n\n",
  NULL};


/* Where the results of query-pr should go. */
static FILE *outfile = NULL;
static int closeOutfile = 0;


/* These are the old hard-coded query options. */
static struct query_opt {
  /* Which field to query. */
  const char *fieldName;
  /* The set of options that specify the field.  For date fields, there
     are two options, one for "before" and one for "after".  */
  const char *options;
  /* Which type of search to perform.  */
  SearchType searchType;
} query_opts[] = {
  { "builtinfield:originator",		"O",	RegFind },
  { "builtinfield:submitter-id",	"S",	RegCmp },
  { "builtinfield:arrival-date",	"ba",	InvalidSearchType },
  { "builtinfield:synopsis",		"y",	RegFind },
  { "builtinfield:confidential",	"C",	RegCmp },
  { "builtinfield:severity",		"e",	RegCmp },
  { "builtinfield:priority",		"p",	RegCmp },
  { "builtinfield:category",		"c",	RegCmp },
  { "builtinfield:quarter",		"Q",	RegFind },
  { "builtinfield:keywords",		"K",	RegFind },
  { "builtinfield:date-required",	"uU",	InvalidSearchType },
  { "builtinfield:cases",		"E",	RegCmp },
  { "builtinfield:release",		"A",	RegFind },
  { "builtinfield:last-modified",	"BM",	InvalidSearchType },
  { "builtinfield:closed-date",		"zZ",	InvalidSearchType },
  { "builtinfield:responsible",		"r",	RegCmp },
  { "builtinfield:state",		"s",	RegCmp },
  { "builtinfield:class",		"L",	RegFind },
  { NULL,				NULL,	InvalidSearchType }
};

static void
queryPrExit (int code)
{
  if (closeOutfile)
    {
      fclose (outfile);
      outfile = NULL;
      closeOutfile = 0;
    }
  client_exit ();
  exit (code);
}

/* Process OPTC as a possible old-style query option.  If it's an old
   option, return a string representation of an expression that
   matches it; otherwise, return NULL.  */

static char *
oldQueryField (int optc, char *optarg)
{
  int z;

  for (z = 0; query_opts[z].options != NULL; z++)
    {
      const char *opts = query_opts[z].options;

      if (strlen (opts) > 1)
	{
	  int wopt;

	  for (wopt = 0; wopt < 2; wopt++)
	    {
	      if (opts[wopt] == optc)
		{
		  SearchType type = (wopt ? GreaterThan : LessThan);
		  char *qarg = quote_string (optarg);
		  char *res;
		  asprintf (&res, "%s%s%s",
			   query_opts[z].fieldName,
			   getSearchOperatorForType (type),
			   qarg);
		  free (qarg);
		  return res;
		}
	    }
	}
      else
	{
	  if (opts[0] == optc)
	    {
	      SearchType type = query_opts[z].searchType;
	      char *qarg = quote_string (optarg);
	      char *res;
	      asprintf (&res, "%s%s%s",
			query_opts[z].fieldName,
			getSearchOperatorForType (type),
			qarg);
	      free (qarg);
	      return res;
	    }
	}
    }
  return NULL;
}

static void
addSearchEntry (char **buf, char boolop, const char *fieldName, 
		const char *searchOp, const char *searchString, int lhs)
{
  char *newsearch;
  char *qarg = NULL;

  if (searchString != NULL)
    {
      qarg = quote_string (searchString);
    }

  if (*buf != NULL && **buf != '\0')
    {
      if (lhs)
	{
	  asprintf (&newsearch, "(%s%s%s)%c(%s)", fieldName,
		   searchOp, (qarg != NULL) ? qarg : "", boolop, *buf);
	}
      else
	{
	  asprintf (&newsearch, "(%s)%c(%s%s%s)", *buf, boolop, fieldName,
		    searchOp, (qarg != NULL) ? qarg : "");
	}
    }
  else
    {
      asprintf (&newsearch, "%s%s%s",  fieldName, searchOp,
	       (qarg != NULL) ? qarg : "");
    }

  if (qarg != NULL)
    {
      free (qarg);
    }

  if (*buf != NULL)
    {
      free (*buf);
    }

  *buf = newsearch;
}

static void
client_print_query (int which, PR *pr, QueryFormat *query_format)
{
  if (pr != NULL)
    {
      if (which > 1)
	{
	  printf ("\n");
	}
      print_pr (outfile, pr, query_format, "\n");
    }
  else
    {
      if (which > 0)
	{
	  printf ("\n");
	}
    }
}

/* XXX ??? !!! We call exit from too many places.  */
int
main (int argc, char **argv)
{
  DatabaseInfo database = NULL;
  int optc;
  int errors = 0;
  int formats = 0, lists = 0;
  char *user = NULL;
  char *passwd = NULL;
  char *hostname = NULL;
  int port = -1;
  int is_network_client = 0;
  static const char *optstring = "a:A:b:B:c:C:Dd:e:f:g:K:L:m:M:n:o:O:p:PQ:s:S:r:t:u:U:v:w:y:z:Z:VFGiIxhqH:RjJklT|&";
  /* If 1, don't allow redirection or viewing of confidential PRs.  */
  int restricted = 0;
  ListTypes whichList = 0;
  const char *query_format_name = "standard";
  int skip_closed = 0;
  char *validValuesField = NULL;
  char *fieldTypeField = NULL;
  char *fieldDescriptionField = NULL;
  char *nameOfDatabase = NULL;
  int printDirectory = 0;
  int printShVars = 0;
  int printServerAddr = 0;
  char *admField = NULL;
  char *admSubfield = NULL;
  char *admKey = NULL;
  char *responsibleAddress = NULL;
  char *exprBuf = NULL;
  char danglingOp;
  char *fieldFlagsField = NULL;
  char *outfile_name = NULL;

  program_name = basename (argv[0]);
  outfile = stdout;
  danglingOp = '&';

  if (argc == 1)
    usage (USAGE, 1);

  while ((optc = getopt_long (argc, argv, optstring, long_options,
			      (int *) 0)) != EOF)
    {
      char *oldQuery = oldQueryField (optc, optarg);
      if (oldQuery != NULL)
	{
	  addSearchEntry (&exprBuf, danglingOp, "", oldQuery, NULL, 0);
	  danglingOp = '&';
	  free (oldQuery);
	  continue;
	}
      switch (optc)
	{
	case EXPR_OPTION:
	  addSearchEntry (&exprBuf, danglingOp, "", optarg, NULL, 0);
	  danglingOp = '&';
	  break;
	case '&':
	case '|':
	  {
	    danglingOp = optc;
	    break;
	  }
	case 'd':
	  nameOfDatabase = optarg;
	  break;

	case 'f':
	  query_format_name = optarg;
	  formats++;
	  break;

	case 'g':
	  if (optarg[0] == '\0')
	    {
              fprintf(stderr, "%s: filename must be non-null\n", program_name);
              exit(1);
	    }
	  global_db_list_file = optarg;
	  break;

	case 'o':
	  if (strcmp (optarg, "-") && !restricted)
	    outfile_name = optarg;
	  break;

	case 'R':
	  restricted = 1;
	  outfile = stdout;
	  break;

	case 'D':
	  debug = 1;
	  break;

	case 'm':
	  {
	    addSearchEntry (&exprBuf, danglingOp, "fieldtype:MultiText", "~",
			    optarg, 0);
	    danglingOp = '&';
	    break;
	  }

	case 't':
	  {
	    addSearchEntry (&exprBuf, danglingOp, "fieldtype:Text", "~", 
			    optarg, 0);
	    danglingOp = '&';
	    break;
	  }

	case 'V':
	  version (PROGRAM_NAME);
	  break;

	case 'F':
	  query_format_name = "full";
	  formats++;
	  break;

	case 'i':
	  query_format_name = "sql";
	  formats++;
	  break;

	case 'I':
	  query_format_name = "sql2";
	  formats++;
	  break;

	case 'q':
	  query_format_name = "summary";
	  formats++;
	  break;

	case 'x':
	  skip_closed = 1;
	  break;

	case 'j':
	  whichList = ListCategories;
	  lists++;
	  break;

	case 'k':
	  whichList = ListResponsible;
	  lists++;
	  break;

	case 'l':
	  whichList = ListSubmitters;
	  lists++;
	  break;

	case 'T':
	  whichList = ListStates;
	  lists++;
	  break;

	case 'H':
	  hostname = optarg;
	  is_network_client = 1;
	  break;

	case PORT_OPTION:
	  port = atoi (optarg);
	  is_network_client = 1;
	  break;

	case 'v':
	  user = optarg;
	  is_network_client = 1;
	  break;

	case 'w':
	  passwd = optarg;
	  is_network_client = 1;
	  break;

	case LISTFIELDNAMES_OPTION:
	  whichList = ListFieldNames;
	  lists++;
	  break;

	case LISTINPUTFIELD_OPTION:
	  whichList = ListInitialInputFields;
	  lists++;
	  break;

	case VALIDVALUES_OPTION:
	  validValuesField = optarg;
	  break;

	case FIELDTYPE_OPTION:
	  fieldTypeField = optarg;
	  break;

	case FIELDDESC_OPTION:
	  fieldDescriptionField = optarg;
	  break;

	case LIST_DATABASES_OPTION:
	  whichList = ListDatabases;
	  lists++;
	  break;

	case PRINT_DIRECTORY_OPTION:
	  printDirectory = 1;
	  break;

	case PRINTSHVARS_OPTION:
	  printShVars = 1;
	  formats++;
	  lists++;
	  break;

	case PRINTSERVERADDR_OPTION:
	  printServerAddr = 1;
	  formats++;
	  lists++;
	  break;

	case ADMFIELD_OPTION:
	  admField = optarg;
	  formats++;
	  lists++;
	  break;

	case ADMSUBFIELD_OPTION:
	  admSubfield = optarg;
	  break;

	case ADMKEYVALUE_OPTION:
	  admKey = optarg;
	  break;

	case RESPONSIBLEADDRESS_OPTION:
	  responsibleAddress = optarg;
	  break;

	case FIELDFLAGS_OPTION:
	  fieldFlagsField = optarg;
	  break;

	case 'h':
	  usage (USAGE, 0);
	  break;

	default:
	  usage (USAGE, 1);
	}
    }

  if (outfile_name != NULL)    	  
    {
      outfile = fopen (optarg, "w+");
      if (outfile == (FILE *) NULL)
	{
	  fprintf (stderr, "%s: cannot open file %s for writing\n", 
		   program_name, optarg);
	  queryPrExit (3);
	}
      closeOutfile = 1;
    }

  if (formats > 1)
    {
      fprintf (stderr, "%s: only one output format may be specified\n",
	       program_name);
      queryPrExit (3);
    }

  if (lists > 1)
    {
      fprintf (stderr, "%s: only one list option may be specified\n",
	       program_name);
      queryPrExit (4);
    }

  if (! is_network_client)
    {
      is_network_client = gnatsdbHasNetconn (nameOfDatabase);
    }

  if (printServerAddr)
    {
      if (! is_network_client)
	{
	  fprintf (stderr, "%s: --print-server-addr only works with a network client\n",
		   program_name);
	  queryPrExit (1);
	}
      scanEnv (&user, &passwd, &hostname, &port, &nameOfDatabase);
      if (user == NULL || passwd == NULL || hostname == NULL
	  || port < 0 || nameOfDatabase == NULL)
	{
	  fprintf (stderr, "%s: Invalid remote database configuration\n",
		   program_name);
	  queryPrExit (1);
	}
      fprintf (outfile, "%s:%d:%s:%s:%s\n", hostname, port, nameOfDatabase,
	       user, passwd);
      queryPrExit (0);
    }

  if (is_network_client)
    {
      ErrorDesc err;

      if (client_init_gnats (&err, user, passwd, hostname, port,
			     nameOfDatabase) != 0)
	{
	  client_print_errors (database, err);
	  queryPrExit (1);
	}
    }
  else
    {
      ErrorDesc err;

      database = init_gnats (program_name, nameOfDatabase, &err);
      if (! databaseValid (database))
	{
	  client_print_errors (database, err);
	  queryPrExit (1);
	}
    }

  if (responsibleAddress != NULL)
    {
      if (is_network_client)
	{
	  fprintf (stderr, "%s: --responsible-address not supported in network mode\n",
		   program_name);
	  queryPrExit (1);
	}
      else
	{
	  char *addr = get_responsible_addr (database, 0, 0,
					     responsibleAddress);
	  fprintf (outfile, "%s\n", addr);
	  free (addr);
	}
      queryPrExit (0);
    }
  else if (admField != NULL || admKey != NULL || admSubfield != NULL)
    {
      if (admField == NULL || admKey == NULL)
	{
	  fprintf (stderr, "%s: Need to specify both --adm-field and --adm-key\n", program_name);
	  queryPrExit (1);
	}
      if (is_network_client)
	{
	  clientGetAdmField (outfile, admField, admSubfield, admKey);
	  queryPrExit (0);
	}
      else
	{
	  ComplexFieldIndex ci = newComplexFieldIndex (database, admField);
	  FieldIndex i = simpleFieldIndexValue (ci);
	  AdmEntry *ent;
	  if (i == InvalidFieldIndex)
	    {
	      fprintf (stderr, "Invalid field name %s\n", admField);
	      queryPrExit (1);
	    }
	  ent = get_adm_record (i, admKey);
	  if (ent == NULL)
	    {
	      fprintf (stderr, "%s: no entry matching %s in field %s\n",
		       program_name, admKey, admField);
	      queryPrExit (1);
	    }
	  else
	    {
	      printAdmSubfield (outfile, "\n", i, ent, admSubfield);
	      free_adm_entry (ent);
	      queryPrExit (0);
	    }
	}
    }

  if (printShVars)
    {
      if (is_network_client)
	{
	  fprintf (stderr,
		   "--print-sh-vars is not supported in network mode\n");
	  queryPrExit (1);
	}
      fprintf (outfile, "GNATSDB=\"%s\"\n", databaseName (database));
      fprintf (outfile, "GNATSDB_VALID=%d\n", databaseValid (database));
      if (databaseValid (database))
	{
	  char *defaultCategoryName = defaultCategory (database);
	  char *defaultStateName = defaultState (database);
	  
	  fprintf (outfile, "GNATSDBDIR=\"%s\"\n", databaseDir (database));
	  fprintf (outfile, "DEBUG_MODE=%d\n", debugMode (database));
	  fprintf (outfile, "DEFAULTCATEGORY=\"%s\"\n", defaultCategoryName);
	  fprintf (outfile, "DEFAULTSTATE=\"%s\"\n", defaultStateName);
	  free (defaultCategoryName);
	  free (defaultStateName);
	}
      queryPrExit (0);
    }

  if (printDirectory)
    {
      if (is_network_client)
	{
	  fprintf (stderr, "%s: --print-directory-for-database not supported in network mode\n", 
		   program_name);
	  queryPrExit (1);
	}
      else if (databaseValid (database))
	{
	  fprintf (outfile, "%s\n", databaseDir (database));
	  queryPrExit (0);
	}
      else
	{
	  fprintf (stderr, "%s: Invalid database name `%s'\n",
		   program_name, nameOfDatabase);
	  queryPrExit (1);
	}
    }

  if (fieldTypeField != NULL)
    {
      if (is_network_client)
	{
	  netFieldType (fieldTypeField);
	}
      else
	{
	  char *p;
	  while (fieldTypeField != NULL)
	    {
	      FieldIndex i;

	      p = strchr (fieldTypeField, ' ');
	      if (p != NULL)
		{
		  *(p++) = '\0';
		}
	      i = find_field_index (database, fieldTypeField);
	      if (i != InvalidFieldIndex)
		{
		  fprintf (outfile, "%s\n", fieldTypeAsString (i->datatype));
		}
	      else
		{
		  fprintf (stderr, "%s: No such field as %s\n", program_name,
			   fieldTypeField);
		  queryPrExit (1);
		}
	      fieldTypeField = p;
	    }
	}
      queryPrExit (0);
    }

  if (fieldFlagsField != NULL)
    {
      if (is_network_client)
	{
	  netFieldFlags (fieldFlagsField);
	}
      else
	{
	  char *p;
	  while (fieldFlagsField != NULL)
	    {
	      FieldIndex i;

	      p = strchr (fieldFlagsField, ' ');
	      if (p != NULL)
		{
		  *(p++) = '\0';
		}
	      i = find_field_index (database, fieldFlagsField);
	      if (i != InvalidFieldIndex)
		{
		  char *flags = getFieldFlags (i);
		  fprintf (outfile, "%s\n", flags);
		  free (flags);
		}
	      else
		{
		  fprintf (stderr, "%s: No such field as %s\n", program_name,
			   fieldFlagsField);
		  queryPrExit (1);
		}
	      fieldFlagsField = p;
	    }
	}
      queryPrExit (0);
    }

  if (validValuesField != NULL)
    {
      if (is_network_client)
	{
	  netValidValues (validValuesField);
	}
      else
	{
	  FieldIndex i = find_field_index (database, validValuesField);
	  if (i == InvalidFieldIndex)
	    {
	      fprintf (stderr, "%s: No such field as %s\n", program_name,
		       validValuesField);
	      queryPrExit (1);
	    }

	  printValidValues (outfile, i, "\n");
	}

      queryPrExit (0);
    }

  if (fieldDescriptionField != NULL)
    {
      if (is_network_client)
	{
	  netFieldDescription (fieldDescriptionField);
	}
      else
	{
	  FieldIndex i = find_field_index (database, fieldDescriptionField);
	  if (i == InvalidFieldIndex)
	    {
	      fprintf (stderr, "%s: No such field as %s\n", program_name,
		       fieldDescriptionField);
	      queryPrExit (1);
	    }
	  if (fieldDefForIndex (i)->description != NULL)
	    {
	      fprintf (outfile, "%s\n", fieldDefForIndex (i)->description);
	    }
	  else
	    {
	      fprintf (outfile, "\n");
	    }
	}
      queryPrExit (0);
    }

  if (skip_closed)
    {
      addSearchEntry (&exprBuf, '&',
		      "(! builtinfield:State[type] = \"closed\")", "",
		      NULL, 0);
    }

  if (restricted)
    {
      addSearchEntry (&exprBuf, '&', "builtinfield:Confidential", "^", "no",
		      1);
    }

  if (is_network_client)
    {
      if (lists > 0)
	{
	  sendRemoteListQuery (whichList, outfile);
	}
      else
	{
	  sendRemoteQuery (exprBuf, ((const char **)argv) + optind,
			   query_format_name, outfile);
	}

      if (exprBuf != NULL)
	{
	  free (exprBuf);
	}
      queryPrExit (0);
    }
  else
    {
      QueryFormat *query_format;
      QueryExpr expr = NULL;

      if (lists)
	{
	  getGnatsFile (database, whichList, NULL, "\n");
	  queryPrExit (0);
	}

      if (exprBuf != NULL && exprBuf[0] != '\0')
	{
	  expr
	    = parseQueryExpression (database, 
				    exprBuf, exprBuf + strlen (exprBuf) - 1);
	  if (expr == NULL)
	    {
	      fprintf (stderr, "query-pr: Cannot parse `%s'\n", exprBuf);
	      free (exprBuf);
	      exprBuf = NULL;
	      queryPrExit (1);
	    }
	}
      else
	{
	  const char *emptyExpr = "";
	  expr = parseQueryExpression (database, emptyExpr, emptyExpr);
	}

      {
	ErrorDesc err;

	query_format = findQueryFormat (database, query_format_name, &err);

	if (query_format == NULL)
	  {
	    client_print_errors (database, err);
	    queryPrExit (1);
	  }
      }

      {
	ErrorDesc err;

	int res = db_query (database, argc - optind, argv + optind, expr,
			   client_print_query, query_format, &err);
	if (res < 0)
	  {
	    client_print_errors (database, err);
	  }
	else if (res == 0)
	  {
	    /* ??? */
	    errors = 1;
	  }
      }

      if (errors > 0)
	{
	  fprintf (stderr, "%s: no PRs matched\n", program_name);
	}

      if (exprBuf != NULL)
	{
	  free (exprBuf);
	  exprBuf = NULL;
	}

      if (expr != NULL)
	{
	  freeQueryExpr (expr);
	  expr = NULL;
	}
      query_format = NULL;

      /* exit non-zero if there were any troubles.  */
      queryPrExit (errors != 0);
    }
  /* NOTREACHED */
  return 0;
}
