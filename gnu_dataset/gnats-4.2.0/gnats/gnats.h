/* Define all procedures which are external to each other.
   Copyright (C) 1993,95,99,2000,01,02,07 Free Software Foundation, Inc.
   Contributed by Tim Wicinski (wicinski@barn.com).
   Hacked by Brendan Kehoe (brendan@cygnus.com).
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

#ifndef _gnats_h_ 
#define _gnats_h_ 



/* System wide includes & definitions */
/* PLEASE!  For the saving of future maintainers, if you add any strange
   declaration here, *explain it*!  Otherwise we will have either to retain
   the whole increasing garbage forever, or to break portability. */

#include "config.h"

/* fileno() is not standard C, and requires the POSIX feature test macro to
   be defined on linux. Avoid the macro on FreeBSD, it breaks other stuff. */
#if defined (__linux__) && !defined (__FreeBSD__)
#define _POSIX_SOURCE 1
#endif
#include <stdio.h>

#include <time.h>

/* On FreeBSD this ends up defining RE_DUP_MAX.  We need to make sure it's
   included before our regex.h. */
#include <sys/param.h>
#include <limits.h>    /* for LONG_MAX */

/* Use some miscelaneous functions; should be included after stdio.h */
#include "ansidecl.h"

/* If system have getopt_long() it seems GNU compliant and likely use
   GNU-compatible getopt() as well. Otherwise, we will use getopt*()
   from libiberty. */
#ifdef HAVE_GETOPT
#define HAVE_DECL_GETOPT 1
#else
#define HAVE_DECL_GETOPT 0
#endif /* HAVE_GETOPT */
#ifdef HAVE_GETOPT_LONG
#include <getopt.h>
#else
#include "gnugetopt.h"
#endif /* HAVE_GETOPT_LONG */

#include <sys/types.h>
#include <sys/stat.h>
#ifndef S_ISREG			/* Doesn't have POSIX.1 stat stuff. */
#define mode_t unsigned short
#endif
#if !defined(S_ISREG) && defined(S_IFREG)
#define	S_ISREG(m) (((m) & S_IFMT) == S_IFREG)
#endif
#ifndef S_ISLNK
#define lstat stat
#endif

#ifdef HAVE_MEMORY_H
#include <memory.h>
#endif
#ifdef HAVE_UNISTD_H
#include <unistd.h>
#endif
#ifdef HAVE_SYSLOG_H
#include <syslog.h>
#endif
#if defined(HAVE_STRING_H) || defined(STDC_HEADERS)
#include <string.h>
#else
#include <strings.h>
#endif

#include <errno.h>
#ifdef STDC_HEADERS
#define getopt system_getopt
#include <stdlib.h>
#undef getopt
#else /* not STDC_HEADERS */
char *getenv ();
extern int errno;
#endif /* STDC_HEADERS */

#if defined(HAVE_FCNTL_H) || defined(_POSIX_VERSION)
#include <fcntl.h>
#else
#include <sys/file.h>
#endif
 
#if defined(HAVE_LIBGEN_H)
#  if defined(HAVE_BASENAME) || defined(HAVE_DIRNAME)
#include <libgen.h>
#  endif
#endif /* HAVE_LIBGEN_H */

#ifndef alloca
/* Make alloca work the best possible way.  */
#ifdef __GNUC__
#define alloca __builtin_alloca
#else /* not __GNUC__ */
#if HAVE_ALLOCA_H
#include <alloca.h>
#else /* not __GNUC__ or HAVE_ALLOCA_H */
#ifndef _AIX /* Already did AIX, up at the top.  */
char *alloca ();
#endif /* not _AIX */
#endif /* not HAVE_ALLOCA_H */ 
#endif /* not __GNUC__ */
#endif /* not alloca */

#if defined(__STDC__) || defined(_AIX)
#include <stdarg.h>
#else
#include <varargs.h>
#endif

#include <ctype.h>
#include <signal.h>
#include <pwd.h>

#if __GNUC__ < 2 || (__GNUC__ == 2 && __GNUC_MINOR__ < 7)
# define __attribute__(x)
#endif

#ifndef ATTRIBUTE_UNUSED
# define ATTRIBUTE_UNUSED __attribute__ ((__unused__))
#endif

extern int asprintf (char **buf, const char *fmt, ...);
extern int vasprintf (char **buf, const char *fmt, va_list args);

#if ! HAVE_DECL_UNSETENV
extern void unsetenv (const char *name);
#endif

#if defined (__svr4__) && defined (__sun__)
extern int gethostname (char *name, int namelen);
#endif



/* Types */

/* A generic boolean type.  */
typedef short bool;
#ifndef TRUE
#define TRUE 1
#endif
#ifndef FALSE
#define FALSE 0
#endif

#ifndef SEEK_SET
#define SEEK_SET 0
#define SEEK_CUR 1
#define SEEK_END 2
#endif

#ifndef RETSIGTYPE
#define RETSIGTYPE void
#endif

/* A cons whose car is a string. */
typedef struct string_list
{
  char *name;
  struct string_list *next;
} StringList;

typedef struct error_desc *ErrorDesc;
typedef struct bad_fields *BadFields;

/* Possible values for the logging mode.  Note "syslog" will only be used
   if the system supports it---otherwise it will relegate to "mail".
   TODO: Fix this comment. */
typedef enum
{
  SYSLOG,
  MAIL,
  LOGFILE,
  STDERR,
  NOLOG
} Logging_Methods;

/* XXX ??? !!! What is this stuff for?  */
#ifndef LOGGING 
# ifdef HAVE_SYSLOG_H
#  define LOGGING SYSLOG
#  ifndef LOG_LEVEL
#   ifdef LOG_USER
#    define LOG_LEVEL LOG_USER
extern int log_level;
#   endif
#  endif
# else /* !HAVE_SYSLOG_H */
/* FIXME */
#  define LOGGING MAIL
#  define LOG_ERR 0
#  define LOG_INFO 1
# endif /* HAVE_SYSLOG_H */
#endif /* LOGGING */
#define LOG_NONE -1



/* Constants */

/* This used to be 30, but we upped it to 50 to make sure that int'l
   timezones will fit. */
/* FIXME: This introduces an arbitrary limit on a buffer size.  It should be
   removed. */
#define GNATS_TIME_LENGTH 50

/* Exit codes. */
extern const int EXIT_OK;
extern const int EXIT_PROGRAM_ERROR;

/* Files that controls access to the database. */
#define DB_ACCESS_FILE "gnatsd.user_access"
#define DB_HOST_ACCESS_FILE "gnatsd.host_access"

/* Default values of field properties. */
#define DEFAULT_MULTIENUM_SEPARATOR ": "



/* Global variables */

/* Current version of GNATS. */
extern char *version_string;

/* The name this program was run with. */
extern const char *program_name;

/* location of global databases list file */
const char *global_db_list_file;

/* default location of global databases list file */
#ifndef GLOBAL_DB_LIST_FILE
#define GLOBAL_DB_LIST_FILE "/etc/databases"
#endif


/* Local includes. */

#include "field.h"
#include "database.h"
#include "pr.h"
#include "query.h"
#include "regex.h"
#include "adm.h"



/* Functions */

/* in btime.c */
extern struct tm *get_response_time (const DatabaseInfo database, 
				     unsigned int responseTimeInDays);

/* in client.c */
extern int get_reply (FILE *);
extern void client_exit (void);
extern void safe_exit (void);
extern void client_chdb (const char *database);
extern int client_lock_gnats (const DatabaseInfo database, ErrorDesc *err);
extern void client_unlock_gnats (void);

extern void sendRemoteQuery (const char *queryExpr,
			     const char **args,
			     const char *queryFormatName, FILE *outfile);

extern void sendRemoteListQuery (ListTypes whichList, FILE *outfile);

extern void scanEnv (char **user, char **passwd,
		     char **hostname, int *port, char **database);

extern int client_init_gnats (ErrorDesc *err,
			      const char *user, const char *password,
			      const char *hostname, int port, 
			      const char *database);

extern void client_print_errors (const DatabaseInfo database,
				 ErrorDesc errDesc);

extern void tohex (char *ptr, char *buf, int sz);
extern int hexdigit (char c);
extern void fromhex (char *ptr, char *buf, int sz);

extern void clientGetAdmField (FILE *outfile, const char *admField,
			       const char *admSubfield, const char *admKey);

extern PR *clientReadPR (const char *prNum);
/* Returns a list of PRs that match EXPR.  */
extern StringList *clientGetPRList (const char *expr);

extern void netCheckPR (FILE *file, int initialPR);

extern void netEditField (FILE *fieldData, const char *prNum, 
			  const char *fieldName,
			  const char *editUserEmailAddr, int appendToField,
			  char *reason);

extern void netSubmitNewPR (FILE *file, int show_prnum);
extern void netModifyPR (FILE *file, const char *prNum,
			 const char *editUserEmailAddr);
extern void netLockDB (void);
extern void netUnlockDB (void);
extern void netLockPR (const char *prNum, const char *username, 
		       const char *processID);
extern void netUnlockPR (const char *prNum);
extern void netDeletePR (const char *prNum, const char *editUserEmailAddr);
extern void netSetEditEmailAddr (const char *address);
extern void netFieldFlags (const char *fieldname);
extern void netValidValues (const char *fieldname);
extern void netFieldDescription (const char *fieldname);
extern void netFieldType (const char *fieldname);

/* in edit.c */
extern int check_pr_file (const DatabaseInfo database, FILE *infile,
			  ErrorDesc *err, int initial_entry);
extern int check_pr (PR *pr,
		     ErrorDesc *err,
		     int initial_entry);
extern int replace_pr (const DatabaseInfo database, FILE *infile, 
		       const char *editUserEmailAddr, ErrorDesc *err);
extern int lock_pr (const DatabaseInfo database,
		    const char *filename, const char *username,
		    const char *processid, ErrorDesc *error);
extern int unlock_pr (const DatabaseInfo database,
		      const char *filename, ErrorDesc *error);
extern int validateFieldValue (FieldIndex field, const char *text,
			       ErrorDesc *err,
			       int dontCheckEnums);
extern int edit_field (const DatabaseInfo database, const char *prnum,
		       FieldIndex fieldIndex, int append, char *newcontents,
		       char *changeReason, const char *editUserEmailAddr,
		       ErrorDesc *err);
extern int applyFieldEdit (PR *pr, FieldEdit *edit, ErrorDesc *err,
			   FormatNamedParameter *params);
extern int deletePR (const DatabaseInfo database, const char *prNum, 
		     const char *editUserEmailAddr, ErrorDesc *err);

/* in fconfigl.l and fconfig.y */
extern void fconferror (const char *string);
extern int fconflex (void);
extern int yylex (void);

/* in file-pr.c */
extern int submit_pr (const DatabaseInfo database, FILE *, ErrorDesc *err);

/* in getdate.c */
extern time_t get_date (char *, ...);

/* in internal.c */
extern void block_signals (void);
extern void unblock_signals (void);
extern int is_gnats_locked (const DatabaseInfo database);
extern int lock_gnats (const DatabaseInfo database, ErrorDesc *);
extern int unlock_gnats (const DatabaseInfo database, ErrorDesc *);
extern void punt (const DatabaseInfo database, int, ...);
extern int copy_file (const char *, const char *);
extern const char *get_prid_from_path (const char*);
extern const char *get_cat_prid_from_path (const char*);
extern char *get_lock_path (const DatabaseInfo database, const char *prID);
extern char *get_curr_date (void);
extern int gnatsdbHasNetconn (const char *database_name);
extern int isPrLocked (const DatabaseInfo database, const char *prNum);
extern int fileExists (const char *filename);
extern void setError (ErrorDesc *errorDesc, int errorCode, ...);
extern BadFields newBadFieldEntry (FieldIndex field, const char *badValue,
				  BadFields next);
extern void freeBadFieldList (BadFields list);
extern void freeErrorDesc (ErrorDesc desc);
extern int getErrorCode (ErrorDesc errorDesc);
extern const char *getErrorMessage (ErrorDesc errorDesc);
extern BadFields getBadFieldList (ErrorDesc errorDesc);
extern const char *badFieldValue (BadFields ent);
extern FieldIndex badFieldIndex (BadFields ent);
extern BadFields nextBadField (BadFields ent);
extern int check_state_type (const DatabaseInfo database,
			     const char *state, const char *type);

/* in lists.c */
extern ListTypes stringToListType (const char *string);
extern const char *listTypeToString (ListTypes which);
extern int getGnatsFile (const DatabaseInfo database, ListTypes which, 
			 const char *file, const char *eolTerminator);

/* in misc.c */
extern DatabaseInfo init_gnats (const char *program_name,
				const char *database_name,
				ErrorDesc *err);
extern void log_msg (int, int, ...);
extern void init_logging (const char *filename);
extern void enable_debugging (void);
extern char *read_line (FILE *input_des, size_t *max_len);
extern char *get_next_field (const char **line, int delim);
extern char *get_token (char *, char *);
extern FILE *open_mail_file (const DatabaseInfo database);
extern void close_mail_file (FILE *);
extern char *quote_string (const char *string);
extern StringList *new_string_list_ent (char *string, StringList *next);
extern PTR xmalloc (size_t size);
extern PTR xrealloc (PTR ptr, size_t size);
extern char *xstrdup (const char *string);
extern char *xstrndup (const char *string, size_t length);
extern const char *temporary_directory (void);
extern int open_temporary_file (char *template, int mode);
extern FILE *fopen_temporary_file (char *template, const char *fopen_mode,
				   const int mode);
extern size_t gnats_strftime (char *s, size_t size, const char *template,
			      const struct tm *brokentime);
extern void usage (const char *const texts[], int exit_code);
extern void version (const char *const program_name);
extern bool value_is_empty (const char *string);
#ifndef HAVE_ASPRINTF
extern int asprintf (char **ret, const char *format, ...) ATTRIBUTE_PRINTF_2;
#endif
#ifndef HAVE_VASPRINTF
extern int vasprintf (char **ret, const char *format, va_list) ATTRIBUTE_PRINTF(2,0);
#endif
#ifndef HAVE_BASENAME
extern char *basename (char *path);
#endif

#endif /* !_gnats_h_ */
