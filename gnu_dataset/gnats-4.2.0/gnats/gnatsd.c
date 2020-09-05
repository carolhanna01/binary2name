/* Remote GNATS server.
   Copyright (C) 1994,95,96,97,99,2000,01,02,07 Free Software Foundation, Inc.
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
#include "gnatsd.h"
#include "query.h"

#ifdef HAVE_LIBCRYPT
#ifdef HAVE_CRYPT_H		/* some systems declare `crypt' in unistd.h */
#include <crypt.h>
#endif
#endif

#if defined (__svr4__) && defined (__sun__)
#undef SIG_IGN
#define SIG_IGN (void (*)(int))1
#endif

/* ??? */
static char myname[MAXHOSTNAMELEN];

/* The base name of the program binary. */
const char *program_name;

/* The name and IP address of the host talking with us and their access
   level. */
const char *current_host;
const char *current_addr;
Access_Level user_access;

/* Whether or not they need to execute CHDB; reset to 0 when they do */
int require_db    = 0;

/* Maximum available access level.  */
static Access_Level max_access_level = ACCESS_ADMIN;

struct option long_options[] =
{
  {"database", 1, NULL, 'd'},
  {"global-databases-file", 1, NULL, 'g'},
  {"not-inetd", 0, NULL, 'n'},
  {"maximum-access-level", 1, NULL, 'm'},
  {"version", 0, NULL, 'V'},
  {"help", 0, NULL, 'h'},
  {NULL, 0, NULL, 0}
};

#define PROGRAM_NAME "gnatsd"

static const char *const USAGE[] = {
  "Usage: " PROGRAM_NAME " [OPTION]...\n\
Communication interface to GNATS databases.\n\
\n",
  "\
  -d --database=DATABASE            use DATABASE\n\
  -g --global-databases-file=PATH   PATH to databases file\n\
  -n --not-inetd                    no need to authorize when using Kerberos\n\
  -m --maximum-access-level=LEVEL   do not allow exceeding access LEVEL\n\
\n\
  -h --help                         display this help and exit\n\
  -V --version                      output version information and exit\n",
  NULL
};

#ifdef HAVE_KERBEROS
/* Non-zero if the client has used Kerberos authentication.  */
int client_authorized;
#endif

/* Non-zero if the host has to use Kerberos.  */
int kerberos_host;

/* gnatsd command definition */
typedef struct _command
{
  const char *s;		/* command string */
  void (*fn)(int ac, char **av); /* the function to be invoked */
  Access_Level access;		/* the minimum required access level */
  bool onearg;			/* ??? */
} Command;
static Command cmds[] =
{
  /* Various unrestricted commands */
  { "QUIT", GNATS_quit, ACCESS_NONE, FALSE },
  { "AUTH", GNATS_auth, ACCESS_NONE, FALSE },
  { "USER", GNATS_user, ACCESS_NONE, FALSE },
  { "HELP", GNATS_help, ACCESS_NONE, FALSE },
  /* Switch the database being viewed.  */
  { "CHDB", GNATS_chdb, ACCESS_NONE, FALSE },
  /* Check that the supplied PR text is valid.  */
  { "CHEK", GNATS_chek, ACCESS_VIEW, FALSE },
  /* Submit a new PR. */
  { "SUBM", GNATS_subm, ACCESS_SUBMIT, FALSE },
  /* Append text to a field in the specified PR. */
  { "APPN", GNATS_appn, ACCESS_EDIT, FALSE },
  /* Replace the contents of a field in the specified PR. */
  { "REPL", GNATS_repl, ACCESS_EDIT, FALSE },
  /* Return the list of available databases. */
  { "DBLS", GNATS_dbls, ACCESS_LISTDB, FALSE},
  /* Return the description of the specified database. */
  { "DBDESC", GNATS_dbdesc, ACCESS_VIEW, TRUE },
  /* Validate that the specified data is valid for the field. */
  { "VFLD", GNATS_vfld, ACCESS_VIEW, TRUE },
  /* Set query constraints (no need to restrict) */
  { "RSET", GNATS_rset, ACCESS_VIEW, FALSE },
  /* The format used when returning query results. */
  { "QFMT", GNATS_qfmt, ACCESS_VIEW, TRUE },
  /* Specify a query expression to use.  */
  { "EXPR", GNATS_expr, ACCESS_VIEW, TRUE },
  /* Query a PR. */
  { "QUER", GNATS_quer, ACCESS_VIEW, FALSE },
  /* List GNATS internals */
  { "LIST", GNATS_list, ACCESS_VIEW, FALSE },
  /* Lock the specified PR, using the specified username and (optional) PID */
  { "LOCK", GNATS_lock, ACCESS_EDIT, FALSE },
  /* Set the email address of the user editing PRs.  */
  { "EDITADDR", GNATS_editaddr, ACCESS_EDIT, TRUE },
  /* Edit a PR; the PR contents are sent and used to replace an existing
     PR. */
  { "EDIT", GNATS_edit, ACCESS_EDIT, FALSE },
  /* Unlock the specified PR. */
  { "UNLK", GNATS_unlk, ACCESS_EDIT, FALSE },
  /* Stop and start the database.  */
  { "LKDB", GNATS_lkdb, ACCESS_EDIT, FALSE },
  { "UNDB", GNATS_undb, ACCESS_EDIT, FALSE },
  /* Return the type of data stored in the field. */
  { "FTYP", GNATS_ftyp, ACCESS_VIEW, FALSE },
  /* Return the field property value specified by the arguments. */
  { "FTYPINFO", GNATS_ftypinfo, ACCESS_VIEW, FALSE},
  /* Return a human-readable description of the field. */
  { "FDSC", GNATS_fdsc, ACCESS_VIEW, FALSE },
  /* Return either a regexp that describes the valid values for the
     specified field, or a list of valid values (for an enum field). */
  { "FVLD", GNATS_fvld, ACCESS_VIEW, TRUE },
  /* List the flags set on this the specified field(s).  */
  { "FIELDFLAGS", GNATS_fieldflags, ACCESS_VIEW, FALSE },
  /* List the default values suggested when an initial PR is input. */
  { "INPUTDEFAULT", GNATS_inputdefault, ACCESS_VIEW, FALSE },
  /* Deletes the specified PR from the database.  */
  { "DELETE", GNATS_delete, ACCESS_ADMIN, FALSE },
  /* Returns an entry from the adm data associated with the specified field
     that matches the specified key.  If a subfield is also specified, only
     the data in that subfield is returned. */
  { "ADMV", GNATS_admv, ACCESS_VIEW, FALSE },
  
  { NULL, NULL, -1, FALSE },
};


/* Return true iff `line' matches the shell `pattern'.
   Iff `matchcase' is false, the matching is case insensitive.*/
static bool
match (const char *line, const char *pattern, bool matchcase)
{
  /* If p is a null string, this is a match (downward compatibility) */
  if (*pattern == '\0')
    {
      return TRUE;
    }
  
  while (*pattern != '\0')
    {
      if (*line == '\0' && *pattern != '*')
	{
	  return FALSE;
	}
      switch (*pattern)
	{
	case '\\':
	  /* Literal match with following character. */
	  pattern++;
	  /* FALLTHROUGH */
	default:
	  if (matchcase)
	    {
	      if (*line != *pattern)
		{
		  return FALSE;
		}
	    }
	  else
	    {
	      if (tolower ((int)(*line)) != tolower ((int)(*pattern)))
		{
		  return FALSE;
		}
	    }
	  break;
	case '?':
	  /* Match anything. */
	  break;
	case '*':
	  {
	    while (*pattern == '*')
	      {
		/* Consecutive stars act just like one. */
		pattern++;
	      }
	    if (*pattern == '\0')
	      {
		/* Trailing star matches everything. */
		return TRUE;
	      }
	    /* This is very inefficient.  */
	    while (*line != '\0')
	      {
		int matched = match (line, pattern, matchcase);

		if (matched)
		  {
		    return matched;
		  }
		line++;
	      }
	    /* Didn't find a match for the trailing pattern, so it failed.  */
	    return FALSE;
	  }
	  break;
	}
      line++;
      pattern++;
    }

  /* We've run out of chars in pattern. If there are more line chars, there is 
     no match. */
  if (*line != '\0')
    {
      return FALSE;
    }
  else
    {
      return TRUE;
    }
}

/* Return true iff `password' matches `hash'.
   `hash' is a possibly encrypted password, according to the $?$ convention. */
static int
password_match (const char *password, const char *hash)
{
  if (strlen(password) && strlen(hash))
    {
      if (! strncmp (hash, "$0$", 3))
	{
	  /* explicit plain-text password */
	  return match (password, hash+3, TRUE);
	}
      else
	{
#ifdef HAVE_LIBCRYPT
	  /* DES crypt or MD5 hash of the password */
	  char *encrypted = crypt (password, hash);
	  return encrypted && ! strcmp (encrypted, hash);
#else
	  /* TODO: log some warning */
	  return FALSE;
#endif
	}
    }
  else
    {
      if (strlen(password))
	{
	  return FALSE;
	}
      else
	{
	  return ! strlen(hash) ;
	}
    }
}

/*  */
static char *
get_name (struct in_addr *host, int type)
{
  char *buf;
  int i;
  struct hostent *hp;
#ifdef h_addr
  char **pp;
#endif

  hp = gethostbyaddr ((char *) host, sizeof (*host), type);
  if (hp == NULL)
    {
      return NULL;
    }
  i = strlen (hp->h_name);
  buf = (char *) xmalloc (i + 1);
  strcpy (buf, hp->h_name);
  hp = gethostbyname (buf);
  if (hp == NULL)
    return NULL;

  /* XXX ??? !!! This is for def sure not the right way to do this; it should
     be done with an autoconf test instead. */
#ifdef h_addr
  for (pp = hp->h_addr_list; *pp; pp++)
    {
      if (memcmp ((char *) &host->s_addr, (char *) *pp, hp->h_length) == 0)
	{
	  break;
	}
    }
  if (*pp == NULL)
    {
      return NULL;
    }
#else
  if (memcmp ((char *) &host->s_addr, (char *) hp->h_addr, hp->h_length) != 0)
    {
      return NULL;
    }
#endif

  return buf;
}


/* Assignment of symbolic values to strings of access levels. */
typedef struct _Access_String
{
  const Access_Level access;
  const char *string;
} Access_String;
static Access_String ACCESS_STRINGS[] =
  {
    {ACCESS_UNKNOWN,  "unknown"},
    {ACCESS_DENY,     "deny"},
    {ACCESS_NONE,     "none"},
    {ACCESS_LISTDB,   "listdb"},
    {ACCESS_SUBMIT,   "submit"},
    {ACCESS_VIEW,     "view"},
    {ACCESS_VIEWCONF, "viewconf"},
    {ACCESS_EDIT,     "edit"},
    {ACCESS_ADMIN,    "admin"},
    {NO_LEVEL, "none"}
  };

/* Return the numeric code of string representation of the access level
   `key'. */
static Access_Level
access_level (char *key)
{
  unsigned int i;
  for (i = 0; ACCESS_STRINGS[i].access != NO_LEVEL; i++)
    {
      if (strcasecmp (key, ACCESS_STRINGS[i].string) == 0)
	return ACCESS_STRINGS[i].access;
    }
  return ACCESS_NONE;
}

/* Return the string representation of the access level `access'. */
const char *
access_level_str (Access_Level access)
{
  int i;
  for (i = 0; ACCESS_STRINGS[i].access != NO_LEVEL; i++)
    {
      if (access == ACCESS_STRINGS[i].access)
	break;
    }
  return ACCESS_STRINGS[i].string;
}

/* Return a non-zero value if the host was found; ACCESS will contain
   the access level.  */
static int
validateHost (AdmEntry *hostList,
	      const char *host, const char *ipaddr, Access_Level *access)
{
  int found = 0;

  /* The format of the file is:
        sitename:access:
     where `sitename' is the hostname allowed; `access' is the access
     level, and the third field is undefined for now, but may be the
     field to control what PRs, categories, or submitter-id PRs are
     allowed, or whatever. */

  while (hostList != NULL)
    {
      /* check hostname and IP */
      if (match (host, hostList->admFields[HostListKey], FALSE)
	  || (ipaddr != NULL 
	      && match (ipaddr, hostList->admFields[HostListKey], FALSE)))
	{
	  found = 1;
	  *access = access_level (hostList->admFields[HostListAccessLevel]);
	  break;
	}
      hostList = hostList->next;
    }

#ifdef HAVE_KERBEROS
  /* ??? */
  if (! found) 
    {
      *access = ACCESS_EDIT;
      found = kerberos_host = 1;
    }
#endif

  return found;
}
/* Returns a non-zero value if an entry was found for the user; the access
   level will be contained in ACCESS.  */
static int
findUserAccessLevel (const char *file, const char *user, const char *passwd,
		     Access_Level *access, const DatabaseInfo database)
{
  FILE *acc;
  int found = 0;
  const char *dName;

  /* The format of the file is:
     userid:passwd:access:database */

  acc = fopen (file, "r");
  if (acc == NULL)
    {
      return 0;
    }

  if (databaseValid (database))
    {
      dName = databaseName (database);
    }
  else
    {
      dName = "";
    }

  while (! found)
    {
      char *buffer = read_line (acc, NULL);
      if (buffer != NULL)
	{
	  AdmEntry *ent = build_adm_entry (buffer, InvalidFieldIndex);

	  free (buffer);
	  /* Is there a mistake on the setup of this line? (must have 3 or
	     4 fields) */
	  if ((ent->fieldcount == 3 || ent->fieldcount == 4)
	      && match (user, ent->admFields[0], TRUE))
	    {
	      if (! password_match (passwd, ent->admFields[1]))
		{
		  /* Username matched but password didn't.  */
		  if (strlen(ent->admFields[1]) && strlen(passwd))
		    {
		      *access = ACCESS_NONE;
		      found = 1;
		    }
		}
	      else
		{
		  if (ent->fieldcount == 4)
		    {
		      /* Compare all given names against the name of the
			 requested database. */
		      const char *l2 = ent->admFields[3];
		      
		      if (l2 == NULL || ! strlen(l2))
			found = 1;
		      
		      while (l2 != NULL && ! found)
			{
			  char *token = get_next_field (&l2, ',');
			  if (match (dName, token, TRUE))
			    {
			      found = 1;
			    }
			  free (token);
			}
		    }
		  else
		    {
		      found = 1;
		    }
		  if (found)
		    {
		      *access = access_level (ent->admFields[2]);
		    }
		}
	    }
	}
      else
	{
	  /* Ran out of entries.  */
	  break;
	}
    }

  fclose (acc);

  if (found && *access > max_access_level)
    {
      *access = max_access_level;
    }

  return found;
}

/* Get the access level for this user. */
static int
get_user_access (const DatabaseInfo database,
		 const char *user, const char *passwd, Access_Level *access)
{
  int found = 0;

  if (databaseValid (database))
    {
      char *path = gnats_adm_dir (database, DB_ACCESS_FILE);
      if (path != NULL)
	{
	  found = findUserAccessLevel (path, user, passwd, access, database);
	  free (path);
	}
    }
  if (! found)
    {
      found = findUserAccessLevel (GNATSD_USER_ACCESS_FILE, user, passwd,
				   access, database);
    }

  return found;
}


/* See if we know about this host.  If we don't, then run with it.  */
int
verifyHostAndUser (const DatabaseInfo database,
		   const char *host, const char *ipaddr,
		   const char *username, const char *passwd,
		   Access_Level *access)
{
  int hostFound = 0;
  int userFound = 0;
  Access_Level hostAccess = ACCESS_NONE;
  Access_Level userAccess = ACCESS_NONE;

  if (databaseValid (database))
    {
      AdmEntry *hostList = getHostList (database);
      hostFound = validateHost (hostList, host, ipaddr, &hostAccess);
    }
  if (! hostFound)
    {
      AdmEntry *hostList = getGlobalHostList ();
      hostFound = validateHost (hostList, host, ipaddr, &hostAccess);
    }

  if (! hostFound)
    {
      *access = ACCESS_NONE;
    }
  else
    {
      userFound = get_user_access (database, username, passwd, &userAccess);
      if (userFound)
	{
	  *access = (userAccess >= hostAccess ? userAccess : hostAccess);
	}
      else
	{
	  *access = hostAccess;
	}
    }

  return hostFound;
}

static void
startConnection (void)
{
  struct sockaddr_in s;
#if HAVE_SOCKLEN_T
  socklen_t len;
#else
  int len;
#endif

  len = sizeof (s);
  if (getpeername (0, (struct sockaddr *)&s, &len) < 0)
    {
      if (! isatty (0))
	{
	  syslog (LOG_ERR, "cannot get peername %m");
	  printf ("%d Why can't I figure out your name?  Later.\r\n",
		  CODE_NO_ACCESS);
	  exit (1);
	}
      else
	{
	  current_host = "stdin";
	  current_addr = current_host;
	}
    }
  else
    {
      if ((s.sin_family != AF_INET)
#if HAVE_DECL_AF_INET6 || defined(AF_INET6)
	  /* AF_INET6 could be an enum or defined, if present */
	  && (s.sin_family != AF_INET6)
#endif
	 )
	{
	  syslog (LOG_ERR, "%s: bad address family %ld",
		  "?", (long) s.sin_family);
	  printf ("%d You're a member of a bad address family.  Later.\r\n",
		  CODE_NO_ACCESS);
	  exit (1);
	}
      else
	{
	  current_addr = (char *) inet_ntoa (s.sin_addr);
	  current_host = get_name (&s.sin_addr, s.sin_family);
	  if (current_host == NULL)
	    {
	      current_host = current_addr;
	    }
	}
    }
}

MsgType
get_line (char **dest, int timeout)
{
  static int count = 0;
  static char buffer[BUFSIZ]; /* The size doesn't matter here */
  static char *bp = NULL;
  struct timeval t;
  fd_set rmask;
  int done = 0;

  int errorCode = PR_ok;

  size_t resultLen = 0;
  char *result = NULL;

  while ((! done) && errorCode == PR_ok)
    {
      if (count == 0)
	{
	  int i;

	  /* Fill the buffer. */
	  FD_ZERO(&rmask);
	  FD_SET(0, &rmask);
	  t.tv_sec = timeout;
	  t.tv_usec = 0;
	  i = select (0 + 1, &rmask, (fd_set *)NULL, (fd_set *)NULL, &t);
	  if (i < 0) 
	    {
	      if (errno == EINTR)
		{
		  continue;
		}
	      else
		{
		  errorCode = PR_timeout;
		}
	    }
	  else if (i == 0 || !FD_ISSET(0, &rmask))
	    {
	      errorCode = PR_timeout;
	    }
	  else
	    {
	      count = read (0, buffer, sizeof buffer);
	      if (count < 0) 
		{
		  /* "This isn't the correct error code, but we'll pass that."
		     -- paraphrased Philip Marlowe */
		  errorCode = PR_timeout;
		}
	      else if (count == 0)
		{
		  errorCode = PR_eof;
		}
	      else
		{
		  bp = buffer;
		}
	    }
	}

      if (errorCode == PR_ok)
	{
	  char *eol = memchr (bp, '\n', count);
	  size_t amtRead;
	  size_t amtToCopy;

	  if (eol != NULL)
	    {
	      /* We don't want to copy the '\n' out.  */
	      amtToCopy = eol - bp;
	      /* Gobble the '\n'.  */
	      amtRead = amtToCopy + 1;
	    }
	  else
	    {
	      amtRead = count;
	      amtToCopy = amtRead;
	    }
	  result = xrealloc (result, resultLen + amtToCopy + 1);
	  memcpy (result + resultLen, bp, amtToCopy);
	  resultLen += amtToCopy;
	  count -= amtRead;
	  bp += amtRead;
	  if (eol != NULL)
	    {
	      done = 1;
	    }
	}
    }

  if (errorCode == PR_ok)
    {
      /* If last two characters are \r\n, kill the \r as well as the \n. */
      if (resultLen > 0 && result[resultLen - 1] == '\r')
	{
	  resultLen--;
	}
      result[resultLen] = '\0';
    }
  else
    {
      if (result != NULL)
	{
	  free (result);
	  result = NULL;
	}
      /* Whatever's in the buffer is garbage. */
      bp = buffer;
      count = 0;
    }
  *dest = result;
  return errorCode;
}

static int
gnatsd_argify(char *line, char ***argvp, Command **cp)
{
  char	**argv;
  char	*lineCopy;
  char *argStart;
  int argc = 1;

  /* Copy the line, which we will split up.  (Tho why we're counting
     \n and ' ' as separators but not other space characters is a mystery,
     especially as \n should never appear here as a separator.)  */
  while (*line == ' ' || *line == '\n')
    {
      line++;
    }

  lineCopy = xstrdup (line);

  argv = (char **) xmalloc (sizeof (char **) * 2);
  argv[0] = lineCopy;
  argStart = strpbrk(lineCopy, " \n");
  if (argStart != NULL)
    {
      *(argStart++) = '\0';
      while (*argStart == ' ' || *argStart == '\n')
	{
	  argStart++;
	}
    }
  for (*cp = cmds; (*cp)->s != NULL; (*cp)++) 
    {
      if (strcasecmp ((*cp)->s, argv[0]) == 0)
	{
	  break;
	}
    }

  if ((*cp)->s == NULL)
    {
      *cp = NULL;
    }

  if (*cp != NULL && (*cp)->onearg)
    {
      if (argStart != NULL)
	{
	  argv = (char **) xrealloc (argv, sizeof (char **) * (argc + 2));
	  argv[argc++] = argStart;
	}
    }
  else
    {
      while (argStart != NULL && *argStart != '\0')
	{
	  char *nextWord = strpbrk (argStart, " \n");

	  argv = (char **) xrealloc (argv, sizeof (char **) * (argc + 2));
	  argv[argc++] = argStart;

	  if (nextWord != NULL)
	    {
	      *(nextWord++) = '\0';
	      while (*nextWord == ' ' || *nextWord == '\n')
		{
		  nextWord++;
		}
	    }
	  argStart = nextWord;
	}
    }
  argv[argc] = NULL;
  *argvp = argv;
  return argc;
}

static void
freeArgs (int ac ATTRIBUTE_UNUSED, char **av)
{
  if (av != NULL) 
    {
      if (av[0] != NULL)
	{
	  free (av[0]);
	}
      free (av);
    }
}

static void
serverMainLoop (void)
{
  char **av;
  int ac = 0;
  Command *cp;
  MsgType r;
  int done = 0;

  while (! done)
    {
      char *buff;

      fflush (stdout);
      r = get_line (&buff, 2 * 60 * 60);
      av = NULL;
      cp = NULL;
      ac = 0;
      if (r == PR_ok)
	{
	  if (buff[0] == '\0')
	    {
	      printf ("%d Unrecognized command.\r\n", CODE_ERROR);
	    }
	  else
	    {
	      ac = gnatsd_argify (buff, &av, &cp);
	    }
	}
      else
	{
	  if (r == PR_timeout)
	    {
	      printf ("%d Read timed out.\r\n", CODE_TIMEOUT);
	    }
	  done = 1;
	}

      if (buff != NULL)
	{
	  free (buff);
	  buff = NULL;
	}

      /* Ignore blank/invalid lines.  */
      if (av != NULL)
	{
	  if (strcasecmp (av[0], "quit") == 0)
	    {
	      freeArgs (ac, av);
	      av = NULL;
	      done = 1;
	    }
	  else if (cp != NULL)
	    {
	      /* Check access level */
	      Access_Level access_required = cp->access;

#ifdef HAVE_KERBEROS
	      /* Did they send AUTH? */
	      if (kerberos_host && (access_required > user_access)
		  && !client_authorized)
		{
		  syslog (LOG_ERR, 
			  "GNATS command `%s' without authentication for %s",
			  cp->s, current_host);
		}
#endif

	      /* require_db means the first command must be CHDB and it must
		 succeed.  */
	      if (require_db && (strcasecmp (av[0], "chdb") != 0))
		{
		  printf ("%d CHDB command required (database invalid).\r\n", 
			  CODE_NO_ACCESS);
		}
	      else if (access_required > user_access)
		{
		  printf ("%d You are not authorized to perform this operation (%s).\r\n",
			  CODE_NO_ACCESS, cp->s);
		}
	      else
		{
		  /* Call the function.  We skip the command itself in the arg 
		     list.  */
		  (*cp->fn) (ac - 1, av + 1);

		  if (require_db)
		    {
		      printf ("%d Required CHDB command failed.\r\n",
			      CODE_NO_ACCESS);
		    }
		}
	    }
	  else
	    {
	      printf ("%d Unrecognized command.\r\n", CODE_ERROR);
	    }
	  freeArgs (ac, av);
	  av = NULL;
	}
    }
}

int
main (int argc, char **argv)
{
  struct hostent *hp;
  int optc;
  int not_inetd  = 0;
  char *database = NULL;
  ErrorDesc err;

  program_name = basename (argv[0]);

  while ((optc = getopt_long (argc, argv, "d:g:nm:Vh",
			      long_options, (int *) 0)) != EOF)
    {
      switch (optc)
	{
	case 'd':
	  database = optarg;
	  break;

	case 'g':
	  if (optarg[0] == '\0')
	    {
              fprintf(stderr, "%s: filename must be non-null\n", program_name);
              exit(1);
	    }
	  global_db_list_file = optarg;
	  break;

	case 'h':
	  usage (USAGE, 0);
	  break;

	case 'm':
          max_access_level = access_level(optarg);
	  break;

	case 'n':
	  not_inetd = 1;
	  break;

	case 'V':
	  version (PROGRAM_NAME);
	  exit (0);
	  break;

	default:
	  usage (USAGE, 1);
	}
    }

  umask (022);

#ifdef HAVE_KERBEROS
  kerberos_host = client_authorized = 0;

  if (not_inetd)
    {
      client_authorized = 1;
    }
#endif

  /* close file descriptors */
  fflush (stderr);
  fflush (stdout);

  init_logging (NULL);  

  if (gethostname (myname, MAXHOSTNAMELEN) != 0)
    {
      fprintf (stderr, "%s: cannot find out the name of this host\n",
	       program_name);
      exit (1);
    }

  /* Now see if we can get a FQDN for this host.  */
  hp = (struct hostent *) gethostbyname (myname);
  if (hp != NULL && (strlen (hp->h_name) > strlen (myname)))
    {
      strncpy (myname, hp->h_name, MAXHOSTNAMELEN);
    }

  /* Don't pay attention to SIGPIPE; read will give us the problem
     if it happens.  */
  signal (SIGPIPE, SIG_IGN);

  if (! not_inetd)
    {
      startConnection ();
    }
  else
    {
      current_host = myname;
      if (hp != NULL)
	{
	  current_addr = (char *) inet_ntoa (*(struct in_addr *) hp->h_addr);
	}
      else
	{
	  current_addr = myname;
	}
    }

  /* Change to the default database. */

  if (gnatsdChdb (database, "", "", 1, &err) != 0)
    {
      fprintf (stderr, "%d GNATS initialization of database failed: %s\n",
	       getErrorCode (err),
	       getErrorMessage (err));

      /* XXX ??? !!! Not sure what to do here exactly.  Badness, really. */
      if (getErrorCode (err) >= 600)
	{
	  /* Fatal error. */
	  exit (1);
	}
      else
	{
	  require_db = 1;
	}
    }

  if (user_access > ACCESS_DENY)
    {
      printf ("%d%c%s GNATS server %s ready.\r\n", CODE_GREETING,
	      kerberos_host ? '-' : ' ', myname, version_string);

      if (kerberos_host)
	{
	  printf ("%d Hi %s; you must use Kerberos authentication.\r\n",
		  CODE_GREETING, current_host);
	}

      serverMainLoop ();

      printf ("%d Later.\r\n", CODE_CLOSING);
    }

  exit (0);
}
