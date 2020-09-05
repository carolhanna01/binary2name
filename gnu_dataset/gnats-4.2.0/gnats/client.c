/* Common functions for each GNATS client.
   Copyright (C) 1994, 1995, 1999, 2000, 2002, 2007 Free Software Foundation, Inc.
   Copyright (C) 2001 Dan Martinez
   Contributed by Brendan Kehoe (brendan@cygnus.com).

This file is part of GNU GNATS.

GNU GNATS is free software; you can redistribute it and/or modify
it under the terms of the GNU General Public License as published by
the Free Software Foundation; either version 3, or (at your option)
any later version.

GNU GNATS is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
GNU General Public License for more detailmails.

You should have received a copy of the GNU General Public License
along with GNU GNATS; see the file COPYING.  If not, see
<http://www.gnu.org/licenses/>.
*/

#include "gnats.h"
#include "gnatsd.h"
#include "query.h"
#include <sys/wait.h>

#ifdef HAVE_MACHINE_ENDIAN_H
#include <machine/endian.h>			/* for htons */
#endif

#ifdef HAVE_KERBEROS
#include <des.h>
#include <krb.h>
#ifndef HAVE_KRB_GET_ERR_TEXT
#define krb_get_err_text(status) krb_err_txt[status]
#endif
int remember_krb_auth_ret = 0; /* yes, I like lisp. go away. */
#endif

/* For the GNATS server we'll be talking with.  */
static FILE *serv_write, *serv_read;

/* If we're reading in a PR, this is it. */
static PR *prBeingRead = NULL;

/* If we're reading in a list of PRs, this is where it goes. */
static StringList **prList;

/* The descriptor for talking to the server in non-stdio ways.  */
static int sockfd, dupfd;

/* If 1, send information as we make the query.  */
int debug = 0;

static int forkedGnatsd = 0;

static void
free_reply (Reply *r)
{
  free (r->text);
  free ((char*)r);
}


static Reply *
server_reply (void)
{
  char *recvline;
  char *p, c;
  Reply *res = NULL;

  recvline = read_line (serv_read, NULL);

  if (recvline == NULL)
    {
      fprintf (stderr, "%s: error reading from server\n", program_name);
      /* This is probably not a good idea, eh?  XXX ??? !!! */
      safe_exit ();
    }

  c = recvline[0];
  if (isascii ((int) c) && isdigit((int) c))
    {
      for (p = &recvline[0];
	   *p && isascii ((int) *p) && isdigit ((int) *p);
	   p++)
	continue;
      if (*p)
	{
	  size_t i;
	  res = (Reply *) xmalloc (sizeof (Reply));

	  if (*p == '-')
	    {
	      res->type = REPLY_CONT;
	    }
	  else
	    {
	      if (*p != ' ')
		{
		  fprintf (stderr, "%s: bad type of reply from server\n",
			   program_name);
		}
	      res->type = REPLY_END;
	    }
	  *p = '\0';
	  res->state = atoi (recvline);
	  res->text = xstrdup (p + 1);
	  i = strlen (res->text) - 2;
	  if (res->text[i] == '\r' || res->text[i] == '\n')
	    {
	      res->text[i] = '\0';
	    }
	  else if (res->text[i + 1] == '\n')
	    {
	      res->text[i + 1] = '\0';
	    }
	}
    }

  free (recvline);
  return res;
}

static void
readPRFromServer (PR *pr)
{
  char *recvline;
  int notdone = 1;

  while (notdone && ((recvline = read_line (serv_read, NULL)) != NULL))
    {
      if (debug)
	{
	  fprintf (stderr, "%s: received `%s'\n", program_name, recvline);
	}

      if (memcmp (recvline, ".\r\n\0", 4) == 0)
	{
	  finishReadPR (pr, 0);
	  notdone = 0;
	}
      else
	{
	  size_t linelen = strlen (recvline);
	  int i;

	  if (linelen > 0)
	    {
	      if (recvline[linelen - 2] == '\r')
		{
		  /* Only correct this if we're actually at the end of a 
		     line.  */
		  recvline[linelen - 2] = '\n';
		  recvline[linelen - 1] = '\0';
		  linelen--;
		}
	    }
	  /* Remove escape character added by write_multiline() */
	  i = 0;
	  if (recvline[0] == '.')
	    {
	      i = 1;
	    }
	  addLineToPR (pr, recvline, recvline + i, linelen - i, 0);
	  recvline = NULL;
	}
      if (recvline != NULL)
	{
	  free (recvline);
	}
    }
}

static void
readPRListFromServer (void)
{
  char *recvline;
  int notdone = 1;

  while (notdone && ((recvline = read_line (serv_read, NULL)) != NULL))
    {
      if (debug)
	{
	  fprintf (stderr, "%s: received `%s'\n", program_name, recvline);
	}

      if (memcmp (recvline, ".\r\n\0", 4) == 0)
	{
	  notdone = 0;
	}
      else
	{
	  size_t linelen = strlen (recvline);
	  int i;

	  if (linelen > 0)
	    {
	      if (recvline[linelen - 2] == '\r')
		{
		  /* Only correct this if we're actually at the end of a 
		     line.  */
		  recvline[linelen - 2] = '\n';
		  recvline[linelen - 1] = '\0';
		  linelen--;
		}
	    }
	  /* Remove escape character added by write_multiline() */
	  i = 0;
	  if (recvline[0] == '.')
	    {
	      i = 1;
	    }
	  while (recvline[i] != '\0' && isspace ((int)(unsigned char) recvline[i]))
	    {
	      i++;
	    }
	  if (isdigit ((int) recvline[i]))
	    {
	      *prList = new_string_list_ent (xstrdup (recvline + i), NULL);
	      prList = &((*prList)->next);
	    }
	}
    }
}

static void
read_server (FILE *outfp)
{
  char *recvline;
  int notdone = 1;

  while (notdone && ((recvline = read_line (serv_read, NULL)) != NULL))
    {
      if (debug)
	{
	  fprintf (stderr, "%s: received `%s'\n", program_name, recvline);
	}

      if (memcmp (recvline, ".\r\n\0", 4) == 0)
	{
	  notdone = 0;
	}
      else
	{
	  int i;
	  if (recvline[1])
	    {
	      size_t linelen = strlen (recvline) - 2;
	      if (recvline[linelen] == '\r')
		{
		  /* Only correct this if we're actually at the end of a 
		     line.  */
		  recvline[linelen] = '\n';
		  recvline[linelen + 1] = '\0';
		}
	    }
	  /* Remove escape character added by write_multiline() */
	  i = 0;
	  if (recvline[0] == '.')
	    {
	      i = 1;
	    }
	  fprintf (outfp, "%s", recvline + i);
	}
      free (recvline);
    }
} 


/* Return the code of the response from the server, or 0 if there wasn't
   any. */
int
get_reply (FILE *outfp)
{
  Reply *r;
  int done = 0;
  int retval = 0;

  /* Make sure anything we've written has gone to them.  */
  if (fflush (serv_write) == EOF || ferror (serv_write))
    {
      fprintf (stderr, "%s: error writing to server\n", program_name);
    }

  while (! done)
    {
      r = server_reply ();

      if (r != NULL)
	{
	  if (r->type != REPLY_CONT)
	    {
	      done = 1;
	    }

	  if (debug)
	    {
	      fprintf (stderr, "%s: received %d `%s'\n", 
		       program_name, r->state, r->text);
	    }

	  switch (r->state)
	    {
	    case CODE_GREETING:
	    case CODE_OK:
	    case CODE_SEND_PR:
	    case CODE_SEND_TEXT:
	    case CODE_SEND_CHANGE_REASON:
	    case CODE_INFORMATION_FILLER:
	      break;

	    case CODE_CLOSING:
	      if (debug)
		{
		  fprintf (stderr, "%s: server closing down\n", program_name);
		}
	      break;

	    case CODE_PR_READY:
	    case CODE_TEXT_READY:
	      /* read it til we get a . */
	      if (prBeingRead != NULL)
		{
		  readPRFromServer (prBeingRead);
		}
	      else if (prList != NULL)
		{
		  readPRListFromServer ();
		}
	      else
		{
		  if (outfp != NULL)
		    read_server (outfp);
		  else
		    {
		      fprintf (stderr, "%s: unexpected %d received\n",
			       program_name, r->state);
		      safe_exit();
		    }
		}
	      break;

	    case CODE_INFORMATION:
	      if (outfp != NULL)
		fprintf (outfp, "%s\n", r->text);
	      break;

	    case CODE_NONEXISTENT_PR:
	    case CODE_UNREADABLE_PR:
	    case CODE_LOCKED_PR:
	    case CODE_FILE_ERROR:
	    case CODE_ERROR:
	    case CODE_NO_ADM_ENTRY:
	    case CODE_INVALID_FIELD_NAME:
	    case CODE_INVALID_PR_CONTENTS:
	    case CODE_INVALID_ENUM:
	    case CODE_INVALID_DATE:
	    case CODE_INVALID_EXPR:
	    case CODE_INVALID_DATABASE:
	    case CODE_INVALID_FIELD_EDIT:
	      fprintf (stderr, "%s: %s\n", program_name, r->text);
	      safe_exit ();
	      break;

	    case CODE_PR_NOT_LOCKED:
	      fprintf (stderr, "%s: PR is not locked\n", program_name);
	      safe_exit ();
	      break;

	    case CODE_GNATS_LOCKED:
	      fprintf (stderr, "%s: database is locked\n", program_name);
	      safe_exit ();
	      break;

	    case CODE_NO_PRS_MATCHED:
	      fprintf (stderr, "%s: no PRs matched\n", program_name);
	      safe_exit ();
	      break;

	    case CODE_NO_KERBEROS:
	      fprintf (stderr,
		       "%s: no Kerberos support, authentication failed\n",
		       program_name);
	      /* FALLTHROUGH */
	    case CODE_NO_ACCESS:
#ifdef HAVE_KERBEROS
	      if (remember_krb_auth_ret)
		fprintf (stderr, "%s: error (%s) while attempting krb4-auth\n",
			 program_name, 
			 krb_get_err_text (remember_krb_auth_ret));
	      else
		fprintf (stderr, "%s: access denied\n", program_name);
#else
	      fprintf (stderr, "%s: access denied\n", program_name);
#endif
	      safe_exit ();
	      break;

	    default:
	      fprintf (stderr, "%s: cannot understand %d `%s'\n",
		       program_name, r->state, r->text);
	      safe_exit ();
	      break;
	    }
	  retval = r->state;
	  
	  free_reply (r);
	}
      else
	{
	  if (debug)
	    {
	      fprintf (stderr, "%s: null reply from the server\n",
		       program_name);
	    }
	  retval = 0;
	  done = 1;
	}
    }

  return retval;
}

void
safe_exit (void)
{
  static int doing_exit = 0;

  if (! doing_exit)
    {
      doing_exit = 1;
      client_exit ();
      exit (1);
    }
}

#ifdef HAVE_KERBEROS

void
tohex (char *ptr, char *buf, int sz)
{
  static const char hex[] = "0123456789abcdef";
  int c;
  while (sz--)
    {
      c = *ptr++ & 0xff;
      *buf++ = hex[c >> 4];
      *buf++ = hex[c & 15];
    }
  *buf++ = 0;
}

int
hexdigit (char c)
{
  if (c >= '0' && c <= '9')
    return c - '0';
  if (c >= 'a' && c <= 'f')
    return c - 'a' + 10;
  if (c >= 'A' && c <= 'F')
    return c - 'A' + 10;
  return -1;
}

void
fromhex (char *ptr, char *buf, int sz)
{
  int val;
  while (sz--)
    {
      val = hexdigit (*buf++);
      val <<= 4;
      val |= hexdigit (*buf++);
      *ptr++ = val;
    }
}
#endif

static int
clientConnect (ErrorDesc *err, const char *hostname, int port)
{
  FILE *file;
  struct hostent *host;
  struct sockaddr_in server;

#if 1
  if (hostname[0] == '/' || hostname[0] == '.')
    {
      int sockets[2];

      setuid (getuid ());
      setgid (getgid ());
      socketpair (PF_UNIX, SOCK_STREAM, 0, sockets);

      forkedGnatsd = fork ();
      if (forkedGnatsd == 0)
	{
	  unsetenv ("GNATSDB");
	  close (0);
	  close (1);
	  dup (sockets[0]);
	  dup (sockets[0]);
	  execl (hostname, hostname, "--not-inetd", NULL);
	  _exit (1);
	}
      else
	{
	  sockfd = sockets[1];
	}
    }
  else
#endif
    {
      memset ((char *) &server, 0, sizeof (server));
      server.sin_family = AF_INET;
      if (isdigit ((int) hostname[0]))
	{
	  server.sin_addr.s_addr = inet_addr (hostname);
	}
      else
	{
	  host = gethostbyname (hostname);
	  if (host == NULL)
	    {
	      setError (err, CODE_INVALID_DATABASE, "%s: No such host",
			hostname);
	      return -1;
	    }
	  memcpy (&server.sin_addr, host->h_addr, sizeof (server.sin_addr));
	}

      server.sin_port = htons (port);

      if (debug)
	{
	  fprintf (stderr, "opening connection to %s\n", hostname);
	}

      sockfd = socket (AF_INET, SOCK_STREAM, 0);
      if (sockfd < 0)
	{
	  setError (err, CODE_ERROR, "could not open a socket");
	  return -1;
	}
      if (connect (sockfd, (struct sockaddr *) &server, sizeof (server)) < 0)
	{
	  setError (err, CODE_ERROR,
		    "Couldn't connect to server on %s, port %d",
		    hostname, port);
	  close (sockfd);
	  return -1;
	}
    }
  if ((file = fdopen (sockfd, "r")) == NULL)
    {
      setError (err, CODE_ERROR, "read fdopen() failed");
      close (sockfd);
      return -1;
    }
  serv_read = file;
  dupfd = dup (sockfd);
  if (dupfd < 0)
    {
      setError (err, CODE_ERROR, "dup() failed");
      close (sockfd);
      return -1;
    }
  if ((file = fdopen (dupfd, "w")) == NULL)
    {
      setError (err, CODE_ERROR, "write fdopen() failed");
      close (sockfd);
      close (dupfd);
      return -1;
    }
  serv_write = file;

  /* Get the hello.  */
  get_reply (stdout);

#ifdef HAVE_KERBEROS
  {
    /* This is ugly, because krb_sendauth/krb_recvauth don't maintain
       a sensible connection state if authentication fails.  This is
       unacceptable here.  So I've broken out most of the interesting
       bits of those routines here.  I've also added a hex encoding of
       the data exchanged instead of the standard authenticator
       format, to maintain the text-only protocol used in GNATS.  */

    Reply *r;
    char *realm, *p, *me;
    int ret, i;
    struct sockaddr_in laddr;
    int laddrlen = sizeof (laddr);
    char hname[MAXHOSTNAMELEN]; /* for canonicalization by krb_mk_auth */
    /* Scratch space for Kerberos library, required to be provided by
       the application.  Keep it all together so we can erase it all
       at once later.  */
    struct {
      CREDENTIALS cred;
      MSG_DAT msg_data;
      Key_schedule sched;
      KTEXT_ST tkt, packet;
      char buf[MAX_KTXT_LEN * 2 + 10];
    } k;
    k.packet.mbz = 0;

#define PUNT(reason,subreason)	do{if (debug) fprintf(stderr,"%s: %s (%s)\n", program_name, reason, subreason);goto krb_exit;}while(0)
#define SKIP(reason)		PUNT("skipping Kerberos authentication",reason)
#define FAIL(reason)		PUNT("Kerberos authentication failed",  reason)

    /* This probably ought to be a crypto checksum of the
       authenticator, but a constant should be nearly as secure.  */
#define CKSUM			0x10291966

    strcpy (hname, host->h_name);

    realm = krb_realmofhost (hname);

    ret = krb_mk_auth (KOPT_DO_MUTUAL, &k.tkt,
		       GNATS_KRB4_PRINCIPAL_NAME, hname, realm,
		       CKSUM, GNATS_KRB4_VERSIONID, &k.packet);
    remember_krb_auth_ret = ret;
    if (ret)
      SKIP (krb_get_err_text (ret));

    ret = krb_get_cred (GNATS_KRB4_PRINCIPAL_NAME, hname, realm, &k.cred);
    /* This should always work unless the ticket file has been changed
       out from under the client at just the wrong time.  */
    if (ret)
      abort ();

    /* Since we don't currently have an interface for specifying a
       "local name" on the server distinct from the Kerberos principal
       name, just use the principal.  */
    me = k.cred.pname;

    if (debug)
      {
	fprintf (stderr, "%s: writing `AUTH krb4-simple'\n", program_name);
      }
    fprintf (serv_write, "AUTH krb4-simple\r\n");
    fflush (serv_write);
    r = server_reply ();
    if (!r || r->state == CODE_AUTH_TYPE_UNSUP)
      {
	SKIP ("not supported by server");
      }
    else if (r->state == CODE_ERROR)
      {
	fprintf (stderr, "%s: server authentication error: %s\n",
		 program_name, r->text);
	goto krb_exit;
      }
    else if (r->state != CODE_OK)
      {
	abort ();
      }
    else
      {
	free_reply (r);
      }

    if (getsockname (sockfd, (struct sockaddr *) &laddr, &laddrlen) < 0)
      {
	fprintf (stderr, "%s: getsockname: %s", program_name,
		 strerror (errno));
	exit (1);
      }

    k.buf[0] = 'x';
    /* This would be better with a base-64 encoding, but I'm too lazy
       to write it write now.  */
    tohex (k.packet.dat, k.buf + 1, k.packet.length);

    fprintf (serv_write, "%s\r\n%s\r\n", me, k.buf);
    fflush (serv_write);
 
    i = fgetc (serv_read);
    if (i == 'x')
      /* ok */
      k.buf[0] = i;
    else
      {
	/* error return */
	ungetc (i, serv_read);
	r = server_reply ();
	fprintf (stderr, "%s: %s\n", program_name, r->text);
	goto krb_exit;
      }
    p = fgets (k.buf + 1, sizeof (k.buf) - 1, serv_read);
    if (!p)
      /* eof */
      goto krb_exit;
    p = strchr (k.buf + 1, '\r');
    if (p)
      *p = 0;
    k.packet.length = (strlen (k.buf) - 1) / 2;
    fromhex (k.packet.dat, k.buf + 1, k.packet.length);
    ret = krb_check_auth (&k.packet, CKSUM, &k.msg_data,
			  k.cred.session, k.sched, &laddr, &server);
    if (ret)
      fprintf (stderr, "%s: authentication failed: %s\n",
	       program_name, krb_get_err_text (ret));

    /* Now that authentication has succeeded (presumably), get an "ok"
       response if authorization succeeds, or an error if it fails.
       (Not everyone listed in the Kerberos database is necessarily
       permitted to retrieve information from GNATS.)  */
    get_reply (stdout);

  krb_exit:
    /* Zero out the Kerberos scratch area, since it includes
       authentication information we don't want going into core files
       etc.  */
    memset ((char *) &k, '?', sizeof (k));
  }
#endif
  return 0;
}

void
client_exit (void)
{
  /* Do nothing, if we're not a network client.  */
  if (serv_read != NULL && serv_write != NULL)
    {
      /* That's it, say bye-bye.  */
      if (debug)
	{
	  fprintf (stderr, "%s: writing `QUIT'\n", program_name);
	}
      fflush (serv_read);
      fprintf (serv_write, "QUIT\r\n");

      fclose (serv_write);
      fclose (serv_read);
      serv_write = NULL;
      serv_read = NULL;
    }
  if (forkedGnatsd != 0)
    {
      int status;

      wait (&status);
    }
}


/* change database command to recognize databse aliases for network
   client commands like nquery-pr. */
void
client_chdb (const char *newRoot)
{
  if (newRoot == NULL)
    {
      newRoot = "default";
    }

  /* send the change db command and get server's reply */
  if (debug)
    {
      fprintf (stderr, "%s: writing `CHDB %s'\n", program_name, newRoot);
    }
  fprintf (serv_write, "CHDB %s\r\n", newRoot);
  get_reply (stdout);			/* this will exit on error */
}

static void
client_user (const char *user, const char *passwd)
{
  /* send the user command and get server's reply */
  if (debug)
    {
      fprintf (stderr, "%s: writing `USER %s %s'\n", program_name,
	       user, passwd);
    }
  fprintf (serv_write, "USER %s %s\r\n", user, passwd);
  get_reply(stdout);			/* this will exit on error */
}

/* Scan the environment and the global database configuration file for the 
   info needed to connect to the gnatsd server.  */
void
scanEnv (char **user, char **passwd, char **hostname,
	 int *port, char **database)
{
  char *vals[5];
  char *evar = *database == NULL ? getenv ("GNATSDB") : *database;
  bool is_net_conn = databaseSpecIsNetConn (evar);

  if (evar == NULL || evar[0] == '\0' || is_net_conn)
    {
      if (*hostname == NULL)
	{
	  const char *specS = databaseSpecServer (evar);
	  if (specS != NULL)
	    {
	      *hostname = xstrdup (specS);
	    }
	}
      if (*port == -1)
	{
	  const char *pname = databaseSpecPort (evar);

	  if (pname != NULL)
	    {
	      if (isdigit ((int) (pname[0])))
		{
		  *port = atoi (pname);
		}
	      else
		{
		  struct servent *s = getservbyname (pname, "tcp");
		  if (s != NULL)
		    {
		      *port = ntohs (s->s_port);
		    }
		}
	    }
	}
    }
  else
    {
      char *place = evar;
      int x;

      for (x = 0; x < 5; x++)
	{
	  if (place != NULL)
	    {
	      char *nplace = strchr (place, ':');
	      if (nplace != NULL)
		{
		  vals[x] = xmalloc (nplace - place + 1);
		  memcpy (vals[x], place, nplace - place);
		  vals[x][nplace - place] = '\0';
		  place = nplace + 1;
		}
	      else
		{
		  vals[x] = xstrdup (place);
		  place = NULL;
		}
	    }
	  else
	    {
	      vals[x] = xstrdup ("");
	    }
	}

      if (*hostname == NULL)
	{
	  *hostname = vals[0];
	}
      else
	{
	  free (vals[0]);
	}

      if (*port == -1 && vals[1][0] != '\0')
	{
	  *port = atoi (vals[1]);
	}

      free (vals[1]);
      if (*database == NULL && vals[2][0] != '\0')
	{
	  *database = vals[2];
	}
      else
	{
	  free (vals[2]);
	}

      if (*user == NULL && vals[3][0] != '\0')
	{
	  *user = vals[3];
	}
      else
	{
	  free (vals[3]);
	}

      if (*passwd == NULL && vals[4][0] != '\0')
	{
	  *passwd = vals[4];
	}
      else
	{
	  free (vals[4]);
	}
    }

  if (*user == NULL)
    {
      /* This is just wrong, but we'll live with it. XXX ??? !!! */
      char *lname = getlogin ();
      if (lname != NULL)
	{
	  *user = xstrdup (lname);
	}
      else
	{
	  struct passwd *p;

	  p = getpwuid (getuid ());
	  if (p != NULL)
	    {
	      *user = xstrdup (p->pw_name);
	    }
	}
    }

  if (*passwd == NULL)
    {
      *passwd = xstrdup ("*");
    }

  if (*database == NULL && ! is_net_conn)
    {
      *database = ((evar == NULL || evar[0] == '\0')
		   ? xstrdup ("default")
		   : xstrdup (evar));
    }

  if (*port == -1)
    {
      struct servent *s = getservbyname (DEFAULT_GNATS_SERVICE, "tcp");
      if (s != NULL)
	{
	  *port = ntohs (s->s_port);
	}
    }
}

int
client_init_gnats (ErrorDesc *err, const char *userArg, const char *passwdArg, 
		   const char *hostnameArg, int port, const char *databaseArg)
{
  char *user = NULL;
  char *passwd = NULL;
  char *hostname = NULL;
  char *database = NULL;
  int res = -1;

  if (userArg != NULL)
    {
      user = xstrdup (userArg);
    }

  if (passwdArg != NULL)
    {
      passwd = xstrdup (passwdArg);
    }

  if (hostnameArg != NULL)
    {
      hostname = xstrdup (hostnameArg);
    }

  if (databaseArg != NULL)
    {
      database = xstrdup (databaseArg);
    }

  scanEnv (&user, &passwd, &hostname, &port, &database);
  
  if (hostname == NULL)
    {
      setError (err, CODE_INVALID_DATABASE,
		"Can't determine hostname to connect to");
    }
  else if (port < 0)
    {
      setError (err, CODE_INVALID_DATABASE,
		"Can't determine port number to connect to");
    }
  else if (user == NULL)
    {
      setError (err, CODE_INVALID_DATABASE,
		"Can't determine username to send to the server");
    }
  else if (passwd == NULL)
    {
      setError (err, CODE_INVALID_DATABASE,
		"Can't determine password to send to the server");
    }
  else if (clientConnect (err, hostname, port) == 0)
    {
      client_chdb (database);
      client_user (user, passwd);
      res = 0;
    }

  if (user != NULL)
    {
      free (user);
    }
  if (passwd != NULL)
    {
      free (passwd);
    }
  if (hostname != NULL)
    {
      free (hostname);
    }
  if (database != NULL)
    {
      free (database);
    }
      
  return res;
}

static DatabaseInfo lockedDatabase = NULL;

int
client_lock_gnats (const DatabaseInfo database, ErrorDesc *err)
{
  if (lockedDatabase != NULL)
    {
      abort ();
    }
  lockedDatabase = database;
  return lock_gnats (database, err);
}

void
client_unlock_gnats (void)
{
  ErrorDesc err;
  DatabaseInfo database = lockedDatabase;
  lockedDatabase = NULL;

  if (database != NULL)
    {
      if (unlock_gnats (database, &err) < 0)
	{
	  client_print_errors (database, err);
	}
    }
}

void
client_print_errors (const DatabaseInfo database, ErrorDesc err)
{
  switch (getErrorCode (err))
    {
    case CODE_NO_INDEX:
    case CODE_FILE_ERROR:
      fprintf (stderr, "%s: %s\n", program_name, getErrorMessage (err));
      punt (database, 1, getErrorMessage (err));
      break;

    case CODE_INVALID_FIELD_CONTENTS:
      {
	BadFields badFieldList = getBadFieldList (err);
	while (badFieldList != NULL)
	  {
	    fprintf (stderr, "%s: invalid value `%s' in field %s\n",
		     program_name,
		     badFieldValue (badFieldList),
		     fieldDefForIndex (badFieldIndex (badFieldList))->name);
	    badFieldList = nextBadField (badFieldList);
	  }
      }
      break;

    case CODE_INVALID_ENUM:
      printf ("%s: invalid fields:\n", program_name);
      {
	BadFields badFieldList = getBadFieldList (err);
	while (badFieldList != NULL)
	  {
	    FieldIndex f = badFieldIndex (badFieldList);
	    printf ("\tNote: There was a bad value `%s' for the field `%s'.\n\tIt was set to the default value of `%s'.\n",
		    badFieldValue (badFieldList),
		    fieldDefForIndex (f)->name,
		    fieldDefForIndex (f)->default_value);
	    badFieldList = nextBadField (badFieldList);
	  }
      }
      break;

    default:
      fprintf (stderr, "%s: %s\n", program_name, getErrorMessage (err));
      break;
    }
}

void
sendRemoteListQuery (ListTypes whichList, FILE *outfile)
{
  const char *listName = listTypeToString (whichList);
  if (listName != NULL)
    {
      if (debug)
	{
	  fprintf (stderr, "%s: writing `LIST %s'\n",
		   program_name, listName);
	}
      fprintf (serv_write, "LIST %s\r\n", listName);
      if (outfile != NULL)
	{
	  get_reply (outfile);
	}
    }
}

static void
sendQueryFormat (const char *name)
{
  if (debug)
    {
      fprintf (stderr, "%s: writing `QFMT %s'\n", program_name, name);
    }
  fprintf (serv_write, "QFMT %s\r\n", name);
  get_reply (NULL);
}

void
sendRemoteQuery (const char *queryString, const char **args,
		 const char *query_format_name, FILE *outfile)
{
  char *buf;
  int i = 0;
  size_t buflen;

  if (queryString != NULL && queryString[0] != '\0')
    {
      if (debug)
	{
	  fprintf (stderr, "%s: writing `EXPR %s'\n", program_name,
		   queryString);
	}
      fprintf (serv_write, "EXPR %s\r\n", queryString);
      get_reply (NULL);
    }

  sendQueryFormat (query_format_name);

  /* I'm not entirely sure why we go through this foolishness of
     building up a string containing the command, only to just 
     print it out--why not call fprintf() a bunch of times?  It'd
     probably be faster.  XXX ??? !!! */
  buf = xstrdup ("QUER");
  buflen = 4;

  while (args != NULL && args[i] != NULL)
    {
      size_t arglen = strlen (args[i]);
      
      buf = xrealloc (buf, buflen + arglen + 2);
      buf[buflen] = ' ';
      memcpy (buf + buflen + 1, args[i], arglen + 1);
      buflen += arglen + 1;
      i++;
    }

  if (debug)
    {
      fprintf (stderr, "%s: writing `%s'\n", program_name, buf);
    }
  fprintf (serv_write, "%s\r\n", buf);
  free (buf);

  if (outfile != NULL)
    {
      get_reply (outfile);
    }
}

void
clientGetAdmField (FILE *outfile, const char *admField, 
		   const char *admSubfield, const char *admKey)
{
  if (admSubfield == NULL)
    {
      admSubfield = "";
    }
  fprintf (serv_write, "ADMV %s %s %s\r\n", admField, admKey, admSubfield);
  get_reply (outfile);
}

/* Read PR PRNUM from the GNATS server, and return it.  */
PR *
clientReadPR (const char *prNum)
{
  PR *res;

  prBeingRead = allocPR (NULL);
  fprintf (serv_write, "RSET\r\n");
  get_reply (NULL);
  fprintf (serv_write, "QFMT full\r\n");
  get_reply (NULL);
  fprintf (serv_write, "QUER %s\r\n", prNum);
  initReadPR (prBeingRead);
  get_reply (NULL);
  res = prBeingRead;
  prBeingRead = NULL;
  return res;
}

/* Return a list of PRs that match the specified query expression. */
StringList *
clientGetPRList (const char *expr)
{
  StringList *res = NULL;

  fprintf (serv_write, "RSET\r\n");
  get_reply (NULL);
  sendRemoteQuery (expr, NULL, "Number", NULL);
  prList = &res;
  get_reply (NULL);
  return res;
}

void
netSetEditEmailAddr (const char *addr)
{
  fprintf (serv_write, "EDITADDR %s\r\n", addr);
  get_reply (NULL);
}

static void
netSendPRCmd (const char *cmd, FILE *file, int show_information)
{
  char *line;
  if (debug)
    {
      fprintf (stderr, "%s: writing `%s'\n", program_name, cmd);
    }
  fprintf (serv_write, "%s\r\n", cmd);
  get_reply (stdout); /* XXX */
  
  while ((line = read_line (file, NULL)) != NULL)
    {
      write_multitext (serv_write, line, "\r\n");
      free (line);
    }
  fprintf (serv_write, ".\r\n");
  /* if get_reply finds errors it writes messages and doesn't return */
  get_reply (show_information ? stdout : NULL);
}

void
netCheckPR (FILE *file, int initial)
{
  netSendPRCmd (initial ? "CHEK INIT" : "CHEK", file, 1);
}

void
netEditField (FILE *fp, const char *prnum, const char *fieldname, 
	      const char *editEmailAddr, int append, char *reason)
{
  const char *cmd;
  char *line;
  int reply;

  netSetEditEmailAddr (editEmailAddr);
  if (append)
    {
      cmd = "APPN";
    }
  else
    {
      cmd = "REPL";
    }
  fprintf (serv_write, "%s %s %s\r\n", cmd, prnum, fieldname);

  do {
    reply = get_reply (stdout);
    switch (reply)
      {
        case CODE_SEND_TEXT:
          while ((line = read_line (fp, NULL)) != NULL)
            {
              write_multitext (serv_write, line, "\r\n");
              free (line);
            }
          fprintf (serv_write, ".\r\n");
	  break;

        case CODE_SEND_CHANGE_REASON:
          if (reason)
	    fprintf (serv_write, "%s\r\n.\r\n", reason);
          else
	    {
	      fprintf (stderr, "%s: server wants a change-reason for %s,"
	         " but none was supplied.\n", program_name, fieldname);
	      safe_exit ();
	    }
	  break;
    
	case CODE_OK:
	  break;

        default:
	  fprintf (stderr, "%s: bad reply from server: %d.\n",
		   program_name, reply);
	  safe_exit ();
	  break;
      }
  } while (reply != CODE_OK);
}

void
netSubmitNewPR (FILE *file, int show_prnum)
{
  netSendPRCmd ("SUBM", file, show_prnum);
}

void
netModifyPR (FILE *file, const char *prNum, const char *editEmailAddr)
{
  char *buf;

  netSetEditEmailAddr (editEmailAddr);
  asprintf (&buf, "EDIT %s", prNum);
  netSendPRCmd (buf, file, 1);
  free (buf);
}

void
netLockDB (void)
{
  if (debug)
    {
      fprintf (stderr, "%s: writing `LKDB'\n", program_name);
    }
  fprintf (serv_write, "LKDB\r\n");
  get_reply (stdout);
}

void
netUnlockDB (void)
{
  if (debug)
    {
      fprintf (stderr, "%s: writing `UNDB'\n", program_name);
    }
  fprintf (serv_write, "UNDB\r\n");
  get_reply (stdout);
}

void
netLockPR (const char *prNum, const char *username, const char *processID)
{
  if (processID != NULL)
    {
      fprintf (serv_write, "LOCK %s %s %s\r\n", prNum, username, processID);
    }
  else
    {
      fprintf (serv_write, "LOCK %s %s\r\n", prNum, username);
    }
  get_reply (stdout);
}

void
netUnlockPR (const char *prNum)
{
  if (debug)
    {
      fprintf (stderr, "%s: writing `UNLK %s'\n", program_name, prNum);
    }
  fprintf (serv_write, "UNLK %s\r\n", prNum);
  get_reply (stdout);
}

void
netDeletePR (const char *prNum, const char *editUserEmailAddr)
{
  netSetEditEmailAddr (editUserEmailAddr);
  if (debug)
    {
      fprintf (stderr, "%s: writing `DELETE %s'\n", program_name, prNum);
    }
  fprintf (serv_write, "DELETE %s\r\n", prNum);
  get_reply (stdout);
}

void
netFieldFlags (const char *fieldname)
{
  fprintf (serv_write, "FIELDFLAGS %s\r\n", fieldname);
  get_reply (stdout);
}

void
netValidValues (const char *fieldname)
{
  fprintf (serv_write, "FVLD %s\r\n", fieldname);
  get_reply (stdout);
}

void
netFieldDescription (const char *fieldname)
{
  fprintf (serv_write, "FDSC %s\r\n", fieldname);
  get_reply (stdout);
}

void
netFieldType (const char *fieldname)
{
  fprintf (serv_write, "FTYP %s\r\n", fieldname);
  get_reply (stdout);
}
