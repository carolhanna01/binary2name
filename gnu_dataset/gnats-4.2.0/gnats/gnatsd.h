/* Codes returned by the GNATS server.
   Copyright (C) 2001 Milan Zamazal
   Copyright (C) 1994,95,96,1997,2007 Free Software Foundation, Inc.
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

#include <netinet/in.h> /* for sockaddr_in */
#include <arpa/inet.h>	/* for inet_ntoa decl */
#include <sys/param.h>
#ifdef HAVE_NETDB_H
#include <netdb.h>	/* for hostent and MAXHOSTNAMELEN */
#endif
#include <sys/socket.h>	/* for AF_INET */
#include <sys/time.h>	/* for timeval */
#ifdef HAVE_SYS_SELECT_H
#include <sys/select.h>	/* for fd_set */
#endif

/* The information we need for a reply from the server.  */
typedef struct _reply
{
  int state;
  char *text;
  int type;
} Reply;

typedef enum _msg_type {
  PR_eof, PR_ok, PR_timeout
} MsgType;

#include "query.h"

/* gnatsd access levels */
typedef enum _Access_Level
  {
    ACCESS_UNKNOWN,
    ACCESS_DENY,
    ACCESS_NONE,
    ACCESS_LISTDB,
    ACCESS_SUBMIT,
    ACCESS_VIEW,
    ACCESS_VIEWCONF,
    ACCESS_EDIT,
    ACCESS_ADMIN,
    NO_LEVEL			/* for internal auxiliary use only */
  } Access_Level;

/* The name and IP address of the host talking with us and their access level.  */
extern const char *current_host;
extern const char *current_addr;
extern Access_Level user_access;

/* in gnatsd.c */
extern MsgType get_line (char **text, int timeout);
extern int verifyHostAndUser (const DatabaseInfo database,
			      const char *host, const char *ipaddr, 
			      const char *username, const char *passwd,
			      Access_Level *access);
extern const char *access_level_str (Access_Level);
extern int gnatsdChdb (const char *database, const char *user,
		       const char *password, int quiet, ErrorDesc *err);

/* cmds.c */
extern void GNATS_quit (int ac, char **av);
extern void GNATS_qfmt (int ac, char **av);
extern void GNATS_quer (int ac, char **av);
extern void GNATS_lock (int ac, char **av);
extern void GNATS_unlk (int ac, char **av);
extern void GNATS_lkdb (int ac, char **av);
extern void GNATS_undb (int ac, char **av);
extern void GNATS_dbls (int ac, char **av);
extern void GNATS_dbdesc (int ac, char **av);
extern void GNATS_chdb (int ac, char **av);
extern void GNATS_chek (int ac, char **av);
extern void GNATS_subm (int ac, char **av);
extern void GNATS_appn (int ac, char **av);
extern void GNATS_repl (int ac, char **av);
extern void GNATS_edit (int ac, char **av);
extern void GNATS_vfld (int ac, char **av);
extern void GNATS_rset (int ac, char **av);
extern void GNATS_auth (int ac, char **av);
extern void GNATS_user (int ac, char **av);
extern void GNATS_list (int ac, char **av);
extern void GNATS_help (int ac, char **av);
extern void GNATS_expr (int ac, char **av);
extern void GNATS_ftyp (int ac, char **av);
extern void GNATS_fdsc (int ac, char **av);
extern void GNATS_fvld (int ac, char **av);
extern void GNATS_admv (int ac, char **av);
extern void GNATS_ftypinfo (int ac, char **av);
extern void GNATS_editaddr (int ac, char **av);
extern void GNATS_fieldflags (int ac, char **av);
extern void GNATS_inputdefault (int ac, char **av);
extern void GNATS_delete (int ac, char **av);


/* Whether we received `STATE-' or `STATE '.  */
#define REPLY_CONT	1
#define REPLY_END	2

/* Used if Kerberos version 4 authentication is enabled.  */
#define GNATS_KRB4_VERSIONID		"GnatsK1.0"
#define GNATS_KRB4_PRINCIPAL_NAME	gnatsAdminMailAddr ()

#include "pcodes.h"
