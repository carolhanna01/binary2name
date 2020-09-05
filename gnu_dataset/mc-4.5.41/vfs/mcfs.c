/* Virtual File System: Midnight Commander file system.
   
   Copyright (C) 1995, 1996, 1997 The Free Software Foundation

   Written by Miguel de Icaza
              Andrej Borsenkow
	      Norbert Warmuth
   
   This program is free software; you can redistribute it and/or
   modify it under the terms of the GNU Library General Public License
   as published by the Free Software Foundation; either version 2 of
   the License, or (at your option) any later version.
   
   This program is distributed in the hope that it will be useful,
   but WITHOUT ANY WARRANTY; without even the implied warranty of
   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
   GNU Library General Public License for more details.

   You should have received a copy of the GNU Library General Public
   License along with this program; if not, write to the Free Software
   Foundation, Inc., 675 Mass Ave, Cambridge, MA 02139, USA.  */

/* Namespace: exports mcfs_vfs_ops, tcp_invalidate_socket */

#include <config.h>
#include <stdio.h>
#include <errno.h>
#include <string.h>
#include <unistd.h>
#include <stdlib.h>
#include <stdarg.h>
#include <fcntl.h>
#include <pwd.h>
#include <sys/types.h>		/* POSIX-required by sys/socket.h and netdb.h */
#include <netdb.h>		/* struct hostent */
#include <sys/socket.h>		/* AF_INET */
#include <netinet/in.h>		/* struct in_addr */
#include <arpa/inet.h>
#ifdef SCO_FLAVOR
#include <sys/timeb.h>	/* alex: for struct timeb definition */
#endif /* SCO_FLAVOR */
#include <time.h>

#ifdef USE_TERMNET
#include <termnet.h>
#endif

#include "utilvfs.h"

#include "vfs.h"
#include "mcfs.h"
#include "tcputil.h"
#include "../src/dialog.h"

#define MCFS_MAX_CONNECTIONS 32
#define mcserver_port 9876

static mcfs_open_connections = 0;
static struct _mcfs_connection {
    char *host;
    char *user;
    char *home;
    int  sock;
    int  port;
    int  version;
} mcfs_connections [MCFS_MAX_CONNECTIONS];

typedef struct _mcfs_connection mcfs_connection;

typedef struct { int handle; mcfs_connection *conn; } mcfs_handle;

static int my_errno;

static char *mcfs_gethome (mcfs_connection *mc);

/* Extract the hostname and username from the path */
/* path is in the form: hostname:user/remote-dir */
static char *mcfs_get_host_and_username (char *path, char **host, char **user,
				  int *port, char **pass)
{
    return vfs_split_url (path, host, user, port, pass, 0, 0); 
}

static void mcfs_fill_names (vfs *me, void (*func)(char *))
{
    int i;
    char *name;
    
    for (i = 0; i < MCFS_MAX_CONNECTIONS; i++){
	if (mcfs_connections [i].host == 0)
	    continue;
	name = g_strconcat ("/#mc:", mcfs_connections [i].user,
			     "@",   mcfs_connections [i].host, NULL);
	(*func) (name);
	g_free (name);
    }
}

static void mcfs_free_bucket (int bucket)
{
    g_free (mcfs_connections [bucket].host);
    g_free (mcfs_connections [bucket].user);
    g_free (mcfs_connections [bucket].home);

    /* Set all the fields to zero */
    mcfs_connections [bucket].host =
	mcfs_connections [bucket].user =
	    mcfs_connections [bucket].home = 0;
    mcfs_connections [bucket].sock =
	mcfs_connections [bucket].version = 0;
}

/* FIXME: This part should go to another c module, perhaps tcp.c */
static int mcfs_invalidate_socket (int);

void tcp_invalidate_socket (int sock)
{
     mcfs_invalidate_socket (sock);
}
/* FIXME end: 'cause it is used not only by mcfs */

static int mcfs_invalidate_socket (int sock)
{
    int i, j = -1;
    extern int mc_chdir (char *);
    
    for (i = 0; i < MCFS_MAX_CONNECTIONS; i++)
	if (mcfs_connections [i].sock == sock) {
	    mcfs_free_bucket (i);
	    j = 0;
	}

    if (j == -1)
        return -1; /* It was not our sock */
    /* Break from any possible loop */
    mc_chdir ("/");
    return 0;
}

/* This routine checks the server RPC version and logs the user in */
static int mcfs_login_server (int my_socket, char *user, int port,
			      int port_autodetected, char *netrcpass,
			      int *version)
{
    int result;
    char *pass;
    
    /* Send the version number */
    rpc_send (my_socket, RPC_INT, *version, RPC_END);
    if (0 == rpc_get (my_socket, RPC_INT, &result, RPC_END))
	return 0;
    
    if (result != MC_VERSION_OK){
	message_1s (1, _(" MCFS "), _(" The server does not support this version "));
	close (my_socket);
	return 0;
    }
	
    /* FIXME: figure out why last_current_dir used to be passed here */
    rpc_send (my_socket, RPC_INT, MC_LOGIN, RPC_STRING, "/",
        RPC_STRING, user, RPC_END);
    
    if (0 == rpc_get (my_socket, RPC_INT, &result, RPC_END))
	return 0;
    
    if (result == MC_NEED_PASSWORD){
	if (port > 1024 && port_autodetected){
	    int v;
#ifndef VFS_STANDALONE
	    v = query_dialog (_(" Warning "),
	        _(" The remote server is not running on a system port \n"
		  " you need a password to log in, but the information may \n"
		  " not be safe on the remote side.  Continue? \n"), 3, 2,
			      _(" Yes "),  _(" No "));
#else
	    message_1s( 1, " MCFS ", _(" The remote server is running on strange port. Giving up.\n"));
	    v = 1;
#endif

	    if (v == 1){
		close (my_socket);
		return 0;
	    }
	}
	if (netrcpass != NULL)
	    pass = g_strdup (netrcpass);
	else
	    pass = vfs_get_password (_(" MCFS Password required "));
	if (!pass){
	    rpc_send (my_socket, RPC_INT, MC_QUIT, RPC_END);
	    close (my_socket);
	    return 0;
	}
	rpc_send (my_socket, RPC_INT, MC_PASS, RPC_STRING, pass, RPC_END);

	wipe_password (pass);

	if (0 == rpc_get (my_socket, RPC_INT, &result, RPC_END))
	    return 0;
	
	if (result != MC_LOGINOK){
	    message_1s (1, " MCFS ", _(" Invalid password "));
	    rpc_send (my_socket, RPC_INT, MC_QUIT, RPC_END);
	    close (my_socket);
	    return 0;
	}
    }
    return my_socket;
}

/* This used to be in utilvfs.c, but as it deals with portmapper, it
   is probably usefull for mcfs */
int
open_tcp_link  (char *host, int *port, int *version, char *caller)
{
    struct   sockaddr_in server_address;
    unsigned long inaddr;
    struct   hostent *hp;
    int      my_socket;

    if (!*host)
	return 0;
    
    bzero ((char *) &server_address, sizeof (server_address));
    server_address.sin_family = AF_INET;
    
    /*  Try to use the dotted decimal number */
    if ((inaddr = inet_addr (host)) != -1)
	bcopy ((char *) &inaddr, (char *) &server_address.sin_addr,
	       sizeof (inaddr));
    else {
	if ((hp = gethostbyname (host)) == NULL){
	    message_2s (1, caller, " Can't locate hostname: %s ", host);
	    return 0;
	}
	bcopy ((char *) hp->h_addr, (char *) &server_address.sin_addr,
	       hp->h_length);
    }

    /* Try to contact a remote portmapper to obtain the listening port */
    if (*port == 0){
	*port = get_remote_port (&server_address, version);
	if (*port < 1)
	    return 0;
    } else
	*version = 1;
    
    server_address.sin_port = htons (*port);
    
    if ((my_socket = socket (AF_INET, SOCK_STREAM, 0)) < 0){
	message_2s (1, caller, " Can't create socket: %s ",
		 unix_error_string(errno));
	return 0;
    }
    if (connect (my_socket, (struct sockaddr *) &server_address,
	     sizeof (server_address)) < 0){
	message_2s (1, caller, " Can't connect to server: %s ",
		 unix_error_string (errno));
	close (my_socket);
	return 0;
    }
    return my_socket;
}

static int mcfs_open_tcp_link (char *host, char *user, 
    int *port, char *netrcpass, int *version)
{
    int my_socket;
    int old_port = *port;
    
    my_socket = open_tcp_link (host, port, version, " MCfs ");
    if (my_socket <= 0)
	return 0;
    
    /* We got the connection to the server, verify if the server
       implements our version of the RPC mechanism and then login
       the user.
    */
    return mcfs_login_server (my_socket, user, *port, old_port == 0,
			      netrcpass, version);
}

static int mcfs_get_free_bucket_init = 1;
static mcfs_connection *mcfs_get_free_bucket ()
{
    int i;
    
    if (mcfs_get_free_bucket_init) {
        mcfs_get_free_bucket_init = 0;
        for (i = 0; i < MCFS_MAX_CONNECTIONS; i++)
	    mcfs_connections [i].host = 0;
    }
    for (i = 0; i < MCFS_MAX_CONNECTIONS; i++){
	if (!mcfs_connections [i].host)
	    return &mcfs_connections [i];
    }
    /* This can't happend, since we have checked for max connections before */
    vfs_die("Internal error: mcfs_get_free_bucket");
    return 0;	/* shut up, stupid gcc */
}

/* This routine keeps track of open connections */
/* Returns a connected socket to host */
static mcfs_connection *mcfs_open_link (char *host, char *user, int *port, char *netrcpass)
{
    int i, sock, version;
    mcfs_connection *bucket;

    /* Is the link actually open? */
    if (mcfs_get_free_bucket_init) {
        mcfs_get_free_bucket_init = 0;
        for (i = 0; i < MCFS_MAX_CONNECTIONS; i++)
	    mcfs_connections [i].host = 0;
    } else for (i = 0; i < MCFS_MAX_CONNECTIONS; i++){
	if (!mcfs_connections [i].host)
	    continue;
	if ((strcmp (host, mcfs_connections [i].host) == 0) &&
	    (strcmp (user, mcfs_connections [i].user) == 0))
	    return &mcfs_connections [i];
    }
    if (mcfs_open_connections == MCFS_MAX_CONNECTIONS){
	message_1s (1, MSG_ERROR, _(" Too many open connections "));
	return 0;
    }

    if (!(sock = mcfs_open_tcp_link (host, user, port, netrcpass, &version)))
	return 0;
    
    bucket = mcfs_get_free_bucket ();
    mcfs_open_connections++;
    bucket->host    = g_strdup (host);
    bucket->user    = g_strdup (user);
    bucket->home    = 0;
    bucket->port    = *port;
    bucket->sock    = sock;
    bucket->version = version;

    return bucket;
}

static int is_error (int result, int errno_num)
{
    if (!(result == -1))
	return my_errno = 0;
    else 
	my_errno = errno_num;
    return 1;
}

static int the_error (int result, int errno_num)
{
    if (result == -1)
	my_errno = errno_num;
    else
	my_errno = 0;
    return result;
}

static char *mcfs_get_path (mcfs_connection **mc, char *path)
{
    char *user, *host, *remote_path;
    char *pass;
    int    port;

    /* An absolute path name, try to determine connection socket */
    if (strncmp (path, "/#mc:", 5))
        return NULL;
    path += 5;

    /* Port = 0 means that open_tcp_link will try to contact the
     * remote portmapper to get the port number
     */
    port = 0;
    if ((remote_path = mcfs_get_host_and_username(path, &host, &user, &port, &pass)))
	if (!(*mc = mcfs_open_link (host, user, &port, pass))){
	    g_free (remote_path);
	    remote_path = NULL;
	}
    g_free (host);
    g_free (user);
    if (pass)
        wipe_password (pass);

    if (!remote_path)
        return NULL;

    /* NOTE: tildes are deprecated. See ftpfs.c */
    {
        int f = !strcmp( remote_path, "/~" );
	if (f || !strncmp( remote_path, "/~/", 3 )) {
	    char *s;
	    s = concat_dir_and_file( mcfs_gethome (*mc), remote_path +3-f );
	    g_free (remote_path);
	    remote_path = s;
	}
    }
    return remote_path;
}

/* Simple function for routines returning only an integer from the server */
static int mcfs_handle_simple_error (int sock, int return_status)
{
    int status, error;
    
    if (0 == rpc_get (sock, RPC_INT, &status, RPC_INT, &error, RPC_END))
	return the_error (-1, EIO);

    if (is_error (status, error))
	return -1;
    if (return_status)
	return status;
    return 0;
}

/* Nice wrappers */
static int mcfs_rpc_two_paths (int command, char *s1, char *s2)
{
    mcfs_connection *mc;
    char *r1, *r2;

    if ((r1 = mcfs_get_path (&mc, s1)) == 0)
	return -1;

    if ((r2 = mcfs_get_path (&mc, s2)) == 0){
	g_free (r1);
	return -1;
    }
    
    rpc_send (mc->sock,
	      RPC_INT, command,
	      RPC_STRING, r1,
	      RPC_STRING, r2,
	      RPC_END);
    g_free (r1);
    g_free (r2);
    return mcfs_handle_simple_error (mc->sock, 0);
}

static int mcfs_rpc_path (int command, char *path)
{
    mcfs_connection *mc;
    char *remote_file;

    if ((remote_file = mcfs_get_path (&mc, path)) == 0)
	return -1;
    
    rpc_send (mc->sock,
	      RPC_INT, command,
	      RPC_STRING, remote_file,
	      RPC_END);

    g_free (remote_file);
    return mcfs_handle_simple_error (mc->sock, 0);
}

static int mcfs_rpc_path_int (int command, char *path, int data)
{
    mcfs_connection *mc;
    char *remote_file;

    if ((remote_file = mcfs_get_path (&mc, path)) == 0)
	return -1;

    rpc_send (mc->sock,
	      RPC_INT,    command,
	      RPC_STRING, remote_file,
	      RPC_INT,    data, RPC_END);

    g_free (remote_file);
    return mcfs_handle_simple_error (mc->sock, 0);
}

static int mcfs_rpc_path_int_int (int command, char *path, int n1, int n2)
{
    mcfs_connection *mc;
    char *remote_file;

    if ((remote_file = mcfs_get_path (&mc, path)) == 0)
	return -1;

    rpc_send (mc->sock,
	      RPC_INT, command,
	      RPC_STRING, remote_file,
	      RPC_INT, n1,
	      RPC_INT, n2,
	      RPC_END);

    g_free (remote_file);
    return mcfs_handle_simple_error (mc->sock, 0);
}

static char *mcfs_gethome (mcfs_connection *mc)
{
    char *buffer;

    if (mc->home)
	return g_strdup (mc->home);
    else {
	rpc_send (mc->sock, RPC_INT, MC_GETHOME, RPC_END);
	if (0 == rpc_get (mc->sock, RPC_STRING, &buffer, RPC_END))
	    return g_strdup (PATH_SEP_STR);
	mc->home = buffer;
	return g_strdup (buffer);
    }
}

/* The callbacks */
static void *mcfs_open (vfs *me, char *file, int flags, int mode)
{
    char *remote_file;
    mcfs_connection *mc;
    int  result, error_num;
    mcfs_handle  *remote_handle;

    if (!(remote_file = mcfs_get_path (&mc, file)))
	return 0;

    rpc_send (mc->sock, RPC_INT, MC_OPEN, RPC_STRING, remote_file,
	      RPC_INT, flags, RPC_INT, mode, RPC_END);
    g_free (remote_file);    

    if (0 == rpc_get (mc->sock, RPC_INT, &result, RPC_INT, &error_num, RPC_END))
	return 0;

    if (is_error (result, error_num))
	return 0;

    remote_handle = g_new (mcfs_handle, 2);
    remote_handle->handle  = result;
    remote_handle->conn    = mc;
    
    return remote_handle;
}

static int mcfs_read (void *data, char *buffer, int count)
{
    mcfs_handle *info = (mcfs_handle *) data;
    int result, error;
    int handle;
    mcfs_connection *mc;

    mc =     info->conn;
    handle = info->handle; 
    
    rpc_send (mc->sock, RPC_INT, MC_READ, RPC_INT, handle,
	      RPC_INT, count, RPC_END);
    
    if (0 == rpc_get  (mc->sock, RPC_INT, &result, RPC_INT, &error, RPC_END))
	return the_error (-1, EIO);
    
    if (is_error (result, error))
	return 0;
    
    if (0 == rpc_get  (mc->sock, RPC_BLOCK, result, buffer, RPC_END))
	return the_error (-1, EIO);
    
    return result;
}

static int mcfs_write (void *data, char *buf, int nbyte)
{
    mcfs_handle *info = (mcfs_handle *) data;
    mcfs_connection *mc;
    int handle;

    mc     = info->conn;
    handle = info->handle;
    
    rpc_send (mc->sock,
	      RPC_INT, MC_WRITE,
	      RPC_INT, handle,
	      RPC_INT, nbyte,
	      RPC_BLOCK, nbyte, buf,
	      RPC_END);
    
    return mcfs_handle_simple_error (mc->sock, 1);
}

static int mcfs_close (void *data)
{
    mcfs_handle *info = (mcfs_handle *) data;
    mcfs_connection *mc;
    int handle, result, error;

    if (!data)
	return -1;
    
    handle = info->handle;
    mc     = info->conn;
    
    rpc_send (mc->sock, RPC_INT, MC_CLOSE, RPC_INT, handle, RPC_END);
    
    if (0 == rpc_get (mc->sock, RPC_INT, &result, RPC_INT, &error, RPC_END))
	return the_error (-1, EIO);
    
    is_error (result, error);

    g_free (data);
    return result;
}

static int mcfs_errno (vfs *me)
{
    return my_errno;
}

typedef struct dir_entry {
    char *text;
    struct dir_entry *next;
    struct stat my_stat;
    int    merrno;
} dir_entry;

typedef struct {
    mcfs_connection *conn;
    int handle;
    dir_entry *entries;
    dir_entry *current;
} opendir_info;

static void *mcfs_opendir (vfs *me, char *dirname)
{
    opendir_info *mcfs_info;
    mcfs_connection *mc;
    int handle, error_num;
    char *remote_dir;
    int  result;

    if (!(remote_dir = mcfs_get_path (&mc, dirname)))
	return 0;

    rpc_send (mc->sock, RPC_INT, MC_OPENDIR, RPC_STRING, remote_dir, RPC_END);
    g_free (remote_dir);
    
    if (0 == rpc_get  (mc->sock, RPC_INT, &result, RPC_INT, &error_num, RPC_END))
	return 0;
    
    if (is_error (result, error_num))
	return 0;
	    
    handle = result;

    mcfs_info = g_new (opendir_info, 1);
    mcfs_info->conn = mc;
    mcfs_info->handle = handle;
    mcfs_info->entries = 0;
    mcfs_info->current = 0;
    
    return mcfs_info;
}

static int get_stat_info (mcfs_connection *mc, struct stat *buf);

static int mcfs_loaddir (opendir_info *mcfs_info)
{
    int  status, error;
    mcfs_connection  *mc = mcfs_info->conn;
    int  link = mc->sock;
    int  first = 1;

    rpc_send (link, RPC_INT, MC_READDIR, RPC_INT, mcfs_info->handle, RPC_END);

    for (;;){
	int  entry_len;
	dir_entry *new_entry;

	if (!rpc_get (link, RPC_INT, &entry_len, RPC_END))
	    return 0;
	
	if (entry_len == 0)
	    break;

	new_entry = g_new (dir_entry, 1);
	new_entry->text = g_new0 (char, entry_len + 1);

	new_entry->next = 0;
	if (first){
	    mcfs_info->entries = new_entry;
	    mcfs_info->current = new_entry;
	    first = 0;
	} else {
	    mcfs_info->current->next = new_entry;
	    mcfs_info->current = new_entry;
	}

	if (!rpc_get (link, RPC_BLOCK, entry_len, new_entry->text, RPC_END))
	    return 0;

	/* Then we get the status from the lstat */
	if (!rpc_get (link, RPC_INT, &status, RPC_INT, &error, RPC_END))
	    return 0;
	
	if (is_error (status, error))
	    new_entry->merrno = error;
	else {
	    new_entry->merrno = 0;
	    if (!get_stat_info (mc, &(new_entry->my_stat)))
		return 0;
	}
    } 
    mcfs_info->current = mcfs_info->entries;
    
    return 1;
}

static void mcfs_free_dir (dir_entry *de)
{
    if (!de)
	return;
    mcfs_free_dir (de->next);
    g_free (de->text);
    g_free (de);
}

/* Explanation:
 * On some operating systems (Slowaris 2 for example)
 * the d_name member is just a char long (Nice trick that break everything,
 * so we need to set up some space for the filename.
 */
static struct {
    struct dirent dent;
#ifdef NEED_EXTRA_DIRENT_BUFFER
    char extra_buffer [MC_MAXPATHLEN];
#endif
} mcfs_readdir_data;

/* The readdir routine loads the complete directory */
/* It's too slow to ask the server each time */
/* It now also sends the complete lstat information for each file */
static struct stat *cached_lstat_info;
static void *mcfs_readdir (void *info)
{
    opendir_info  *mcfs_info;
    char *dirent_dest;

    mcfs_info = (opendir_info *) info;

    if (!mcfs_info->entries)
	if (!mcfs_loaddir (mcfs_info))
	    return NULL;

    if (mcfs_info->current == 0){
	cached_lstat_info = 0;
	mcfs_free_dir (mcfs_info->entries);
	mcfs_info->entries = 0;
	return NULL;
    }
    dirent_dest = &(mcfs_readdir_data.dent.d_name [0]);
    strcpy (dirent_dest, mcfs_info->current->text);
    cached_lstat_info = &mcfs_info->current->my_stat;
    mcfs_info->current = mcfs_info->current->next;

#ifndef DIRENT_LENGTH_COMPUTED
    mcfs_readdir_data.dent.d_namlen = strlen (mcfs_readdir_data.dent.d_name);
#endif
    
    return &mcfs_readdir_data;
}

static int mcfs_closedir (void *info)
{
    opendir_info *mcfs_info = (opendir_info *) info;
    dir_entry *p, *q;

    rpc_send (mcfs_info->conn->sock, RPC_INT, MC_CLOSEDIR,
	      RPC_INT, mcfs_info->handle, RPC_END);
    
    for (p = mcfs_info->entries; p;){
	q = p;
	p = p->next;
	g_free (q->text);
	g_free (q);
    }
    g_free (info);
    return 0;
}

static time_t mcfs_get_time (mcfs_connection *mc)
{
    int       sock = mc->sock;
    
    if (mc->version == 1) {
	struct tm tt;

	rpc_get (sock,
		 RPC_INT, &tt.tm_sec,
		 RPC_INT, &tt.tm_min,
		 RPC_INT, &tt.tm_hour,
		 RPC_INT, &tt.tm_mday,
		 RPC_INT, &tt.tm_year,
		 RPC_INT, &tt.tm_mon,
		 RPC_END);
	tt.tm_year -= 1900;
	tt.tm_isdst = 0;

	return mktime (&tt);
    } else {
	char  *buf;
	long  tm;

	rpc_get (sock,
		 RPC_STRING, &buf,
		 RPC_END);
	sscanf (buf, "%lx", &tm);
	g_free (buf);

	return (time_t) tm;
    }
}

static int get_stat_info (mcfs_connection *mc, struct stat *buf)
{
    long mylong;
    int  sock = mc->sock;

    buf->st_dev = 0;
    
    rpc_get (sock, RPC_INT, &mylong, RPC_END);
#ifdef HAVE_ST_RDEV
    buf->st_rdev = mylong;
#endif
    rpc_get (sock, RPC_INT, &mylong, RPC_END);
    buf->st_ino = mylong;
    rpc_get (sock, RPC_INT, &mylong, RPC_END);
    buf->st_mode = mylong;
    rpc_get (sock, RPC_INT, &mylong, RPC_END);
    buf->st_nlink = mylong;
    rpc_get (sock, RPC_INT, &mylong, RPC_END);
    buf->st_uid = mylong;
    rpc_get (sock, RPC_INT, &mylong, RPC_END);
    buf->st_gid = mylong;
    rpc_get (sock, RPC_INT, &mylong, RPC_END);
    buf->st_size = mylong;
    
    if (!rpc_get (sock, RPC_INT, &mylong, RPC_END))
	return 0;
#ifdef HAVE_ST_BLOCKS
    buf->st_blocks = mylong;
#endif
    buf->st_atime = mcfs_get_time (mc);
    buf->st_mtime = mcfs_get_time (mc);
    buf->st_ctime = mcfs_get_time (mc);
    return 1;
}

static int mcfs_stat_cmd (int cmd, char *path, struct stat *buf)
{
    char *remote_file;
    mcfs_connection *mc;
    int  status, error;
    
    if ((remote_file = mcfs_get_path (&mc, path)) == 0)
	return -1;

    rpc_send (mc->sock, RPC_INT, cmd, RPC_STRING, remote_file, RPC_END);
    g_free (remote_file);
    if (!rpc_get (mc->sock, RPC_INT, &status, RPC_INT, &error, RPC_END))
	return the_error (-1, errno);

    if (is_error (status, error))
	return -1;

    if (get_stat_info (mc, buf))
	return 0;
    else
	return the_error (-1, EIO);
}

static int mcfs_stat (vfs *me, char *path, struct stat *buf)
{
    return mcfs_stat_cmd (MC_STAT, path, buf);
}

static int mcfs_lstat (vfs *me, char *path, struct stat *buf)
{
    int path_len = strlen (path);
    int entry_len = strlen (mcfs_readdir_data.dent.d_name);

    /* Hack ... */
    if (strcmp (path + path_len - entry_len,
		mcfs_readdir_data.dent.d_name) == 0 &&
	cached_lstat_info){
	*buf = *cached_lstat_info;
	return 0;
    }
    return mcfs_stat_cmd (MC_LSTAT, path, buf);
}

static int mcfs_fstat (void *data, struct stat *buf)
{
    mcfs_handle *info = (mcfs_handle *) data;
    int result, error;
    int handle, sock;
    
    sock = info->conn->sock;
    handle = info->handle;
    
    rpc_send (sock, RPC_INT, MC_FSTAT, RPC_INT, handle, RPC_END);
    if (!rpc_get (sock, RPC_INT, &result, RPC_INT, &error, RPC_END))
	return the_error (-1, EIO);

    if (is_error (result, error))
	return -1;

    if (get_stat_info (info->conn, buf))
	return 0;
    else
	return the_error (-1, EIO);
}

static int mcfs_chmod (vfs *me, char *path, int mode)
{
    return mcfs_rpc_path_int (MC_CHMOD, path, mode);
}

static int mcfs_chown (vfs *me, char *path, int owner, int group)
{
    return mcfs_rpc_path_int_int (MC_CHOWN, path, owner, group);
}

static int mcfs_utime (vfs *me, char *path, struct utimbuf *times)
{
    mcfs_connection   *mc;
    int status;
    char *file;

    if (!(file = mcfs_get_path (&mc, path)))
	return -1;

    status = 0;
    if (mc->version >= 2) {
	char   abuf[BUF_SMALL];
	char   mbuf[BUF_SMALL];
	long   atime, mtime;

	atime = (long) times->actime;
	mtime = (long) times->modtime;

	g_snprintf (abuf, sizeof(abuf), "%lx", atime);
	g_snprintf (mbuf, sizeof(mbuf), "%lx", mtime);

	rpc_send (mc->sock, RPC_INT, MC_UTIME,
			RPC_STRING, file,
			RPC_STRING, abuf,
			RPC_STRING, mbuf,
			RPC_END);
	status = mcfs_handle_simple_error (mc->sock, 0);
    
    }
    g_free (file);
    return (status);
}

static int mcfs_readlink (vfs *me, char *path, char *buf, int size)
{
    char *remote_file, *stat_str;
    int  status, error;
    mcfs_connection *mc;

    if (!(remote_file = mcfs_get_path (&mc, path)))
	return -1;

    rpc_send (mc->sock, RPC_INT, MC_READLINK, RPC_STRING, remote_file, RPC_END);
    g_free (remote_file);
    if (!rpc_get (mc->sock, RPC_INT, &status, RPC_INT, &error, RPC_END))
	return the_error (-1, EIO);

    if (is_error (status, errno))
	return -1;

    if (!rpc_get (mc->sock, RPC_STRING, &stat_str, RPC_END))
	return the_error (-1, EIO);
    
    strncpy (buf, stat_str, size);
    g_free (stat_str);
    return strlen (buf);
}

static int mcfs_unlink (vfs *me, char *path)
{
    return mcfs_rpc_path (MC_UNLINK, path);
}

static int mcfs_symlink (vfs *me, char *n1, char *n2)
{
    return mcfs_rpc_two_paths (MC_SYMLINK, n1, n2);
}

static int mcfs_rename (vfs *me, char *a, char *b)
{
    return mcfs_rpc_two_paths (MC_RENAME, a, b);
}

static int mcfs_chdir (vfs *me, char *path)
{
    char *remote_dir;
    mcfs_connection *mc;
    int  status, error;

    if (!(remote_dir = mcfs_get_path (&mc, path)))
	return -1;

    rpc_send (mc->sock, RPC_INT, MC_CHDIR, RPC_STRING, remote_dir, RPC_END);
    g_free (remote_dir);
    if (!rpc_get (mc->sock, RPC_INT, &status, RPC_INT, &error, RPC_END))
	return the_error (-1, EIO);
    
    if (is_error (status, error))
	return -1;
    return 0;
}

static int mcfs_lseek (void *data, off_t offset, int whence)
{
    mcfs_handle *info = (mcfs_handle *) data;
    int handle, sock;

    sock = info->conn->sock; 
    handle = info->handle;

    rpc_send (sock,
	      RPC_INT, MC_LSEEK,
	      RPC_INT, handle,
	      RPC_INT, offset,
	      RPC_INT, whence,
	      RPC_END);
    return mcfs_handle_simple_error (sock, 1);
}

static int mcfs_mknod (vfs *me, char *path, int mode, int dev)
{
    return mcfs_rpc_path_int_int (MC_MKNOD, path, mode, dev);
}

static int mcfs_mkdir (vfs *me, char *path, mode_t mode)
{
    return mcfs_rpc_path_int (MC_MKDIR, path, mode);
}

static int mcfs_rmdir (vfs *me, char *path)
{
    return mcfs_rpc_path (MC_RMDIR, path);
}

static int mcfs_link (vfs *me, char *p1, char *p2)
{
    return mcfs_rpc_two_paths (MC_LINK, p1, p2);
}

/* We do not free anything right now: we free resources when we run
 * out of them
 */
static vfsid mcfs_getid (vfs *me, char *p, struct vfs_stamping **parent)
{
    *parent = NULL;
    
    return (vfsid) -1;
}

static int mcfs_nothingisopen (vfsid id)
{
    return 0;
}

static void mcfs_free (vfsid id)
{
    /* FIXME: Should not be empty */
}

/* Gives up on a socket and reopnes the connection, the child own the socket
 * now
 */
static void
my_forget (char *path)
{
    char *host, *user, *pass, *p;
    int  port, i, vers;

    if (strncmp (path, "/#mc:", 5))
	return;
    
    path += 5;
    if (path[0] == '/' && path[1] == '/')
	path += 2;

    if ((p = mcfs_get_host_and_username (path, &host, &user, &port, &pass)) == 0) {
	g_free (host);
	g_free (user);
	if (pass)
	    wipe_password (pass);
	return;
    }
    for (i = 0; i < MCFS_MAX_CONNECTIONS; i++){
	if ((strcmp (host, mcfs_connections [i].host) == 0) &&
	    (strcmp (user, mcfs_connections [i].user) == 0) &&
	    (port == mcfs_connections [i].port)){

	    /* close socket: the child owns it now */
	    close (mcfs_connections [i].sock);

	    /* reopen the connection */
	    mcfs_connections [i].sock = mcfs_open_tcp_link (host, user, &port, pass, &vers);
	}
    }
    g_free (p);
    g_free (host);
    g_free (user);
    if (pass)
	wipe_password (pass);
}

static int 
mcfs_setctl (vfs *me, char *path, int ctlop, char *arg)
{
    switch (ctlop) {
        case MCCTL_FORGET_ABOUT:
	    my_forget(path);
	    return 0;
    }
    return 0;
}

vfs vfs_mcfs_ops = {
    NULL,	/* This is place of next pointer */
    "Midnight Commander's private remote filesystem",
    F_NET,	/* flags */
    "mc:",	/* prefix */
    NULL,	/* data */
    0,		/* errno */
    NULL,
    NULL,
    mcfs_fill_names,
    NULL,

    mcfs_open,
    mcfs_close,
    mcfs_read,
    mcfs_write,
    
    mcfs_opendir,
    mcfs_readdir,
    mcfs_closedir,
    NULL,
    NULL,

    mcfs_stat,
    mcfs_lstat,
    mcfs_fstat,

    mcfs_chmod,
    mcfs_chown,
    mcfs_utime,

    mcfs_readlink,
    mcfs_symlink,
    mcfs_link,
    mcfs_unlink,

    mcfs_rename,
    mcfs_chdir,
    mcfs_errno,
    mcfs_lseek,
    mcfs_mknod,
    
    mcfs_getid,
    mcfs_nothingisopen,
    mcfs_free,
    
    NULL,
    NULL,

    mcfs_mkdir,
    mcfs_rmdir,
    NULL,
    mcfs_setctl

MMAPNULL
};
