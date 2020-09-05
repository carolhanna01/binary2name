/*------------------------->  ANSI C - sourcefile  <-------------------------*/
/* Copyright (C) 199x by International Computer Science Institute            */
/* This file is part of the GNU Sather library. It is free software; you may */
/* redistribute  and/or modify it under the terms of the GNU Library General */
/* Public  License (LGPL)  as published  by the  Free  Software  Foundation; */
/* either version 3 of the license, or (at your option) any later version.   */
/* This  library  is distributed  in the  hope that it will  be  useful, but */
/* WITHOUT ANY WARRANTY without even the implied warranty of MERCHANTABILITY */
/* or FITNESS FOR A PARTICULAR PURPOSE. See Doc/LGPL for more details.       */
/* The license text is also available from:  Free Software Foundation, Inc., */
/* 59 Temple Place, Suite 330, Boston, MA 02111-1307 USA                     */
/*------------>  Please email comments to <bug-sather@gnu.org>  <------------*/

/* Author: David Bailey <dbailey@icsi.berkeley.edu>
 *
 * January 1993 - Sather 0.2 version
 * April 1995 - Sather 1.0 version
 *
 * Support for SOCKET class.
 * Designed for SunOS 4 and 5.
 */

#include <sather.h>

#include <stdio.h>
#include <errno.h>
#include <sys/errno.h>
#include <sys/types.h>
#include <sys/socket.h>
#include <sys/un.h>
#include <netinet/in.h>
#include <netdb.h>
#include <sys/time.h>
#include <sys/resource.h>
#include <signal.h>
#include <unistd.h>

/***********************************************************************/
/* Routine definitions in this file: */

int make_initiating_socket_unix(char *);
int make_connecting_socket_unix(char *);
int make_initiating_socket_inet(int);
int make_connecting_socket_inet(char *,int);
void close_socket(int);

int receive_len(int);
int receive_str(int,char *,int);
int send_str(int,char *,int);

int able_to_read(int);
int able_to_write(int);
int is_healthy(int);

void block_until_can_read(int);
void block_until_can_write(int);

int min_port_num(void);
void ignore_broken_pipe_signals(void);

int C_SOCKET_nconnatt(void);

/* Private: */

int set_reuseaddr(int);
void unlink_server_and_exit();
void socket_warn_errno(char *);
void socket_warn(char *);


/***********************************************************************/
/* Routines and globals used by this file: */


/***********************************************************************/
/* Globals declared by this file: */
  
static struct sockaddr_un Unix_server_addr;
   /* global so Cntl-C handler can get to it */


/***********************************************************************/
/* Routine definitions: */

/***********************************************************************/
/* Create a Unix-domain server socket named `server_name', wait for a
 * connection to it, then return the resulting socket id.
 * Return -1 if a problem occurs.
 */
int make_initiating_socket_unix(char *server_name)
{
  int server_id;
  int this_end_id;
  struct sockaddr_un that_end_addr;
  int that_end_addr_size = sizeof(struct sockaddr_un);
  void (*old_int_handler)();
  void (*old_term_handler)();

  if ((server_id = socket(PF_UNIX, SOCK_STREAM, 0)) < 0) 
    {
      socket_warn_errno("creation of server socket");
      return(-1);
    }

  /* prepare binding name/address */
  Unix_server_addr.sun_family = AF_UNIX;
  strcpy(Unix_server_addr.sun_path, server_name);  /* no size checking done! */

  /* temporarily install Control-C and 'kill' handlers to unlink the
     current server socket */

  old_int_handler = signal(SIGINT, unlink_server_and_exit); 
  if ((int)old_int_handler == -1)
    {
      socket_warn_errno("attempt to redirect SIGINT signal");
      close(server_id);
      unlink(Unix_server_addr.sun_path);
      return(-1);
    }

  old_term_handler = signal(SIGTERM, unlink_server_and_exit);
  if ((int)old_term_handler == -1)
    {
      socket_warn_errno("attempt to redirect SIGTERM signal");
      close(server_id);
      unlink(Unix_server_addr.sun_path);
      return(-1);
    }

  /* Note: SIGINT/TERM right in here will cause the unlink() call in
     unlink_server_and_exit to fail ... hope this is ok. */
  
  if (bind(server_id, (struct sockaddr *)&Unix_server_addr, 
	   sizeof(Unix_server_addr)) < 0)
    {
      /* Omit error message if address is already in use. */
      if (errno != EADDRINUSE) socket_warn_errno("binding of server socket");
      close(server_id);
      return(-1);
    }

  if (listen(server_id, 1) < 0)    /* 1 is limit on backlog */
    {
      socket_warn_errno("listen call on server socket");
      close(server_id);
      unlink(Unix_server_addr.sun_path);
      return(-1);
    }

  if ((this_end_id = accept(server_id, (struct sockaddr *)&that_end_addr,
			    &that_end_addr_size))
      < 0)
    {
      socket_warn_errno("accepting connection on server socket");
      close(server_id);
      unlink(Unix_server_addr.sun_path);
      return(-1);
    }
  
  /* re-install old handlers */

  old_int_handler = signal(SIGINT, old_int_handler);
  if ((int)old_int_handler == -1)
    {
      socket_warn_errno("attempt to restore original SIGINT handler");
      close(server_id);
      close(this_end_id);
      unlink(Unix_server_addr.sun_path);
      return(-1);
    }

  old_term_handler = signal(SIGTERM, old_term_handler);
  if ((int)old_term_handler == -1)
    {
      socket_warn_errno("attempt to restore original SIGTERM handler");
      close(server_id);
      close(this_end_id);
      unlink(Unix_server_addr.sun_path);
      return(-1);
    }
  
  if (close(server_id) < 0)
    {
      /* perhaps this is a little overcautious */
      socket_warn_errno("trying to close server socket");
      close(this_end_id);
      return(-1);
    }
  
  unlink(Unix_server_addr.sun_path); /* don't error-check since person might */
                                     /* delete file on their own */

  return(this_end_id);
}

/***********************************************************************/
/* Create a socket connecting to an existing Unix-domain socket named 
 * server_name, and return the new socket's id.
 * Make up to C_SOCKET_nconnatt() attempts to connect
 * (one per second) to allow for slow initiators.
 * Return -1 if a problem occurs.
 */
int make_connecting_socket_unix(char *server_name)
{
  int this_end_id;
  struct sockaddr_un server_addr;
  int connected, num_failures;
  
  if ((this_end_id = socket(PF_UNIX, SOCK_STREAM, 0)) < 0) 
    {
      socket_warn_errno("socket creation");
      return(-1);
    }

  /* prepare binding name/address */
  server_addr.sun_family = AF_UNIX;
  strcpy(server_addr.sun_path, server_name);   /* should check size!! */

  connected = 0;
  num_failures = 0;
  while ((num_failures < C_SOCKET_nconnatt())
	 && (!connected))
    {
      if (connect(this_end_id, (struct sockaddr *)&server_addr,
		  sizeof(server_addr)) < 0) 
	{
	  num_failures++;
	  sleep(1);
	}
      else
	connected = 1;
    }

  if (connected)
    {
      return(this_end_id);
    }
  else
    {
      /* suppress message since this is probably "normal",
       * i.e., user tried to connect before initiating. 
       */
      /* socket_warn_errno("socket connect call, tried multiple times"); */
      close(this_end_id);
      return(-1);
    }
}

/***********************************************************************/
/* Create an Internet-domain server socket on port `port', wait for a
 * connection to it, then return the resulting socket id.
 * Return -1 if a problem occurs.
 */
int make_initiating_socket_inet(int port)
{
  int server_id;
  int this_end_id;
  struct sockaddr_in inet_server_addr, that_end_addr;
  int that_end_addr_size = sizeof(struct sockaddr_in);

  if ((server_id = socket(PF_INET, SOCK_STREAM, 0)) < 0) 
    {
      socket_warn_errno("creation of server socket");
      return(-1);
    }

  /* Configure to free up local address after creation. */
  if (set_reuseaddr(server_id) < 0) 
    {
      socket_warn_errno("setting reuseaddr on server socket");
      close(server_id);
      return(-1);
    }

  /* prepare own binding name/address, bind, then become a server */
  inet_server_addr.sin_family = AF_INET;
  inet_server_addr.sin_port = htons((u_short)port);
  inet_server_addr.sin_addr.s_addr = htonl(INADDR_ANY);

  if (bind(server_id, (struct sockaddr *)(&inet_server_addr),
	   sizeof(inet_server_addr)) < 0)
    {
      /* Omit error message if address is already in use. */
      if (errno != EADDRINUSE) socket_warn_errno("binding of server socket");
      close(server_id);
      return(-1);
    }

  if (listen(server_id, 1) < 0)    /* 1 is limit on backlog */
    {
      socket_warn_errno("listen call on server socket");
      close(server_id);
      return(-1);
    }

  if ((this_end_id = accept(server_id, (struct sockaddr *)&that_end_addr,
			    &that_end_addr_size))
      < 0)
    {
      socket_warn_errno("accepting connection on socket");
      close(server_id);
      return(-1);
    }

  if (close(server_id) < 0)
    {
      /* perhaps this is a little overcautious */
      socket_warn_errno("trying to close server socket");
      close(this_end_id);
      return(-1);
    }
  
  return(this_end_id);
}

/***********************************************************************/
/* Create a socket connecting to an existing Internet-domain socket on
 * port `server_port' of machine `server_hostname', and return the new 
 * socket's id.
 * Make up to C_SOCKET_nconnatt() attempts to connect
 * (one per second) to allow for slow initiators.
 * Return -1 if a problem occurs.
 */
int make_connecting_socket_inet(char *server_hostname, int server_port)
{
  int this_end_id;
  struct sockaddr_in own_addr, server_addr;
  struct hostent *he;
  int connected, num_failures;
  
  /* prepare server's name/address, and connect */
  server_addr.sin_family = AF_INET;
  server_addr.sin_port = htons((u_short)server_port);

  if ((he = gethostbyname(server_hostname)) == NULL)
    {
      /* Better suppress message since could be typo in machine name */
      char *msgbuf = (char *)malloc(80+strlen(server_hostname));
      sprintf(msgbuf, "failed to get host by name (%s)", server_hostname);
      /* socket_warn(msgbuf); */
      free(msgbuf);
      return(-1);
    }
  if (he->h_addrtype != AF_INET)
    {
      socket_warn("host entry is not of type AF_INET");
      return(-1);
    }
  if (he->h_addr_list == NULL)
    {
      socket_warn("no address list returned");
      return(-1);
    }
  if (*(he->h_addr_list) == NULL)
    {
      socket_warn("address list is empty");
      return(-1);
    }
  memcpy((char *)&server_addr.sin_addr, 
	 *(he->h_addr_list),
	 sizeof(he->h_length));

  connected = 0;
  num_failures = 0;
  while ((num_failures < C_SOCKET_nconnatt())
	 && (!connected))
    {
      if ((this_end_id = socket(PF_INET, SOCK_STREAM, 0)) < 0) 
	{
	  socket_warn_errno("socket creation");
	  return(-1);
	}

      if (connect(this_end_id, (struct sockaddr *)&server_addr,
		  sizeof(server_addr)) < 0) 
	{
	  close(this_end_id);
	  num_failures++;
	  sleep(1);
	}
      else
	connected = 1;
    }

  if (connected)
    {
      return(this_end_id);
    }
  else
    {
      /* suppress message since this is probably "normal",
       * i.e., user tried to connect before initiating. 
       */
      /* socket_warn_errno("socket connect call, tried multiple times"); */
      return(-1);
    }
}

/***********************************************************************/
/* Close the socket.
 */
void close_socket(int fd)
{
  close(fd);
}

/***********************************************************************/
/* Waits for and returns a string-length arriving at socket sock.
 * If any problem occurs, return -1.
 */
int receive_len(int sock)
{
  int bytes_received;
  long int s_size;  /* not including terminator */
  int real_s_size;  /* s_size converted from network byte order to 
		       host byte order, and back to int */
  
  if ((bytes_received = recv(sock, (char *)&s_size, sizeof(s_size), 0))
      < sizeof(s_size))
    {
      /* Suppress message since other end probably died. */
      /* Probably need to fix so message appears when truly anomalous. */
      /* something like:  if (errno != EPIPE) */
      /* socket_warn_errno("receiving string length"); */
      return(-1);
    }
  else
    {
      real_s_size = (int)ntohl(s_size);
      /* printf("Length received: %d\n", real_s_size); */
      return(real_s_size);
    }
}

/***********************************************************************/
/* Waits for and reads 's_size' characters from socket sock and puts them
 * into the string 's'.  Return 1 if successful, 0 if failure.
 */
int receive_str(int sock, char *s, int s_size)
{
  int bytes_received;
  int chars_received = 0;
  /* int times_through_loop = 0; */
  
  while (chars_received < s_size)
    {
      bytes_received = recv(sock, s+chars_received, s_size-chars_received, 0);
      if (bytes_received <= 0)
	{
	  /* Suppress message since other end probably died. */
	  /* Probably need to fix so message appears when truly anomalous. */
	  /* something like:  if (errno != EPIPE) */
	  /* socket_warn_errno("receiving string contents"); */
	  return(0);
	}
      chars_received += bytes_received;
      /* times_through_loop++; */
    }
  /* printf("String received in %d pieces: \"%s\"\n",times_through_loop,s); */
  return(1);
}

/***********************************************************************/
/* Send a string over socket `sock', returning 1 if successful or
 * a 0 if an error occurred.
 */
int send_str(int sock, char *s, int s_size)
{
  long int net_s_size; /* converted to network byte order from host order */

  net_s_size = htonl((long int)s_size);
  if (send(sock, (char *)&net_s_size, sizeof(net_s_size), 0) < 0)
    {
      /* Suppress message since other end probably died. */
      /* Probably need to fix so message appears when truly anomalous. */
      /* something like:  if (errno != EPIPE)  */
      /* socket_warn_errno("sending string's length"); */
      return(0);
    }

  if (s_size == 0) return(1);
  if (send(sock, s, s_size, 0) < 0)
    {
      /* Suppress message since other end probably died. */
      /* Probably need to fix so message appears when truly anomalous. */
      /* something like:  if (errno != EPIPE) */
      /* socket_warn_errno("sending string's contents"); */
      return(0);
    }
  /* printf("String after send: \"%s\"\n",s); */

  return(1);
}

/***********************************************************************/
/* Returns 1 if there is something in fd's input buffer (fd may be
 * a socket or file id), indicating that a recv (or read) operation
 * would not block; else, it returns 0.
 * Returns 0 if any sort of problem.
 */
int able_to_read(int fd)
{
  int table_size;
  struct timeval zero_wait;
  fd_set readlist;
  int status;
  struct rlimit nfds;

  if ((status = getrlimit(RLIMIT_NOFILE, &nfds)) < 0)
    {
      socket_warn_errno("getrlimit in able_to_read");
      return(0);
    }

  zero_wait.tv_sec = 0L;
  zero_wait.tv_usec = 0L;

  FD_ZERO(&readlist);
  FD_SET(fd, &readlist);

  status = select(nfds.rlim_max, &readlist, NULL, NULL, &zero_wait);
  switch (status)
    {
    case 0: return(0);
    case 1: return(1);
    default:
      {
	socket_warn_errno("select in able_to_read"); 
	return(0);
      }
    }
}
      
/***********************************************************************/
/* Returns 1 if fd is writable (fd may be a socket, or (maybe) a file id),
 * indicating that a send (or, (maybe) write) operation would not block.
 * Returns 0 if any sort of problem.
 */
int able_to_write(int fd)
{
  int table_size;
  struct timeval zero_wait;
  fd_set writelist;
  int status;
  struct rlimit nfds;

  if ((status = getrlimit(RLIMIT_NOFILE, &nfds)) < 0)
    {
      socket_warn_errno("getrlimit in able_to_write");
      return(0);
    }

  zero_wait.tv_sec = 0L;
  zero_wait.tv_usec = 0L;

  FD_ZERO(&writelist);
  FD_SET(fd, &writelist);

  status = select(nfds.rlim_max, NULL, &writelist, NULL, &zero_wait);
  switch (status)
    {
    case 0: return(0);
    case 1: return(1);
    default:
      {
	socket_warn_errno("select in able_to_write"); 
	return(0);
      }
    }
}
      
/***********************************************************************/
/* Returns 1 if fd does not have a pending exception (fd may be a socket,
 * or (maybe) a file id), indicating that a send (or, (maybe) write) 
 * operation would not crash the program!
 * Returns 0 if any sort of problem.
 */
int is_healthy(int fd)
{
  int table_size;
  struct timeval zero_wait;
  fd_set exceptlist;
  int status;
  struct rlimit nfds;

  if ((status = getrlimit(RLIMIT_NOFILE, &nfds)) < 0)
    {
      socket_warn_errno("getrlimit in is_healthy");
      return(0);
    }

  zero_wait.tv_sec = 0L;
  zero_wait.tv_usec = 0L;

  FD_ZERO(&exceptlist);
  FD_SET(fd, &exceptlist);

  status = select(nfds.rlim_max, NULL, NULL, &exceptlist, &zero_wait);
  switch (status)
    {
    case 0: return(1);
    case 1: return(0);
    default:
      {
	socket_warn_errno("select in is_healthy"); 
	return(0);
      }
    }
}

/***********************************************************************/
/* Blocks in a non-busy wait until socked 'fd' can read without blocking.
 */
void block_until_can_read(int fd)
{
  int table_size;
  fd_set readlist;
  int status;
  struct rlimit nfds;

  if ((status = getrlimit(RLIMIT_NOFILE, &nfds)) < 0)
    {
      socket_warn_errno("getrlimit in block_until_can_read");
      return;
    }

  FD_ZERO(&readlist);
  FD_SET(fd, &readlist);

  status = select(nfds.rlim_max, &readlist, NULL, NULL, NULL);
  switch (status)
    {
    case 1: return;
    default:
      {
	socket_warn_errno("select in block_until_can_read"); 
	return;
      }
    }
}
      
/***********************************************************************/
/* Blocks in a non-busy wait until socked 'fd' can be written to 
 * without blocking.
 */
void block_until_can_write(int fd)
{
  int table_size;
  fd_set writelist;
  int status;
  struct rlimit nfds;

  if ((status = getrlimit(RLIMIT_NOFILE, &nfds)) < 0)
    {
      socket_warn_errno("getrlimit in block_until_can_write");
      return;
    }

  FD_ZERO(&writelist);
  FD_SET(fd, &writelist);

  status = select(nfds.rlim_max, NULL, &writelist, NULL, NULL);
  switch (status)
    {
    case 1: return;
    default:
      {
	socket_warn_errno("select in block_until_can_write"); 
	return;
      }
    }
}
      
/***********************************************************************/
/* Return the smallest allowable port number which an Internet socket
 * can use.
 */
int min_port_num(void) 
{
  return(IPPORT_RESERVED);
}

/***********************************************************************/
/* Sets up a signal handler for SIGPIPE, to prevent "Broken pipe"
 * crashes when writing to a newly-disconnected socket.
 */
void ignore_broken_pipe_signals(void)
{
  int status;
  status = (int)signal(SIGPIPE, SIG_IGN);
  /* printf("*** First time, old SIGPIPE handler was %d\n",status); */
  if (status < 0)
    {
      socket_warn_errno("setting up ignoring of SIGPIPE signals (will \
ignore error)");
    }
  /* status = (int)signal(SIGPIPE, SIG_IGN); */
  /* printf("*** Second time, old SIGPIPE handler was %d\n",status); */
}

/***********************************************************************/
/* Private routines: */

/***********************************************************************/
/* Set REUSEADDR in socket, to prevent clogging up portnumbers between
 * program runs.
 */
int set_reuseaddr(int sock_id)
{
  int on=1;
  return setsockopt(sock_id, SOL_SOCKET, SO_REUSEADDR, (char *)&on, sizeof on);
}

/***********************************************************************/
/* SIGINT/SIGTERM handler.  Unlinks Server_addr.sa_data and exits. 
 */
void unlink_server_and_exit(void)
{
  /* This may fail if file is not created yet, but hopefully we don't care. */
  unlink(Unix_server_addr.sun_path);
  /* Message used during development only...*/
  /* fprintf(stderr, "Exiting via unlink_server_and_exit(), so no stray \
server sockets will be lying around.\n"); */
  exit(1);
}

/***********************************************************************/
/* Print "unexpected" error message including errno to stderr.
 */
void socket_warn_errno(char *msg)
{
  fprintf(stderr, "socket_support.c: unexpected error (%d) at: %s.\n", 
	  errno, msg);
}

/***********************************************************************/
/* Print "unexpected" error message to stderr.
 */
void socket_warn(char *msg)
{
  fprintf(stderr, "SOCKET_SUPPORT: unexpected error at: %s.\n", msg);
}


