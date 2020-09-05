/*
 * EDMA: Entorno de Desarrollo Modular y Abierto
 * Object Oriented and Componetware Framework
 * Copyright (C) 1998, 2002, 2003, 2004, 2005, 2006, 2007, 2008, 2010
 *    David Martínez Oliveira
 *
 * This file is part of EDMA.
 *
 * EDMA is free software: you can redistribute it and/or modify
 * it under the terms of the GNU Lesser General Public License as published by
 * the Free Software Foundation, either version 3 of the License, or
 * (at your option) any later version.
 *
 * EDMA is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU Lesser General Public License for more details.
 *
 * You should have received a copy of the GNU Lesser General Public License
 * along with EDMA.  If not, see <http://www.gnu.org/licenses/>.
 */

#include <stdio.h>
#include <stdlib.h>
#include <unistd.h>
#include <errno.h>
#include <string.h>
#include <sys/types.h>
#include <sys/stat.h>
#include <signal.h>
#include <sys/un.h>
#include <fcntl.h>

#include <stdarg.h>

#include <sys/ipc.h>
#include <sys/shm.h>
#include <sys/file.h>

/* Network Specific */
#include <sys/time.h>
#include <sys/socket.h>
#include <netinet/in.h>
#include <netdb.h>
#include <arpa/inet.h>

#include <syslog.h>

#include "const.h"

#define SOCK_PATH "/tmp/sally_socket"
//#define LOCK_FILE "/var/run/emerald.pid"
#define LOCK_FILE "/tmp/sally.pid"
#define LOCKMODE (S_IRUSR|S_IWUSR|S_IRGRP|S_IROTH)

#define BUF_INC 512

typedef struct client_t
{
  int    s; /* Socket */
  char   *buffer;
  int    off;
  int    len;
} CLIENT;

static CLIENT *c = NULL;
static int    n_clients = 0;

static int *shm_id = NULL;
static int n_shm = 0;
static int c_shm = 0;

static int _daemon = 0;


/* Daemon thing */
int
eprintf (char *fmt, ...)
{
  int     res = 0;
  va_list p;

  va_start (p,fmt);
  if (_daemon)
    vsyslog (LOG_INFO, fmt, p);
  else
    {
      res = vprintf (fmt, p);
      printf ("\n");
    }

  va_end(p);
  return res;
}


int
eprintf_err (char *id, char *fmt, ...)
{
  int     res = 0;
  va_list p;

  va_start (p,fmt);
  if (_daemon)
    {
      vsyslog (LOG_ERR, fmt, p);
      syslog (LOG_ERR, "%s: %m", id);
    }
  else
    {
      res = vfprintf (stderr, fmt, p);
      perror (id);
    }

  va_end(p);
  return res;
}


int
daemon_one_instance ()
{
  int  fd, len;
  char pid_str[32];

  /* Open the lock file */
  if ((fd = open (LOCK_FILE, O_RDWR | O_CREAT, LOCKMODE)) < 0)
    {
      eprintf_err ("open:", "Fatal: Cannot create lock file (%s)", LOCK_FILE);
      exit (1);
    }
  /* Check locking state */

  if ((flock (fd, LOCK_EX | LOCK_NB)) < 0)
    {
      if (errno == EWOULDBLOCK)
	{
	  eprintf ("Sally already running...");

	  close (fd);
	  exit (1);
	}
      eprintf_err ("flock:", "Fatal: Cannot lock file.");

      exit (1);

    }


  /* Everything is OK, then we save our pid */
  ftruncate (fd, 0);
  len = snprintf (pid_str, 32, "%ld", (long) getpid());
  write (fd, pid_str, len);

  eprintf ("Run, Sally run!");
  
  return 0;
}

void 
daemon_closeall (int fd)
{
  int fdlimit = sysconf(_SC_OPEN_MAX);
  
  while (fd < fdlimit)
    close(fd++);
}

int
daemon_go ()
{
  /* Remove UMASK */
  umask(0);

  /* Deattch console -> session leader */
  switch (fork())
    {
    case 0: break;
    case -1: 
      {
	perror ("fork:");
	return -1;
      }
    default: _exit(0);          /* exit the original process */
    }

  setsid();
  /* fork again for system V */
  switch (fork())
    {
    case 0:  break;
    case -1: 
      {
	perror ("fork:");
	return -1;
      }
    default: _exit(0);
    }
  /* Change to root directory */
  chdir ("/");

  /* Now we close all file descriptors and set standard
   * input/output/error to /dev/null
   */
  daemon_closeall (0);
  open("/dev/null",O_RDWR);
  dup(0); dup(0);

  /* For daemons we log to syslog */
  openlog("sally", LOG_CONS, LOG_DAEMON);
  syslog (LOG_INFO, "sally runing around.");

  _daemon = 1;

  return 0;
}

int
find_id (int id)
{
  int   i;
  
  for (i = 0; i < n_shm; i++)
    if (shm_id[i] == id) return i;
  
  return -1;
}


int
find_shm_hole ()
{
  int   i;

  for (i = 0; i < c_shm; i++)
    if (shm_id[i] == -1) return i;

  return -1;
}


int
find_hole ()
{
  int   i;
  for (i = 0; i < n_clients; i++)
    if (c[i].s == -1) return i;
  return -1;
}

int
add_client (int s)
{
  int  indx;

  if ((indx = find_hole ()) < 0)
    {
      /* Reallocate client vector */
      if ((c = realloc (c, sizeof (CLIENT) * (n_clients+1))) == NULL)
	{
	  eprintf_err ("realloc:", "Cannot allocate memory for new client...");
	  close (s);

	  return -1;
	}
      indx = n_clients;
      if ((c[indx].buffer = malloc (BUF_INC)) == NULL)
	{
	  eprintf_err ("malloc:", "Cannot allocate memory for client buffer");
	  c[indx].s = -1;
	  return -1;
	}
      n_clients ++;
    }
  c[indx].s = s;

  c[indx].len = BUF_INC;
  memset (c[indx]. buffer, 0, c[indx].len);
  c[indx].off = 0;
  
  return indx;
}


key_t
salloc (int size, char *n) 
{
  int     a, shmflg, f, indx;
  char    name[1024];
  key_t   k;
  struct stat buf;
  
  /* We create a file for filemapping shared memory */
  snprintf (name, 1024, "%s/%s", TMPDIR, n);
  if (stat (name, &buf) < 0)
    {
      f = open (name, O_WRONLY | O_CREAT | O_TRUNC);
      if (f == -1) 
	{
	  if (errno != EEXIST) 
	    {
	      perror ("Create File");
	      return -1;
	    }
	}
      
      chmod (name, S_IRWXU | S_IRWXG | S_IRWXO);
    }
  else
    f = -1;

  if ((k = ftok (name, 0)) == -1)
    {
      perror ("[edma_salloc] (ftok)");
      return -1;
    }

  shmflg = 0;

  a = shmget (k,size, IPC_CREAT | 0777);

  eprintf ("Getting shared memory with key: %d "
	   "(id: %d str: '%s' size:%d)\n",  k, a, n, size);

  if ((a == -1) && (errno != EEXIST)) 
    {
      eprintf_err ("shmget:", "[salloc] ERROR in shared block : %s "
		   "(key:%d|size:%d)", name, k, size);

      return -1;
    }
  if (f >=0) close (f);

  /* Keep track of new created segment */  
  if ((indx = find_id (a)) < 0)
    {
      // Look for a hole
      if ((indx = find_shm_hole ()) < 0)
	{
	  /* If not found... reallocate memory*/
	  //printf ("No hole found... Reallocating vector\n");
	  /* ID not found... add to the list */
	  if ((shm_id = realloc (shm_id, sizeof (int) * (c_shm + 1))) == NULL)
	    {
	      eprintf_err ("realloc:", "Cannot allocate memory for shared ids");
	      /* XXX: Here, we should dump the current list */
	      exit (1);
	    }
	  /* Add item at the end of the vector */
	  indx = c_shm;
	  c_shm++;
	}
      n_shm++;  
    }
  shm_id[indx] = a;


  return k;
}


void
_process_cmd (int i, char *aux)
{
  int    size, len;
  char   key[1024], buffer[1024];
  key_t _the_key;

  /* Parse command */
  if (strncasecmp (aux, "SALLOC", 6) == 0)
    {
      sscanf (aux, "SALLOC %d %s", &size, key);
      
      _the_key = salloc (size, key);
      len = snprintf (buffer, 1024, "+SALLOC %d\n", _the_key);
    }

  if ((write (c[i].s, buffer, len)) < 0)
    {
      eprintf ("Removing client %d from list", i);

      close (c[i].s);
      c[i].s = -1;
      c[i].off = 0;
      c[i].len = 0;
      n_clients--;
    }
  return;
}

int
process_client (int i)
{
  int   r;
  int   process = 1;
  char  *aux, *aux1;

  if ((i < 0) || (i > n_clients))
    {
      eprintf_err ("internal:", "Invalid index %d. "
		   "Client processing aborted\n", i);

      return -1;
    }
  /* Read data available from network */
  /* FIXME: For now we consider the client buffer static...*/
  if ((r = read (c[i].s, c[i].buffer + c[i].off, c[i].len - c[i].off)) <= 0)
    {
      eprintf_err ("read:", "Closing connection for client %d\n", i);

      /* Close this connection */
      close (c[i].s);
      c[i].s = -1;
      c[i].off = 0;
      c[i].len = 0;
      n_clients--;
      return -1;
    }
  c[i].off += r;

  /* Process commands */
  aux1 = c[i].buffer;
  while (process)
    {
      if ((aux = strchr (aux1, '\n')) == NULL)
	{
	  /* No more commands to process... finishing */
	  process = 0;
	  continue;
	}
      *aux = 0;
      _process_cmd (i, aux1);
      c[i].len -= (strlen (aux1));
      aux1 = aux + 1;
    }
  /* Pack buffer */
  memmove (c[i].buffer, aux1, c[i].len);
  c[i].off = 0;
  fflush (NULL);
  return 0;

}

#define GC_LOOP 10

static int gc_cnt = 0;
int
sally_gc ()
{
  struct shmid_ds  ds;
  int              i, did_something;

  did_something = 0;
  if (n_shm == 0) return 0;
  for (i = 0; i < GC_LOOP; i++)
    {
      gc_cnt ++;
      if (gc_cnt >= c_shm) gc_cnt = 0;
      if (shm_id[gc_cnt] < 0) continue;
      /* Check attached clients and remove */
      if ((shmctl (shm_id[gc_cnt], IPC_STAT, &ds)) < 0)
	{
	  eprintf_err ("shmctl:", "Error accessing is %d. "
		       "Removing from list.\n", 
		       shm_id[gc_cnt]);

	  shm_id[gc_cnt] = -1;
	  n_shm--;
	  continue;
	}

      if (ds.shm_nattch == 0)
	{
	  eprintf ("Garbage Collector: Deleting id %d\n", 
		   shm_id[gc_cnt]);

	  /* Free memory */
	  if ((shmctl (shm_id[gc_cnt], IPC_RMID, &ds)) < 0)
	    {
	      eprintf_err ("shmctl:", "Cannot delete id %d", shm_id[gc_cnt]);

	      continue;
	    }
	  shm_id[gc_cnt] = -1;
	  n_shm--;
	  did_something = 1;
	}
    }

  if (did_something)
    {
      eprintf ("STATS: %d clients and %d shared segments "
	      "(cap: %d. gc_cnt: %d)\n",
	      n_clients, n_shm, c_shm, gc_cnt);

    }

  return 0;
}

void
handler  (int s)
{
  unlink (SOCK_PATH);
  unlink (LOCK_FILE);

  eprintf ("Sally stopping");


  exit (1);
}

int 
main (int argc, char *argv[])
{
  fd_set             rfds;
  struct timeval     tv;
  int                i, max, n_res, len, j;
  int                loop4ever = 1;
  struct sockaddr_in client;
  socklen_t          sa_len = sizeof(struct sockaddr_in);
  int s, s2;
  struct sockaddr_un local;


  /* First of all check if some other instance is already running */
  if (argc > 1 && !strcasecmp (argv[1], "--daemon"))
    {
      if (daemon_go () < 0)
	{
	  fprintf (stderr, "Cannot start in daemon mode...\n");
	  exit (1);
	}
    }

  daemon_one_instance ();

  signal (SIGQUIT, handler);
  signal (SIGKILL, handler);
  signal (SIGINT, handler);
  signal (SIGTERM, handler);
  signal (SIGSEGV, handler);



  /* Create socket */
  if ((s = socket(AF_UNIX, SOCK_STREAM, 0)) == -1) 
    {
      perror("socket");
      exit(1);
    }

  local.sun_family = AF_UNIX;
  strcpy(local.sun_path, SOCK_PATH);
  unlink(local.sun_path);
  len = strlen(local.sun_path) + sizeof(local.sun_family);
  if (bind(s, (struct sockaddr *)&local, len) == -1) 
    {
      perror("bind");
      exit(1);
    }
  
  if (listen(s, 5) == -1) 
    {
      perror("listen");
      exit(1);
    }
  
  chmod (SOCK_PATH, 0777);

  /* We are ready... go into select loop */
  loop4ever = 1;
  while (loop4ever)
    {
      /* Build File Descriptor set */
      FD_ZERO(&rfds);
      max = s + 1;
      FD_SET(s, &rfds);

      j = 0;
      for (i = 0; j < n_clients; i++)
	  {
	    if (c[i].s < 0) continue;
	    FD_SET(c[i].s, &rfds);
	    if (c[i].s >= max) max = c[i].s + 1;
	    j++;
	  }

      /* 4 sec Timeout*/
      tv.tv_sec = 4;
      tv.tv_usec = 0;

      if ((n_res = select (max, &rfds, NULL, NULL, &tv)) < 0)
	{
	  eprintf_err ("select:", "Error on select");

	}
      else
	{
	  if (n_res)
	    {

	      /* Check for incomming connections */
	      if (FD_ISSET(s, &rfds))
		{
		  /* Process TCP Connections */
		  if ((s2 = accept (s, (struct sockaddr*) &client, &sa_len)) <0)
		    {
		      eprintf_err ("accept:", "Error accepting connection");

		      break;
		    }
		  /* Add new client with specified socket*/
		  add_client (s2);
		  eprintf ("New client connected (%d clients)", n_clients);

		}

	      /* Check client sockets */
	      /* ---------------------------------------------*/
	      j = 0;
	      for (i = 0; j < n_clients; i++)
		{
		  if (c[i].s == -1) continue;
		  j++;
		  if (FD_ISSET(c[i].s, &rfds))
		    {
		      /* Process client */
		      process_client (i);
		    }
		}
	    }
	  else
	    {
	      /* Add here your idle operation */
	      /* Here we will run the garbage collector */
	      sally_gc ();
	    }
	}
    }
  
  return 0;
}
