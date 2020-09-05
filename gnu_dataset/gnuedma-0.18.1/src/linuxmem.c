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

/*
 * Febraury, 7th, 2001
 * Code cleanup and comment translation
 * --------------------------------------------------------
 * October, 14th,2001
 * Added PRealloc function
 * ---------------------------------------------------------
 * November, 18th, 2001
 * Now temporally file to get a uinique key are created in TMPDIR
 * a constant defined in conts.h
 * 
 * Compile warnnings removal
 * ------------------------------------------------------------
 * march, 2nd, 2002
 * Code cleanup
 */ 

#include <sys/types.h>
#include <sys/ipc.h>
#include <sys/shm.h>
#include <sys/stat.h>
#include <fcntl.h>
#include <unistd.h>
#include <stdio.h>
#include <string.h>
#include <stdlib.h>
#include <errno.h>
#include "portable.h"
#include "const.h"


#include <sys/socket.h>
#include <sys/un.h>

#include "shmem.h"

EChar   Buffer[1024];

EUint32  n_palloc = 0;
EUint32  n_pget = 0;
EUint32  n_pfree = 0;
EUint32  n_prealloc = 0;

/* Remote share memory manager */
#define SOCK_PATH "/tmp/sally_socket"


static int _sally = -1;
static int initialised = 0;

/* Initial RESMA implementation. For now, this is not thread safe */
#define BUFFER_INC   1024
//char *emrald_buffer;
int  sally_offset = 0;
int  sally_len = 0;

int
_edma_sally_init ()
{
  int                len;
  struct sockaddr_un remote;

  initialised = 1;
  if (_sally >= 0) return _sally;

  if ((_sally = socket(AF_UNIX, SOCK_STREAM, 0)) == -1) 
    {
      perror("socket");
      return -1;
    }
  
  printf ("Trying to connect to EDMA Shared Allocator Agent ...");

  remote.sun_family = AF_UNIX;
  strcpy (remote.sun_path, SOCK_PATH);
  len = strlen (remote.sun_path) + sizeof(remote.sun_family);

  if (connect(_sally, (struct sockaddr *)&remote, len) == -1) 
    {
        perror("connect");
	close (_sally);
	_sally = -1;
	printf ("\n-- Connection failed. Falling back to local shared memory "
		"allocator...\n");
        return -1;
    }
  printf (" CONNECTED\n");
  printf ("++ Using Sally Shared Allocator\n");
  return _sally;
}

key_t
_sally_shm_alloc (char *n, int size)
{
  char   buffer[1024];
  key_t  _the_key;
  int    len;

  /* Send request to sally server*/
  len = snprintf (buffer, 1024, "SALLOC %d %s\n", size, n);
  write (_sally, buffer, len);

  /* Read response*/
  memset (buffer, 0, 1024);
  len = read (_sally, buffer, 1024);
  /* parse and return response */
  sscanf (buffer, "+SALLOC %d\n", &_the_key);

  return _the_key;
}



/* edma_salloc
 *   Shared Memory version of malloc
 */

HMEM EDMAPROC 
edma_salloc (EUint32 size, EPChar n) 
{
  EUint32   a, shmflg, f;
  EChar     name[EDMA_PATH_LEN];
  key_t     k;
  
  if (!initialised && _sally < 0)
    {
      /* Try to initialise*/
      _edma_sally_init ();
    }

  if (_sally > 0)
    {
      k = _sally_shm_alloc (n, size);
      //printf ("(ESMA) Requesting %d bytes of shared memory (%s)", size, n);
    }
  else
    {
      /* If sally is not working we have to fallback to previous system.
       * We create a file for filemapping shared memory */
      //printf ("ESMA not found. Falling back to local allocator...\n");
      snprintf (name, EDMA_PATH_LEN, "%s/%s", TMPDIR, n);
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
      if ((k = ftok (name, 0)) == -1)
	{
	  perror ("[edma_salloc] (ftok)");
	  return -1;
	}
    }
  shmflg = 0;
  
  a = shmget (k,size, IPC_CREAT | 0777);
  if ((a == -1) && (errno != EEXIST)) 
    {
      printf ("\n[edma_salloc] ERROR in shared block : %s (key:%d|size:%ld)",
	      name, k, size);
      printf ("\n ");
      perror("[edma_salloc] (shmget):");
      return -1;
    }
  if (_sally <= 0) close (f);
  return ((HMEM)a);
}

/* edma_sget
 *   Used to provide support to system managing memory with handlers
 */

EPVoid EDMAPROC 
edma_sget (HMEM a) 
{
  EPVoid     p;
  
  p = shmat ((EUint32) a, 0, SHM_RND);
  if ((int)p == -1) 
    {
      perror("[edma_sget] :");
      return (EPVoid)-1;
    }
  
  return p;
}

/* edma_sunget
 *    Deattaches shared memory for process 
 *    Used to provide support to systems which handler-based memory management
 */
EPVoid EDMAPROC 
edma_sunget (EPVoid p) 
{
  EUint32    i;
  
  i = shmdt(p);
  if (i == -1) 
    {
      perror ("[edma_sunget] :");
      return (EPVoid)-1;
    }
  
  return p;
}

/* edma_sfree
 *  Shared version of free
 */

void EDMAPROC 
edma_sfree (HMEM i, EPVoid p) 
{
  ESint32         a;
  struct shmid_ds buf;

  if (_sally < 0)  
    {
      a = shmctl (i, IPC_RMID, &buf);
      if (a == -1) 
	{
	  perror("[edma_sfree] (IPC_RMID):");
	  return;
	}
    }
  /* Here we should delete the file */
}

/************************************************************
 * Private memory management 
 *************************************************************/

/* edma_palloc
 *   Normal malloc, with data initialisation
 */

HMEM EDMAPROC 
edma_palloc (EUint32 size) 
{
  EPVoid p;
  EPChar t;
  EUint32 i;
  
  p = malloc (size);
  t = (EPChar) p;
  if (p != NULL)
    for (i = 0; i < size; i++) *(t + i) = 0;

  n_palloc++;
  return ((HMEM) p);
}

/* edma_prealloc
 *  Normal Realloc, simple wrapper
 */

HMEM EDMAPROC 
edma_prealloc (HMEM a, EUint32 new_size) 
{
  if (a == 0)
    n_palloc++;
  else
    n_prealloc++;

  a = (HMEM) realloc ((EPVoid) a, new_size);

  return a;
}

/* edma_pget
 *   Function to support handler-based memory management systems
 */

EPVoid EDMAPROC 
edma_pget (HMEM a) 
{
  n_pget++;
  return (EPVoid)a;
}

/* edma_pfree
 *  Simple free function
 */

void  EDMAPROC 
edma_pfree (HMEM a, EPVoid p) 
{
  n_pfree++;
  if (p == NULL)
    return;
  free (p);
}

void EDMAPROC
edma_show_pmem ()
{
  printf ("palloc   : %ld times\n", n_palloc);
  printf ("pget     : %ld times\n", n_pget);
  printf ("prealloc : %ld times\n", n_prealloc);
  printf ("pfree    : %ld times\n", n_pfree);
}
      
