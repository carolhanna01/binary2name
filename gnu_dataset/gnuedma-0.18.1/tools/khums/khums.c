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

/**********************************************************************************/
/* Killer HiTech Unuseful Multithread Server -=KHUMS=- :)                         */
/* --==00 GNU/EDMA RuLeZ 00==---                                                  */
/**********************************************************************************/
/* compile with:                                                                  */
/*      gcc `edma-config --cflags-exe` -o khums khums.c `edma-config --libs-exe`  */
/* run with                                                                       */
/*     ./khums channel_class://localhost:port  service_class                      */
/* ej: ./khums SOCKET_TCP://localhost:6666 ECHO_SERVICE                           */
/**********************************************************************************/

#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#include <edma.h>

#define MAX_CLIENTS 10
#define VERSION "0.1"


ESint32 manage_request (OBJID IdObj,OBJID c);
EChar   service_class[256];

/* Tribute to the DOORS*/
void 
this_is_the_end (char *err) 
{ 
  fprintf (stderr, err);
  EDMAEnd (); 
  exit (1);
}

/* The whole thing */
int 
main(int argc, char *argv[])
{
  OBJID id;
  
  printf ("KHUMS v %s\n", VERSION);
  if (argc != 3) 
    {
      printf ("Parameter number incorrect. Try:\n"
	      "\tkhums SOCKET_TCP://localhost:port SERVICE_CLASS\n\n");
      return -1;
    }
  
  EDMAInit (); /* Initialize EDMA System */
  
  strncpy (service_class, argv[2], 256);
  if ((id = edma_new_obj ("MTSERVER_SKEL")) == -1)
    this_is_the_end ("Can't create object MTSERVER_SKEL\n");
  
  /* Inits multithread server to manage upto 10 simultaneous clients*/
  if (-1 == (ESint32) edma_met3 (id, "init", argv[1], MAX_CLIENTS)) 
    this_is_the_end ("Can't init server...\n");
  
  /* Sets the function to manage each request*/
  edma_over_met (id, "onRequest", NULL, (PPROC) manage_request);
  
  /* Launch server's main loop */
  if (-1 == (ESint32) edma_met3 (id, "run")) 
    this_is_the_end ("Can't run server main loop\n");
  
  /* THis point will never be reached... 
   * let GNU/EDMA default signal handlers do the cleanup*/
  return 0;
}

/*******************************************************************************
 * manage_request: This function manages each request that arrives to the server
 * We create a 'service_object' and we run 'service' method on it to do the work
 ********************************************************************************/  
ESint32 manage_request (OBJID id,OBJID idc) {
  EChar         remote[256];
  OBJID         serv_id;
  
  edma_rprop3 (idc, "Resource", remote);
  printf ("\n[INFO] Request from : %s\n", remote);
  
  if ((serv_id = edma_new_obj (service_class)) == -1)
    {
      fprintf (stderr, "[ERROR] Can't create '%s' service class\n", service_class);
      return 0;
    }
  edma_met3 (serv_id, "service", idc);
  edma_free_obj (serv_id);
  printf ("\n[INFO] Processing finished for %s\n", remote);
  
  return 0;
}


