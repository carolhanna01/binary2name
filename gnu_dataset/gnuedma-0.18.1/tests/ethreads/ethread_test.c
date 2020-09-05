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
#include <edma.h>



int et0_flag = 1;
int et1_flag = 1;

void *
et0_main (ETHREAD_PARAMS p)
{
  OBJID    id;

  edma_thread_register ();
  edma_printf ("[et0] %s", "Initiating et0 Main Loop");

  while (et0_flag)
    {
      edma_printf ("[et0] %s", "et0 working...");

      id = edma_new_obj ("HELLO_WORLD");
      edma_free_obj (id);
      sleep (1);
    }

  edma_thread_unregister();
  return 0;
}


void *
et1_main (ETHREAD_PARAMS p)
{
  edma_thread_register ();
  edma_printf ("[et1] %s", "Initiating et1 Main Loop");

  while (et1_flag)
    {
      edma_printf ("[et1] %s", "et1 working...");
      sleep (1);
    }

  edma_thread_unregister();
  return 0;
}

int 
main (int argc, char *argv[])
{
  OBJID		id, id1;
  ETHREAD       et0, et1;


  EDMAInit();
  edma_printf ("%s", "[Main] Creating Threads....");

  edma_thread_list ();
  sleep (1);
  edma_thread_create (&et0, et0_main, NULL);
  sleep (1);
  edma_thread_list ();
  edma_thread_create (&et1, et1_main, NULL);
  sleep (1);
  edma_thread_list ();

  sleep (6);

  edma_thread_list ();
  edma_printf ("[Main] %s", "Killing Threads...");

  et0_flag = 0;
  sleep (2);
  edma_thread_list ();

  et1_flag = 0;
  sleep (2);
  edma_thread_list ();

  edma_printf ("[Main] %s", "done.");

  EDMAEnd();
}

