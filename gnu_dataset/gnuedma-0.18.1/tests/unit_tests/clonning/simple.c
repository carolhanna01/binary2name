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

main(int argc,char *argv[])
{
  OBJID            id;
  OBJID            id_clon;
  EChar            str_clon[100];
  EUint32          num_clon;

  /* Initialize EDMA System*/
  EDMAInit();

  edma_printf ("%s", "******************** SIMPLE CLONNING TEST **********************\n");
  if ((id = edma_new_obj ("HELLO_WORLD")) == -1)
    {
      fprintf (stderr, "%s", "Can't create object. Aborting\n");
      EDMAEnd();
      return -1;
    }
  edma_printf ("%s", "+ Setting properties => num:<12345> str:<ABCDEFG>");
  edma_wprop3 (id, "num", 12345);
  edma_wprop3 (id, "str", "ABCDEFG");

  edma_printf ("%s", "+ Shallow Clonning Object");
  if ((id_clon = edma_shallow_clone_obj (id)) == -1)
    {
      fprintf (stderr, "%s", "**ERROR** Can't clone object. Aborting");
      edma_free_obj (id);
      EDMAEnd();
      return -1;
    }
  edma_printf ("%s", "+ Rewritting Original Properties:: num:<54321> str:<GFEDCBA>");
  edma_wprop3 (id, "num", 54321);
  edma_wprop3 (id, "str", "GFEDCBA");
  
  edma_printf ("%s", "+ Recovering Clon Properties");
  edma_rprop3 (id_clon, "num", &num_clon);
  edma_rprop3 (id_clon, "str", &str_clon);
  edma_printf ("   - num:<%d> str:<%s>\n", num_clon, str_clon);

  /************************************************************************/
  edma_free_obj (id_clon);
  edma_printf ("%s", "+ Deep Clonning Object");
  if ((id_clon = edma_clone_obj (id)) == -1)
    {
      fprintf (stderr, "%s", "**ERROR** Can't clone object. Aborting");
      edma_free_obj (id);
      EDMAEnd();
      return -1;
    }
  edma_printf ("%s", "+ Rewritting Original Properties:: num:<23456> str:<PQRST>");
  edma_wprop3 (id, "num", 23456);
  edma_wprop3 (id, "str", "PQRST");

  edma_printf ("%s", "+ Recovering Clon Properties");
  edma_rprop3 (id_clon, "num", &num_clon);
  edma_rprop3 (id_clon, "str", &str_clon);
  edma_printf ("   - num:<%d> str:<%s>\n", num_clon, str_clon);

  edma_printf ("%s", "****[Destroing objects]****\n");
  edma_free_obj (id);
  edma_free_obj (id_clon);
  /* Shutdown EDMA Suystem */
  EDMAEnd();
  return 0;
}
