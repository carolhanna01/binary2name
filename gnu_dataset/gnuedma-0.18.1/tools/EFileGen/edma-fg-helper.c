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
#include <string.h>
#include <math.h>

#include <edma.h>

#define VERSION "0.0"

int 
main(int argc,char *argv[])
{
  OBJID        id;
  OBJID        id1;
  EChar        ModuleName[1024],*aux;
  EChar        HelperClass[1024];

  /* Initialize EDMA System*/
  EDMAInit();
  
  /* Put your C code here */
  printf ("%s", "\n------------------------------------------------\n");
  printf ("%s", "EDMA File Generator Helper\n");
  printf ("%s", "(c) David Martínez Oliveira, May,2001\n");
  printf ("Version %s\n\n", VERSION);

  if (argc != 3) 
    {
      printf ("%s", "Parameter Number Incorrect. Usage:\n");
      printf ("%s", "\t./edma-fg-helper HELPER_CLASS output_file\n\n");

      EDMAEnd ();
      exit (1);
  }

  strcpy (HelperClass, argv[1]);
  /* Quick hack for common files*/
  if (strcmp (HelperClass, "app_src") == 0)
      strcpy (HelperClass, "EDMA_APP_SKEL_BUILDER");
  if (strcmp (HelperClass, "app_mk") == 0)
    strcpy (HelperClass, "EDMA_MAKE_SKEL_BUILDER");

  printf ("+ Using Helper Class : %s\n", HelperClass);

  if ((id = edma_new_obj (HelperClass)) == -1) 
    {
      EDMAEnd ();
      exit (1);
    }
  
  if ((id1 = edma_new_obj ("CLASS_METADATA")) == -1) 
    {
      EDMAEnd ();
      exit (1);
    }

  strcpy (ModuleName, argv[2]);
  aux = strchr (ModuleName, '.');
  *aux = 0;
  edma_wprop3 (id1, "ClassName", ModuleName);

  printf ("%s", "\n+ Generating File....");
  edma_met3 (id, "build", id1,argv[2]);

  printf ("%s", "\n+ All Done!!");
  edma_free_obj (id);

  printf ("%s", "\n------------------------------------------------");

  /* Shutdown EDMA Suystem */
  EDMAEnd();
  return 0;
}
