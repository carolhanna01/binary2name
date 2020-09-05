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

#include <edma.h>

#define NAL_VERSION "0.1.1"

/* The whole thing */
int 
main(int argc, char *argv[])
{
  OBJID id;
  
  printf ("NAL v %s (GNU/EDMA v %s)\n", NAL_VERSION, VERSION);
  if (argc < 2) 
    {
      printf ("Parameter number incorrect. Try:\n"
	      "\tnal APPLICATION_CLASS app_parameters\n\n");
      return -1;
    }
  
  EDMAInit (); /* Initialize EDMA System */
  
  /* Create application object and launch it */
  if ((id = edma_new_obj (argv[1])) < 0)
    {
      edma_printf_err ("Cannot create '%s' object.", argv[1]);
      EDMAEnd();
      exit (1);
    }

  /* For now, just run application */
  edma_met3 (id, "run", argc, argv);

  EDMAEnd();
  return 0;
}

