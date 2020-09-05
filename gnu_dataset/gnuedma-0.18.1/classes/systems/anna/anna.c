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
#include <signal.h>
#include <time.h>
#include <unistd.h>

#include <edma.h>

#define VERSION "0.0"

/* ANNAconda is the GNU/EDMA extended python interpreter 
 * It's a simple Python interpreter using ANNA_PYTHON_INTERP class that
 * allows GNU/EDMA applications to embed a Python interpreter extended with
 * the edma module that allows Python scripts to access GNU/EDMA functionalities
 */
main(int argc,char *argv[])
{
  ESint32          i;
  OBJID            id;
  OBJID            id_sc;

  if (argc != 2)
    {
      fprintf (stderr, "GNU/EDMA ANNAconda interpreter version %s\n", VERSION);
      fprintf (stderr, "No script provided. Aborting.\n");
      return -1;
    }
  /* Initialize EDMA System*/
  EDMAInit();
  printf ("========================================================\n");
  /* Put your C code here */
  /* First we create our ANNA interpreter */
  if ((id = edma_new_obj ("ANNA_PYTHON_INTERP"))==-1)  {
    printf ("Can't create SUPER_CLASS object");
    EDMAEnd();
    exit(1);
  }
  edma_met3 (id, "Initialize");
  edma_met3 (id, "Run_Script", argv[1]);
  edma_met3 (id,"Finalize");
  printf ("========================================================\n");
  edma_free_obj (id);

  /* Shutdown EDMA Suystem */
  EDMAEnd();
  return 0;
}

