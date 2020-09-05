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
#include <stdlib.h>
#include <edma.h>

main(int argc,char *argv[])
{
  ESint32          i;
  OBJID            id;
  OBJID            id_sc;

  /* Initialize EDMA System*/
  EDMAInit();

  /* Put your C code here */
  edma_printf ("\nANNA SIU Proxy Test");
  edma_printf ("===================");
  if ((id = edma_new_obj ("ANNA_PYTHON_INTERP"))==-1)  {
    edma_printf ("Can't create ANNA_PYTHON_INTERP object");
    EDMAEnd();
    exit(1);
  }
  edma_met3 (id, "Initialize");
  edma_met3 (id, "Run_SimpleString", "print 'Hello World from Python!!!'");

  edma_met3 (id, "Run_SimpleString", "my_obj = edma.new_obj "
	     "(\"REALIZATION\")");
  edma_met3 (id, "Run_SimpleString", "edma.met3 (my_obj, 'met1', 'si', "
	     "('Hello World!', 125))");
  edma_met3 (id, "Run_SimpleString", "edma.met3 (my_obj, 'met2', 's', "
	     "('Hello Again!', ))");
  edma_met3 (id, "Run_SimpleString", "edma.free_obj (my_obj)");
  edma_printf ("--------------------------------------------------------");
  edma_printf ("Running test.py script\n");
  edma_met3 (id, "Run_Script", "./test.py");
  edma_printf ("\n---------------------------------------------------------\n");
  edma_met3 (id,"Finalize");

  edma_free_obj (id);

  /* Shutdown EDMA Suystem */
  EDMAEnd();
  return 0;
}

