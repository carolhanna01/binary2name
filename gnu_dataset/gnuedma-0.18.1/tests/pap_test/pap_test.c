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
  ESint32          res;
  OBJID            id;
  OBJID            id_sc,id_sc1;

  /* Initialize EDMA System*/
  EDMAInit();

  /* Put your C code here */
  edma_printf ("\n Preferer Anchor Point Test");
  edma_printf ("+ Creating NFILE object...");
  if ((id = edma_new_obj ("NFILE"))==-1)  {
    edma_printf ("Can't create SUBCLASE2 object");
    EDMAEnd();
    exit(1);
  }
  edma_printf ("Object Report for NFILE object");
  edma_obj_report (id);
  edma_printf ("--------------------------------");

  edma_printf ("+ Upcasting object NFILE to STREAM...");
  id_sc1 = edma_upcast_obj (id, "STREAM");
  edma_printf ("Object Report for STREAM superobject");
  edma_obj_report (id_sc1);
  edma_show_subobjects_down (id_sc1, "****", 0);
  edma_printf ("--------------------------------------");

  edma_free_obj (id);

  /* Shutdown EDMA Suystem */
  EDMAEnd();
  return 0;
}
