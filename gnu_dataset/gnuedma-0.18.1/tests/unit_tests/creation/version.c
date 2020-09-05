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

  /* Initialize EDMA System*/
  EDMAInit();

  id = edma_new_obj_with_version ("HELLO_WORLD", 2, 0); 
  if (id == -1)
    {
      edma_printf ("%s", "Can't create object. Aborting\n");
      EDMAEnd();
      return -1;
    }
  edma_obj_report (id);
 
  edma_printf ("%s", "\n");
  edma_printf ("%s", "==== BEGIN::[Subobject Tree] =========================================");
  edma_show_subobjects_down (id, "[The Object]", 0);
  edma_printf ("%s", "====== END::[Subobject Tree] =========================================\n");
  edma_printf ("%s", "==== BEGIN::[Superobject Tree] =======================================");
  edma_show_subobjects_up (id, "[The Object]", 0);
  edma_printf ("%s", "====== END::[Superobject Tree] =======================================\n");

  edma_printf ("%s", "==== BEGIN: Object Interface =========================================");
  edma_show_object_interface (id);

  edma_printf ("****[Destroy '%s' object]****\n", argv[1]);
  edma_free_obj (id);
  /* Shutdown EDMA Suystem */
  EDMAEnd();
  return 0;
}
