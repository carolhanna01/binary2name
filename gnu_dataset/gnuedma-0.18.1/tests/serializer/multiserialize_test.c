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

int main (int argc, char *argv[])
{
  OBJID		id, id1;
  EChar         a[80];
  
  EDMAInit();
  id1 = edma_new_obj ("MULTI_SERIALIZER");
  id = edma_new_obj ("ID");
  
  edma_met3 (id, "SetDB", "test.db");
  /* Add one superobject and one subobject*/
  edma_wprop3 (id, "AP_TEST@INNER<Id", "SUBOBJECT");
  //edma_wprop3 (id, "PERSON@INNER<name", "John Doe");
  edma_wprop3 (id, "AP_TEST@SUPER>Id", "SUPEROBJECT");


  //edma_wprop3 (id, "INNER<Id", "XXXXXX");
  edma_rprop3 (id, "SUPER>Id", a);
  printf ("SUPER: %s\n", a);
  edma_rprop3 (id, "INNER<Id", a);
  printf ("INNER: %s\n", a);

  edma_obj_report (id);
  edma_show_subobjects_up (id, "--", 0);
  edma_show_subobjects_down (id, "--", 0);
  /* Marshall compound object*/
  edma_met3 (id1, "marshall", id);
  edma_met3 (id1, "save", "multidata.dat");
  edma_free_obj (id);
  edma_free_obj (id1); 
  printf ("------------------------------------------------------\n");
  EDMAEnd();
}
