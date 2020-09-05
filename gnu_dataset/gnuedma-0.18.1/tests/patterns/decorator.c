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

ESint32 EDMAPROC
title_display (OBJID id)
{
  EChar   title[80];

  edma_rprop3 (id, ".title", title);
  printf ("%s, ", title);
  edma_met3 (id, "SUPER>display");
  printf ("\n");
  return 0;
}

int main ()
{
  OBJID    id, id1;
  CLASSID  cid;

  EDMAInit ();
  
  edma_printf ("%s", "*** Simple Decorator example ***");
  /* Create Decorator class dynamically */
  edma_printf ("%s", "+ Dynamic definition of 'TITLE_DECORATION' Class");
  //cid = edma_get_local_class_id ();
  cid = edma_idf_get_free_class_id (EDMA_LOCAL_CLASS);

  edma_idf_set_class_name (cid, "TITLE_DECORATOR");
  edma_idf_set_class_namespace (cid, "LOCAL");
  edma_idf_set_class_version (cid, 0, 0);

  edma_add_local_class_property (cid, "title", DT_EZSTRING, E_L, 0);
  edma_add_local_class_method (cid, "display", "", (PPROC) title_display, 0, 0, 0);

  //edma_local_class_finish (cid);
  edma_idf_set_class_id (cid);

  edma_printf ("%s", "+ Creating and Initialization object of class 'PERSON'");
  id = edma_new_obj ("PERSON");
  edma_wprop3 (id, "name", "John Smith");
  
  edma_printf ("%s", "+ Attaching 'TITLE_DECORATOR' to PERSON object");
  edma_wprop3 (id, "TITLE_DECORATOR@TITLE|SUPER<title", "M.S.");
  edma_met3 (id, "display");

  edma_printf ("%s", "+ Nesting another 'TITLE_DECORATOR'");
  edma_wprop3 (id, "TITLE<TITLE_DECORATOR@TITLE2|SUPER<title", "B.S.");
  edma_met3 (id, "display");

  edma_free_obj (id);

  edma_printf ("%s", "+ Choosable three decorators. Object Roles");

  id = edma_new_obj ("PERSON");
  
  edma_printf ("%s", "+ Attaching HOSPITAL, UNIVERSITY, CV Roles");
  edma_wprop3 (id, "TITLE_DECORATOR@HOSPITAL|SUPER<title", "Doctor");
  edma_wprop3 (id, "TITLE_DECORATOR@UNIVERSITY|SUPER<title", "Professor");
  edma_wprop3 (id, "TITLE_DECORATOR@CV|SUPER<title", "M.D.");

  edma_wprop3 (id, "name", "John Smith");

  /* Change Role */
  edma_printf ("%s", "\n+ Changing CURRENT_ROLE to HOSPITAL role");
  edma_rename_subclass_ap (id, "HOSPITAL", "CURRENT_ROLE");

  edma_met3 (id, "CURRENT_ROLE<display");

  edma_printf ("%s", "\n+ Changing CURRENT_ROLE to UNIVERSITY role");
  edma_rename_subclass_ap (id, "CURRENT_ROLE", "HOSPITAL");
  edma_rename_subclass_ap (id, "UNIVERSITY", "CURRENT_ROLE");
  edma_met3 (id, "CURRENT_ROLE<display");

  edma_printf ("%s", "\n+ Changing CURRENT_ROLE to CV role");
  edma_rename_subclass_ap (id, "CURRENT_ROLE", "UNIVERSITY");
  edma_rename_subclass_ap (id, "CV", "CURRENT_ROLE");
  edma_met3 (id, "CURRENT_ROLE<display");

  edma_rename_subclass_ap (id, "CURRENT_ROLE", "CV");
  edma_printf ("%s", "+ Using Roles with Downcasting");

  edma_printf ("%s", "+ Downcasting to HOSPITAL role");
  id1 = edma_downcast_obj (id, "HOSPITAL");
  edma_met3 (id1, "display");

  edma_printf ("%s", "\n+ Downcasting to UNIVERSITY role");
  id1 = edma_downcast_obj (id, "UNIVERSITY");
  edma_met3 (id1, "display");

  edma_printf ("%s", "\n+ Downcasting to CV role");
  id1 = edma_downcast_obj (id, "CV");
  edma_met3 (id1, "display");

  edma_free_obj (id);
  edma_printf ("\n");
  EDMAEnd();
}
