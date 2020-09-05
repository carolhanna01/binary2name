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
set_person_name (OBJID id, EPChar name)
{
  edma_wprop3 (id, "name", name);
  return 0;
}

ESint32 EDMAPROC 
get_person_name (OBJID id, EPChar name)
{
  edma_rprop3 (id, "name", name);

  return 0;
}

ESint32 EDMAPROC
whoareu (OBJID id)
{
  printf ("Who are you?. I'm ");
  edma_met3 (id, "TARGET>display");

  return 0;
}


int
main ()
{
  OBJID    id;
  CLASSID  cid;

  EDMAInit ();

  edma_printf ("%s", "+ Creating class PERSON_ADAPTER");
  //cid = edma_get_local_class_id ();
  cid = edma_idf_get_free_class_id (EDMA_LOCAL_CLASS);

  edma_idf_set_class_name (cid, "PERSON_ADAPTOR");
  edma_idf_set_class_namespace (cid, "LOCAL");
  edma_idf_set_class_version (cid, 0, 0);

  edma_add_local_class_method (cid, "set_name", "Z", 
			       (PPROC) set_person_name, 0, 0, 0);
  edma_add_local_class_method (cid, "get_name", "rZ", 
			       (PPROC) get_person_name, 0, 0, 0);

  edma_add_local_class_method (cid, "who_are_you", "", 
			       (PPROC) whoareu, 0, 0, 0);

  
  //edma_local_class_finish (cid);
  edma_idf_set_class_id (cid);

  edma_printf ("%s", "+ Creating 'PERSON' instance");
  id = edma_new_obj ("PERSON");

  edma_printf ("%s", "+ Attaching PERSON_ADAPTOR to PERSON object");
  edma_add_subclass (id, cid, "ADAPTOR", "TARGET");
  edma_printf ("%s", "+ Accessing PERSON using New Interface.");

  edma_met3 (id, "set_name", "John Smith");
  edma_met3 (id, "who_are_you");


  edma_printf ("%s", "\n+ Accessing PERSON Using Old Interface");
  edma_met3 (id, "display");
  edma_printf ("%s", "\n");
  

  edma_free_obj (id);

  EDMAEnd ();
  return 0;
}
