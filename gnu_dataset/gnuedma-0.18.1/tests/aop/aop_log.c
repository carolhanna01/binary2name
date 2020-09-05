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

#include <edma.h>

/* SIU LOGGER Aspect method implementation */
/* ---------------------------------------------------- */
OBJID EDMAPROC
create_obj (OBJID id, EPChar classname)
{
  OBJID   obj;
  
  edma_printf ("[LOGGING] New object of class '%s'", classname);

  /* Create the real object */
  obj = edma_new_obj (classname);

  /* Store the object reference for further reference 
   * Note that for each attached object an instance of LOG_PROXY
   * is created with its own member fields
   */
  edma_wprop3 (id, "obj", obj);
  edma_printf ("[LOGGING] Object Identifier = %d", obj);

  return 0;
}

void EDMAPROC
destroy_obj (OBJID id)
{
  OBJID    obj;

  /* Recover object id */
  edma_rprop3 (id, "obj", &obj);
  edma_printf ("[LOGGING] Destroying object %d", obj);

  edma_free_obj (obj);

}

ESint32 EDMAPROC
write_prop (OBJID id, CLASSID cid, EPChar Prop, EPVoid val)
{
  OBJID   obj;

  edma_rprop3 (id, "obj", &obj);
  edma_printf ("[LOGGING] Writting propery '%s' on object %d", Prop, obj);
  edma_wprop3_pargs (obj, Prop, val);

  return 0;
}

ESint32 EDMAPROC
read_prop (OBJID id, CLASSID cid, EPChar Prop, EPVoid val)
{
  OBJID   obj;

  edma_rprop3 (id, "obj", &obj);
  edma_printf ("[LOGGING] Readding propery '%s' on object %d", Prop, obj);

  return edma_rprop3_pargs (id, Prop, val);
}

ESint32 EDMAPROC
run_method (OBJID id, CLASSID cid, EPChar met_name, EPVoid val)
{
  OBJID   obj;

  edma_rprop3 (id, "obj", &obj);
  edma_printf ("[LOGGING] Running method '%s' on object %d", met_name, obj);

  return edma_met3_pargs (obj, met_name, NULL, 1, val);
}

/* ---------------------------------------------------- */

int
main ()
{
  CLASSID   cid;
  OBJID     id;

  EDMAInit ();

  /* Run-Time Creation of the LOGGER Aspect */
  cid = edma_idf_get_free_class_id (EDMA_LOCAL_CLASS);

  edma_idf_set_class_name (cid, "LOG_PROXY");
  edma_idf_set_class_namespace (cid, "LOCAL");
  edma_idf_set_class_version (cid, 0, 0);

  edma_add_local_class_property (cid, "obj", DT_EOBJECT, E_L, 0);
  edma_add_local_class_method (cid, "WProp3", "", (PPROC) write_prop, 0, 0, 0);
  edma_add_local_class_method (cid, "RProp3", "", (PPROC) read_prop, 0, 0, 0);
  edma_add_local_class_method (cid, "Met3", "", (PPROC) run_method, 0,0,0);
  edma_add_local_class_method (cid, "NewObj", "", (PPROC) create_obj, 0,0,0);
  edma_add_local_class_method (cid, "FreeObj", "", (PPROC) destroy_obj, 0,0,0);

  /* Marks the class a a SIU proxyu */
  edma_idf_set_class_attribs (cid, 1, 0, 0);

  edma_idf_set_class_id (cid);
  /* LOG_PROXY Class creation ends */
  /* ----------------------------------------------------*/

  /* Create a HELLO_WORK object attaching the just created logger ASPECT */
  id = edma_new_obj ("LOG_PROXY:HELLO_WORLD");

  /* Some interaction with the object to show the LOGGER aspect at work*/
  edma_wprop3 (id, "str", "Hoooooola");
  edma_wprop3 (id, "num", 123);
  edma_met3 (id, "say");

  edma_free_obj (id);

  EDMAEnd ();

  return 0;
}

