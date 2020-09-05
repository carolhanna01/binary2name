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

//#include <stdio.h>
#include <edma.h>

/* SIU LOGGER Aspect method implementation */
/* ---------------------------------------------------- */

OBJID EDMAPROC
create_obj (OBJID id, EPChar classname)
{
  OBJID   obj;
  
  edma_printf ("-->[LOGGING] New object of class %s", classname);
  obj = edma_new_obj (classname);
  edma_wprop3 (id, "obj", obj);
  edma_printf ("-->[LOGGING] Object Identifier = %d", obj);
  return 0;
}

void EDMAPROC
destroy_obj (OBJID id)
{
  OBJID    obj;

  edma_rprop3 (id, "obj", &obj);
  edma_printf ("-->[LOGGING] Destroying object %d", obj);

  edma_free_obj (obj);

}

ESint32 EDMAPROC
write_prop (OBJID id, CLASSID cid, EPChar Prop, EPVoid val)
{
  OBJID   obj;

  edma_rprop3 (id, "obj", &obj);
  edma_printf ("-->[LOGGING] Writting property '%s' on object %d", Prop, obj);
  edma_wprop3_pargs (obj, Prop, val);

  return 0;
}

ESint32 EDMAPROC
read_prop (OBJID id, CLASSID cid, EPChar Prop, EPVoid val)
{
  OBJID   obj;

  edma_rprop3 (id, "obj", &obj);
  edma_printf ("-->[LOGGING] Reading property '%s' on object %d", Prop, obj);

  return edma_rprop3_pargs (id, Prop, val);
}

ESint32 EDMAPROC
run_method (OBJID id, CLASSID cid, EPChar met_name, EPVoid val)
{
  OBJID   obj, ex; 

  edma_rprop3 (id, "obj", &obj);
  edma_printf ("-->[LOGGING] Running method '%s' on object %d", met_name, obj);
  EDMA_TRY 
    {
      return edma_met3_pargs (obj, met_name, NULL, 1, val);
    }
  EDMA_CATCH (ex)
    {
      edma_printf ("-->[LOGGING] Exception running method '%s' on object %d", 
		   met_name, obj);
      edma_met3 (ex, "brief");
    }
  EDMA_TRY_END;

  return 0;
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
  edma_add_local_class_method (cid, "Met3",   "", (PPROC) run_method, 0,0,0);
  edma_add_local_class_method (cid, "NewObj",   "", (PPROC) create_obj, 0,0,0);
  edma_add_local_class_method (cid, "FreeObj",   "", (PPROC) destroy_obj, 0,0,0);

  /* Mark class as a SIU Proxy */
  edma_idf_set_class_attribs (cid, 1, 0, 0);

  edma_idf_set_class_id (cid);

  /* Create a normal object of HELLO_WORLD class */
  edma_printf ("\nRun object normally");
  edma_printf ("************************");
  
  id = edma_new_obj ("HELLO_WORLD");

  /* Interact normally with the object*/
  edma_wprop3 (id, "str", "Hoooooola");
  edma_wprop3 (id, "num", 123);
  edma_met3 (id, "say");

  edma_printf ("\nAttach a logger aspect");
  edma_printf ("**********************");

  /* Attach the LOGGER Aspect as a SIU proxy */
  edma_attach_proxy (id, "LOG_PROXY");

  /* Interact again */
  edma_wprop3 (id, "str", "Aaaaadiiios");
  edma_wprop3 (id, "num", 321);
  edma_met3 (id, "say");
  edma_met3 (id, "not_found");

  edma_printf ("\nDetach a log proxy");
  edma_printf ("********************");

  /* Deattach the LOGGER Aspect and check that behavior is restored */
  edma_deattach_proxy (id);

  edma_wprop3 (id, "str", "Bye Bye bye...");
  edma_wprop3 (id, "num", 456);
  edma_met3 (id, "say");

  edma_free_obj (id);

  EDMAEnd ();

  return 0;
}
