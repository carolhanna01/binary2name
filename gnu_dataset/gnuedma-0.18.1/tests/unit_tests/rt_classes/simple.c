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
CLASS1_Met1 (OBJID IdObj, EPChar str)
{
  EChar  s[80];

  edma_printf_obj (IdObj, "Hello %s", str);

  edma_rprop3 (IdObj, "my_str", &s);
  edma_printf_obj (IdObj, "My string is: %s", s);

  return 0;
}

ESint32 EDMAPROC
CLASS1_Met2 (OBJID IdObj, ESint32 n)
{
  ESint32   num;

  edma_printf_obj (IdObj, "Did you say %ld?", n);
  edma_rprop3 (IdObj, "my_num", &num);
  edma_printf_obj (IdObj, "... I say %ld", num);

  return 0;
}

ESint32 EDMAPROC
SUPER_Met1 (OBJID IdObj, EPChar str)
{
  edma_printf_obj (IdObj, "SUPER Hello!!!!");
  return 0;
}

ESint32 EDMAPROC
SUPER_other (OBJID IdObj)
{
  edma_printf_obj (IdObj, "Other Method");
  return 0;
}

main(int argc,char *argv[])
{
  CLASSID          cid;
  OBJID            id;

  /* Initialize EDMA System*/
  EDMAInit();

  //cid = edma_get_local_class_id ();
  cid = edma_idf_get_free_class_id (EDMA_LOCAL_CLASS);

  edma_idf_set_class_name (cid, "LOCAL_SUPER");
  edma_idf_set_class_namespace (cid, "LOCAL");
  edma_idf_set_class_version (cid, 0, 0);

  edma_add_local_class_method (cid, "met1", "Z", (PPROC) SUPER_Met1, 0, 0, 0);
  edma_add_local_class_method (cid, "other", "", (PPROC) SUPER_other, 0, 0, 0);

  //edma_local_class_finish (cid);
  edma_idf_set_class_id (cid);


  //cid = edma_get_local_class_id ();
  cid = edma_idf_get_free_class_id (EDMA_LOCAL_CLASS);
  edma_idf_set_class_name (cid, "LOCAL_TEST");
  edma_idf_set_class_namespace (cid, "LOCAL");
  edma_idf_set_class_version (cid, 0, 0);

  edma_add_local_class_property (cid, "my_str", DT_EZSTRING, E_L, 0);
  edma_add_local_class_property (cid, "my_num", DT_ESINT32, E_L, 0);


  edma_add_local_class_method (cid, "met1", "Z", (PPROC) CLASS1_Met1, 0, 0, 0);
  edma_add_local_class_method (cid, "met2", "S32", (PPROC) CLASS1_Met2, 0, 0, 0);

  edma_add_local_class_superclass_by_name (cid, "LOCAL_SUPER", "SUPER1", "INNER1");
  edma_add_local_class_superclass_by_name (cid, "LOCAL_SUPER", "SUPER2", "INNER2");

  //edma_local_class_finish (cid);
  edma_idf_set_class_id (cid);

  id = edma_new_obj ("LOCAL_TEST");
  if (id == -1)
    {
      edma_printf ("%s", "Can't create object. Aborting\n");
      EDMAEnd();
      return -1;
    }
  edma_obj_report (id);
 
  edma_wprop3 (id, "my_num", 123);
  edma_wprop3 (id, "my_str", "Bye bye!");

  edma_met3 (id, "met1", "World!!!");
  edma_met3 (id, "met2", 321);
  edma_met3 (id, "SUPER1>other");
  edma_met3 (id, "SUPER2>met1", "Boo!");

  edma_free_obj (id);

  /* Shutdown EDMA Suystem */
  EDMAEnd();
  return 0;
}
