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

void EDMAPROC tl_handler1 (OBJID id, int val)
{
  if (val == 10)
    edma_printf_obj (id, "Handler 1");
  else
    edma_met3 (id, "INNER<.handler1", val);
}

void EDMAPROC tl_handler2 (OBJID id, int val)
{
  if (val == 20)
    edma_printf_obj (id, "Handler 2");
  else
    edma_met3 (id, "INNER<.handler2", val);
}

void EDMAPROC m1_handler1 (OBJID id, int val)
{
  if (val == 100)
    edma_printf_obj (id, "Handler 1");
  else
    edma_met3 (id, "INNER<.handler1", val);
}

void EDMAPROC m2_handler2 (OBJID id, int val)
{
  if (val == 200)
    edma_printf_obj (id, "Handler 2");
  else
    edma_met3 (id, "INNER<.handler2", val);
}

void EDMAPROC l1_handler1 (OBJID id, int val)
{
  if (val == 1000)
    edma_printf_obj (id, "Handler 1");
  else
    edma_met3 (id, "INNER<.handler1", val);

}

void EDMAPROC l1_handler2 (OBJID id, int val)
{
  /* This one do not propagate */
  edma_printf_obj (id, "Handler 2");
}


int
main ()
{
  CLASSID    cid;
  OBJID      id, id1;

  EDMAInit();

  //cid = edma_get_local_class_id ();
  cid = edma_idf_get_free_class_id (EDMA_LOCAL_CLASS);

  edma_idf_set_class_name (cid, "TOP_LEVEL");
  edma_idf_set_class_namespace (cid, "LOCAL");
  edma_idf_set_class_version (cid, 0, 0);

  edma_add_local_class_method (cid, "handler1", "", (PPROC) tl_handler1, 0, 0, 0);
  edma_add_local_class_method (cid, "handler2", "", (PPROC) tl_handler2, 0, 0, 0);

  edma_idf_set_class_id (cid);

  cid = edma_idf_get_free_class_id (EDMA_LOCAL_CLASS);

  edma_idf_set_class_name (cid, "MEDIUM1");
  edma_idf_set_class_namespace (cid, "LOCAL");
  edma_idf_set_class_version (cid, 0, 0);

  edma_add_local_class_method (cid, "handler1", "", (PPROC) m1_handler1, 0, 0, 0);

  //edma_add_local_class_superclass_by_name (cid, "TOP_LEVEL", "CHILD", "PARENT");

  edma_idf_set_class_id (cid);

  cid = edma_idf_get_free_class_id (EDMA_LOCAL_CLASS);

  edma_idf_set_class_name (cid, "MEDIUM2");
  edma_idf_set_class_namespace (cid, "LOCAL");
  edma_idf_set_class_version (cid, 0, 0);

  edma_add_local_class_method (cid, "handler2", "", (PPROC) m2_handler2, 0, 0, 0);

  //edma_add_local_class_superclass_by_name (cid, "TOP_LEVEL", "CHILD", "PARENT");

  edma_idf_set_class_id (cid);


  /* Dynamic responsability */
  cid = edma_idf_get_free_class_id (EDMA_LOCAL_CLASS);

  edma_idf_set_class_name (cid, "LOW");
  edma_idf_set_class_namespace (cid, "LOCAL");
  edma_idf_set_class_version (cid, 0, 0);

  edma_add_local_class_method (cid, "handler1", "", (PPROC) l1_handler1, 0, 0, 0);
  edma_add_local_class_method (cid, "handler2", "", (PPROC) l1_handler2, 0, 0, 0);

  //edma_add_local_class_superclass_by_name (cid, "TOP_LEVEL", "CHILD", "PARENT");

  edma_idf_set_class_id (cid);

  /* Build Chain of Responsabilities dynamically */
  id = edma_new_obj ("TOP_LEVEL");
  edma_apply_classpath (id, "MEDIUM1@INNER|SUPER<LOW@INNER|SUPER<");


  edma_printf ("-------------------------------------");
  edma_printf ("Handler 1");
  edma_printf ("-------------------------------------");
  edma_printf ("-- Param: 10 ------------------------");
  edma_met3 (id, ".handler1", 10);
  edma_printf ("-- Param: 100 -----------------------");
  edma_met3 (id, ".handler1", 100);
  edma_printf ("-- Param: 1000 ----------------------");
  edma_met3 (id, ".handler1", 1000);
  edma_printf ("-- Param: 2000 ----------------------");
  edma_met3 (id, ".handler1", 2000);
  edma_printf ("-------------------------------------");
  edma_printf ("Handler 2");
  /* Update classpath */
  edma_apply_classpath (id, "MEDIUM2@!INNER|SUPER<LOW@!INNER|SUPER<");
  edma_printf ("-------------------------------------");
  edma_printf ("-- Param: 20 ----------------------");
  edma_met3 (id, ".handler2", 20);
  edma_printf ("-- Param: 200 ----------------------");
  edma_met3 (id, ".handler2", 200);
  edma_printf ("-- Param: 2000 ----------------------");
  edma_met3 (id, ".handler2", 2000);
  edma_printf ("-- Param: 100 ----------------------");
  edma_met3 (id, ".handler2", 1000);



  edma_free_obj (id);


  EDMAEnd();
}
