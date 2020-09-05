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

/* Method implementation*/
void EDMAPROC
test_0_0 (OBJID id)
{
  edma_printf_obj (id, "%s", "[TEST 0.0] Method");
}

void EDMAPROC
test_1_0 (OBJID id)
{
  edma_printf_obj (id, "%s", "[TEST 1.0] Method");
}

void EDMAPROC
test_2_0 (OBJID id)
{
  edma_printf_obj (id, "%s", "[TEST 2.0] Method");
}

int main (int argc, char *argv[])
{
  CLASSID       cid;
  OBJID		id, id1;
  
  EDMAInit();

  /* Definition of class TEST v 0.0 */
  cid = edma_idf_get_free_class_id (EDMA_LOCAL_CLASS);

  edma_idf_set_class_name (cid, "TEST");
  edma_idf_set_class_namespace (cid, "LOCAL");
  edma_idf_set_class_version (cid, 0, 0);

  edma_add_local_class_method (cid, "test", "", (PPROC) test_0_0, 0, 0, 0);

  edma_idf_set_class_id (cid);  

  /* Definition of class TEST v 1.0 */
  cid = edma_idf_get_free_class_id (EDMA_LOCAL_CLASS);

  edma_idf_set_class_name (cid, "TEST");
  edma_idf_set_class_namespace (cid, "LOCAL");
  edma_idf_set_class_version (cid, 1, 0);

  edma_add_local_class_method (cid, "test", "", (PPROC) test_1_0, 0, 0, 0);

  edma_idf_set_class_id (cid);  

  /* Definition of class TEST v 2.0 */
  cid = edma_idf_get_free_class_id (EDMA_LOCAL_CLASS);

  edma_idf_set_class_name (cid, "TEST");
  edma_idf_set_class_namespace (cid, "LOCAL");
  edma_idf_set_class_version (cid, 2, 0);

  edma_add_local_class_method (cid, "test", "", (PPROC) test_2_0, 0, 0, 0);

  edma_idf_set_class_id (cid); 

  /* Simple Object Creation */
  id = edma_new_obj ("TEST");
  edma_met3 (id, "test");
  edma_free_obj (id);
  
  /* Force old class for this process */
  edma_printf ("%s", "--------------------------------------------------------");
  edma_printf ("%s", "Forcing application to use TEST version 0.0");
  edma_class_set_actual_version ("TEST", 0, 0);

  /* Simple Object Creation */
  id = edma_new_obj ("TEST");
  edma_met3 (id, "test");
  edma_free_obj (id);

  /* Force old class for this process */
  edma_printf ("%s", "--------------------------------------------------------");
  edma_printf ("%s", "Forcing application to use TEST version 2.0");
  edma_class_set_actual_version ("TEST", 2, 0);

  /* Simple Object Creation */
  id = edma_new_obj ("TEST");
  edma_met3 (id, "test");
  edma_free_obj (id);

  /* Force old class for this process */
  edma_printf ("%s", "--------------------------------------------------------");
  edma_printf ("%s", "Forcing application to use TEST version 1.0");
  edma_class_set_actual_version ("TEST", 1, 0);

  /* Simple Object Creation */
  id = edma_new_obj ("TEST");
  edma_met3 (id, "test");
  edma_free_obj (id);

  EDMAEnd();
}
