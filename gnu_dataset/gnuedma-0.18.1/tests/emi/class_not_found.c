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

/* Method Implementation */
ESint32 EDMAPROC
EMITEST_Init (OBJID id)
{
  CLASSID     cid;

  edma_printf ("Initialising EMI Handler....");

  /* Register emi handler for CLASS_NOT_FOUND exception */
  cid = edma_get_obj_class_id (id);
  edma_hook_get_class (cid);

  edma_printf ("EMI Handler succesfully hooked");

  return 0;
}

ESint32 EDMAPROC
EMITEST_GetClass (OBJID id, EPChar class_name)
{
  edma_printf_obj (id, "----------------------------------------------");
  edma_printf_obj (id, "Looking for class '%s'...", class_name);

  /* Write here your code to locate class in other ways */
  edma_printf_obj (id, "-> Since this is a test, no class get found...");
  edma_printf_obj (id, "----------------------------------------------");
  return -1; /* Class Not found */
}

int
main(int argc,char *argv[])
{
  CLASSID          cid;
  OBJID            id;
  

  /* Initialize EDMA System*/
  EDMAInit();

  /* Create EMI Handler and register it */
  cid = edma_idf_get_free_class_id (EDMA_LOCAL_CLASS);

  edma_idf_set_class_name (cid, "DEFAULT_EMI_HANDLER");
  edma_idf_set_class_namespace (cid, "LOCAL");
  edma_idf_set_class_version (cid, 0, 0);
  edma_idf_set_class_attribs (cid, 0, 0, 1); /* EMI Handler */

  edma_add_local_class_method (cid, "Init", "", (PPROC) EMITEST_Init, 0, 0, 0);
  edma_add_local_class_method (cid, "GetClass", "Z", (PPROC) EMITEST_GetClass, 0, 0, 0);

  edma_idf_set_class_id (cid);

  id = edma_new_obj ("HELLO_WORLD");
  edma_free_obj (id);

  id = edma_new_obj ("BYE_BYE_WORLD");

  /* Shutdown EDMA Suystem */
  EDMAEnd();
  return 0;
}

