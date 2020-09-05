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
#include <stdlib.h>
#include <string.h>
#include <math.h>
#include <signal.h>
#include <time.h>
#include <unistd.h>

#include <edma.h>

main(int argc,char *argv[])
{
  OBJID            id, id1;
  OBJID            id_sc,id_sc1;
  POBJ             pObj;

  /* Initialize EDMA System*/
  EDMAInit();
  
  /* Put your C code here */
  id = edma_new_obj ("FINAL");
  edma_show_subobjects_up (id, "***", 0);
  edma_print ("==========================================");
  id1 = edma_upcast_obj (id, "BASE_CLASE");
  edma_show_subobjects_down (id1, "***", 0);
  edma_free_obj (id);

  /* Shutdown EDMA Suystem */
  EDMAEnd();
  return 0;
}
