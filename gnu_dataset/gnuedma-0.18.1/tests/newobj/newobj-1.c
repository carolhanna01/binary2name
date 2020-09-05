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
  
  EDMAInit();

  /* Simple Object Creation */
  id = edma_new_obj ("HELLO_WORLD");
  edma_obj_report (id);
  edma_free_obj (id);


  /* Single Static Inheritance*/
  id = edma_new_obj ("GRADUATE");
  edma_obj_report (id);
  edma_free_obj (id);

  /* SIU PROXY does exists*/
  id = edma_new_obj ("RUNABLE:HELLO_WORLD");
  edma_met3 (id, "say");
  edma_obj_report (id);
  edma_free_obj (id);

  /* SIU PROXY doesn't exists*/
  id = edma_new_obj ("UNKNOWN_PROXY:HELLO_WORLD");
  edma_obj_report (id);
  edma_free_obj (id);

  EDMAEnd();
}
