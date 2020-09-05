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

#include <edma.h>

int
main(int argc,char *argv[])
{
  OBJID            id, id1;
  OBJID            id_sc,id_sc1;
  POBJ             pObj;

  /* Initialize EDMA System*/
  EDMAInit();
  
  /* Put your C code here */
  edma_print ("\nINNER Inheritance Test\n");
  edma_print ("==> Creating object of class GRADUATE_INNER1");
  if ((id = edma_new_obj ("GRADUATE_INNER1")) == -1)  
    {
      edma_print ("**ERR** Can't create object of class 'PERSON_INNER'");
      EDMAEnd();
      exit(1);
    }
  edma_wprop3 (id, "name", "John Smith");
  edma_wprop3 (id, "degree", "PhD Computer Science");
  id1 = edma_upcast_obj (id, "PERSON_INNER");
  edma_met3 (id1, ".display");
  printf ("\n---------------------------------------\n");
  edma_show_subobjects_down (id1, "***", 0);
  printf ("\n---------------------------------------\n");
  printf ("**** Adding new GRADUATE_INNER1 subobject\n");
  printf ("---------------------------------------\n");
  

  /* Don't attach the new object because SUPER anchor point already exists
   * Should it link with the existing anchor point?
   * We need to enumerate the possible cases*/
  //edma_wprop3 (id, "GRADUATE_INNER1@INNER|SUPER<degree", "M.S Biology");
  edma_wprop3 (id, "GRADUATE_INNER1@INNER|SUPER1<degree", 
	       "M.S Electrical Engineering");
  edma_met3 (id1, ".display");
  printf ("\n---------------------------------------\n");
  edma_show_subobjects_down (id1, "***", 0);
  printf ("\n---------------------------------------\n");
  id1 = edma_downcast_obj (id, "INNER");
  edma_show_subobjects_up (id1, "***", 0);

  edma_free_obj (id);


  /* Shutdown EDMA Suystem */
  EDMAEnd();
  return 0;
}
