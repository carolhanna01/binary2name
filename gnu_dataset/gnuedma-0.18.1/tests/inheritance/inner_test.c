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
  edma_print ("\nINNER Inheritance Test");
  edma_print ("******************************************");
  edma_print ("==> Creating object of class PERSON_INNER");
  if ((id = edma_new_obj ("PERSON_INNER")) == -1)  
    {
      edma_print ("**ERR** Can't create object of class 'PERSON_INNER'");
      EDMAEnd();
      exit(1);
    }
  edma_met3 (id, "display");
  edma_print ("\n==> Adding GRADUATE_INNER to our PERSON_INNER object");
  edma_wprop3 (id, "GRADUATE_INNER@INNER<degree", "Mr");
  edma_print ("==> Running '.display' on object 'PERSON_INNER'");
  edma_met3 (id, ".display");
  edma_print ("\n==> Running 'display' on object 'PERSON_INNER'");
  edma_met3 (id, "display");
  edma_free_obj (id);  

  edma_print ("\n==> Repeating the process with a gap down in class hierarchy");
  id = edma_new_obj ("PERSON_INNER");
  edma_wprop3 (id, "name", "John Smith");
  /* use on-deman inheritance to add GAP class*/
  edma_met3 (id, "GAP@INNER|SUPER<gap");

  edma_wprop3 (id, "INNER<GRADUATE_INNER@INNER|SUPER<degree", "B.S.");
  edma_show_subobjects_down (id, "****", 0);
  id1 = edma_downcast_obj (id, "GRADUATE_INNER");
  edma_print ("=======================================================");
  edma_show_subobjects_up (id1, "****", 0);
  edma_print ("******************************************************");

  edma_met3 (id, "side_display");
  edma_met3 (id, ".display");
  edma_print ("\n******************************************************");
  edma_wprop3 (id, "INNER<INNER<EMPLOYER_INNER@INNER|SUPER<employer_number", 
	       123);
  edma_print ("\n******************************************************");
  edma_met3 (id, ".display");
  edma_print ("\n");
  edma_met3 (id, "pay", 500);

  edma_free_obj (id);

  id1 = edma_new_obj ("EMPLOYER_INNER");
  /* CAUTION: Next line add PERSON_INNER class but then access employer_number 
   * in superclass EMPLOYER_INNER */
  edma_wprop3 (id1, "PERSON_INNER@INNER|SUPER<employer_number", 127);
  //edma_wprop3 (id1, "PERSON_INNER@INNER|SUPER<name", "James Hutson");
  
  edma_wprop3 (id1, "INNER<GRADUATE_INNER@INNER|SUPER<degree", 
	       "B.S. Computer Science,");
  edma_printf ("%s", "#################################################"
	       "###################");
  edma_show_subobjects_up (id1, "****", 0);
  edma_show_subobjects_down (id1, "****", 0);
  edma_printf ("%s", "#################################################"
	       "###################");
  edma_wprop3 (id1, "GRADUATE_INNER<GRADUATE_INNER@INNER|SUPER<degree", 
	       "M.S. Electrical Engineering,");
  edma_print ("\n******************************************************");
  edma_show_subobjects_down (id1, "-----", 0);
  //edma_wprop3 (id1, "employer_number", 125);
  edma_met3 (id1, "display");
  printf ("\n");
  edma_met3 (id1, ".display");
  printf ("\n");
  edma_print ("\n******************************************************");
  edma_met3 (id1, "pay", 1000);
  edma_met3 (id1, "GRADUATE_INNER<.display");
  edma_free_obj (id1);

  printf ("\nMain -----------------------------------------------------\n");
  /* Shutdown EDMA Suystem */
  EDMAEnd();
  return 0;
}
