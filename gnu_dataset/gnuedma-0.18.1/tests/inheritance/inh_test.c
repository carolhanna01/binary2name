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

main(int argc,char *argv[])
{
  OBJID            id, id1;
  OBJID            id_sc,id_sc1;
  POBJ             pObj;

  /* Initialize EDMA System*/
  EDMAInit();
  
  /* Put your C code here */
  edma_print ("Creating a SQUIZO object. It has two PERSON subobjects");
  id = edma_new_obj ("SQUIZO");
  edma_print ("* Subobjects down from the SQUIZO Object");
  edma_print ("----------------------------------------");
  edma_show_subobjects_up (id, "***", 0);
  edma_print ("==========================================");
  id1 = edma_upcast_obj (id, "SUPER1");
  edma_print ("* Subobject up from first PERSON object");
  edma_print ("---------------------------------------");
  edma_show_subobjects_down (id1, "***", 0);
  edma_print ("==========================================");
  id1 = edma_upcast_obj (id, "SUPER2");
  edma_print ("* Subobject up from second PERSON object");
  edma_print ("----------------------------------------");
  edma_show_subobjects_down (id1, "***", 0);
  edma_print ("==========================================");
  edma_free_obj (id);

  /* The code bellow is part of the tests on repeated inheritance.
   * Not fully works
   */
  /*
  edma_print ("\n\n-------------------------------------------------------");
  edma_print ("* Creating a PERSON object and setting name to 'Walter'");
  edma_print ("-------------------------------------------------------");
  id = edma_new_obj ("PERSON");
  edma_wprop3 (id, "name", "Walter");

  edma_print ("* Running 'SQUIZO@INNER1|SUPER1<SUPER1>display' method");
  edma_print ("------------------------------------------------------");
  edma_met3 (id, "SQUIZO@INNER1|SUPER1<SUPER1>display");

  edma_print ("* Upcast to SQUIZO and showing subobjects up");
  edma_print ("--------------------------------------------");
  id1 = edma_downcast_obj (id, "SQUIZO");
  edma_show_subobjects_up (id1, "***", 0);
  edma_print ("==========================================");
  edma_print ("* Showing subobjects down from PERSON object");
  edma_print ("--------------------------------------------");
  edma_show_subobjects_down (id, "***", 0);
  edma_print ("\n*** Running 'SUPER1>display'");
  edma_met3 (id1, "SUPER1>display");
  edma_print ("\n*** Running 'SUPER2>display'");
  edma_met3 (id1, "SUPER2>display");
  edma_print ("\n*** Running '.display'");
  edma_met3 (id, ".display");
  getchar();
  
  edma_free_obj (id);
  edma_print ("\n********************************************");
  */
  edma_print ("\n\n-------------------------------------------------------");
  edma_print ("==> Creating object of class GRADUATE");
  edma_print ("-------------------------------------------------------");
  if ((id = edma_new_obj ("GRADUATE")) == -1)  
    {
      edma_print ("**ERR** Can't create object of class 'GRADUATE'");
      edma_print ("        Be sure class GRADUATE is installed. It'll be enough to\n"
		  "        run ./ines_class_register GRADUATE.ines as root\n\n"
		  "        GRADUATE uses class PERSON. Be sure it is also installed");
      
      EDMAEnd();
      exit(1);
    }
  edma_print ("==> Setting name, degree and running display on GRADUATE object");
  edma_wprop3 (id, "degree", "M.S.");
  edma_wprop3 (id, "name", "John Smith");
  edma_met3 (id, "display");
  edma_wprop3 (id, "EMPLOYER@INNER|SUPER<employer_number", 125);

  /* Show subobject struct */
  edma_print ("\n-----------------------------------------------------");
  edma_show_subobjects_up (id, "***", 0);
  edma_print ("\n-----------------------------------------------------");
  id1 = edma_downcast_obj (id, "EMPLOYER");
  edma_show_subobjects_up (id1, "***", 0);

  printf ("\n==> Running 'display' on GRADUATE object\n");
  edma_met3 (id, "display");
  edma_print ("\n-----------------------------------------------------");
  printf ("\n==> Running 'PERSON>display' on GRADUATE object\n");
  edma_met3 (id, "PERSON>display");
  edma_print ("\n-----------------------------------------------------");
  printf ("\n==> Running 'EMPLOYER<display' on GRADUATE object\n");
  edma_met3 (id, "EMPLOYER<display");
  printf ("\n==> Running 'PERSON>display' on GRADUATE object\n");
  edma_met3 (id, "PERSON>display");


  edma_print ("\n-----------------------------------------------------");

  edma_print ("==> Using GENERAL_TWO_SUPER");
  if (( id1 = edma_new_obj ("GENERAL_TWO_SUPER")) == -1)
    {
      edma_print ("**ERR** Can't create object of class GENERAL_TWO_SUPER");
      edma_print ("        Be sure the class is installed");
      EDMAEnd();
      exit (1);
    }
  
  edma_print ("    * GENERAL_TWO_SUPER run display in the two superclasses \n"
	      "      attached on SUPER1 and SUPER2 anchor points");

  edma_print ("\n-----------------------------------------------------");
  edma_printf ("-> Adding PERSON and GRADUATE_MIXIN to object");
  edma_wprop3 (id1, "PERSON@SUPER1>name", "John Smith");
  edma_wprop3 (id1, "GRADUATE_MIXIN@SUPER2>degree", "B.S.");
  
  edma_met3 (id1, "display"); 
  edma_free_obj (id1);
  
  printf ("\n");
  if (( id1 = edma_new_obj ("GENERAL_TWO_SUPER")) == -1)
    {
      edma_print ("**ERR** Can't create object of class GENERAL_TWO_SUPER");
      edma_print ("        Be sure the class is installed");
      EDMAEnd();
      exit (1);
    }  
  
  edma_printf ("\n-> Adding DOG and GRADUATE_MIXIN to object");
  edma_wprop3 (id1, "DOG@SUPER1>name", "Bobby"); 
  edma_wprop3 (id1, "GRADUATE_MIXIN@SUPER2>degree", "K9 Bomb Squad"); 
  edma_met3 (id1, "display");   
  printf ("\n");
  edma_free_obj (id1);  
  edma_free_obj (id);  

  /* Shutdown EDMA Suystem */
  EDMAEnd();
  return 0;
}
