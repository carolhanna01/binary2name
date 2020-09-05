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
  OBJID            id;
  OBJID            id_sc;

  /* Initialize EDMA System*/
  EDMAInit ();

  /* Put your C code here */
  edma_printf ("Anchor Point Test");
  edma_printf ("-------------------------------------------------------");

  edma_printf ("******************************************************");
  edma_printf ("Test using SubClasses ");
  edma_printf ("******************************************************");


  if ((id = edma_new_obj ("AP_TEST")) == -1)  
    {
      EDMAEnd();
      exit(1);
    }

  edma_wprop3 (id, "Id", "I'm an Object");
  edma_printf ("--> Creating AP_TEST and adding single subclass GAP");
  /* Add a new subclass to object if of class GAP at anchor point INNER */
  edma_met3 (id, "GAP@INNER<gap");
  /* Show report of current object */
  edma_obj_report (id);
  printf ("-> Running Display on object : ");  
  edma_met3 (id, "display"); 
  edma_printf ("--> Overwriting INNER anchor point ");
  /* Override anchor point INNER with a new object of class DOG */
  edma_wprop3 (id, "DOG@!INNER<name", "Bobby");
  printf ("-> Running 'display' on object : ");
  edma_met3 (id, "display"); 
  printf ("\n");
  printf ("-> Running '.display' on object : ");
  edma_met3 (id, ".display"); 
  printf ("\n");
  /* Show the changes in the hierarchy */
  edma_obj_report (id);
  edma_show_subobjects_down (id, "My Obj", 2);
  id_sc = edma_downcast_obj (id, "INNER");
  edma_obj_report (id_sc);
  edma_show_subobjects_up (id_sc, "Inner Obj", 2);

  /* Finished. Destroying object */
  edma_free_obj (id);

  edma_printf ("******************************************************");
  edma_printf ("Test using SuperClasses ");
  edma_printf ("******************************************************");

  /* Repeat the test using superclasses*/
  id = edma_new_obj ("AP_TEST"); 
  edma_printf ("--> Creating AP_TEST and adding single subclass GAP");
  edma_wprop3 (id, "Id", "I'm another Object");

  edma_met3 (id, "GAP@SUPER>gap");
  edma_obj_report (id);

  printf ("-> Running Display on object : ");  
  edma_met3 (id, "display"); 
  edma_printf ("--> Overwriting SUPER anchor point ");
  /* Override anchor point INNER with a new object of class DOG */
  edma_wprop3 (id, "DOG@!SUPER>name", "Bobby");
  printf ("-> Running 'SUPPER>display' on object : ");
  edma_met3 (id, "SUPER>display"); 
  printf ("\n");
  printf ("-> Running 'display' on object : ");
  edma_met3 (id, "display"); 
  printf ("\n");

  edma_obj_report (id);
  edma_show_subobjects_up (id, "Original", 2);
  id_sc = edma_upcast_obj (id,"SUPER");
  edma_obj_report (id_sc);
  edma_show_subobjects_down (id_sc, "Super Obj", 2);
  edma_free_obj (id);  
  /* Shutdown EDMA Suystem */
  EDMAEnd ();
  return 0;
}
