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
  OBJID            id_sc,id_sc1;
  POBJ             pObj;

  /* Initialize EDMA System*/
  EDMAInit ();


  /* TEST 1:
   * An object with two superclasses.
   * 
   * Both superclasses must be destroyed when the main object is free
   */
  edma_printf ("** TEST 1: Object with two superclasses");
  edma_printf ("***************************************");
  edma_printf ("-> Create test object (AP_TEST) Superclasses: GAP, HELLO_WORLD");
  if ((id = edma_new_obj ("AP_TEST")) == -1)  
    {
      EDMAEnd ();
      exit (1);
    }
  edma_wprop3 (id, "Id", "BASE");
  edma_met3 (id, "display");

  /* Attach a superobject */
  edma_met3 (id, "GAP@SUPER2>gap");
  edma_wprop3 (id, "HELLO_WORLD@SUPER1>str", "SUPER1");

  id_sc = edma_cast_obj (id, "SUPER1");
  edma_met3 (id_sc, "display");
  
  /* Show object reports */
  edma_obj_report (id);
  edma_obj_report (id_sc);

  /* Remove link to superobject on id*/
  //edma_remove_superclass_ap (id, "SUPER1");
  edma_free_obj (id);
  edma_printf ("--> Sub Object were destroyed autyomatically (ERRORS below)");
  edma_obj_report (id);
  edma_obj_report (id_sc);

  /* TEST 2: An object with two subobjects
   * Both subobjects must be destroyed
   */

  printf ("%s", "**********************************************************\n");
  printf ("%s", "********************  TEST 2 ********************************\n");
  printf ("%s", "**********************************************************\n");
  printf ("%s", "Press Any Key to Continue...\n");
  getchar ();

  id = edma_new_obj ("AP_TEST");
  edma_wprop3 (id, "HELLO_WORLD@INNER1<str", "INNER1");
  edma_met3 (id, "GAP@INNER2<gap");

  edma_obj_report (id);
  edma_free_obj (id);
  printf ("%s", "\n-----------------------------------------------------\n");
  edma_obj_report (id);

  /*
   * TEST 3: A combination of TEST1 & TEST2
   * Hierarchy has subobjects and superobjects
   */
  printf ("%s", "**********************************************************\n");
  printf ("%s", "********************** TEST 3 ****************************\n");
  printf ("%s", "**********************************************************\n");
  printf ("%s", "Press Any Key to Continue...\n");
  getchar ();

  id = edma_new_obj ("AP_TEST");

  /* Add two subobjects */
  edma_wprop3 (id, "HELLO_WORLD@INNER1|A1<str", "INNER1");
  edma_met3 (id, "GAP@INNER2|A2<gap");

  /* Add two superobjects */
  edma_wprop3 (id, "HELLO_WORLD@SUPER1|A3>str", "INNER2");
  edma_met3 (id, "GAP@SUPER2|A4>gap");


  edma_show_subobjects_down (id, "----", 2);
  edma_show_subobjects_up (id, "----", 2);

  edma_obj_report (id);
  edma_free_obj (id);

  /*
   * TEST 4:
   * A shared parent
   */
  printf ("%s", "**********************************************************\n");
  printf ("%s", "*********************** TEST 4  ***************************\n");
  printf ("%s", "**********************************************************\n");
  printf ("%s", "Press Any Key to Continue...\n");
  getchar ();

  id = edma_new_obj ("AP_TEST");
  printf ("[%d]--------------------------------------\n", id);

  edma_wprop3 (id, "HELLO_WORLD@INNER1<str", "INNER1");
  edma_met3 (id, "GAP@INNER2<gap");
  printf ("--------------------------------------\n");

  edma_obj_report (id);
  id_sc = edma_downcast_obj (id, "INNER2");
  printf ("--------------------------------------\n");

  edma_obj_report (id_sc);
  edma_free_obj (id_sc);
  edma_obj_report (id);

  id_sc = edma_cast_obj (id, "INNER1");
  edma_free_obj (id);
  edma_free_obj (id_sc);
  printf ("\n-----------------------------------------------------\n");
  /* Shutdown EDMA Suystem */
  EDMAEnd();
  return 0;
}
