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

#include <edma.h>

int
main(int argc,char *argv[])
{
  OBJID            id;
  OBJID            id_sc,id_sc1;
  POBJ             pObj;

  /* Initialize EDMA System*/
  EDMAInit();

  /* Put your C code here */
  /*edma_set_debug_level (0);*/
  edma_printf ("%s", "[**] *******************************************");
  edma_printf ("%s", "[**] Creating object of Class 'AP_TEST_SUBCLASS'");
  edma_printf ("%s", "[**] *******************************************");
  if ((id = edma_new_obj ("AP_TEST_SUBCLASS")) == -1)  
    {
      EDMAEnd();
      exit(1);
    }
  edma_printf ("%s", "[**] *******************************************");
  edma_printf ("%s", "[**] Showing information about object and superobject");
  edma_printf ("%s", "[**] ************************************************");

  edma_obj_report (id);
  id_sc = edma_upcast_obj (id, "AP_TEST");
  edma_obj_report (id_sc);

  edma_printf ("%s", "[**] *******************************************");
  edma_printf ("%s", "[**] Showing Inheritance tree"); 
  edma_printf ("%s", "[**] *******************************************");
  edma_show_subobjects_down (id_sc, "[-- BASE --]", 2);
  printf ("-----------------------------------------------------\n");
  edma_show_subobjects_up (id, "[-- LAST --]", 2);
  edma_printf ("%s", "[**] *******************************************");
  edma_printf ("%s", "[**] Changing 'CONCRETE' subobject");
  edma_printf ("%s", "[**] --> edma_met3 (id_sc, \"AP_TEST_SUBCLASS1@!CONCRET>met1\");");
  edma_printf ("%s", "[**] *******************************************");

  /*
   * THis was a bug sometime ago...
  edma_printf ("%s", "$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$");
  edma_met3 (id_sc, "AP_TEST_SUBCLASS1@!CONCRETE>met1");
  edma_printf ("%s", "$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$");
  */
  edma_wprop3 (id_sc, "AP_TEST_SUBCLASS1@!CONCRET<Id", "Hello");
  edma_met3 (id_sc, "met1");
  edma_printf ("%s", "[**] *******************************************");
  edma_printf ("%s", "[**] Showing information about object and superobject");
  edma_printf ("%s", "[**] ************************************************");
  edma_obj_report (id_sc);
  id_sc1 = edma_downcast_obj (id_sc, "CONCRET");
  edma_obj_report (id_sc1);
  edma_printf ("%s", "[**] *******************************************");
  edma_printf ("%s", "[**] Showing Inheritance tree");
  edma_printf ("%s", "[**] *******************************************");
  edma_show_subobjects_down (id_sc, "[-- BASE --]", 2);
  printf ("-----------------------------------------------------\n");
  edma_show_subobjects_up (id_sc1, "[-- LAST --]", 2);
  printf ("-----------------------------------------------------\n");
  printf ("-----------------------------------------------------\n");
  /* Try some illegal anchor points*/
  edma_printf ("%s", "**** Non existing classes and anchor points");
  edma_met3 (id, "NON_CLASS@NON_ANCHOR_POINT>aProp");
  edma_met3 (id, "NON_CLASS@NON_ANCHOR_POINT<aProp");
  printf ("-----------------------------------------------------\n");
  edma_printf ("%s", "**** Non existing classses");
  edma_met3 (id_sc1, "NON_CLASS@CONCRETE<aProp");
  edma_met3 (id_sc1, "NON_CLASS@!CONCRETE<aProp");

  edma_met3 (id_sc, "NON_CLASS@CONCRETE>aProp");
  edma_met3 (id_sc, "NON_CLASS@!CONCRETE>aProp");
  printf ("-----------------------------------------------------\n");
  edma_show_subobjects_down (id_sc, "[-- BASE --]", 2);
  printf ("-----------------------------------------------------\n");
  edma_show_subobjects_up (id_sc1, "[-- LAST --]", 2);
  printf ("-----------------------------------------------------\n");
  
  /* Some complex links */
  edma_wprop3 (id_sc, "CONCRET<AP_TEST_SUBCLASS@CONCRET1<Id", "Deeper");
  id_sc1 = edma_downcast_obj (id_sc, "CONCRET1");
  printf ("--- id_sc --------------------------------------------------\n");
  edma_show_subobjects_down (id_sc, "[-- BASE --]", 2);
  edma_show_subobjects_up (id_sc, "[-- BASE --]", 2);
  printf ("---- id_sc1 -------------------------------------------------\n");
  edma_show_subobjects_up (id_sc1, "[-- CONCRET1 --]", 2);
  printf ("-----------------------------------------------------\n");

  id_sc1 = edma_downcast_obj (id_sc, "CONCRET");
  printf ("-----------------------------------------------------\n");
  edma_show_subobjects_down (id_sc1, "[-- CONCRET --]", 2);
  printf ("-----------------------------------------------------\n");
  edma_show_subobjects_up (id_sc1, "[-- CONCRET --]", 2);
  printf ("-----------------------------------------------------\n");
  edma_obj_report (id_sc);
  edma_free_obj (id_sc);
  printf ("\n-----------------------------------------------------\n");
  /* Shutdown EDMA Suystem */
  EDMAEnd();
  return 0;
}
