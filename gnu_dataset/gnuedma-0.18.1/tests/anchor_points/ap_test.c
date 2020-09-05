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
  edma_printf ("%s", "Anchor Point Test");
  edma_printf ("%s", "-----------------------------------------------------");
  if ((id = edma_new_obj ("AP_TEST")) == -1)  
    {
      EDMAEnd ();
      exit (1);
    }

  /* Build a complex class hierarchy using On-Demand Inheritance */
  edma_printf ("-- Building Hierarchy with On-Demand Inheritance ----------");
  edma_wprop3 (id, "Id", "Father Object");
  edma_wprop3 (id, "AP_TEST@SUPER>Id", "Super 1");
  edma_wprop3 (id, "SUPER>AP_TEST@SUPER>Id", "Super 1.0");
  edma_wprop3 (id, "SUPER>AP_TEST@SUPER1>Id", "Super 1.1");
  edma_wprop3 (id, "SUPER>SUPER1>AP_TEST@MOST_TOP>Id", "Most Top");
  edma_wprop3 (id, "SUPER>SUPER>AP_TEST@MOST_TOP2>Id", "Most Top 2");
  edma_wprop3 (id, "SUPER>SUPER1>AP_TEST@MIDDLE>Id", "Middle");


  /* Get a reference to subobject at anchor point MOST_TOP */
  id_sc = edma_upcast_obj (id, "MOST_TOP"); 
  edma_show_subobjects_up (id, "My Object", 0);
  edma_printf ("---------------------------------------");
  edma_show_subobjects_down (id_sc, "Top Most", 0);
  edma_printf ("---------------------------------------\n");
  /* Show information of subobject at MOST_TOP anchor point */

  edma_printf ("-- Running display method on different levels -------------");
  /* Run some methods with specific anchor points */
  printf ("--> Running 'display'                  : ");
  edma_met3 (id_sc, "display"); 
  printf ("--> Running 'SUPER>SUPER1>display'     : ");
  edma_met3 (id, "SUPER>SUPER1>display");
  printf ("--> Running 'MIDDLE>display'           : ");
  edma_met3 (id, "MIDDLE>display");
  // Command below produces an inheritance cycle... 
  // See reanme below anchor points needs renaming
  printf ("--> Running 'AP_TEST<display' TOP_MOST : ");
  edma_met3 (id_sc,"AP_TEST<display");
  edma_printf ("%s", "---------------------------------------------------\n\n");


  edma_printf ("-- Renaming AP_TEST to INNER (MOST_TOP) ---------------");
  edma_printf ("-- To avaoid lookup cycle -----------------------------");
  /* Rename subclass anchor point AP_TEST in object id_sc to SUPER*/
  edma_rename_subclass_ap (id_sc, "AP_TEST", "INNER");
  printf ("--> Running 'INNER<display' TOP_MOST : ");
  edma_met3 (id_sc,"INNER<display");

  /* Show hierarchy again to show change of name */
  edma_show_subobjects_down (id_sc, "Root", 0);
  edma_printf ("%s", "-----------------------------------------------------");

  /* Use classpath operators to edit anchor points */
  /* Operator | allows to set at one time anchor points for up and down links */
  edma_printf ("-- Seting both anchor points with | ------------------------");
  edma_printf ("--> Setting Id for 'AP_TEST@SUPPER2|INNER1>");
  edma_wprop3 (id, "AP_TEST@SUPER2|INNER1>Id", "Super 2");
  edma_met3 (id, "SUPER2>display");

  edma_printf ("%s", "--- Showing New Hierarchy ---------------------------");
  edma_show_subobjects_up (id, "My Object", 0);
  edma_printf ("%s", "-----------------------------------------------------");
  id_sc = edma_upcast_obj (id, "SUPER2");
  edma_wprop3 (id_sc, ".Id", "Super2 Updated");
  edma_show_subobjects_down (id_sc, "SUPER2 SuperClass", 0);
  edma_printf ("%s", "-----------------------------------------------------");

  /* Get another reference to other object in the hierarchy 
   * and perform some other ops*/


  edma_printf ("%s", "--- Add Subobject to original object ------------------");
  edma_wprop3 (id, "AP_TEST@OINNER1|ORIGINAL<Id", "OINNER1");
  edma_met3 (id, "OINNER1<display");
  edma_printf ("%s", "-----------------------------------------------------");


  id_sc = edma_downcast_obj (id, "OINNER1");
  edma_show_subobjects_up (id_sc, "OINNER1", 0);
  edma_printf ("%s", "\n-----------------------------------------------------");
  edma_show_subobjects_down (id, "Original Obj", 0);
  edma_wprop3 (id, "OINNER1<Id", "Root Object (OINNER1)");
  edma_met3 (id, "display");

  edma_printf ("%s", "\n=== Some More examples ==============================");
  edma_met3 (id, "SUPER2>display");
  edma_met3 (id, "OINNER1<display");
 edma_printf ("%s", "\n=== Setting Object FInal (ERROR) =====================");

  /* Set object id to final so no more on-demand inheritance is allowed*/
  edma_set_obj_final (id,1);
  /* Comand bellow will fail */
  edma_met3 (id, "AP_TEST@UPFINAL|DOWNFINAL>display");

  edma_free_obj (id);

  edma_printf ("\n-----------------------------------------------------");
  /* Shutdown EDMA Suystem */
  EDMAEnd();
  return 0;
}
