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

#include <stdlib.h>

#include <edma.h>

int
main(int argc,char *argv[])
{
  OBJID            id, id_clon;
  OBJID            id_sc, id_sc1;

  /* Initialize EDMA System*/
  EDMAInit();

  /* TEST 1:
   * An object with two superclasses.
   * 
   * Both superclasses must be destroyed when the main object is free
   */

  if ((id = edma_new_obj ("AP_TEST")) == -1)  
    {
      EDMAEnd();
      exit(1);
    }
  edma_wprop3 (id, "Id", "BASE");

  /* Attach two superobject using On-Demand Inheritance */
  edma_met3 (id, "GAP@SUPER2>gap");
  edma_wprop3 (id, "HELLO_WORLD@SUPER1>str", "SUPER1");

  /* Attach two subobjects using On-Demand Inheritance */
  edma_met3 (id, "GAP@INNER2<gap");
  edma_wprop3 (id, "HELLO_WORLD@INNER1<str", "INNER1");

  id_sc = edma_cast_obj (id, "SUPER1");
  id_sc1 = edma_cast_obj (id, "SUPER2");
  
  /* Show object reports */
  edma_obj_report (id);

  //edma_obj_report (id_sc);
  //edma_obj_report (id_sc1);

  edma_show_subobjects_up (id, "Our Object", 2);
  edma_printf ("%s", "---[View Down from SUPER1 (HELLO_WORLD)]--------------");
  edma_show_subobjects_down (id_sc, "SUPER1", 2);
  edma_printf ("%s", "---[View Down from SUPER2 (GAP)]----------------------");
  edma_show_subobjects_down (id_sc1, "SUPER2", 2);
  edma_printf ("%s", "-----------------------------------------------------");

  id_clon = edma_shallow_clone_obj (id);
  /* Show object reports */
  edma_obj_report (id_clon);

  //edma_obj_report (id_sc);
  //edma_obj_report (id_sc1);

  edma_printf ("%s", "---[View Up from Cloned Object]----------------------");
  edma_show_subobjects_up (id_clon, "Our Clon", 2);
  edma_printf ("%s", "---[View Down from SUPER1 (HELLO_WORLD)]--------------");
  edma_show_subobjects_down (id_sc, "SUPER1", 2);
  edma_printf ("%s", "---[View Down from SUPER2 (GAP)]----------------------");
  edma_show_subobjects_down (id_sc1, "SUPER2", 2);
  edma_printf ("%s", "-----------------------------------------------------");
  edma_printf ("NOTE: Child objects are not shallow cloned as cycles can appear");

  /************************************/
  /* Checking cloning */
  edma_printf ("---[Running 'display' method]-------------------------------");
  edma_met3 (id, "display");
  edma_met3 (id_clon, "display");
  edma_printf ("---[Running 'say' method]----------------------------");
  edma_wprop3 (id, "SUPER1>str", "Changed from original");
  edma_met3 (id, "SUPER1>say");
  edma_met3 (id_clon, "SUPER1>say");
  edma_printf ("---[Running 'say' method]----------------------------");
  edma_wprop3 (id_clon, "SUPER1>str", "Changed from clon");
  edma_met3 (id, "SUPER1>say");
  edma_met3 (id_clon, "SUPER1>say");


  /* Changing clone properties */  
  edma_wprop3 (id_clon, "Id", "CLONED!!!");
  edma_printf ("---[Running 'display' method. After Updating properties]--");
  edma_met3 (id, "display");
  edma_met3 (id_clon, "display");

  edma_printf ("%s", "-----------------------------------------------------");  
  edma_free_obj (id);
  edma_free_obj (id_clon);


  /* Shutdown EDMA Suystem */
  EDMAEnd();
  return 0;
}
