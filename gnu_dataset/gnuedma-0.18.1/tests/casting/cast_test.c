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
  ESint32          res;
  OBJID            id;
  OBJID            id_sc,id_sc1;
  EPChar           SC_List[]= {"SUPER_CLASS",NULL};
  //EPChar           SC_Id[]={"MIID",NULL};
  EPChar           SC_Id[]={"MY_ID",NULL};

  /* Initialize EDMA System*/
  EDMAInit();

  /* Put your C code here */
  edma_printf ("Casting Test");

  /* An object of class SUBCLASS2 is created
   * This will create the following inheritance class:
   *   SUBCLASS2 => SUBCLASS1 => SUPER_CLASS
   *
   * where => means inherits from
   */
  edma_printf ("----------------------------------------------------");
  edma_printf ("+ Creating SUBCLASS2 object... and showing hierarchy");
  edma_printf ("----------------------------------------------------");
  if ((id = edma_new_obj ("SUBCLASS2",NULL)) == -1)  {
    edma_printf ("Can't create SUBCLASE2 object");
    EDMAEnd();
    exit(1);
  }

  /* Shows the inheritance diagram including anchor points from
   * the provided object up in the inheritance hierarchy*/
  edma_show_subobjects_up (id,"<My SUBCLASS2 Object>",0);
  
  /* Now, we will run some methods. These method are provided by
   * SUBCLASS2 that is the most "concrete" class in the hierarchy.
   * Met4 is actually provided by SUBCLASS1 and not SUBCLASS2
   */
  edma_printf ("-------------------------------------------------");
  edma_printf ("+ Running MetX in SUBCLASS2 object (%d)...", id);
  edma_printf ("-------------------------------------------------");

  edma_met3 (id,"Met1");
  edma_met3 (id,"Met2");
  edma_met3 (id,"Met3");
  edma_met3 (id,"Met4");

  /* Now we "upcast" the object, that is, we get a reference to
   * a subobject "up" in the inheritance hierarchy
   * The target for the upcast call is SUBCLASS1, that is
   * the direct superclass of our current object.
   *
   * The casting objects always returns a reference to the
   * associated subobject
   */
  edma_printf ("---------------------------------------------");
  edma_printf ("+ Upcasting object SUBCLASS2 to SUBCLASS1...");
  edma_printf ("---------------------------------------------");

  /* Upcast and information dump. Note as this subobject is linked
   * to superclass subobjects and derived class subobjects*/
  id_sc1 = edma_upcast_obj (id, "SUBCLASS1");

  /* Now run methods from our SUBCLASS1 reference. No diff*/
  edma_printf ("+ Running Met1 in SUBCLASS1 object (%d)...", id_sc1);
  edma_met3 (id_sc1, "Met1");
  edma_met3 (id_sc1, "Met4");
 
  /* The same but now from the very top of the hierarchy */
  edma_printf ("---------------------------------------------");
  edma_printf ("+ Upcasting object SUBCLASS2 to SUPERCLASS...");
  edma_printf ("---------------------------------------------");

  id_sc = edma_upcast_obj (id, "SUPER_CLASS");
  edma_printf ("+ Running MetX in SUPER_CLASS object [%d]. ", id_sc); 
  edma_met3 (id_sc, "Met1");
  edma_met3 (id_sc, "Met3");
  edma_met3 (id_sc, "Met4");
  edma_printf ("---------------------------------------------");

  /* Now upcast and down cast from a reference to the middle (SUBCLASS1) */
  id_sc = edma_upcast_obj (id_sc1, "SUPER_CLASS");
  edma_printf ("+ Upcast to SUPER_CLASS from SUBCLASS1 [%d]..."
	       " [%d]",id_sc1, id_sc);
  id_sc = edma_downcast_obj (id_sc1, "SUBCLASS2");
  edma_printf ("+ Downcast to SUBCLASS2 from SUBCLASS1[%d]..."
	       " [%d]", id_sc1, id_sc);

  edma_printf ("---------------------------------------------");
  edma_printf ("+ Now we add SUPER_CLASS to SUBCLASS2 object");
  edma_printf ("+ Assigning  MY_ID anchor point to the path");
  edma_printf ("---------------------------------------------");

  /* And change property for easy identification */
  edma_mutate_obj ( edma_get_obj_pobj(id), SC_List, SC_Id);
  edma_wprop3 (id, "MY_ID>prop1", 2);
  /* Both commands above are equivalent to this:
    edma_wprop3 (id, "SUPER_CLASS@MY_ID|INNER>prop1", 2);
  */

  /* Showing the current inheritance hierarchy */
  edma_show_subobjects_up (id,"<Our Nice Object>",0);

  edma_wprop3 (id, "SUBCLASS1>SUPER_CLASS>prop1", 1);

  edma_printf ("---------------------------------------------");

  /* Now we upcast our object through the two different paths */
  id_sc = edma_upcast_obj (id,"MY_ID");
  edma_printf ("+ Upcast from SUBCLASS2 [%d] to MY_ID...[%d]", id, id_sc);
  id_sc1 = edma_upcast_obj (id,"SUPER_CLASS");
  edma_printf ("+ Upcast from SUBCLASS2 [%d] to SUPER_CLASS...[%d]", 
	       id, id_sc1);


  /* Most concrete methods always resolves to the same*/
  edma_printf ("+ Running Met1 from any super class produces same result...");
  edma_met3 (id_sc,"Met1");
  edma_met3 (id_sc1, "Met1");

  edma_printf ("+ But each one can be access separatelly...");
  edma_met3 (id,"MY_ID>Met1");
  /* SUPER_CLASS is resolved through the first inheritance path */
  edma_met3 (id,"SUPER_CLASS>Met1");
  
  edma_free_obj (id);

  /* Shutdown EDMA Suystem */
  EDMAEnd();
  return 0;
}
