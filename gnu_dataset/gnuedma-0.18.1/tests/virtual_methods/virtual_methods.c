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


ESint32 EDMAPROC my_func (OBJID IdObj)
{
  edma_printf ("[Main App] Invoked Met1 on SUPER_CLASS object");
  return 0;
}

int main (int argc, char *argv[])
{
  OBJID		id, id1;
  POBJ          pObj;
  
  EDMAInit();
  id = edma_new_obj ("SUBCLASS1");
  id1 = edma_upcast_obj (id, "SUPER_CLASS");
  
  /* Test Case 1. Run original methods */
  edma_printf ("%s", "--- Running Original methods ----------------------");
  edma_met3 (id, "Met1");
  edma_met3 (id, "SUPER_CLASS>Met1");

  edma_printf ("%s", "---- Showing Object Reports -------------------");
  edma_obj_report (id);
  edma_obj_report (id1);

  edma_printf ("%s", "\n\n");  
  edma_printf ("%s", "----------------------------------------------");
  edma_printf ("%s", "Override Met1 on SUPER_CLASS with function "
	       "in main app");
  edma_printf ("%s", "----------------------------------------------");

  edma_over_met (id, "SUPER_CLASS>Met1", NULL, (PPROC)my_func);
  edma_printf ("%s", "-> Running method 'SUPER_CLASS>Met1' on object");  
  edma_met3 (id, "SUPER_CLASS>Met1");
  edma_obj_report (id1);

  edma_printf ("%s", "\n\n");
  edma_printf ("%s", "----------------------------------------------");
  
  edma_printf ("%s", "Running original Met1 on SUPER_CLASS");  
  edma_printf ("%s", "----------------------------------------------");
  edma_old_met3 (id, "SUPER_CLASS>Met1");


  edma_printf ("%s", "\n\n");
  edma_printf ("%s", "----------------------------------------------");
  edma_printf ("%s", "Removing override on Met1 on SUPER_CLASS");
  edma_printf ("%s", "----------------------------------------------");
  edma_restore_met (id, "SUPER_CLASS>Met1");
  edma_printf ("%s", "-> Running method 'SUPER_CLASS>Met1' on object");  
  edma_met3 (id, "SUPER_CLASS>Met1");

  edma_printf ("%s", "\n\n");  
  edma_printf ("%s", "----------------------------------------------");
  edma_printf ("%s", "Override Met1 on SUPER_CLASS with Met4 on SUBCLASS1 "
	       "(over_met3)");

  printf ("-------------------------------------------\n");

  edma_over_met3 (id, "SUPER_CLASS>Met1", "Met4");
  printf ("---- Running Met1 on SUPER_CLASS------------------\n");

  edma_met3 (id, "SUPER_CLASS>Met1");
  printf ("---- Running Original Met1 on SUPER_CLASS ----------\n");
  edma_old_met3 (id, "SUPER_CLASS>Met1");
  printf ("-------------------------------------------\n");
  edma_obj_report (id1);
  printf ("-------------------------------------------\n");

  edma_printf ("%s", "----------------------------------------------");
  edma_printf ("%s", "** Restore Met1 on SUPER_CLASS");

  edma_restore_met (id, "SUPER_CLASS>Met1");
  edma_met3 (id, "SUPER_CLASS>Met1");
  edma_met3 (id, "Met4");
  edma_obj_report (id1);
  edma_printf ("%s", "Press any key to continue....");

  edma_free_obj (id);
  edma_printf ("%s", "----------------------------------------------");
  EDMAEnd();
}
