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

/************************************************************************
 * Test for virtual method support
 *-----------------------------------------------------------------------
 * This program checks virtual method support in GNU/EDMA
 ************************************************************************/


#include <stdio.h>
#include <edma.h>


int
my_func (OBJID id)
{
  edma_printf ("[Main-App] New code for method BASE_CLASS::Met1");
  return 0;
}

int
my_func_fw (OBJID id)
{
  edma_printf ("[Main-App] New code for method BASE_CLASS::Met1");
  edma_old_met3 (id, "Met1");
  edma_old_met3 (id, ".Met1");
  /* Executing edma_met3 (id, ".Met1"); will create an infinite loop */
  /* The loop is detected by GNU/EDMA*/
  return 0;
}



main(int argc,char *argv[])
{
  OBJID            id;

  /* Initialize EDMA System*/
  EDMAInit();
  edma_printf ("****----[ Virtual Method Tests ]----****");
  if ((id = edma_new_obj ("SUBCLASS1")) == -1)
    {
      edma_printf ("%s", "Can't create object. Aborting\n");
      EDMAEnd();
      return -1;
    }

  edma_printf ("\n****[Run method Met1] => Met1() ********");
  edma_met3 (id, "Met1");

  edma_printf ("\n****[Overwrite method SUPER_CLASS::Met1 with function in main app] "
	       "=> SUPER_CLASS::Met1() ********");
  edma_over_met (id, "SUPER_CLASS>Met1", NULL, (PPROC)my_func);
  edma_met3 (id, "SUPER_CLASS>Met1");

  edma_printf ("\n****[Run original method SUPER_CLASS::Met1 (edma_old_met3)] "
	       "=> SUPER_CLASS::Met1() ********");
  edma_old_met3 (id, "SUPER_CLASS>Met1");

  edma_printf ("\n****[Restoring original method SUPER_CLASS::Met1] => SUPER_CLASS::Met1() ********");
  edma_restore_met (id, "SUPER_CLASS>Met1");
  edma_met3 (id, "SUPER_CLASS>Met1");


  edma_printf ("\n****[Overwrite method SUPER_CLASS::Met1 with function in main app (forwarding)] "
	       "=> SUPER_CLASS::Met1() ********");
  edma_over_met (id, "SUPER_CLASS>Met1", NULL, (PPROC)my_func_fw);
  edma_met3 (id, "SUPER_CLASS>Met1");

  edma_printf ("\n****[Restoring original method SUPER_CLASS::Met1] => SUPER_CLASS::Met1() ********");
  edma_restore_met (id, "SUPER_CLASS>Met1");
  edma_met3 (id, "SUPER_CLASS>Met1");






  edma_printf ("\n****[Overwritting SUPER_CLASS::Met1 with method Met2] => SUPER_CLASS::Met1() ********");
  edma_over_met3 (id, "SUPER_CLASS>Met1", "Met2");
  edma_met3 (id, "SUPER_CLASS>Met1");

  edma_printf ("%s", "\n****[Destroy object]****\n");
  edma_free_obj (id);
  /* Shutdown EDMA Suystem */
  EDMAEnd();
  return 0;
}


