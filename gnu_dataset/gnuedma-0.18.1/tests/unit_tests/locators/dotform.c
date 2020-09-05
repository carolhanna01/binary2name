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

main(int argc,char *argv[])
{
  OBJID            id, id_super;

  /* Initialize EDMA System*/
  EDMAInit();
  edma_printf ("****----[ Method Access Tests ]----****");
  if ((id = edma_new_obj ("FINAL")) == -1)
    {
      edma_printf ("%s", "Can't create object. Aborting\n");
      EDMAEnd();
      return -1;
    }

  id_super = edma_upcast_obj (id, "BASE_CLASE");

  edma_wprop3 (id_super, "name", "Final Name");
  edma_wprop3 (id_super, "final_priv", "PRIVATE::Final Name");

  edma_wprop3 (id_super, "SC1_LEVEL1<.name", "Level1 SuperClass1");
  edma_wprop3 (id_super, "sc_level1_priv", "PRIVATE::Level1 SuperClass1");

  edma_wprop3 (id_super, "SC2_LEVEL1<.name", "Level1 SuperClass2");
  edma_wprop3 (id_super, ".sc2_level1_priv", "PRIVATE::Level1 SuperClass2");

  edma_wprop3 (id_super, ".name", "Base Class");
  edma_wprop3 (id_super, "base_priv", "PRIVATE::Base Class");



  edma_printf ("****[Run method in current object] => .display() ********");
  edma_met3 (id_super, ".display");

  edma_printf ("\n****[Run method in subobject] => SC1_LEVEL1::.display() ********");
  edma_met3 (id_super, "SC1_LEVEL1<.display");

  edma_printf ("\n****[Run method in subobject] => SC2_LEVEL1::display() ********");
  edma_met3 (id_super, "SC2_LEVEL1<display");

  edma_printf ("\n****[Run method in subobject] => sc1_level1_display() ********");
  edma_met3 (id_super, "sc1_level1_display");
  // Core Dump
  edma_printf ("\n****[Run method in subobject] => sc2_level1_display() ********");
  edma_met3 (id_super, "sc2_level1_display");

  edma_printf ("\n****[Run method] => display() ********");
  edma_met3 (id_super, "display");

  edma_printf ("\n****[Run method in base_class] => base_display() ********");
  edma_met3 (id_super, "base_display");

  edma_printf ("%s", "\n****[Destroy object]****\n");
  edma_free_obj (id);
  /* Shutdown EDMA Suystem */
  EDMAEnd();
  return 0;
}
