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
 * Test locator functions
 *-----------------------------------------------------------------------
 * This program access properties and methods in a diamond-like
 * inheritance hierarchy to check locator functions in GNU/EDMA
 ************************************************************************/


#include <stdio.h>
#include <edma.h>

main(int argc,char *argv[])
{
  OBJID            id;

  /* Initialize EDMA System*/
  EDMAInit();
  edma_printf ("****----[ Method Access Tests ]----****");
  if ((id = edma_new_obj ("FINAL")) == -1)
    {
      edma_printf ("%s", "Can't create object. Aborting\n");
      EDMAEnd();
      return -1;
    }

  /* Set Subobject properties to check that the right method is called */
  /* - Non ambiguous properties can be accessed directly by its name */
  /* - Ambiguous properties (homonym ones) requires classpath to be accessed */
  edma_wprop3 (id, "name", "Final Name");
  edma_wprop3 (id, "final_priv", "PRIVATE::Final Name");

  edma_wprop3 (id, "SC1_LEVEL1>name", "Level1 SuperClass1");
  edma_wprop3 (id, "sc_level1_priv", "PRIVATE::Level1 SuperClass1");

  edma_wprop3 (id, "SC2_LEVEL1>name", "Level1 SuperClass2");
  edma_wprop3 (id, "sc2_level1_priv", "PRIVATE::Level1 SuperClass2");

  edma_wprop3 (id, "BASE_CLASE>name", "Base Class");
  edma_wprop3 (id, "base_priv", "PRIVATE::Base Class");

  /* Test begins */
  /*
   * Case 1: Normal method execution. Most concret method gets executed
   *         Run method display, defined in all the subclasses
   *         The most general method gets executed (the one in FINAL class)
   */
  edma_printf ("****[Run method in current object] => display() ********");
  edma_met3 (id, "display");

  /*
   * Case 2: Run ambiguous method. Classpath is required
   *         Runs method display in class SC1_LEVEL1 and in SC2_LEVEL1
   *         This is the same case that invoking a 'super' in Java or
   *         CLASS::display in C++
   */
  edma_printf ("\n****[Run method in superobject] => SC1_LEVEL1::display() ********");
  edma_met3 (id, "SC1_LEVEL1>display");

  edma_printf ("\n****[Run method in superobject] => SC2_LEVEL1::display() ********");
  edma_met3 (id, "SC2_LEVEL1>display");

  /*
   * Case 3: Run non-abiguous method provided by superclasses
   *         Runs method sc1_level1_display and sc2_level1_display defined in
   *         SC1_LEVEL1 and SC2_LEVEL1 superclasses.
   *
   *         Access can be done using a full classpath or just the name
   *         because a non-ambiguous methoid is being executed
   */
  edma_printf ("\n****[Run method in superobject] => sc1_level1_display() ********");
  edma_met3 (id, "sc1_level1_display");

  edma_printf ("\n****[Run method in superobject] => sc2_level1_display() ********");
  edma_met3 (id, "SC2_LEVEL1>sc2_level1_display");

  /*
   * Case 4: Run superclass method through partial classpath
   *         Runs method base_display, provided just a hint to achieve the right
   *         subobject, or, in general, an incomplete classpath.
   *        
   *         When using classpath method look up goes up the point indicated by
   *         the classpath
   */
  edma_printf ("\n****[Run method in base_class] => SC1_LEVEL1:base_display() ********");
  edma_met3 (id, "SC1_LEVEL1>base_display");

  edma_printf ("\n****[Run method in base_class] => BASE_CLASE:base_display() ********");
  edma_met3 (id, "BASE_CLASE>base_display");

  /* Case 5: Just like cases 3 and 4 but testing access to a three-level-deep class */
  edma_printf ("\n****[Run method in base_class] => base_display() ********");
  edma_met3 (id, "base_display");

  edma_printf ("\n****[Run method in base_class] => BASE_CLASE::display() ********");
  edma_met3 (id, "BASE_CLASE>display");
  
  edma_printf ("\n****[ Check 'dotNamed' methods ] *******************************************************");
  edma_printf ("\n****[Run method in base_class] => BASE_CLASE::.display() ********");
  edma_met3 (id, "BASE_CLASE>.display");

  edma_printf ("\n****[ ERRORS ] *******************************************************");
  edma_printf ("\n****[Run non existing method] => display1() ********");
  edma_met3 (id, "display1");

  edma_printf ("\n****[Run non existing method on existing classpath] => BASE_CLASE::display1() ********");
  edma_met3 (id, "BASE_CLASE>display1");

  edma_printf ("\n****[Run non existing method on non existing classpath] => BASE_CLASE1::display1() ********");
  edma_met3 (id, "BASE_CLASE1>display1");

  edma_printf ("%s", "\n****[Destroy object]****\n");
  edma_free_obj (id);
  /* Shutdown EDMA Suystem */
  EDMAEnd();
  return 0;
}
