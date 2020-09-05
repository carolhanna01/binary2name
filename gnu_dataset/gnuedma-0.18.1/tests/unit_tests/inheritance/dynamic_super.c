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

/*
 * This test dynamically builds a super-like inheritance hierarchy
 * to test dynamic inheritance primitives as well as super-like
 * hierarchies
 */

#include <stdio.h>
#include <edma.h>

main(int argc,char *argv[])
{
  OBJID            id, id1, id_aux;
  CLASSID          cid;

  /* Initialize EDMA System*/
  EDMAInit();

  /* Builds the basic hierarchies using PERSON hierarchy */
  edma_printf ("%s", "****[ Dynamic Specialization ]****");
  edma_printf ("%s", "****[ Create simple object of class PERSON]****");

  if (( id = edma_new_obj ("PERSON")) == -1)
    {
      fprintf (stderr, "Can't create object of class PERSON\n");
      EDMAEnd();
      return 1;
    }
  /* Change PERSON object on a GRADUATE object*/
  edma_printf ("%s", "****[Change PERSON object on a GRADUATE object]****");
  cid = edma_get_class_id ("GRADUATE");
  edma_add_subclass (id, cid, "DEGREE", "SUPER");
  edma_printf ("%s", "****[Show current object hierarchy ]****");
  edma_show_subobjects_down (id, "[ROOT]", 0);
  id_aux = edma_downcast_obj (id, "GRADUATE");
  edma_show_subobjects_up (id_aux, "[MOST_CONCRET]", 0);


  /* Add a third class to the hierarchy for testing purpose */
  edma_printf ("%s", "****[Create object of class EMPLOYER]****");
  if ((id_aux = edma_new_obj ("EMPLOYER")) == -1)
    {
      fprintf (stderr, "Can't create object of class EMPLOYER\n");
      EDMAEnd();
      return 1;
    }
  edma_printf ("%s", "****[Attach EMPLOYER object to our current hierarchy]****");
  id1 = edma_downcast_obj (id, "DEGREE");
  
  /* Now we link the object manually. Using add_superobject and add_subobject primitives
   * to directly manipulate subobject tables*/
  edma_add_subobject (id1, id_aux, "POSITION");
  edma_add_superobject (id_aux, id1, "SUPER");
  edma_printf ("%s", "****[Show the new object hierarchy]****");
  edma_show_subobjects_down (id, "[ROOT]", 0);
  edma_show_subobjects_up (id_aux, "[MOST_CONCRET]", 0);

  /* Method execution test */
  /* First set properties to see something when executing method display*/
  edma_printf ("%s", "****[Set properties to run 'display' method]****");
  edma_wprop3 (id_aux, "employer_number", 123);
  edma_wprop3 (id_aux, "degree", "M.S.");
  edma_wprop3 (id_aux, "name", "John Smith");

  edma_printf ("%s", "****[Run 'display' method]****");
  /* Note that we achieve the same result calling 'display' from the most general object
   * 'PERSON' than from the most general one 'GRADUATE'
   */
  edma_met3 (id, "display");
  /*edma_met3 (id_aux, "display");*/

  /* Now we will insert a new object between EMPLOYER and GRADUATE */
  cid = edma_get_class_id ("GAP");
  edma_insert_superclass (id_aux, cid, "SUPER", "EMPLOYER");
  edma_printf ("%s", "\n****[Show the new object hierarchy]****");
  edma_show_subobjects_down (id, "[ROOT]", 0);
  edma_show_subobjects_up (id_aux, "[MOST_CONCRET]", 0);

  edma_printf ("%s", "\n");
  edma_free_obj (id);
  /* Shutdown EDMA Suystem */
  EDMAEnd();
  return 0;
}
