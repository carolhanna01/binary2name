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
  OBJID     id;
  OBJID     the_exception;
  EChar     name[80];
  /* Initialize EDMA System*/
  EDMAInit();

  edma_printf ("INVALID CLASS...");
  edma_printf ("----------------------------------");
  id = edma_new_obj ("A_CLASS");
  edma_printf ("----------------------------------");
  edma_printf ("INVALID OBJECT...");
  edma_printf ("----------------------------------");
  edma_met3 (-1,"aaaa");
  edma_printf ("----------------------------------");
  edma_printf ("INVALID CLASS IDENTIFIER...");
  edma_printf ("----------------------------------");
  edma_get_class_name (-1, name);
  edma_printf ("----------------------------------");
  edma_printf ("INVALID METHOD IDENTIFIER...");
  edma_printf ("----------------------------------");
  id = edma_new_obj ("HELLO_WORLD");
  edma_met3 (id, "A_method");
  edma_print ("==============================");

  edma_print ("EXCEPTION CATCH (NESTING)....");
  edma_printf ("----------------------------------");
  EDMA_TRY 
    {
      edma_met3 (id,"say");
      EDMA_TRY 
	{
	  edma_wprop3 (id,"A_Property");
	  edma_met3 (id, "Other_method");
	} 
      EDMA_CATCH (the_exception) 
	{
	  edma_printf ("----------------------------------");
	  edma_printf ("INNER exception catched!!!!!!!!");
	  edma_printf ("----------------------------------");
	  edma_met3 (the_exception, "print");
	} 
      EDMA_TRY_END;
      edma_met3 (id, "Just_Other_method");
      edma_print ("==============================");
    } 
  EDMA_CATCH (the_exception) 
    {
      edma_printf ("----------------------------------");
      edma_printf ("OUTER exception catched !!!!!!!!!!");
      edma_printf ("----------------------------------");
      edma_met3 (the_exception, "print");
      edma_print ("==============================");
    } 
  EDMA_TRY_END;

  edma_free_obj (id);
  edma_printf ("----------------------------------");
  edma_printf ("INVALID OBJECT IDENTIFIER free_obj...");
  edma_printf ("----------------------------------");
  edma_free_obj (-1);
  /* Shutdown EDMA Suystem */
  EDMAEnd();
  return 0;
}
