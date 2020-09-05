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
#include <math.h>
#include <signal.h>
#include <time.h>
#include <unistd.h>

#include <edma.h>

ESint32 EDMAPROC my_met1 (OBJID, EPChar, ESint32);
ESint32 EDMAPROC my_met2 (OBJID, EPChar);

int
main(int argc,char *argv[])
{
  OBJID    id;

  /* Initialize EDMA System */
  EDMAInit ();
  
  /* Optionally you can create a file named edma_repos.lst with
   * the list of repositories you want to load at application 
   * start up
   */
  edma_add_std_repo ("edma_test.cfg");
  printf ("%s", "----------------------------------\n");

  /* Create an object of class ABSTRACT2
   * ABSTRACT2 is a class containing some abstract methods
   * met1 is pure abstract
   */
  if ((id = edma_new_obj ("ABSTRACT2")) == -1) 
    {
      printf ("%s", "Can't create ABSTRACT2 object. Aborting");
      EDMAEnd ();
      exit (1);
    }

  /* GNU/EDMA default abstract method handler will be executed for met1
   * a warnning will be shown in console the other methods are executed
   * normally.
   */
  edma_met3 (id, "met1", "Hello", 2);
  edma_met3 (id, "met2", "Hello");
  edma_met3 (id, "met3", "Hello");
  printf ("%s", "----------------------------------\n");
  
  /* Override virtual methods */
  /* Virtual methods on abstract class ABSTRACT2 are overriden
   * by main application functions 
   */
  edma_over_met (id, "met1", NULL, (PPROC) my_met1);
  edma_over_met (id, "met2", NULL, (PPROC) my_met2);

  /* Re-run ABSTRACT2 method. met3 is still bind to default abstract method */
  edma_met3 (id, "met1", "Hello", 2);
  edma_met3 (id, "met2", "Hello");
  edma_met3 (id, "met3", "Hello");
  printf ("%s", "----------------------------------\n");

  /* Finish with current object */
  edma_free_obj (id);

  /* Now, we test, pure abstract clases*/
  if ((id = edma_new_obj ("ABSTRACT1")) == -1) 
    {
      printf ("%s", "Can't create ABSTRACT1 object");
      EDMAEnd ();
      exit (1);
    }

  /* ABSTRACT1 is a pute virtual class. No method implementation */
  edma_met3 (id, "met1", "Hello", 2);
  edma_met3 (id, "met2", "Hello");  

  /* Override virtual methods on ABSTRACT1 */
  edma_over_met (id, "met1", NULL, (PPROC) my_met1);
  edma_over_met (id, "met2", NULL, (PPROC) my_met2);
  printf ("%s", "----------------------------------\n");
  edma_met3 (id, "met1", "Hello", 2);
  edma_met3 (id, "met2", "Hello");
  edma_free_obj (id);
  printf ("%s", "----------------------------------\n");

  /* Now, we test a realization of this interface*/
  if ((id = edma_new_obj ("REALIZATION")) == -1) 
    {
      printf ("%s", "Can't create ABSTRACT1 object");
      EDMAEnd ();
      exit (1);
    }

  edma_met3 (id, "met1", "Hello", 2);
  edma_met3 (id, "met2", "Hello");  

  /* Override virtual methods*/
  edma_over_met (id, "met1", NULL, (PPROC) my_met1);
  edma_over_met (id, "met2", NULL, (PPROC) my_met2);
  printf ("%s", "----------------------------------\n");
  edma_met3 (id, "met1", "Hello", 2);
  edma_met3 (id, "met2", "Hello");
  edma_free_obj (id);

  /* Shutdown EDMA Suystem */
  EDMAEnd ();
  return 0;
}

/* Overrider functions*/
ESint32 EDMAPROC 
my_met1 (OBJID IdObj, EPChar a, ESint32 b) 
{
  edma_printf_obj (IdObj, "New code for met1. Par1: '%s' Par2: %d",a ,b);
  
  return 0;
}

ESint32 EDMAPROC 
my_met2 (OBJID IdObj, EPChar a) 
{
  edma_printf_obj (IdObj, "New code for met2. Par1: '%s'",a);
  
  return 0;
}
