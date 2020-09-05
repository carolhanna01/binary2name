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

#include <edma.h>

int test();

main(int argc,char *argv[])
{
  ESint32          i;
  OBJID            id;
  OBJID            id_sc;

  /* Initialize EDMA System*/
  EDMAInit();

  /* Put your C code here */
  edma_printf ("\n Runnable SIU Proxy Test");
  edma_printf ("+ Creating SUPER_CLASS object...");
  if ((id = edma_new_obj ("RUNABLE:SUPER_CLASS")) == -1)  
    {
      edma_printf ("Can't create SUPER_CLASS object");
      EDMAEnd();
      exit(1);
    }
  edma_printf ("Object Report for SUPER_CLASS object");
  edma_obj_report (id);
  edma_printf ("--------------------------------");
  edma_rprop3 (id, "id", &id_sc);
  edma_over_met (id_sc, "Met1", NULL, (PPROC)test);
  edma_met3 (id, "Met1");
  for (i = 0; i < 20; sleep(1), i++)
    edma_printf ("[INFO-main] Iteration %d on main thread",i);

  edma_free_obj (id);

  /* Shutdown EDMA Suystem */
  EDMAEnd();
  return 0;
}

/* Test method for runnable SIU Proxy*/
int test() {
  int     i;
  
  for (i=0;i<10;sleep(1),i++)
    edma_printf ("[INFO-test] Iteration %d on other thread",i);

  return 0;
}
