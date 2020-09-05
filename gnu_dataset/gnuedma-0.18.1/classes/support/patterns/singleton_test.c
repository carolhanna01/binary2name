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

main(int argc,char *argv[])
{
  OBJID    id,id_sc,id1;

  EDMAInit();
  printf ("\n-------------------------------------\n");
  /* Put your C code here */

  edma_smet3 ("SINGLETON","Register","SUBCLASS1",-1);
  edma_smet3 ("SINGLETON","Register","SUPER_CLASS",-1);

  id = (OBJID) edma_smet3 ("SINGLETON","Instance","SUBCLASS1");
  id_sc = (OBJID) edma_smet3 ("SINGLETON","Instance","SUPER_CLASS");
  printf ("\nSingleton object id : %d. for superclass: %d\n",id,id_sc);

  if ((id1 = edma_new_obj ("SUBCLASS2"))==-1) {
    fprintf (stderr,"Can't create object of SUBCLASS2");
  } else {
    edma_smet3 ("SINGLETON","Register","SUBCLASS2",id1);
    id = (OBJID) edma_smet3 ("SINGLETON","Instance","SUBCLASS1");
    id_sc = (OBJID) edma_smet3 ("SINGLETON","Instance","SUBCLASS2");
    printf ("\nSingleton object SUBCLASS1 : %d. SUBCLASS2: %d\n",id,id_sc);
  }
  printf ("\n-------------------------------------\n");
  /* Shutdown EDMA Suystem */
  EDMAEnd();
  return 0;
}
