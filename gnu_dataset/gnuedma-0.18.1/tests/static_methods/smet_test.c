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

int
main(int argc,char *argv[])
{
  ESint32          res;
  /* Initialize EDMA System*/
  EDMAInit();

  /* Put your C code here */
  edma_smet3 ("UNKNOWN_CLASS","SMet1","Hola Mundo!!!",50);
  edma_printf ("===================================================");
  edma_smet3 ("STATIC_MET_TEST","UnknownMethod","Hola Mundo!!!",50);

  edma_printf ("-> RUnning SMet1");
  edma_smet3 ("STATIC_MET_TEST","SMet1","Hola Mundo!!!",50);
  edma_printf ("-> RUnning SMet2");
  res = (ESint32) edma_smet3 ("STATIC_MET_TEST","SMet2",10,20);
  edma_printf ("Result : %d\n",res);

  /* Shutdown EDMA Suystem */
  EDMAEnd();
  return 0;
}
