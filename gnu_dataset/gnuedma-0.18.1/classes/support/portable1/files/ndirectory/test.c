/*
 * EDMA: Entorno de Desarrollo Modular y Abierto
 * Object Oriented and Componetware Framework
 * Copyright (C) 1998, 2002, 2003, 2004, 2005, 2006, 2007, 2008. 2010
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

main ()
{
  OBJID    id;
  EPChar   name;
  ESint32  type;
  
  EDMAInit();
  if ((id = edma_new_obj ("NDIRECTORY")) == -1)
    {
      fprintf (stderr, "Can't create object\n");
      exit (1);
    }
  edma_met3 (id, "opendir", ".");
  edma_printf ("%s", "------------------------------\n");

  while ( -1 != edma_met3 (id, "readdir", &name, &type))
    printf ("[%d] -> %s \n", type, name);

  edma_met3 (id, "closedir");
  edma_printf ("%s", "------------------------------\n");
  
  EDMAEnd();
}
