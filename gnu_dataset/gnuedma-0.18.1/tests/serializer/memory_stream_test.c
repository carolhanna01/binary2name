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

//#include <stdio.h>
#include <string.h>

#include <edma.h>

int main (int argc, char *argv[])
{
  OBJID		id;
  EDMAT_BUFFER  wbuf, rbuf;
  ESint32       len;
  
  EDMAInit();
  id = edma_new_obj ("MEMORY_STREAM");
  edma_buffer_alloc (&wbuf, 20);
  edma_buffer_alloc (&rbuf, 50);
  memcpy (wbuf.dat, "Hello World!!!", 14);
  len = 14;
  edma_met3 (id, "write", wbuf, &len);
  edma_met3 (id, "write", wbuf, &len);
  len = 60;

  edma_met3 (id, "rewind");
  edma_met3 (id, "read", &rbuf, &len);
  edma_printf ("%ld bytes read. buffer is: %s\n", len, rbuf.dat);

  edma_met3 (id, "read", &rbuf, &len);
  edma_printf ("%ld bytes read. buffer is: %s\n", len, rbuf.dat);

  printf ("=======================================================\n");
  edma_free_obj (id);
  EDMAEnd();
}
