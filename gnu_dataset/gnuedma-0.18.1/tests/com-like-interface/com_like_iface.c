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
 * Simple example about how to map a single object in a way similar to
 * XX-COM systems.
 *
 * We use GNU/EDMA edma_new_obj as a generic object factory
 */
#include <edma.h>

typedef struct HELLO_WORLD_iface_t
{
  ESint32        (*born)(OBJID);
  ESint32        (*rip)(OBJID);
  EPVoid         (*say)(OBJID);
} IHELLO_WORLD;

int 
main ()
{
  OBJID        id;
  IHELLO_WORLD *iface;
  int          n;

  EDMAInit();

  id = edma_new_obj ("HELLO_WORLD");

  n = edma_get_all_met_func (id, (PPROC**)&iface);
  edma_printf ("Object %ld of class 'HELLO_WORLD' has %d methods",
	       id, n);

  /* For data member access use GNU/EDMA default accessors */
  edma_set_prop_sint32 (id, "num", 123);
  edma_set_prop_strz (id, "str", "Hello World!!!");
  iface->say(id);
  
  edma_free_obj (id);
  EDMAEnd();

}
