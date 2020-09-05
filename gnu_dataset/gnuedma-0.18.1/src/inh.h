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

/**********************************************************
 * Entorno de Desarrollo Modular y Abierto
 * (c) David Martínez Oliveira
 * Versión Beta 0.3r1
 * Vigo 22 de Junio de 1997
 *---------------------------------------------------------
 * Fichero de cabecera para módulo de funciones para herencia
 * REVISIONES:------------------------------------------------
 * Febraury, 20th, 2001
 * Code cleanup and comment translation
 *********************************************************
 */
#ifndef INH31_H
#define INH31_H

#include "portable.h"

#ifdef __cplusplus
extern "C"{
#endif
  /* Static inheritance Primitives*/
  ESint32 EDMAPROC edma_derive_class (EPChar,EPChar*,EPChar *, EPChar *);
  
  /* Dynamic inheritance Primitives*/
  ESint32 EDMAPROC edma_add_superclasses_obj (POBJ,EPChar*,EPChar*);
  ESint32 EDMAPROC edma_over_superclasses_obj (POBJ,EUint32,EPChar,EPChar);
  ESint32 EDMAPROC edma_mutate_obj (POBJ,EPChar*,EPChar*);

  /* Object Merging Primitives*/
  ESint32 EDMAPROC edma_merge_superclass_obj (OBJID,EPChar,OBJID);
  ESint32 EDMAPROC edma_free_superclass_obj (OBJID,EPChar);

  OBJID EDMAPROC edma_add_superclass (OBJID id, CLASSID cid, 
				      EPChar apoint1, EPChar apoint2);
  ESint32 EDMAPROC edma_add_superobject (OBJID id, OBJID subid, 
					 EPChar apoint);
  OBJID EDMAPROC edma_insert_superclass (OBJID id, CLASSID cid, 
					 EPChar apoint1, EPChar apoint2);
  OBJID EDMAPROC edma_insert_superobject (OBJID id, OBJID superobj, 
					 EPChar apoint1, EPChar apoint2);
#ifdef __cplusplus
}
#endif
#endif
