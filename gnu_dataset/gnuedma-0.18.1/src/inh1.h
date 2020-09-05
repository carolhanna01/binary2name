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
 * Vigo 25 de Abril de 1999
 *---------------------------------------------------------
 * Fichero de cabecera para módulo de funciones para herencia
 *REVISIONES:------------------------------------------------
 * Se incluyen las funciones para trabajar con la tabla de subclases
 * de objetos. Esta modificación forma parte del desarrollo del
 * sistema de herencia dinámicop SISI
 *-------------------------------------------------
 * Febraury, 20th, 2001
 * Code cleanup and comment translation
 * --------------------------------------------------
 * March 2nd, 2002
 * Include define was wrong. Fixed
 *********************************************************
 */
#ifndef INH1_31_H
#define INH1_31_H

#include "portable.h"

#ifdef __cplusplus
extern "C"{
#endif
  ESint32 EDMAPROC edma_add_subclasses_obj (POBJ,EPChar*,EPChar*);
  ESint32 EDMAPROC edma_over_subclasses_obj (POBJ,EUint32,EPChar,EPChar);
  ESint32 EDMAPROC edma_mutate_obj1 (POBJ,EPChar*,EPChar*);
  
  ESint32 EDMAPROC edma_merge_subclass_obj (OBJID,EPChar,OBJID);
  ESint32 EDMAPROC edma_free_subclass_obj (OBJID,EPChar);
  ESint32 EDMAPROC edma_add_subobject (OBJID id, OBJID subid, 
				       EPChar apoint);
  OBJID EDMAPROC edma_add_subclass (OBJID id, CLASSID cid, 
				    EPChar apoint1, EPChar apoint2);
  OBJID EDMAPROC edma_insert_subclass (OBJID id, CLASSID cid, 
				       EPChar apoint1, EPChar apoint2);
  OBJID EDMAPROC edma_insert_subobject (OBJID id, OBJID cid, 
				       EPChar apoint1, EPChar apoint2);

#ifdef __cplusplus
}
#endif
#endif
