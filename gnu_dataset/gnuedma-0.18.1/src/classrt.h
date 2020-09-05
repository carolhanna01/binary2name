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
 * Class Run-Time Management Functions
 * REVISIONES:-------------------------------------------------------------
 * May, 17th, 2003
 * File creation
*/

#ifndef CLART_H
#define CLART_H

#include "portable.h"
#include "tclass.h"

#ifdef __cplusplus
extern "C"{
#endif

  CLASSID EDMAPROC _edma_get_local_class_id ();
  ESint32 EDMAPROC edma_add_local_class_property (CLASSID cid, EPChar name, EUint32 type, 
						  ESint32 access, EUint32 nelems);
  ESint32 EDMAPROC edma_add_local_class_method (CLASSID cid, EPChar name, EPChar sig, 
						PPROC f, 
						ESint32 mvirtual, 
						ESint32 mstatic, 
						ESint32 mabstract);
  ESint32 EDMAPROC edma_add_local_class_superclass (CLASSID cid, CLASSID supercid, EPChar pap, EPChar pap1);
  ESint32 EDMAPROC edma_add_local_class_superclass_by_name (CLASSID cid, EPChar classname, EPChar pap, EPChar pap1);

  ESint32 EDMAPROC edma_local_class_finish (CLASSID cid);
#ifdef __cplusplus
}
#endif

#endif
