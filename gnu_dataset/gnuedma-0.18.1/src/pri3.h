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
 Entorno de Desarrollo Modular y Abierto
 (c) David Martínez Oliveira
 Versión Beta 0.3r1
 2 de Julio de 1997

 Primitivas de Objeto de nivel 3 (HEADER)

 REVISIONES:-------------------------------------------------------------
 * Febraury, 24th, 2001
 * Code cleanup and comment translation
*/

#ifndef PRI3_H
#define PRI3_H
#include "portable.h"
#include "tobj.h"
#include "tclass.h"

#ifdef __cplusplus
extern "C"{
#endif

  EPVoid  EDMAPROC edma_get_data_ref (OBJID);
  ESint32 EDMAPROC edma_met3 (OBJID IdObj, EPChar Id1,...);
  ESint32 EDMAPROC edma_met3s (OBJID IdObj, EPChar Id1, EPChar Sig,...);
  ESint32 EDMAPROC edma_wprop3 (OBJID IdObj, EPChar Id1,...);
  ESint32 EDMAPROC edma_rprop3 (OBJID IdObj, EPChar Id1,...);  

  ESint32 EDMAPROC edma_set_prop_sint32 (OBJID IdObj, EPChar Id1, ESint32 val);
  ESint32 EDMAPROC edma_set_prop_uint32 (OBJID IdObj, EPChar Id1, EUint32 val);
  ESint32 EDMAPROC edma_set_prop_sint16 (OBJID IdObj, EPChar Id1, ESint16 val);
  ESint32 EDMAPROC edma_set_prop_uint16 (OBJID IdObj, EPChar Id1, EUint16 val);
  ESint32 EDMAPROC edma_set_prop_sint8  (OBJID IdObj, EPChar Id1, ESint8 val);
  ESint32 EDMAPROC edma_set_prop_uint8  (OBJID IdObj, EPChar Id1, EUint8 val);
  ESint32 EDMAPROC edma_set_prop_strz   (OBJID IdObj, EPChar Id1, EPChar val);
  ESint32 EDMAPROC edma_set_prop_obj    (OBJID IdObj, EPChar Id1, OBJID val);

  ESint32 EDMAPROC edma_get_prop_sint32 (OBJID IdObj, EPChar Id1);
  EUint32 EDMAPROC edma_get_prop_uint32 (OBJID IdObj, EPChar Id1);
  ESint16 EDMAPROC edma_get_prop_sint16 (OBJID IdObj, EPChar Id1);
  EUint16 EDMAPROC edma_get_prop_uint16 (OBJID IdObj, EPChar Id1);
  ESint8 EDMAPROC  edma_get_prop_sint8  (OBJID IdObj, EPChar Id1);
  EUint8 EDMAPROC  edma_get_prop_uint8  (OBJID IdObj, EPChar Id1);

  EPChar EDMAPROC edma_get_prop_strz (OBJID IdObj, EPChar Id1, EPChar val);
  EDMAT_BUFFER *EDMAPROC edma_get_prop_buffer (OBJID IdObj, EPChar Id1, 
					       EDMAT_BUFFER *buf);
  OBJID EDMAPROC edma_get_prop_obj (OBJID IdObj, EPChar Id1);

#ifdef __cplusplus
}
#endif

#endif
