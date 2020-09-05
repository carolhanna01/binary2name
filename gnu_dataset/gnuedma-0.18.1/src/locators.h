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

#ifndef LOCATORS_H
#define LOCATORS_H

#ifdef __cplusplus
extern "C" {
#endif
#include "portable.h"
#include "tobj.h"

  ESint32 EDMAPROC edma_look4_met (POBJ *pObj1,EPChar MetName, EPChar Signature);
  ESint32 EDMAPROC edma_look4_prop (POBJ *pObj1, EPChar PropName);
  ESint32 EDMAPROC edma_look4_met_ext (POBJ *pObj1,EPChar MetName, EPChar Signature);
  ESint32 EDMAPROC edma_look4_prop_ext (POBJ *pObj1,EPChar PropName);

  OBJID EDMAPROC _edma_locate_method (OBJID IdObj, EPChar Id1, EPChar Signature,
				      EPSint32 has_cp, EPSint32 met_indx);

  OBJID EDMAPROC _edma_locate_property (OBJID IdObj, EPChar Id1, 
				      EPSint32 has_cp, EPSint32 prop_indx);

  ESint32 EDMAPROC _edma_locate_uplink_by_name (OBJID IdObj, EPChar anchor_point);
  ESint32 EDMAPROC _edma_locate_downlink_by_name (OBJID IdObj, EPChar anchor_point);
  ESint32 EDMAPROC _edma_locate_uplink_by_pobj (POBJ IdObj, POBJ pObj);
  ESint32 EDMAPROC _edma_locate_downlink_by_pobj (POBJ IdObj, POBJ pObj);
  ESint32 EDMAPROC _edma_locate_uplink_by_obj (OBJID IdObj, OBJID pObj);
  ESint32 EDMAPROC _edma_locate_downlink_by_obj (OBJID IdObj, OBJID pObj);
#ifdef __cplusplus
}
#endif 

#endif
