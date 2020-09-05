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


/* Revisions:
 * ---------------------------------------------------
 * March, 2nd, 2002
 * File Creation
*/
#ifndef ANCHOR_POINTS_H
#define ANCHOR_POINTS_H

#include "portable.h"
#include "tobj.h"

#ifdef __cplusplus
extern "C" {
#endif

  //OBJID   EDMAPROC edma_parse_classpath (OBJID IdObj,EPChar eclass_path, ESint32 len);
  OBJID   EDMAPROC edma_apply_classpath (OBJID IdObj, EPChar classpath);
  OBJID   EDMAPROC edma_parse_classpath (OBJID IdObj,EPChar eclass_path, ESint32 len, ESint32 ex);
  ESint32 EDMAPROC _edma_parse_classpath_items (EPChar id, EPChar *classname
						, EPChar *apoint1
						, EPChar *apoint2
						, EPSint32 op);
  
  ESint32 EDMAPROC _edma_query_subclass_ap (OBJID id, EPChar anchor_point
					    , OBJID *obj, CLASSID *cid);
  
  ESint32 EDMAPROC _edma_query_superclass_ap (OBJID id, EPChar anchor_point
					      , OBJID *obj, CLASSID *cid);
  
  ESint32 EDMAPROC edma_query_subclass_ap (OBJID id, EPChar anchor_point
					   , OBJID *obj, CLASSID *cid);
  
  ESint32 EDMAPROC edma_query_superclass_ap (OBJID id, EPChar anchor_point
					     , OBJID *obj, CLASSID *cid);
  
  ESint32 EDMAPROC
  edma_remove_superclass_ap (OBJID IdObj, EPChar anchor_point);

  ESint32 EDMAPROC
  edma_remove_subclass_ap (OBJID IdObj, EPChar anchor_point);

  ESint32 EDMAPROC  edma_rename_superclass_ap (OBJID IdObj
					       ,EPChar OldName
					       ,EPChar NewName) ;
  ESint32 EDMAPROC  edma_rename_subclass_ap (OBJID IdObj
					     ,EPChar OldName
					     ,EPChar NewName);
  ESint32 EDMAPROC  _edma_add_super_ap (OBJID id, EPChar apoint, 
				       CLASSID cid, OBJID superid, ESint32 flags);

  ESint32 EDMAPROC  _edma_add_sub_ap (OBJID id, EPChar apoint, 
				      CLASSID cid, OBJID subid, ESint32 flags);
  OBJID EDMAPROC edma_set_sub_ap (OBJID id, EPChar apoint, OBJID new_id);
  OBJID EDMAPROC edma_set_super_ap (OBJID id, EPChar apoint, OBJID new_id);
#ifdef __cplusplus
}
#endif

#endif
