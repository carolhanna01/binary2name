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
 *--------------------------------------------------
 * Febraury, 24th, 2001
 * Code cleanup and comment translation
 */
#ifndef OBJQ_H
#define OBJQ_H
#include "portable.h"
#include "tobj.h"
#ifdef __cplusplus
extern "C" {
#endif

  POBJ	  EDMAPROC edma_get_obj_pobj  (OBJID);
  OBJID	  EDMAPROC edma_get_obj_father_id (OBJID);
  OBJID   EDMAPROC edma_get_obj_pseudofather_id (OBJID);
  EPChar  EDMAPROC edma_get_obj_class_name (OBJID ,EPChar *);
  CLASSID EDMAPROC edma_get_obj_class_id (OBJID);
  
  ESint32 EDMAPROC edma_get_obj_status (OBJID);   
  EUint32 EDMAPROC edma_get_num_objects (void);
  ESint32 EDMAPROC edma_get_obj_num_superobjects (OBJID);
  OBJID   EDMAPROC edma_get_obj_superobject (OBJID, ESint32);
  ESint32 EDMAPROC edma_get_obj_superobject_ap (OBJID Id, ESint32 i, EPChar ap);
  ESint32 EDMAPROC edma_get_obj_num_subobjects (OBJID);
  OBJID   EDMAPROC edma_get_obj_subobject (OBJID, ESint32);
  ESint32 EDMAPROC edma_get_obj_subobject_ap (OBJID Id, ESint32 i, EPChar ap);
  OBJID   EDMAPROC edma_get_obj_app (OBJID);
  EUint32 EDMAPROC edma_get_app_id (void);
  ESint32 EDMAPROC edma_obj_report (OBJID);
  
  ESint32 EDMAPROC edma_show_subobjects_up (OBJID IdObj,EPChar Id, ESint32 level);
  ESint32 EDMAPROC edma_show_subobjects_down (OBJID IdObj,EPChar Id, ESint32 level);

  ESint32 EDMAPROC edma_show_object_interface (OBJID IdObj);

#ifdef __cplusplus
}
#endif
#endif


