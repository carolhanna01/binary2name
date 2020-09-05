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
 *---------------------------------------------------------
 * Febraury, 20th, 2001
 * Code cleanup and comment tranlation
 * ----------------------------------------------------------
 * November, 11th,2001
 * Added prototypes for IsMetAbstract and IsMetStatic primitives
 */

#ifndef CLASSQ_H
#define CLASSQ_H
#include "portable.h"
#include "tclass.h"
#include "tobj.h"
#ifdef __cplusplus
extern "C" {
#endif

  ESint32 EDMAPROC edma_get_class_name (CLASSID,EPChar);   
  SOID	  EDMAPROC edma_get_class_so_id (CLASSID);
  MAQID	  EDMAPROC edma_get_class_arch_id (CLASSID);
  ESint32 EDMAPROC edma_get_class_module (CLASSID,EPChar);
  ESint32 EDMAPROC edma_get_class_namespace (CLASSID,EPChar);

  ESint32 EDMAPROC edma_get_class_major_version (CLASSID);
  ESint32 EDMAPROC edma_get_class_minor_version (CLASSID);
  ESint32 EDMAPROC edma_get_class_current_version (CLASSID);

  EPChar EDMAPROC  edma_get_class_repo_dir (CLASSID cid);
  ESint32 EDMAPROC edma_get_class_repo_type (CLASSID cid);

  EUint32 EDMAPROC edma_get_num_reg_classes (void);

  CLASSID EDMAPROC edma_get_real_id (EPChar, CLASSID);
  CLASSID EDMAPROC edma_get_class_id_with_version (EPChar, ESint32, ESint32);
  CLASSID EDMAPROC edma_get_class_id (EPChar);
  EPChar EDMAPROC edma_get_idf_file_path (CLASSID cid);
  EPChar EDMAPROC edma_get_impl_file_path (CLASSID cid);

  ESint32 EDMAPROC edma_is_class_IDF_parser (CLASSID id);
  ESint32 EDMAPROC edma_is_class_SIU_proxy (CLASSID id);
  ESint32 EDMAPROC edma_is_class_EMI_handler (CLASSID id);

  ESint32 EDMAPROC edma_get_class_num_superclasses (CLASSID id);
  CLASSID EDMAPROC edma_get_class_superclass (CLASSID id, ESint32 Indx);
  CLASSID EDMAPROC edma_get_class_superclass (CLASSID id, ESint32 Indx);

  ESint32 EDMAPROC edma_get_prop_num (CLASSID);
  ESint32 EDMAPROC edma_get_prop_name (CLASSID,EUint32,EPChar);
  ESint32 EDMAPROC edma_get_prop_indx (CLASSID ,EPChar);
  ESint32 EDMAPROC edma_get_prop_type (CLASSID,EUint32,EPChar);
  
  ESint32 EDMAPROC edma_get_met_num (CLASSID);
  ESint32 EDMAPROC edma_get_met_name (CLASSID,EUint32,EPChar);
  ESint32 EDMAPROC edma_get_met_sig (CLASSID,EUint32,EPChar);
  ESint32 EDMAPROC edma_is_met_virtual (CLASSID,EUint32);
  ESint32 EDMAPROC edma_is_met_abstract (CLASSID,EUint32);
  ESint32 EDMAPROC edma_is_met_static (CLASSID,EUint32);
  ESint32 EDMAPROC edma_get_met_indx (CLASSID ,EPChar);
  PPROC*  EDMAPROC edma_get_met_func (CLASSID ,EPChar);
  ESint32 EDMAPROC edma_set_met_func (CLASSID ,EPChar, PPROC*);
  ESint32 EDMAPROC edma_get_all_met_func (OBJID IdObj, PPROC **list);
  
  ESint32 EDMAPROC edma_get_prop_type_id (CLASSID,EUint32);
  ESint32 EDMAPROC edma_get_prop_type_sig (CLASSID,EUint32,EPChar);
  ESint32 EDMAPROC edma_get_prop_num_elements (CLASSID,EUint32);
  ESint32 EDMAPROC edma_get_type_size (EUint32);
  EUint32 EDMAPROC edma_get_type_sig (EUint32,EPChar);  
  EUint32 EDMAPROC edma_get_type_id (EPChar pType);
  ESint32 EDMAPROC edma_get_prop_indx3 (OBJID,EPChar,CLASSID*); 

  CLASSID EDMAPROC edma_get_next_class (CLASSID); 
#ifdef __cplusplus
}
#endif
//---------------------------------------------------------------------------
#endif
