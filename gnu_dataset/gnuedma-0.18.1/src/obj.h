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
 Entorno de Desarrollo Modular y ABierto
 (c) Favid Martínez Oliveira
 Versión Beta 0.3r1
 15 de Junio de 1997

 REVISIONES:-------------------------------------------------------
 * Febraury, 24th, 2001
 * Code cleanup and comment translation
 * September, 12th, 2001
 * Added new primitives to manage static methods, casting and clonning
 * ---------------------------------------------------------------------
 * December, 6th, 2001
 * NewObj splitted to allow correct dynamic inheritance management
 * ---------------------------------------------------------------------
 * Febraury, 7th, 2004
 * Reworked newobj primitive
*/
#ifndef OBJ_H
#define OBJ_H
#include "portable.h"
#include "tobj.h"

#ifdef __cplusplus
extern "C"{
#endif
  OBJID EDMAPROC edma_new_obj (EPChar,...);
  OBJID EDMAPROC _edma_new_obj (EPChar, OBJID, EPVoid);
  OBJID EDMAPROC _edma_new_obj_internal (EPChar, CLASSID, CLASSID, OBJID, EPVoid);

  OBJID EDMAPROC _edma_newobj_basic_stage (CLASSID, CLASSID, EPVoid);
  OBJID EDMAPROC _edma_newobj_vm_stage (OBJID);
  OBJID EDMAPROC _edma_newobj_inh_stage (OBJID , OBJID,EPVoid);
  OBJID EDMAPROC _edma_newobj_final_stage (OBJID, EPChar, EPVoid);

  EUint32 EDMAPROC edma_free_obj (OBJID);
  EUint32 EDMAPROC edma_free_one_obj (OBJID);
  ESint32 EDMAPROC edma_obj_commit_suicide (OBJID id);

  EDWord  EDMAPROC edma_make_obj_virtual (OBJID );
  EUint32 EDMAPROC edma_set_obj_final (OBJID,EUint32);

  EUint32 EDMAPROC _edma_internal_free_obj (OBJID IdObj, OBJID old,ESint32 flag);
  ESint32 EDMAPROC edma_swap_obj (OBJID, OBJID);

  /* TEST */
  OBJID EDMAPROC _edma_new_obj1 (EPChar, OBJID, EPVoid);
  OBJID EDMAPROC _edma_new_obj_internal1 (EPChar, CLASSID, CLASSID, OBJID, EPVoid);
  OBJID EDMAPROC _edma_newobj_inh_stage1 (OBJID , OBJID,EPVoid);


#ifdef __cplusplus
}
#endif


#endif
