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

/* March, 16th, 2002
 * File Creation
 * Basic helper functions
 * ------------------------------------------------------------------
 * Febraury. 7th, 2004
 * Added functions to parse class names
 */

#ifndef HELPER_H
#define HELPER_H
#include "portable.h"
#include "tobj.h"
#include "tclass.h"

#ifdef __cplusplus
extern "C" {
#endif

  ESint32 EDMAINLINE edma_check_obj_id (OBJID id, EPChar msg);
  ESint32 EDMAINLINE edma_check_obj_id1 (OBJID id, EPChar msg, EPChar str);
  ESint32 EDMAINLINE edma_check_class_id (CLASSID cid, EPChar msg);
  ESint32 EDMAINLINE _edma_parse_class_name (EPChar class_name, 
					     CLASSID *cid, CLASSID *siu_cid,
					     ESint32 vmajor, ESint32 vminor);
#ifdef __cplusplus
}
#endif
#endif
