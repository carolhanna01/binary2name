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

#ifndef SUBSYSTEMS_H
#define SUBSYSTEMS_H
#include "portable.h"
#include "tclass.h"

#ifdef __cplusplus
extern "C" {
#endif 
  ESint32 EDMAPROC edma_subsystem_get_item (ESint32, EPChar);
  ESint32 EDMAPROC edma_subsystem_add_item (ESint32, EPChar,EPChar);
  ESint32 EDMAPROC edma_subsystem_get_num_items (ESint32);
  CLASSID EDMAPROC edma_subsystem_get_item_class (ESint32, ESint32);
  EPChar  EDMAPROC edma_subsystem_get_item_id (ESint32, ESint32);
  ESint32 EDMAPROC _edma_subsystem_add_item_with_classid (ESint32 ss, EPChar Name, CLASSID IdClass);
#ifdef __cplusplus
}
#endif
#endif
