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

#ifndef HOTSWAP_H
#define HOTSWAP_H
#include "portable.h"
#include "tclass.h"
#include "systyp.h"  /* EDMAUPDATE structure */

#ifdef __cplusplus
extern "C" {
#endif 
  ESint32 EDMAPROC edma_hotswap (EPChar);
  ESint32 EDMAPROC edma_attach_proxy (OBJID IdObj, EPChar proxy);
  ESint32 EDMAPROC edma_deattach_proxy (OBJID IdObj);

  ESint32 EDMAPROC _edma_stack_execution_add_obj (OBJID id);
  ESint32 EDMAPROC _edma_stack_execution_del_obj (OBJID id);
  ESint32 EDMAPROC _edma_is_obj_in_execution_stack (OBJID id);
  ESint32 EDMAPROC _edma_print_stack_execution ();

  ESint32 EDMAPROC edma_add_update (EDMA_UPDATE update);
  ESint32 EDMAPROC edma_remove_update (ESint32 indx);
#ifdef __cplusplus
}
#endif
#endif
