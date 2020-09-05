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
 *----------------------------------------------
 * Febraury, 20th, 2001
 * Code cleanup and comment translation
*/
#ifndef EMI_H
#define EMI_H
#include "portable.h"
#include "tclass.h"

#ifdef __cplusplus
extern "C" {
#endif

  ESint32 EDMAPROC edma_get_EMI_handler (EPChar);
  ESint32 EDMAPROC edma_add_EMI_handler (EPChar,EPChar);
  ESint32 EDMAPROC edma_hook_get_class (CLASSID);
  ESint32 EDMAPROC edma_actualize_cfg ();
  
  ESint32 EDMAPROC edma_get_EMI_num_handlers (void);
  CLASSID EDMAPROC edma_get_EMI_handler_class (ESint32 i);
#ifdef __cplusplus
}
#endif
#endif
