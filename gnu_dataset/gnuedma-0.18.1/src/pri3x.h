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
 * Entorno de Desarrollo Modular y Abierto
 * Version 0.3r1
 * (c) David Martínez Oliveira
 * 
 * Fichero de cabecera para extensión de primitivas de nivel 3
 * 
 * Revisiones:
 * 10/12/1997
 *   Creación del fichero
 * -----------------------------------------------------------------
 * Febraury, 24th, 2001
 * Code cleanup and comment translation
*/

#ifndef PRI3X_H
#define PRI3X_H
#include "portable.h"
#include "tobj.h"
#include "tclass.h"

#include <stdarg.h>

#ifdef __cplusplus
extern "C"{
#endif

  ESint32 EDMAPROC edma_wprop3_pargs (OBJID,EPChar,va_list);
  ESint32 EDMAPROC edma_rprop3_pargs (OBJID,EPChar,va_list);
  ESint32 EDMAPROC edma_met3_pargs (OBJID,EPChar,EPChar,ESint32,va_list);

#if 0
  ESint32 EDMAPROC edma_wprop3_pargs (OBJID,EPChar,EPVoid);
  ESint32 EDMAPROC edma_rprop3_pargs (OBJID,EPChar,EPVoid);
  ESint32 EDMAPROC edma_met3_pargs (OBJID,EPChar,EPChar,ESint32,EPVoid);
#endif

#ifdef __cplusplus
}
#endif

#endif
