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

/*******************************************************
 * Entorno de Desarrollo Modular y Abierto
 * (c) David Martínez Oliveira
 * Versión Beta 0.3r1
 * 15 de Junio de 1997
 *
 * Primitivas de Objeto de nivel1 (HEADER)
 * REVISIONES:-------------------------------------------------------------
 * 15 de Junio de 1997
 * -----------------------------------------------------
 * Febraury, 24th, 2001
 * Code cleanup and comment translation
*/

#ifndef PRI1_H
#define PRI1_H

#include <stdarg.h>

#include "portable.h"
#include "tobj.h"

#ifdef __cplusplus
extern "C"{
#endif
  
  EPVoid  EDMAPROC edma_wprop1 (OBJID,EUint32,...);
  EPVoid  EDMAPROC edma_rprop1 (OBJID,EUint32,...);
#if 0
  EUint32 EDMAPROC edma_met1 (OBJID,EUint32,EByte,EPVoid);
  
  EPVoid  EDMAPROC _edma_wprop1_pargs (OBJID,EUint32,EPVoid);
  EPVoid  EDMAPROC _edma_rprop1_pargs (OBJID,EUint32,EPVoid);
  EUint32 EDMAPROC _edma_met1_pargs (OBJID,EUint32,EByte,EPVoid);
#endif
 
  EUint32 EDMAPROC edma_met1 (OBJID,EUint32,EByte,EPVoid);
  
  EPVoid  EDMAPROC _edma_wprop1_pargs (OBJID,EUint32,va_list);
  EPVoid  EDMAPROC _edma_rprop1_pargs (OBJID,EUint32,va_list);
  EUint32 EDMAPROC _edma_met1_pargs (OBJID,EUint32,EByte,va_list);
  
  EUint32 EDMAPROC edma_prop1_size (OBJID IdObj,EUint32 Ind) ;
#ifdef __cplusplus
}
#endif

#endif
