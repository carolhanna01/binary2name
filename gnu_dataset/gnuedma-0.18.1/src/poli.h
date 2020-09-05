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

/**********************************************************
 * Entorno de Desarrollo Modular y Abierto
 * (c) David Martínez Oliveira
 * Vigo 3 de Julio de 1997
 *
 * Versión 0.3r1
 *---------------------------------------------------------
 * Fichero de cabecera par módulo de implementación de
 * sobrecarga de métodos
 * REVISIONS:--------------------------------------------------
 * Febraury, 24th, 2001
 * Code cleanup and comment translation
*/
#ifndef POLI_H
#define POLI_H
#include "portable.h"

#ifdef __cplusplus
extern "C"{
#endif
  EUint32 EDMAPROC edma_over_met (OBJID,EPChar,POBJ,PPROC);
  EUint32 EDMAPROC edma_over_met1 (OBJID,EPChar,POBJ,PPROC,EPChar);
  EUint32 EDMAPROC edma_over_met3 (OBJID,EPChar,EPChar);
  EUint32 EDMAPROC edma_restore_met (OBJID,EPChar);
  EUint32 EDMAPROC edma_old_met3 (OBJID,EPChar,...);
#ifdef __cplusplus
}
#endif
#endif

