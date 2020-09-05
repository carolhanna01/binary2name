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
 * -------------------------------------------------------
 * Febraury, 24th, 2001
 * Code cleanup and comment translation
 * ---------------------------------------------------------
 * October, 14th,2001
 * Added PRealloc Function
 */
#ifndef ShMemH
#define ShMemH
#include "portable.h"

#ifdef __cplusplus
extern "C" {
#endif
  /* Shared Memory Primitives */
  HMEM EDMAPROC edma_salloc (EUint32,EPChar);
  void EDMAPROC	edma_sfree (HMEM,EPVoid);
  EPVoid EDMAPROC edma_sget (HMEM);
  EPVoid EDMAPROC edma_sunget (EPVoid);
  
  /* Private Memory Primitives*/ 
  HMEM EDMAPROC edma_palloc (EUint32);
  HMEM EDMAPROC edma_prealloc (HMEM,EUint32);
  EPVoid EDMAPROC edma_pget (HMEM);
  void EDMAPROC	edma_pfree (HMEM,EPVoid);
  void EDMAPROC	edma_show_pmem ();
#ifdef __cplusplus
}
#endif
#endif
