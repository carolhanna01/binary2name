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
 * Vigo 17 de Octubre de 1996
 *
 * Versión 0.3r1
 * ---------------------------------------------------------
 * Fichero de Cabecera para Módulo de gestión sistemas
 * Operativos y hardware
 *
 *********************************************************
 * --------------------------------------------
 * Febraury, 19th, 2001
 * Code cleanup and coment translation
*/
#ifndef SYS31_h
#define SYS31_h
#include "portable.h"

typedef ESint32		SOID;
typedef ESint32		MAQID;
typedef struct
{
  EChar	        Nombre[32];
  EUint32	Id;
} SYS_SO;

typedef struct
{
  EChar	        Nombre[32];
  EUint32	Id;
} SYS_MAQ;


#ifdef __cplusplus
extern "C"{
#endif
  ESint32 _edma_ini_sys_table (EPChar);
  EUint32 EDMAPROC edma_get_so_id (EPChar);
  EUint32 EDMAPROC edma_get_arch_id (EPChar);
  EUint32 EDMAPROC edma_get_so_name (EUint32,EPChar*);
  EUint32 EDMAPROC edma_get_arch_name (EUint32,EPChar*);
  EUint32 EDMAPROC edma_get_so_num (void);
  EUint32 EDMAPROC edma_get_arch_num (void);
#ifdef __cplusplus
}
#endif
#endif

