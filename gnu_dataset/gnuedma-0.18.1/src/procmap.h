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
 Entorno de Desarrollo Modular y Abierto
 (c) David Martínez Oliveira
 Versión Beta 0.3r1
 15 de Octubre de 1997

 ENlace dinámico de EDMA

 REVISIONES:------------------------------------------------
 * Version Changed to 0.5.1
 *----------------------------------------------------
 * Febraury, 19th, 2001
 * Code Cleanup and comment translation
 * --------------------------------------------------------
 * October, 14th,2001
 * Added Casting and clonning functions
 * Added EBufferRealloc and PRealloc functions
 * ---------------------------------------------------------

*/

#ifndef PROCMAP_H
#define PROCMAP_H

#include <stdio.h>
#include "portable.h"

#ifdef __cplusplus
extern "C"{
#endif
EUint32 BuildProcMap(void);
#ifdef __cplusplus
}
#endif
#endif
