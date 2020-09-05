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
 * (c) David Martínez Oliveira
 * 21 de Octubre de 1997
 * 
 * Fichero de cabecera para dynlink.c
 */

#ifndef WIN32DL_H
#define WIN32DL_H

#ifdef __cplusplus
extern "C" {
#endif
     
#include "portable.h"

HMEM LoadLib (EPChar);
EUint32 UnloadLib (HMEM);
EPVoid GetAddress (HMEM,EPChar);

#ifdef __cpluplus
}
#endif

#endif
