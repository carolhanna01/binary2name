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
 * Servidor de visualizacion en consola
 * 3 de Julio de 1997
 *
 * Fichero de cabecera
 * Compilador  :Cygnus-WIN32 beta 18
 * Descripcion :
 *       Pruebas de herramientas de sincronizacion WIN32 para
 *       EDMA32
 *
 * */


#ifndef VSERV_H
#define VSERV_H

#include <stdio.h>
#include <string.h>
#include "portable.h"

#ifdef __cplusplus
extern "C"{
#endif

typedef struct
{
   HMEM     Mutex;                      // Acceso en exclusion mutua
   HMEM     Sync;                       // Objeto de sincronizacion

   EChar    Msg[200];
   ESint32	Color;
   ESint32	Size;
   HMEM     h;                          // Para borrado de memoria
   char     full;
   EChar	Init;					// Indica si la consola está inicializada
} VSERV;

// Esta funci¢n crea estructura compartida y se mapea en el
// espacio de direccionamiento del proceso que la llama
/*
EPVoid InitServer(EPChar);
EPVoid MapServer(EPChar);
EUint32 FreeServer (VSERV*);
EUint32 UnmapServer (VSERV*);
*/
EUint32 vDrawString(VSERV*,EPChar);
/*
EUint32 vDrawStringEx(VSERV*,EPChar,ESint32,ESint32);
EUint32 vWaitString(VSERV*,EPChar,EPSint32,EPSint32);
EUint32	IsInitialized(EPChar);
*/
#ifdef __cplusplus
}
#endif
#endif
