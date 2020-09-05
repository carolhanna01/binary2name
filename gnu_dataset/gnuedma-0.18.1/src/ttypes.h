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
 * Versi¢n Beta 0.3r1
 * Tipos de Datos EDMA
 * (C) David Mart¡nez Oliveira
*/
/*** IDENTIFICADORES DE TIPOS *********************************
 * 2 de Septiembre de 1997
 * Modificación para añadir los nuevos tipos de la versión 0.1r1
 * 14 de Noviembre de 1997
 * Comenzamos la modificación de tipos para la version 0.3r1
 * 
 * ------------------------------------------------------------------
 * Febraury, 20th, 2001
 * Code cleanup and comment translation
 *
*/

#ifndef TTYPES_H
#define TTYPES_H
#include "portable.h"

#ifdef __cplusplus
extern "C"{
#endif
   
#define DT_EUINT8     0
#define DT_EUINT16    1
#define DT_EUINT32    2     
#define DT_ESINT8     3
#define DT_ESINT16    4
#define DT_ESINT32    5
#define DT_EBYTE      6
#define DT_EWORD      7
#define DT_EDWORD     8
#define DT_ECHAR      9
#define DT_EBOOL     10
#define DT_EREAL32   11
#define DT_EREAL64   12
#define DT_EZSTRING  13
#define DT_EBUFFER   14
#define DT_EOBJECT   15
#define DT_EUSER     16   
#define DT_EPOINTER  17   
   
#define     MAX_TIPOS         18

  typedef struct
  {
    EChar	Id[32];
    EUint32	Ind;
    EUint32     tam;
    EChar	Sig[50];
  } TIPOS;
  
  typedef struct 
  {
    HMEM     h;
    EUint32  Size;
    EPVoid   dat;
  }EDMAT_BUFFER;
  
#ifdef __cplusplus
}
#endif
#endif
