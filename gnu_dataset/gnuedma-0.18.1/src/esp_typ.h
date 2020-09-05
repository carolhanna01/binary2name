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
 * 
 * Definición de tipos específicos del SO de EDMA
 *
 * -------------------------------------------------------------
 * Revisiones:
 * ----------------------------------------
 * Febraury, 20th, 2001
 * Code cleanup and comment translation
 * --------------------------------------------------------
 * November, 28th, 2001
 * Begins modification to add implicit anchor point info for inheritance
 *********************************************************
*/
#ifndef ESP_TYPE_H
#define ESP_TYPE_H

#include "portable.h"
#include "ptypes.h"
#include "const.h"
#ifdef __cplusplus
extern "C"{
#endif

  /* Platform Dependand structure for classes. Memory/Library Handlers */
  typedef struct 
  {
    HMEM	hDatosSO;	                    /*  Handlers for CLASE pointers */
    HMEM	hDatosMaquina;
    HMEM	hLib;		                    /* Shared library Handler */
    EChar	ModuleName[EDMA_CLASS_MODULE_LEN];  /* Shared library Name */
    EPChar	*ListaDll;	                    /* Shared library List. Not used */
  } SYS_CLASS;
  
  /* System Dependant structure for classes. Memory handlers for class data */
  typedef struct 
  {
    HMEM     hSCList,hProp,hMet,hNot,hSCIdList, hSubCIdList;
    HMEM     hMetFunc;
    HMEM     MySelf;         /* Memory handler for dynamically allocated classes*/
  } SYS_CLASS1;
  
  /* System Dependant structure for objects. Memory handlers for object structures */
  typedef struct 
  {
    HMEM	hObj;
    HMEM	hData;	    /* Object Private information */
    HMEM	hUpTable;   /* Dynamic Class table */
    HMEM 	hDownTable;
    HMEM	hvTable;    /* Virtual Method Table */
  } SYS_OBJ;

#ifdef __cplusplus
}
#endif

#endif
