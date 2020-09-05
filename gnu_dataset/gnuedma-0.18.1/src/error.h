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
 * Vigo 18 de Octubre de 1996
 * 
 * Versión 0.3r1
 * ---------------------------------------------------------
 *
 * Definiciones de constantes de error
 * ----------------------------------------------------------
 * Code cleanup and comment translation
 ********************************************************
*/
#ifndef ERROR_H
#define ERROR_H

#ifdef __cplusplus
extern "C" {
#endif

#include <setjmp.h>
#include "tobj.h"  

  ESint32 EDMAPROC edma_error ();
  ESint32 EDMAPROC edma_exception_try(jmp_buf*);
  ESint32 EDMAPROC edma_exception_throw(OBJID);
  ESint32 EDMAPROC edma_exception_clean (OBJID id);
  ESint32 EDMAPROC _edma_system_exception (EPChar fmt,...);
  ESint32 EDMAPROC _edma_system_exception_clean ();

#ifdef __cplusplus
}
#endif

/*Defines de errores*/
#if 0
#define CLASS_TABLE_NO_MEMORY		0
#define OBJECT_TABLE_NO_MEMORY		1
#define APP_TABLE_NO_MEMORY		2
#define CFG_FILE_NOT_FOUND		3
#define	DLLINSTALL_NOT_IMPLEMENT	4
#define NO_MORE_OBJECTS			5
#define NO_MORE_APPS			6
#define SYS_FILE_NOT_FOUND		7
#define MAQ_NOT_FOUND			8
#define SO_NOT_FOUND       		9
#define UNKNOWN_SO			10
#define NOT_INFO_QUERY_FIELD		11
#define NOT_SYSINFO_QUERY_FIELD		12
#define NO_MORE_CLASS			13
#define LISTASC_ALLOC_ERROR		14
#define OBJECT_NOT_EXIST		15
#define OBJECT_NOT_VIRTUAL		16
#endif

#endif

