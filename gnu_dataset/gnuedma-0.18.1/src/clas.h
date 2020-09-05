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
 * Versión Beta 0.3r1
 * 15 de Junio de 1997
 *
 * Gestión de Clases (HEADER)
 * 
 * REVISIONES:-------------------------------------------------------------
 * 15 de Junio de 1997
 * ---------------------------------------------------------
 * 3 de Enero de 1998
 * Añadimos funciones de carga dinámica de interfaces
 * --------------------------------------------------------
 * Febraury, 20th, 2001
 * Code cleanup and comment translation
 * -----------------------------------------------------------
 * August, 8th, 2003
 * Added field UpdateScript to CLASS_INFO structure
*/

#ifndef CLA_H
#define CLA_H

#include <stdio.h>
#include "portable.h"
#include "tclass.h"

#ifdef __cplusplus
extern "C"{
#endif

  typedef struct
  {
    EPChar		ClassName;
    EPChar              NameSpace;
    EPChar		SOName;
    EPChar		MaqName;
    EPChar		IDFName;
    EPChar		SIUName;
    EByte		IsIDFParser;
    EByte		IsSIUProxy;
    EByte		IsEMIComp;
    ESint32             MajorVer;
    ESint32             MinorVer;
    EPChar              UpdateScript;
  } CLASS_INFO;
  
  ESint32 EDMAPROC edma_load_class_imp (CLASSID);
  ESint32 EDMAPROC edma_free_class_imp (CLASSID);
  ESint32 EDMAPROC edma_load_class_int (CLASSID);
  ESint32 EDMAPROC edma_unload_class_int (CLASSID);
  ESint32 EDMAPROC edma_add_stock_class (CLASS_INFO ,EPChar,EPChar);
  ESint32 EDMAPROC edma_del_stock_class (EPChar);
  ESint32 EDMAPROC edma_del_stock_class_id (CLASSID);

  ESint32 EDMAPROC _edma_class_alloc_priv_data (CLASSID);
  ESint32 EDMAPROC _edma_class_free_priv_data (CLASSID);
  ESint32 EDMAPROC _edma_class_update_versions (CLASSID);
  ESint32 EDMAPROC _edma_class_update_all_versions ();
  ESint32 EDMAPROC _edma_class_update_actual_versions (CLASSID);

  ESint32 EDMAPROC edma_class_set_actual_version (EPChar, ESint32, ESint32);
#ifdef __cplusplus
}
#endif

#endif
