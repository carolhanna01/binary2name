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
 * Febraury, 24th, 2001
 * Code cleanup and comment translation
 */
#ifndef MULTIIDF_H
#define MULTIIDF_H
#include "portable.h"
#include "tclass.h"

#ifdef __cplusplus
extern "C" {
#endif
  ESint32 EDMAPROC edma_ingridf_get_num_parsers (void);
  CLASSID EDMAPROC edma_ingridf_get_parser_class (ESint32 i);
  ESint32 EDMAPROC edma_ingridf_get_parser (EPChar);
  ESint32 EDMAPROC edma_ingridf_add_parser (EPChar,EPChar);

  /* IDF Parser interface*/
  CLASSID EDMAPROC edma_idf_get_free_class_id (ESint32);
  CLASSID EDMAPROC _edma_get_shared_class_id ();
  ESint32 EDMAPROC edma_idf_set_general (CLASSID ,EPChar,EPChar,EPChar,EPChar);

  ESint32 EDMAPROC edma_idf_set_class_name (CLASSID, EPChar);
  ESint32 EDMAPROC edma_idf_set_class_namespace (CLASSID, EPChar);
  ESint32 EDMAPROC edma_idf_set_class_version (CLASSID, ESint32, ESint32);
  ESint32 EDMAPROC edma_idf_set_class_attribs (CLASSID, ESint32, ESint32, ESint32);
  ESint32 EDMAPROC edma_idf_set_class_arch (CLASSID, ESint32, ESint32);
  ESint32 EDMAPROC edma_idf_set_class_impl (CLASSID, EPChar);

  ESint32 EDMAPROC edma_idf_set_def (CLASSID,EUint32,EUint32,EUint32);
  ESint32 EDMAPROC edma_idf_set_prop (CLASSID,EUint32,EPChar,EPChar,EPChar,EUint32, EPChar);
  ESint32 EDMAPROC edma_idf_set_met (CLASSID iC,EUint32 iP,EPChar ,EPChar,EByte,EByte,EByte);
  ESint32 EDMAPROC edma_idf_set_sclist (CLASSID ,EPChar	*, EPChar *, EPChar *);
  ESint32 EDMAPROC _edma_idf_set_subsystems (CLASSID class_id);
  ESint32 EDMAPROC edma_idf_set_class_id (CLASSID );
#ifdef __cplusplus
}
#endif
#endif
