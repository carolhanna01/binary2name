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
 *---------------------------------------------------------
 * Módulo caecera para funciones miscelánea
 *********************************************************
 * Febraury, 24th, 2001
 * Code cleanup and comment translation
 * ---------------------------------------------------------
 * Added EBufferRealloc function
*/

/* Prototypes */
#ifndef MISC_H
#define MISC_H

#include "portable.h"
#include "tobj.h"
#include "ttypes.h"
#include "systyp.h"

#ifdef __cplusplus
extern "C"{
#endif
  EUint32 EDMAPROC edma_print (EPChar);
  EUint32 EDMAPROC edma_printf_obj (OBJID IdObj,EPChar f,...);
  EUint32 EDMAPROC edma_printf (EPChar f,...);
  EUint32 EDMAPROC edma_log (EPChar f,...);
  EUint32 EDMAPROC edma_printf_err (EPChar f,...);
  EUint32 EDMAPROC edma_printf_dbg (EUint32 l,OBJID IdObj,EPChar f,...);

  EUint32 EDMAPROC edma_set_app_name (EPChar);
  EUint32 EDMAPROC edma_set_debug_level (EUint32 l);
  EPChar  EDMAPROC edma_get_system_path ();

  ESint32 EDMAPROC edma_buffer_alloc (EDMAT_BUFFER *,EUint32);
  ESint32 EDMAPROC edma_buffer_free (EDMAT_BUFFER*);   
  ESint32 EDMAPROC edma_buffer_realloc (EDMAT_BUFFER*,EUint32);

  /* Dictionaries */
  ESint32 EDMAINLINE edma_dict_map_string (EDMA_DICT, EPChar);
  EDMA_DICT EDMAPROC edma_dict_new (ESint32);
  ESint32 EDMAPROC edma_dict_free (EDMA_DICT);
  ESint32 EDMAPROC edma_dict_add_entry (EDMA_DICT, EPChar, ESint32);
  ESint32 EDMAPROC edma_dict_get_index (EDMA_DICT, EPChar);
  ESint32 EDMAPROC edma_dict_get_next_index (EDMA_DICT, EPChar, ESint32*);
  
#ifdef __cplusplus
}
#endif
#endif
