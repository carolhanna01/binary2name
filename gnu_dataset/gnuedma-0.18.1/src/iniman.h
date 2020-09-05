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
 * Version 0.2r2
 * (c) David Martínez Oliveira
 * 
 * Module   : INIMAN
 * Type     : Header file
 * Category : System dependency
 * Description :
 *    Simulates WIN32 API functios for managing Windows .INI files-like
 * 
 * Revisions:
 * 15/10/1997
 *   Creation
 * -----------------------------------------------
 * Febraury, 24th, 2001
 * Code Cleanup and comment translation
 */
 
#include <stdio.h>
#include "portable.h"

#ifndef INIMAN_H
#define INIMAN_H

#ifdef __cplusplus
extern "C" {
#endif

  /* Types definition */
#ifndef INIMAN_C
  typedef void *PINIFILE;
#endif
  
  PINIFILE EDMAPROC edma_open_ini (EPChar);
  ESint32  EDMAPROC edma_close_ini (PINIFILE);
  ESint32  EDMAPROC edma_clean_ini_string (EPChar);
  ESint32  EDMAPROC edma_get_ini_int (PINIFILE, EPChar, EPChar, ESint32);
  EUint32  EDMAPROC edma_get_ini_string (PINIFILE, EPChar, EPChar, EPChar, EPChar, EUint32);

#ifdef __cplusplus
}
#endif

#endif
