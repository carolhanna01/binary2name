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
 * Versión Beta 0.3r1
 * (c) David Martínez Oliveira
 *
 * Modulo para gestión Extensiones EMI (Extension Management Interface)
 * Revisiones: ---------------------------------------------------------
 * 2 de Octubre
 * Añdimos funciones para la gestión EMI.
 * Inicialmente solo excepción CLASS_NOT_FOUND
 *
 * Añadimos en este módulo las funciones para actualizar el fichero
 * EDMA32.CFG
 * --------------------------------------------------------------------
 * 19 de Febrero de 2000
 * Hacemos que se modifique el registro real de clases, para poder 
 * utilizar las funciones de soporte de instalación dinámica de clases.
 *-------------------------------------------------------------------
 * Revisions:
 * November, 18th, 2001
 * Fixed ActualizeCFG which didn't write the correct IDF parser
 * for classes using an IngrIDF parser
 * -------------------------------------------------------------
 * March, 2nd, 2002
 * Code cleanup
 * ------------------------------------------------------------
 * July, 13th, 2002
 * Now, we save version information in the GNU/EDMA registry
 * ---------------------------------------------------------------
 * May, 10th, 2003
 * Modification to support changes to internal class structures
 * ------------------------------------------------------------------
 * July, 3rd, 2003
 * Cosmetic changes, parameter sanity checks
 * --------------------------------------------------------
 * January, 6th, 2004
 * Modifications due to GNU/EDMA SubSystem Management API Unification
 */
 
#include <stdio.h>
#include "portable.h"
#include "subsystems.h"
#include "multiidf.h"
#include "vglobal.h"
#include "classq.h"
#include "misc.h"
#include "inh.h"

#include "subsystems.h"
#include "helper.h"

/* edma_get_EMI_handler
 *    Deprecated. Use edma_subsystem API
 */

ESint32 EDMAPROC 
edma_get_EMI_handler (EPChar handler_name) 
{
  return edma_subsystem_get_item (SS_EMI, handler_name);
}

/* edma_add_EMI_handler
 *   Register class 'ClassName' as an EMI handler for 'Name'
 *   DEPRECATED: Use edma_subsystem APO
 */

ESint32 EDMAPROC 
edma_add_EMI_handler (EPChar handler_name,EPChar class_name) 
{
  return edma_subsystem_add_item (SS_EMI, handler_name, class_name);
}

/* edma_hook_get_class
 *   Makes class 'IdClass' current handler for 'GetClass' 
 */
EUint32 EDMAPROC 
edma_hook_get_class (CLASSID IdClass) 
{
  if ((edma_check_class_id (IdClass, "edma_hook_get_class")) == -1)
    return -1;

  GVar->GetClassEMI=IdClass;
  return 0;
}

/* edma_get_EMI_num_handlers
 *   Returns the number of EMI handlers registered
 *   DEPRECATED: Use edma_system API
 */

ESint32 EDMAPROC 
edma_get_EMI_num_handlers (void) 
{
  return edma_subsystem_get_num_items (SS_EMI);
}


/* edma_get_EMI_handler_class
 *   Returns class if the i-th EMI handler registered
 *   DEPRECATED: Use edma_subsystem API
 */
CLASSID EDMAPROC 
edma_get_EMI_handler_class (ESint32 i) 
{
  return edma_subsystem_get_item_class (SS_EMI, i);
}

