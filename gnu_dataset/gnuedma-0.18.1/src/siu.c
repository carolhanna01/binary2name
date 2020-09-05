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

/*********************************************************************
 * Entorno de Desarrollo Modular y Abierto
 * Versión Beta 0.3r1
 * (c) David Martínez Oliveira
 *
 * Modulo para gestión de proxies SIU
 * Revisiones: ---------------------------------------------------------
 * 31 de Agosto de 1997
 * INtentamos añadir funciones de gestión de proxies SIU
 * ----------------------------------------------------------
 * Febraury, 19th, 2001
 * Code cleanup and comment translation
 * -------------------------------------------------------
 * March 2nd, 2002
 * Minimum code cleanup
 * -------------------------------------------------------------------
 * January, 6th, 2004
 * Modification due to GNU/EDMA SubSystem Management API Unification
 */

#include <stdio.h>
#include "portable.h"
#include "const.h"
#include "multiidf.h"
#include "vglobal.h"
#include "classq.h"
#include "misc.h"
#include "inh.h"

#include "subsystems.h"

/* edma_siu_get_proxy
 *   Returns the subsystem entry for the specified SIU Proxy name
 */

ESint32 EDMAPROC 
edma_siu_get_proxy (EPChar Name) 
{
  return edma_subsystem_get_item (SS_SIU, Name);

}

/* edma_siu_add_proxy
 *   Adds a new SIU proxy to the system
 */
ESint32 EDMAPROC 
edma_siu_add_proxy (EPChar name,EPChar class_name) 
{
  return edma_subsystem_add_item (SS_SIU, name, class_name);
}

/* edma_siu_get_num_proxies
 *   Returns the number of SIU proxies registered in the system
 */

ESint32 EDMAPROC 
edma_siu_get_num_proxies (void) 
{
  return edma_subsystem_get_num_items (SS_SIU);
}

/* edma_siu_get_proxy_class
 *   Returns the associated class for the i-the SIU proxy 
 * registered in the system
 */

CLASSID EDMAPROC 
edma_siu_get_proxy_class (ESint32 i) 
{
  return edma_subsystem_get_item_class (SS_SIU, i);

}
