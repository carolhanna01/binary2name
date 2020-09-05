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
 * January, 6th, 2004
 * File Creation. GNU/EDMA Subsystem unification
 */

#include <stdio.h>
#include "portable.h"
#include "const.h"
#include "multiidf.h"
#include "vglobal.h"
#include "classq.h"
#include "misc.h"
#include "inh.h"

/* edma_subsystem_get_item
 *   Gets an item by name from the indicated subsystem
 */

ESint32 EDMAPROC 
edma_subsystem_get_item (ESint32 ss, EPChar Name) 
{
  EUint32		i, n;
  
  if (!Name)  return -1;

  n = GVar->n_SubSystem[ss];

  for (i = 0; i < n; i++) 
    {
      if (strncmp (GVar->SubSystem[ss][i].Id, Name, 
		   EDMA_GENERAL_ID_LEN) == 0)
	break;
    }
  
  if (i == n)
    return -1;
  else
    return i;
}

/* edma_subsystem_add_item
 *   Add the indicated item (name + classname) to the indicated subsystem
 */

ESint32 EDMAPROC 
edma_subsystem_add_item (ESint32 ss, EPChar Name, EPChar ClassName) 
{
  ESint32	Id;
  CLASSID	IdClass;

  if (!Name) return -1;
  if (!ClassName) return -1;

  /* We check if the proxy already exists */
  if (edma_subsystem_get_item (ss, Name) != -1) 
    {
      edma_printf_err ("[edma_subsystem_add_item] Item '%s' already "
		       "exists in subsystem '%d'", Name, ss);
      return -1;
    }
  
  /* if not, we ad it */
  if ((IdClass = edma_get_class_id (ClassName)) == -1)
    {
      edma_printf_err ("[edma_subsystem_add_item] Class %s not found", 
		       ClassName);
      return -1;
    }

  Id = GVar->n_SubSystem[ss];
  strncpy (GVar->SubSystem[ss][Id].Id, Name, EDMA_GENERAL_ID_LEN);
  GVar->SubSystem[ss][Id].IdClass = IdClass;
  GVar->n_SubSystem[ss]++;

  return 0;
}


/* edma_subsystem_add_item
 *   Add the indicated item (name + classname) to the indicated subsystem
 */

ESint32 EDMAPROC 
_edma_subsystem_add_item_with_classid (ESint32 ss, EPChar Name, 
				       CLASSID IdClass) 
{
  ESint32	Id;

  if (!Name) return -1;

  /* We check if the proxy already exists */
  if (edma_subsystem_get_item (ss, Name) != -1) 
    {
      edma_printf_err ("[edma_subsystem_add_item_with_clid] Item '%s' already "
		       "exists in subsystem '%d'", Name, ss);
      return -1;
    }
  
  Id = GVar->n_SubSystem[ss];
  strncpy (GVar->SubSystem[ss][Id].Id, Name, EDMA_GENERAL_ID_LEN);
  GVar->SubSystem[ss][Id].IdClass = IdClass;
  GVar->n_SubSystem[ss]++;

  return 0;
}





/* edma_subsystem_get_num_items
 *   Returns the number of items registered in a given subsystem
 */

ESint32 EDMAPROC 
edma_subsystem_get_num_items (ESint32 ss) 
{
  return GVar->n_SubSystem[ss] ;
}

/* edma_subsystem_get_item_class
 *    Returns the associated classid for i-th item in the given subsystem
 */

CLASSID EDMAPROC 
edma_subsystem_get_item_class (ESint32 ss, ESint32 i) 
{
  if ((i < 0) || (i > GVar->n_SubSystem[ss]))
    return -1;

  return GVar->SubSystem[ss][i].IdClass;
}

/* edma_subsystem_get_item_id
 *  Returns the i-th item name in the given subsystem
 */

EPChar EDMAPROC 
edma_subsystem_get_item_id (ESint32 ss, ESint32 i) 
{
  if ((i < 0) || (i > GVar->n_SubSystem[ss]))
    return NULL;

  return GVar->SubSystem[ss][i].Id;
}
