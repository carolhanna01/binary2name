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

/******************************************************************
 * March, 16th, 2002
 * File Creation
 * Basic helper functions
 *****************************************************************
 * May, 10th, 2003
 * Modification to support changes to internal class structures
 * -------------------------------------------------------------------
 * Febraury, 7th, 2004
 * Added functions to parse ClassNames (SIU, Version, namespace,....)
 */   
#include <stdlib.h>
#include <string.h>

#include "portable.h"
#include "vglobal.h"
#include "tobj.h"
#include "tclass.h"
#include "misc.h"

#include "classq.h"
#include "clas.h"
#include "siu.h"


#include "error.h"

/* edma_check_obj_id
 *   Returns -1 if the object idenfier 'id' is out of range or if it
 *   points to a free entry in the current object table
 */
ESint32 EDMAINLINE
edma_check_obj_id (OBJID id, EPChar msg) 
{

  /* Check for version updates */
  if (last_checked_versions < GVar->time_last_version_update)
    _edma_class_update_all_versions ();


  if ((id < 0) || (id > nMaxObj)) 
    return _edma_system_exception ("[%s] Invalid Object Identifier: %d", 
				   msg, id);
  if (gObj[id] == NULL)
    return _edma_system_exception ("[%s] Object %d doesn't exist", msg, id);



return 0;
}

ESint32 EDMAINLINE
edma_check_obj_id1 (OBJID id, EPChar msg, EPChar str) 
{
  if ((id < 0) || (id > nMaxObj)) 
    return _edma_system_exception ("[%s] Invalid Object Identifier: "
				   "%d (Id=%s)", msg, id, str);
  if (gObj[id] == NULL)
    return _edma_system_exception ("[%s] Object %d doesn't exist; (Id=%s)", 
				   msg, id, str);

return 0;
}

/* edma_check_class_id
 *   Returns -1 if the class identifier 'cid' is out of range or it points
 *   to a currently undefined class
 */
ESint32 EDMAINLINE
edma_check_class_id (CLASSID cid, EPChar msg)
{




  if ((cid < 0) || (cid > (GVar->nMaxClases + nMaxLocalClasses)))
      return _edma_system_exception ("[%s] Invalid Class Identifier: %d", 
				    msg, cid);

  if (gClass[cid] == 0)
    if ((ProcMapTable[cid] == CLASS_FREE) || (cid < GVar->nMaxClases))
    return _edma_system_exception ("[%s] Class %d not defined", msg, cid);

  /* Check for version updates */
  if (last_checked_versions < GVar->time_last_version_update)
    _edma_class_update_all_versions ();

  return 0;
}

/* _edma_parse_class_name
 *    Parses a class name to determine if a SIU proxy was specified 
 */
/* FUnction returns offset between SIU Name and Class Name*/
ESint32 EDMAINLINE
_edma_parse_class_name (EPChar class_name, CLASSID *cid, CLASSID *siu_cid, 
			ESint32 vmajor, ESint32 vminor)
{
  EPChar   aux;
  EPChar   proxy;

  if (class_name == NULL)
    return _edma_system_exception ("[edma_parse_class_name] %s", "Invalid "
				   "ClassName string");

  *siu_cid = -1;

  /* Check if SIU information is available */
  /* If a SIU Proxy was specified within class_name */
  if ( (aux = strchr (class_name, ':')) != NULL)
    {
#ifdef DARWIN
      proxy = (EPChar) malloc (aux - class_name);
      strncpy (proxy, class_name, aux - class_name);
#else
      proxy = (EPChar) strndup (class_name, aux - class_name);
#endif
      *cid = *siu_cid = edma_siu_get_proxy_class (edma_siu_get_proxy (proxy));

      if (*siu_cid == -1)
	{
	  edma_printf_err ("[edma_parse_class_name] SIU Proxy '%s' not found",  
			   proxy);
	  free (proxy);
	  return -1;
	}
      free (proxy);
      return aux - class_name + 1;
    }
  else /* No SIU Proxy specified */
    {
      if (vmajor == -1)
	*cid = edma_get_class_id (class_name);  /* Get class id by name... 
						 * newest version*/
      else
	*cid = edma_get_class_id_with_version (class_name, vmajor, vminor);
      if (*cid == -1)
	{
	  if (vmajor == -1)
	    return _edma_system_exception ("[edma_parse_class_name] "
					   "Class '%s' not found", class_name);
	  else
	    return _edma_system_exception ("[edma_parse_class_name] "
					   "Class '%s' version %ld.%ld "
					   "not found", 
					   class_name, vmajor, vminor);
	}
      

      /* Check if class uses a blind Proxy */
      if (gClass[*cid]->SIUProxy != -1)
	{
	  *siu_cid = *cid;
	  *cid = edma_siu_get_proxy_class (gClass[*cid]->SIUProxy);
	}
      return 0;
    }
  edma_printf ("[edma_parse_class_name] %s", "Unreacheable!!!!!");

  return -1;
}


