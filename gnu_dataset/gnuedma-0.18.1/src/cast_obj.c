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

/*------------------------------------------------------------------------
 * April, 17th, 2003
 * File creation. obj.c was chopped
*/
 
#include <stdio.h>
#include <string.h>
#include <stdarg.h>
#include <stdlib.h>
#include "portable.h"
#include "tobj.h"
#include "vglobal.h"
#include "shmem.h"
#include "clas.h"
#include "classq.h"
#include "obj.h"
#include "pri1.h"
#include "pri3.h"
#include "misc.h"
#include "siu.h"
#include "poli.h"

#include "helper.h"
#include "error.h"
#include "locators.h"
#include "inh.h"
#include "inh1.h"
#include "anchor_points.h"
#include "cast_obj.h"

/********************************************************************
 * Cast Support Functions
 *********************************************************************/

/* edma_downcast_obj
 *   Searchs superclass set on a given object to locate a given class or anchor point
 */

OBJID EDMAPROC 
edma_upcast_obj (OBJID IdObj, EPChar classname) 
{
  CLASSID      idc;
  ESint32      i,ap;
  OBJID        f;
  POBJ         pObj,pObj2;

  if ((edma_check_obj_id (IdObj, "edma_upcast_obj")) == -1)
    return -1;  

  /* Define start and end point*/
  pObj = gObj[IdObj];
  idc = edma_get_class_id (classname);
  ap = 0;
  /* We must to search through superclasses*/
  if (idc != -1)  
    { /* classname is a class name*/
      if (pObj->IdClass == idc)
	return pObj->IdObj;
    }  
  else /* classname isn't a class name, so consider it an anchor point*/
    ap = 1;
    
  for (i = 0; i < pObj->nUpTable; i++) 
    {
      if (pObj->UpTable[i].Obj != -1)
	{
	  pObj2 = gObj[pObj->UpTable[i].Obj];
	  if (ap)
	    {
	      if (strncmp (classname, pObj->UpTable[i].Id, 
			   EDMA_GENERAL_ID_LEN) == 0) 
		/* We found the anchor point*/
		return pObj2->IdObj;
	      else 
		{
		  f = edma_upcast_obj (pObj2->IdObj, classname);
		  if (f != -1) return f;
		}
	      continue;
	    }
	  /* If we aren't looking for an anchor point. 
	   * Continue looking for class*/
	  f = edma_upcast_obj (pObj2->IdObj, classname);
	  if (f != -1)
	    return f; 	
	}
    }
  return -1;
}

/* edma_downcast_obj
 *   Searchs subclass set on a given object to locate a given class 
 * or anchor point
 */

OBJID EDMAPROC 
edma_downcast_obj (OBJID IdObj, EPChar classname) 
{
  CLASSID      idc;
  ESint32      i,ap;
  OBJID        f;
  POBJ         pObj,pObj2;
  
  if ((edma_check_obj_id (IdObj, "edma_downcast_obj")) == -1)
    return -1;

  /* Define start and end point*/
  pObj = gObj[IdObj];
  idc = edma_get_class_id (classname);
  ap = 0;
  /* We must to search through superclasses*/
  if (idc != -1)  
    { /* classname is a class name*/
      if (pObj->IdClass == idc)
	return pObj->IdObj;
    }  
  else /* classname isn't a class name, so condider it an anchor point*/
    ap = 1;
    
  for (i = 0; i < pObj->nDownTable; i++) 
    {
      pObj2 = gObj[pObj->DownTable[i].Obj];
      if (pObj2 == NULL)
	{
	  edma_printf ("[%s] Entry %d '%s' still not linked", 
		       __FUNCTION__, i, pObj->DownTable[i].Id);
	  continue;
	}
      if (ap)
	{
	  if (strncmp (classname, pObj->DownTable[i].Id, 
		       EDMA_GENERAL_ID_LEN) == 0) 
	    /* We found the anchor point*/
	    return pObj2->IdObj;
	      else 
		{
		  f = edma_downcast_obj (pObj2->IdObj, classname);
		  if (f != -1) return f;
		}
	  continue;
	}
      
      /* If we aren't lookkin for an anchor point. Continue looking for class*/
      f = edma_downcast_obj (pObj2->IdObj, classname);
      if (f != -1)
	return f;	
    }
  return -1;
}

/* Simple implementation. First looks up, then looks down*/

/* edma_cast_obj
 *   Search super and sub class sets on a given objet to locate a given class 
 * or anchor point
 */

OBJID EDMAPROC 
edma_cast_obj (OBJID IdObj, EPChar classname) 
{
  OBJID id;

  if ((edma_check_obj_id (IdObj, "edma_cast_obj")) == -1)
    return -1;
  
  id = edma_upcast_obj (IdObj, classname);
  if (id == -1)
    id = edma_downcast_obj (IdObj, classname);
  return id;
}
