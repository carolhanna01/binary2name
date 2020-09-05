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

/* ----------------------------------------------------------------------
 * April, 17th, 2003
 * File creation. obj.c was chopped
 * -------------------------------------------------------------------------
 * May, 10th, 2003
 * Modifications to support changes to internal class structures
 * ------------------------------------------------------------------------------
 * Febraury, 7th, 2004
 * Modification due to newobj primitive reworking
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
#include "clone_obj.h"


/* Partial implementation. Don't manage
   - Clonning dynamical classes added to this object
   - Clonning data on static superclasses for this object
   - Clonning dynamical data like EDMAT_BUFFER or string... this is a
      shallow_clonning
*/

/* _edma_clone_simple_obj
 *    Internal function that just clones a subobject without inheritance 
 * information 
 */
OBJID EDMAPROC
_edma_clone_simple_obj (OBJID IdObj, ESint32 copy)
{
  OBJID          id;
  CLASSID        idc;
  EChar          ClassNam1[EDMA_CLASS_NAME_LEN];
  ESint32        i, n, offset;
  EDMAT_BUFFER   *src_pbuf, *dest_pbuf;
  EPByte         src_ptr, dest_ptr;

  idc = gObj[IdObj]->IdClass;
  strcpy (ClassNam1, gClass[idc]->ClassName);

  /*********************************************************************/
  /* FIXME: MODIFICATION BELLOW STILL NOT TESTED*/
  /***********************************************************************/
  /* First we build the clonedobject without inheritance stage*/
  if ((id = _edma_newobj_basic_stage (idc, gObj[IdObj]->IdSIU, NULL)) == -1)
    return _edma_system_exception ("[edma_shallow_clone] Basic Stage Failed."
				   "Can't create object of class '%s'", 
				   ClassNam1);
  
  /* Then the virtual method management*/
  if ((_edma_newobj_vm_stage (id)) == -1) 
    return _edma_system_exception ("[edma_new_obj] Virtual Method Stage "
				   "Failed. "
				   "Can't create object of class '%s'", 
				   ClassNam1);
  
  /* Last, run constructor if any*/
  if ((_edma_newobj_final_stage (id, NULL, NULL)) == -1)
    return _edma_system_exception ("[edma_new_obj] Final Stage Failed. "
				   "can't create object of class '%s'", 
				   ClassNam1);

  /* Now we copy the object's data memory chunk*/
  memcpy (gObj[id]->Data, gObj[IdObj]->Data, gClass[idc]->TamDatos);

  /* Finally, we should clone dynamically allocated properties */
  if (copy)
    {
      n = gClass[idc]->nProp;
      for (i = 0; i < gClass[idc]->nProp; i++) 
	{ 
	  if (pClass[idc]->Prop[i].nElem > 0) 
	    {
	      /* Arrays still not implemented... we are not using them */
	      edma_printf ("[DEBUG] CloneObj. Located ARRAY type... "
			   "Still Not implemented");
	    }
	  offset = pClass[idc]->Prop[i].Off;
	  src_ptr = (EPByte) (gObj[IdObj]->Data) + offset;
	  dest_ptr = (EPByte) (gObj[id]->Data) + offset;
	  switch (pClass[idc]->Prop[i].Tipo)
	    {
	      /* TODO:
	       * User defined types -> Not used at this moment
	       * Objects -> At this moment we store a reference => No clone
	       */
	      /* DT_EBUFFER Still not tested */
	    case DT_EBUFFER:
	      src_pbuf = (EDMAT_BUFFER *) src_ptr;
	      dest_pbuf = (EDMAT_BUFFER *) dest_ptr;
	      if ((edma_buffer_alloc (dest_pbuf, src_pbuf->Size)) == -1)
		{
		  edma_printf_err ("[%s] Can't allocate buffer for "
				   "cloned object", __FUNCTION__);
		  /* FIXME:.... roll back transaction... destroy object */
		}
	      memcpy (dest_pbuf->dat, src_pbuf->dat, src_pbuf->Size);
	      break;
	    case DT_EZSTRING:
	      edma_log ("[%s] Clonning string '%s'", __FUNCTION__, 
			*(EPChar*)src_ptr);
	      if ((*(EPChar*)src_ptr))
		*(EPChar*)dest_ptr = (EPChar) strdup (*(EPChar*)src_ptr);
	      else
		*(EPChar*)dest_ptr = (EPChar) NULL;
	      break;
	    }
	}
    }
  return id;
}


/* _edma_clone_subobjects
 *   Internal functions which clones the subobject set of a given object
 *   id_from indicates the origin object in order to correct linking 
 * cloned subobjects
 */

OBJID EDMAPROC
_edma_clone_subobjects (OBJID IdObj, OBJID id_from)
{
  OBJID    id_subobj, aux;
  POBJ     pObj2, pObj;
  ESint32  i, n, i1, n1;

  pObj = gObj[IdObj];
  n = gObj[IdObj]->nUpTable;
 
  if (n == 0)
    return 0;

  for (i = 0; i < n; i++)
    {
      aux = gObj[IdObj]->UpTable[i].Obj;
      id_subobj = _edma_clone_simple_obj (aux, 1);
      if (id_subobj == -1)
	{
	  edma_printf_err ("[%s] Can't clone subobject....", __FUNCTION__);
	  return -1;
	}
      /* Link the subobject */
      pObj2 = gObj[aux];
      edma_add_superobject (id_from, id_subobj, pObj->UpTable[i].Id);
      
      n1 = pObj2->nDownTable;
      if (n1)
	{
	  for (i1 = 0; i1 < n1; i1++)
	    if (pObj2->DownTable[i1].Obj == IdObj)
	      break;
	  
	  /* Now we must look for the correct identifier */
	  edma_add_subobject (id_subobj, id_from, pObj2->DownTable[i1].Id);      
	}
      else
	{
	  /************************************************************** 
	   *If subobject has no uplink, we show a warnning and go on....*/
	  edma_log ("[%s] WARNNING... subobject has no uplink", __FUNCTION__);
	}
      /* Now we let the subobject create it subobjects */
      _edma_clone_subobjects (pObj2->IdObj, id_subobj);
    }
  return 0;
}


/* _edma_clone_superobjects
 *   Clones the superobjects set of a given objects 
 *   id_from indicates the origin object in order to correct linking 
 * cloned subobjects
 */

OBJID EDMAPROC
_edma_clone_superobjects (OBJID IdObj, OBJID id_from)
{
  OBJID    id_subobj, aux;
  POBJ     pObj2, pObj;
  ESint32  i, n, i1, n1;

  pObj = gObj[IdObj];
  n = gObj[IdObj]->nDownTable;
 
  if (n == 0)
    return 0;

  for (i = 0; i < n; i++)
    {
      aux = gObj[IdObj]->DownTable[i].Obj;

      id_subobj = _edma_clone_simple_obj (aux, 1);
      if (id_subobj == -1)
	{
	  edma_printf_err ("[%s] Can't clone subobject....", __FUNCTION__);
	  return -1;
	}

      /* Link the subobject */
      pObj2 = gObj[aux];
      edma_add_subobject (id_from, id_subobj, pObj->DownTable[i].Id);
      n1 = pObj2->nUpTable;
      if (n1)
	{
	  for (i1 = 0; i1 < n1; i1++)
	    if (pObj2->UpTable[i1].Obj == IdObj)
	      break;
	  
	  /* Now we must look for the correct identifier */
	  edma_add_superobject (id_subobj, id_from, pObj2->UpTable[i1].Id);
	}
      else
	{
	  /************************************************************** 
	   *If subobject has no uplink, we show a warnning and go on....*/
	  edma_log ("[%s] WARNNING... subobject has no uplink", __FUNCTION__);
	}
      /* Now we let the subobject create it subobjects */
      _edma_clone_superobjects (pObj2->IdObj, id_subobj);
    }
  return 0;
}

/* edma_clone_obj
 *   Deep clones the indicated object
 */

OBJID EDMAPROC 
edma_clone_obj (OBJID IdObj) 
{
  OBJID    id_clone;

  /* Test for valid object reference*/
  if (edma_check_obj_id (IdObj, "edma_clone_obj") == -1)
    return -1;

  id_clone = _edma_clone_simple_obj (IdObj, 1);

  /* Here we must repeat the process for all the sub/superobjects */
  _edma_clone_superobjects (IdObj, id_clone);
  _edma_clone_subobjects (IdObj, id_clone);

  return id_clone;
}

/* edma_shallow_clone_obj
 *   Shallow clones a given objects. 
 * Subobjects are not cloned only references to them
 *   Current implementation does not clone subobjects to avoid loops 
 * on lookup algorithm
 */

OBJID EDMAPROC 
edma_shallow_clone_obj (OBJID IdObj) 
{
  OBJID    id;
  EChar    aux[EDMA_GENERAL_ID_LEN];
  ESint32  i, n;

  /* Test for valid object reference*/
  if (edma_check_obj_id (IdObj, "edma_shallow_clone_obj") == -1)
    return -1;

  id = _edma_clone_simple_obj (IdObj, 1);
  /* Finally we should link the new cloned subobjects with the 
   * shared parents and childs*/
  n = gObj[IdObj]->nUpTable;
  for (i = 0; i < n; i++)
    {
      /* Build a unique identifier for the parent to child downlink*/
      sprintf (aux, "child-%ld-%ld", id, i);
      edma_add_superobject (id, gObj[IdObj]->UpTable[i].Obj,
			    gObj[IdObj]->UpTable[i].Id);
      edma_add_subobject (gObj[IdObj]->UpTable[i].Obj, id, aux);
    }
  /***************************************************************
   * NOTE:
   * To clone child objects can generate loops in the method lookup procedure
   * The lookup process only works if:
   *   - The method exists. Invoking a non existing method will 
   *     produce a infinite loop
   *   - If the method belongs to the cloned subobject and it isn't 
   *     in any shared object
   *     we must invoke it with the ".method_name" form 
   *   - Generalizing the above sentence it's safe to invoke method 
   *     using a non-ambiguous  classpaht.
   * -------------------------------------------------------------------
   * For now, we will remove the child object clonning functionality.
   * Just clonning parent objects we get the same functionality found in Self
   * 
   * We must check if child clonning can be useful and it will be provided as an
   * independent call that programmers can use carefully.
   */  
#if 0
  n = gObj[IdObj].nTabla1;
  for (i = 0; i < n; i++)
    {
      /* Build a unique identifier for the child to parent uplink*/
      sprintf (aux, "father-%ld-%ld", id, i);
      edma_add_subobject (id, ((POBJ)gObj[IdObj].Tabla1[i].Obj)->IdObj, 
			    gObj[IdObj].Tabla1[i].Id);
      edma_add_superobject (((POBJ)gObj[IdObj].Tabla1[i].Obj)->IdObj, id, aux);
    }
#endif
  /* We are done.*/
  return id;
}
