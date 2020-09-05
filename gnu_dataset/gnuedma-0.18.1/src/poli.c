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
 * Versión Beta 0.3r1
 * Vigo 3 de Julio de 1997
 *
 * Módulo para implementación sobrecarga de métodos
 * REVISIONES:--------------------------------------------------
 * 3 de Julio de 1997
 *
 * Comenzamos la conversión a WIN32
 * -------------------------
 * 21 de Julio de 1997
 * Modificamos el interface para que acepte un OBJID en lugar de
 * un POBJ.
 * Testeamos y corregimos las funciones de manejo de métodos virtuales
 * Todavía no vamos a implementar el tema de las signaturas    
 *********************************************************
 * Febraury, 7th, 2001
 * Code cleanup and comment tranlation
 * ------------------------------------------------------------
 * November, 17th, 2001
 * Compile warnning removal
 * ---------------------------------------------------------
 * March, 2nd, 2002
 * Code cleanup
 * -------------------------------------------------------------------------
 * April, 2nd, 2003
 * Modification to support changes to OBJ struct
 * ---------------------------------------------------------------------------
 * April, 13th, 2003
 * Code clean up and update to work with new locator functions
 * ---------------------------------------------------------------------------
 * May, 10th, 2003
 * Modifications to support changes to internal class structures
 * ---------------------------------------------------------------------------
 * July, 4th, 2003
 * Input Parameters sanity checks
*/
 
#include <stdio.h>
#include <string.h>
#include <stdarg.h>
#include "portable.h"
#include "classq.h"
#include "shmem.h"
#include "vglobal.h"
#include "clas.h"
#include "obj.h"
#include "pri3.h"
#include "pri3x.h"
#include "poli.h"
#include "misc.h"
#include "helper.h"
#include "anchor_points.h"
#include "error.h"
#include "locators.h"

/***************************************************************************
 * FIXME: We must provide function for overwrite using Signatures
 ***************************************************************************/

/* edma_over_met 
 *   Overrrides the indicated method in the given object redirecting
 *   it to the new provided object and function.
 *   If object is NULL original object is associated to the target function
 */

EUint32 EDMAPROC 
edma_over_met (OBJID IdObj, EPChar Id1, POBJ pObj1, PPROC Func) 
{
  OBJID         r;
  ESint32       indx,pos,i;
  ESint32       Tipo;
  POBJ          pObj;

  /* First check for valid object identifier*/
  if ((edma_check_obj_id (IdObj, "edma_over_met")) == -1) 
    return -1;

  if (Id1 == NULL)
    {
      edma_printf_err ("%s", "[edma_over_met] Invalid "
		       "Method Identifier (NULL)");
      return -1;
    }

  if (Func == NULL)
    {
      edma_printf_err ("%s", "[edma_over_met] Invalid Overwritter "
		       "function (NULL)");
      return -1;
    }

  r = _edma_locate_method (IdObj, Id1, NULL, &pos, &indx);
  
  /* We arrive here with the real object and Method index to execute. */
  if (r == -1)
    {
      return _edma_system_exception ("[edma_over_met] Can't run method '%s'"
				     " on object %d of class '%s'", 
				     Id1, IdObj, 
				     gClass[gObj[IdObj]->IdClass]->ClassName);
    }
  pObj = gObj[r];
  
  /* If a classpath was specified and we reach the same object -> 
     throw an exception*/
  if ((r == IdObj) && pos)
    {
      edma_printf_err ("[edma_over_met] Inheritance cycle detected, running "
		       "method '%s' in object %ld of class '%s'", Id1, IdObj,
		       gClass[gObj[IdObj]->IdClass]->ClassName);
      return -1;
    }
  
  Tipo = pObj->IdClass;

  if (indx == -1)
    {
      edma_printf_err ("[edma_over_met] Property not found... "
		       "Checking for SIU");
      edma_printf_err ("[edma_over_met] Still NOT IMPLEMENTED");
    }
  else /* Code to override method*/
    {
      if (pClass[Tipo]->Met[indx].Virtual) 
	{
	  for (i = 0; i < gClass[Tipo]->nMetVir; i++)
	    if (pObj->vTable[i].Ind == indx) 
	      break;
	  if (i == gClass[Tipo]->nMetVir) 
	    {
	      edma_printf_err ("[edma_over_met] Method %s in Class %s "
			       "isn't Virtual", Id1, gClass[Tipo]->ClassName);
	      return -1;
	    } 
	  else 
	    {
	      pObj->vTable[i].Flag = 2; /*XXXX Change to a constant**/
	      pObj->vTable[i].Func = Func;
	      if(pObj1 == 0)
		pObj->vTable[i].Obj = pObj;
	      else
		pObj->vTable[i].Obj = pObj1;
	    }
	}
      else
	{
	  edma_printf_err ("[edma_over_met] Method %s in Class %s "
			   "isn't Virtual", Id1, gClass[Tipo]->ClassName);
	  return -1;
	}
    }
  return 0;
}

/* edma_over_met1
 *   Identical to edma_over_met but allows to change associated identifier
 *   to the virtual method being overriden ???
 */

EUint32 EDMAPROC 
edma_over_met1 (OBJID IdObj,EPChar Id1,POBJ pObj1,
		PPROC Func,EPChar OverId) 
{
  OBJID         r;
  ESint32       indx, pos, i;
  ESint32       Tipo;
  POBJ          pObj;
  POBJ		OverObj = NULL;
  
  /* First check for valid object identifier*/
  if ((edma_check_obj_id (IdObj, "edma_over_met1")) == -1) 
    return -1;

  if (Id1 == NULL)
    {
      edma_printf_err ("%s", "[edma_over_met1] Invalid "
		       "Target Method Identifier (NULL)");
      return -1;
    }

  if (OverId == NULL)
    {
      edma_printf_err ("%s", "[edma_over_met1] Invalid "
		       "Source Method Identifier (NULL)");
      return -1;
    }

  if (Func == NULL)
    {
      edma_printf_err ("%s", "[edma_over_met] Invalid "
		       "Overwritter function (NULL)");
      return -1;
    }

  r = _edma_locate_method (IdObj, Id1, NULL, &pos, &indx);
  pObj = gObj[r];
  
  /* We arrive here with the real object and Method index to execute. */
  if (r == -1)
    {
      return _edma_system_exception ("[edma_over_met1] Can't run method '%s'"
				     " on object %d of class '%s'", 
				     Id1, IdObj, 
				     gClass[gObj[IdObj]->IdClass]->ClassName);
    }
  
  /* If a classpath was specified and we reach the same object -> 
     throw an exception*/
  if ((r == IdObj) && pos)
    {
      edma_printf_err ("[edma_over_met1] Inheritance cycle detected, running "
		       "method '%s' in object %ld of class '%s'", Id1, IdObj,
		       gClass[gObj[IdObj]->IdClass]->ClassName);
      return -1;
    }
  
  Tipo = pObj->IdClass;

  if (indx == -1)
    {
      edma_printf_err ("[edma_over_met1] Property not found... "
		       "Checking for SIU");
      edma_printf_err ("[edma_over_met1] Still NOT IMPLEMENTED");
    }
  else
    /* The code */
    {
      if (pClass[Tipo]->Met[indx].Virtual) 
	{
	  for (i = 0; i < gClass[Tipo]->nMetVir; i++) 
	    {
	      if (pObj->vTable[i].Ind == indx) 
		break;
	    }
	  if (i == gClass[Tipo]->nMetVir) 
	    {
	      edma_printf_err ("Method %s in Class %s isn't Virtual",
			       Id1, gClass[Tipo]->ClassName);
	      return -1;
	    } 
	  else 
	    {
	      pObj->vTable[i].Flag = 1;
	      strncpy ((EPChar)pObj->vTable[i].Id, OverId, EDMA_MET_NAME_LEN);
	      pObj->vTable[i].Func = Func;
	      if(pObj1 == 0)
		pObj->vTable[i].Obj = OverObj;
	      else
		pObj->vTable[i].Obj = pObj1;
	    }
	}
    }

  return 0;
}

/* edma_over_met3
 *   Overrides the method Id2 of object IdObj with method Id1 of object IdObj
 */

EUint32 EDMAPROC 
edma_over_met3 (OBJID IdObj, EPChar Id2, EPChar Id1)
{
  OBJID         r; 
  ESint32       indx, pos;
  POBJ          pObj;
  PPROC		Func;

  /* First check for valid object identifier*/
  if ((edma_check_obj_id (IdObj, "edma_over_met")) == -1) 
    return -1;

  if (Id2 == NULL)
    {
      edma_printf_err ("%s", "[edma_over_met1] Invalid "
		       "Target Method Identifier (NULL)");
      return -1;
    }

  if (Id1 == NULL)
    {
      edma_printf_err ("%s", "[edma_over_met1] Invalid "
		       "Target Method Identifier (NULL)");
      return -1;
    }

  r = _edma_locate_method (IdObj, Id1, NULL, &pos, &indx);

  /* We arrive here with the real object and Method index to execute.*/
  if (r == -1) 
    {
      return _edma_system_exception ("[edma_over_met3] Can't found method '%s'"
				     " on object %d of class '%s'", 
				     Id2, IdObj, 
				     gClass[gObj[IdObj]->IdClass]->ClassName);
    }
  pObj = gObj[r];   
  /* If a classpath was specified and we reach the same object -> 
     throw an exception*/
  if ((r == IdObj) && pos)
    {
      edma_printf_err ("[edma_over_met3] Inheritance cycle detected, running "
		       "method '%s' in object %ld of class '%s'", Id1, IdObj,
		       gClass[gObj[IdObj]->IdClass]->ClassName);
      return -1;
    }
 
  if (indx == -1)
    {
      edma_printf_err ("[edma_over_met3] Method not found... Checking for SIU");
      edma_printf_err ("[edma_over_met3] Still NOT IMPLEMENTED");
    }
  /* The code */
  else 
    {
      Func = (PPROC)pClass[pObj->IdClass]->met_func[indx].Func;
      edma_over_met1 (IdObj, Id2, pObj, Func, Id1 + pos);
    }
  return 0;
}

/* edma_restore_met
 *   Restores method Id1 of object IdObj to its default class method 
 * implementation
 */

EUint32 EDMAPROC 
edma_restore_met (OBJID IdObj,EPChar Id1)
{
  OBJID         r;
  ESint32       indx,pos,i;
  ESint32       Tipo;
  POBJ          pObj;

  /* First check for valid object identifier*/
  if ((edma_check_obj_id (IdObj, "edma_over_met")) == -1) 
    return -1;

  if (Id1 == NULL)
    {
      edma_printf_err ("%s", "[edma_over_met1] Invalid "
		       "Target Method Identifier (NULL)");
      return -1;
    }

  r = _edma_locate_method (IdObj, Id1, NULL, &pos, &indx);

  /* We arrive here with the real object and Method index to execute. */
  if (r == -1) 
    {
      return _edma_system_exception ("[edma_over_met] Can't run method '%s'"
				     " on object %d of class '%s'", 
				     Id1, IdObj, 
				     gClass[gObj[IdObj]->IdClass]->ClassName);
    }
  
  pObj = gObj[r];
  /* If a classpath was specified and we reach the same object ->  
   * throw an exception*/
  if ((r == IdObj) && pos)
    {
      edma_printf_err ("[edma_over_met] Inheritance cycle detected, running "
		       "method '%s' in object %ld of class '%s'", Id1, IdObj,
		       gClass[gObj[IdObj]->IdClass]->ClassName);
      return -1;
    }
  
  Tipo = pObj->IdClass;

  if (indx == -1)
    {
      edma_printf_err ("[edma_over_met] Property not found... "
		       "Checking for SIU");
      edma_printf_err ("[edma_over_met] Still NOT IMPLEMENTED");
    }

  else /* The code */
    {
      if ((pClass[Tipo]->Met[indx].Virtual)) 
	{
	  for (i = 0; i < gClass[Tipo]->nMetVir; i++)
	    if (pObj->vTable[i].Ind == indx) 
	      break;
	  if (i == gClass[Tipo]->nMetVir) 
	    {
	      edma_printf_err ("Method %s in Class %s isn't Virtual"
			       ,Id1 ,gClass[Tipo]->ClassName);
	      return -1;
	    } 
	  else 
	    {
	      pObj->vTable[i].Func = (PPROC)pClass[Tipo]->met_func[indx].Func;
	      pObj->vTable[i].Obj = pObj;

	      pObj->vTable[i].Flag = 0;
	      strncpy (pObj->vTable[i].Id, pClass[Tipo]->Met[indx].IdMet, 
		       EDMA_MET_NAME_LEN);
	    }
	}
    }

  return 0;
}


/* edma_old_met3
 *  Allows to execute the original method Id on object IdObj, even if this
 *  method has been overriden
 */

EUint32	EDMAPROC 
edma_old_met3 (OBJID IdObj,EPChar Id,...) 
{
  va_list	 p;
  ESint32        r;
  
  va_start(p,Id);
  r = edma_met3_pargs (IdObj, Id, NULL, 0, p);
  va_end(p);

  return r;
}

