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
 * Support for Run-Time LOCAL class definition
 * REVISIONES:-------------------------------------------------------------
 * May, 17th, 2003
 * File creation
 * ------------------------------------------------------------------------
 * July, 2nd, 2003
 * Cosmetic changes
 * --------------------------------------------------------------------------
 * September, 9th, 2003
 * These function become helper function for local class definition.
 * The whole class definition can be now done using IngrIDF function (multiidf.c)
 */

#include <stdio.h>
#include <unistd.h>
#include "portable.h"
#include "const.h"
#include "vglobal.h"
#include "shmem.h"
#include "dynlink.h"

#include "idf.h"
#include "multiidf.h"
#include "siu.h"
#include "emi.h"
#include "misc.h"

#include "classq.h"
#include "clas.h"
#include "obj.h"
#include "pri3.h"
#include "spri3.h"

#include "helper.h"

/* 
 * edma_get_class_id
 * Locates a free entry in the local class pool and initialize base structures
 * Resize local class table if required
 */
CLASSID EDMAPROC 
_edma_get_local_class_id ()
{
  ESint32     i, j, n, ret;
  HMEM        h;

  j = 0;
  n = MAX_CLASE + nLocalClasses;
  /* FIXME: Multi-threading --> Lock table at this point */
  for (i = MAX_CLASE; j < n; i++)
    if (gClass[i] == 0)
      break;

  if (i == MAX_CLASE + nMaxLocalClasses) /* The pool is full. Resizing */
    {
      /* FIXME: We must realloc ProcTable and pClass along with gClass */
      edma_printf ("[%s] Resizing of local class pool still not implemented",
		   __FUNCTION__);
      return -1;
    }
  /* Now, dynamically create the class structure and fill in default values */
  if (( h = edma_palloc (sizeof (CLASE))) == 0)
    {
      edma_printf_err ("%s", "[edma_get_local_class_id] Can't alloc memory "
		       "for new local class");
      return -1;
    }

  gClass[i] = edma_pget (h);
  /* FIXME Multithreading: Unlock table at this point */

  /* Class initialization */
  memset (gClass[i], 0, sizeof (CLASE));
  ProcMapTable[i] = CLASS_TEMP; /* Class exists but it is still not usable. */
  gClass[i]->Status = CLASS_TEMP;
  gClass[i]->repo_type = EDMA_LOCAL_REPO;
  gClass[i]->repo_id = -1;

  gClass[i]->CurrentVer = i;

  if ((ret = _edma_class_alloc_priv_data (i)) < 0)
    {
      /* FIXME: Undo transaction */
      edma_pfree (h, gClass[i]);
      ProcMapTable[i] = CLASS_FREE;
      return ret;
    }

  pClass[i]->SysClass.MySelf = h;

  gClass[i]->SIUProxy = -1;
  gClass[i]->IDFParser = -1;

  return i;
}


/* edma_add_local_class_property
 *   Adds a new property to a local class (per-process defined class)
 */

ESint32 EDMAPROC 
edma_add_local_class_property (CLASSID class_id, EPChar prop_name, 
			       EUint32 type, ESint32 access, EUint32 nelems)
{
  ESint32   n, indx;

  if (gClass[class_id] == 0)
    {
      edma_printf_err ("[edma_local_class_add_method] Invalid Class "
		       "Identifier [%d]", class_id);
      return -1;
    }

  if (prop_name == NULL)
    return -1;

  if (ProcMapTable[class_id] != CLASS_TEMP)
    {
      edma_printf_err ("[edma_local_class_add_property] Class %d "
		       "not in definition state", class_id);
      return -1;
    }
  
  /* Check if the property already exists */
  if ((edma_get_prop_indx (class_id, prop_name)) != -1)
    {
      edma_printf_err ("[edma_local_class_set_property] Property '%s' "
		       "already defined", prop_name);
      return -1;
    }
  /* Alloc memory to store new property */
  n = indx = gClass[class_id]->nProp;
  n ++;

  if ((pClass[class_id]->SysClass.hProp = 
       edma_prealloc (pClass[class_id]->SysClass.hProp, 
		      sizeof (PROP) * n)) == 0)
    {
      edma_printf_err ("[edma_local_class_add_property] Can't alloc memory for "
		       " property '%s'", prop_name);
      return -1;
    }
  
  pClass[class_id]->Prop = edma_pget(pClass[class_id]->SysClass.hProp);
  /* Store data */
  strncpy (pClass[class_id]->Prop[indx].IdProp, prop_name, EDMA_PROP_NAME_LEN);
  pClass[class_id]->Prop[indx].Tipo = type;
  pClass[class_id]->Prop[indx].ioTipo = access;
  pClass[class_id]->Prop[indx].nElem = nelems;
  pClass[class_id]->Prop[indx].Off = gClass[class_id]->TamDatos;

  /* Update nProp, TamDatos */
  gClass[class_id]->nProp++;

  gClass[class_id]->TamDatos += edma_get_type_size (type);
  return indx;
}

/* edma_add_local_class_method
 *   Adds a new method to a local class (per-process defined class)
 */

ESint32 EDMAPROC 
edma_add_local_class_method (CLASSID class_id, EPChar met_name, EPChar met_sig, 
			     PPROC *f, 
			     ESint32 mvirtual, 
			     ESint32 mstatic, 
			     ESint32 mabstract)
{
  ESint32    n, indx;
  PROCMET    aux;

  /* FIXME: Change parameter checking for macros as in GTK for example */
  if (gClass[class_id] == 0)
    {
      edma_printf_err ("[edma_local_class_add_method] Invalid "
		       "Class Identifier [%d]", class_id);
      return -1;
    }

  if (met_name == NULL)
    {
      edma_printf_err ("%s", "[edma_local_class_add_method] Invalid "
		       "method name");
      return -1;
    }

  if (met_sig == NULL)
    {
      edma_printf_err ("%s", "[edma_local_class_add_method] Invalid "
		       "method signature");
      return -1;
    }

  if (f == NULL)
    {
      edma_printf_err ("%s", "[edma_local_class_add_method] Invalid function");
      return -1;
    }

  if (ProcMapTable[class_id] != CLASS_TEMP)
    {
      edma_printf_err ("[edma_local_class_add_method] Class %d "
		       "not in definition state", class_id);
      return -1;
    }
  
  /* Check if the method already exists (name + signature)*/
  /* FIXME: call '_edma_look4_met_single@locators.h' */
  if ((edma_get_met_indx (class_id, met_name)) != -1)
    {
      edma_printf_err ("[edma_local_class_add_method] Method '%s:%s' "
		       "already defined",
		       met_name, met_sig);
      return -1;
    }

  /* Alloc memory to store new method */
  n = indx = gClass[class_id]->nMet;
  n ++;

  if ((pClass[class_id]->SysClass.hMet =
       edma_prealloc (pClass[class_id]->SysClass.hMet, sizeof(MET) * n)) == 0)
    {
      edma_printf_err ("[edma_local_class_add_method] Can't alloc memory "
		       "for method '%s:%s'",
		       met_name, met_sig);
      return -1;
    }
  /* Alloc Memory to estore pointer to method code*/
  if ((pClass[class_id]->SysClass.hMetFunc =
       edma_prealloc (pClass[class_id]->SysClass.hMetFunc, 
		      sizeof(PROCMET) * n)) == 0)
    {
      edma_printf_err ("[edma_local_class_add_method] Can't alloc memory "
		       "for method '%s:%s'",
		       met_name, met_sig);
      return -1;
    }

  pClass[class_id]->Met = edma_pget (pClass[class_id]->SysClass.hMet);
  pClass[class_id]->met_func = edma_pget (pClass[class_id]->SysClass.hMetFunc);
  /* Store data */
  strncpy (pClass[class_id]->Met[indx].IdMet, met_name, EDMA_MET_NAME_LEN);
  strncpy (pClass[class_id]->Met[indx].Sign, met_sig, EDMA_MET_SIG_LEN);
  pClass[class_id]->Met[indx].Virtual = mvirtual;
  pClass[class_id]->Met[indx].Abstract = mabstract;
  pClass[class_id]->Met[indx].Static = mstatic;

  aux.Func = f;
  pClass[class_id]->met_func[indx] = aux;

  /* Number of methods and virtual methods is calculated by 
   * edma_local_class_finish */

  /* Update nMet, nMetVir */
  gClass[class_id]->nMet++;

  return indx;
}


/* edma_add_local_class_superclass
 *   Adds a new superclass to a loca class (per-process defined class)
 */
 
ESint32 EDMAPROC 
edma_add_local_class_superclass (CLASSID class_id, CLASSID superclass_id, 
				 EPChar pap, EPChar pap1)
{
  ESint32   n, indx;

  if (gClass[class_id] == 0)
    {
      edma_printf_err ("[edma_add_local_class_superclass] Invalid "
		       "Class Identifier [%d]", class_id);
      return -1;
    }

  if (pap == NULL)
    {
      edma_printf_err ("%s", "[edma_add_local_class_superclass] Invalid "
		       "first preferer anchor point");
      return -1;
    }

  if (pap1 == NULL)
    {
      edma_printf_err ("%s", "[edma_add_local_class_superclass] Invalid "
		       "second preferer anchor point");
      return -1;
    }


  /* Check if superclass already exists (class + pap) */
  /* Alloc memory to store new superclass */
  n = indx = gClass[class_id]->Derived;
  n++;

  if ((pClass[class_id]->SysClass.hSCList = 
       edma_prealloc (pClass[class_id]->SysClass.hSCList, 
		      sizeof (CLASSID) * n)) == 0)
    {
      edma_printf_err ("[edma_local_class_add_superclass] Can't alloc "
		       "memory for superclass %d:'%s'",
		       class_id, gClass[class_id]->ClassName);
      return -1;
    }
  /* Allocate espace for the preferer anchor point */
  if ((pClass[class_id]->SysClass.hSCIdList = 
       edma_prealloc (pClass[class_id]->SysClass.hSCIdList, 
		      n * sizeof (ID))) == 0)
    {
      edma_printf_err ("[edma_local_class_add_superclass] Can't alloc "
		       "memory for superclass %d:'%s'",
		       class_id, gClass[class_id]->ClassName);
      return -1;
      
    }

  /* Allocate espace for the preferer anchor point */
  if ((pClass[class_id]->SysClass.hSubCIdList = 
       edma_prealloc (pClass[class_id]->SysClass.hSubCIdList, 
		      n * sizeof (ID))) == 0)
    {
      edma_printf_err ("[edma_local_class_add_superclass] Can't alloc memory "
		       "for superclass %d:'%s'",
		       class_id, gClass[class_id]->ClassName);
      return -1;
      
    }

  pClass[class_id]->SCList = edma_pget (pClass[class_id]->SysClass.hSCList);
  pClass[class_id]->SCIdList = edma_pget (pClass[class_id]->SysClass.hSCIdList);
  pClass[class_id]->SubCIdList = edma_pget (pClass[class_id]->SysClass.hSubCIdList);
  /* Store data */
  pClass[class_id]->SCList[indx] = superclass_id;

  if (pap != NULL)
    {
      if ((edma_get_class_id (pap)) != -1)
	edma_printf ("[edma_add_local_class_add_superclass] WARNNING: "
		     "Uplink Anchor Point '%s' matches "
		     "a classname this may crash your application", pap);
      strncpy (pClass[class_id]->SCIdList[indx], pap, sizeof (ID));
    }
  else
    strncpy (pClass[class_id]->SCIdList[indx], gClass[class_id]->ClassName, 
	     sizeof (ID));

  if (pap1 != NULL)
    {
      if ((edma_get_class_id (pap1)) != -1)
	edma_printf ("[edma_add_local_class_add_superclass] WARNNING: "
		     "Downlink Anchor Point '%s' matches "
		     "a classname this may crash your application", pap1);
      strncpy (pClass[class_id]->SubCIdList[indx], pap1, sizeof (ID));
    }
  else
    strncpy (pClass[class_id]->SubCIdList[indx], gClass[class_id]->ClassName, 
	     sizeof (ID));

  /* Update Derived*/
  gClass[class_id]->Derived++;
  return indx;
}

/* edma_add_local_class_superclass_by_nane
 *   Adds a new superclass to a loca class (per-process defined class) 
 * by name specification
 */

ESint32 EDMAPROC 
edma_add_local_class_superclass_by_name (CLASSID class_id, EPChar class_name, 
					 EPChar pap, EPChar pap1)
{
  CLASSID    super_cid;
  
  /* Only check class_name parameter. Let edma_add_local_superclass 
   * check the other*/
  if (class_name == NULL)
    {
      edma_printf_err ("%s", "[edma_local_class_add_superclass_by_name] "
		       "Invalid class_name");
      return -1;
    }

  super_cid = edma_get_class_id (class_name);
  if (super_cid == -1)
    {
      edma_printf_err ("[edma_local_class_add_superclass_by_name] Class %s "
		       "doesn't exist", class_name);
      return -1;
    }

  return edma_add_local_class_superclass (class_id, super_cid, pap1, pap);
}


/* edma_local_class_finish
 *   Deperecared function use: edma_idf_set_class_id
 *   Function kept temporaly after all example code is updated
 */

ESint32 EDMAPROC edma_local_class_finish (CLASSID class_id)
{
  /* Mark the class as usable... Required to register as SIU Proxy, etc...*/
  ProcMapTable[class_id] = CLASS_LOADED;
  /*gClass[class_id]->CurrentVer = class_id; */
  nLocalClasses ++;

  edma_idf_set_class_id (class_id);

  return 0;
}

