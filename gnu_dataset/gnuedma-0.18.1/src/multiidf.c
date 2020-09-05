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

/****************************************************************
 * Entorno de Desarrollo Modular y Abierto
 * Versión Beta 0.3r1
 * (c) David Martínez Oliveira
 *
 * Modulo para gestión de multiples IDFs
 * Revisiones: ---------------------------------------------------------
 * 24 de Julio de 1997
 * -----------------------------
 * 24 de Agosto de 1997
 * Intentamos añadir las funciones de soporte para la creación
 * de parsers IDF
 *---------------------------------------------------
 * Febraury, 7th, 2001
 * Code cleanup and comment translation
 * --------------------------------------------------------
 * November, 17th, 2001
 * Updated IDFSetMet to set abstract and static flags
 * ------------------------------------------------------------
 * November, 18th, 2001
 * Compile Warnnings removal
 * --------------------------------------------
 * November, 20th, 2001
 * Added new primitives GetNParsers and GetParserClass
 * ---------------------------------------------------------
 * March, 2nd,2002
 * Code cleanup
 * -----------------------------------------------------------
 * May, 10th, 2003
 * Modification to support changes to internal class structure
 * --------------------------------------------------------------
 * July, 4th, 2003
 * Input parameters sanity checks
 * ----------------------------------------------------------------
 * October, 11th, 2003
 * FIxing bug on reading local repositories
 * --------------------------------------------------------------------
 * January, 6th, 2004
 * Unification of GNU/EDMA Subsystem Management
 * ---------------------------------------------------------------------
 * January, 7th, 2004
 * Support for Include-like primitive
 */
 
#include <stdio.h>
#include "portable.h"
#include "const.h"
#include "shmem.h"
#include "multiidf.h"
#include "vglobal.h"
#include "clas.h"
#include "classq.h"
#include "misc.h"
#include "inh.h"
#include "helper.h"
#include "classrt.h"

#include "subsystems.h"
#include "siu.h"

/* For EMI support */
#include "emi.h"
#include "obj.h"
#include "pri3.h"

/* edma_ingridf_get_num_parsers
 *  Returns the number of IDF Parser registered in the system
 */

ESint32 EDMAPROC 
edma_ingridf_get_num_parsers (void) 
{
  return GVar->n_SubSystem[SS_INGRIDF];
}

/* edma_ingridf_get_parser_class
 *   Returns the class identifier associated to the i-th IDF Parser
 *   registered in the system
 */

CLASSID EDMAPROC 
edma_ingridf_get_parser_class (ESint32 i) 
{
  return edma_subsystem_get_item_class (SS_INGRIDF, i);
}

/* edma_ingridf_get_parser
 *   Gets index in parser table for the indicated IDF parser
 */

ESint32 EDMAPROC 
edma_ingridf_get_parser (EPChar parser_name) 
{
  return edma_subsystem_get_item (SS_INGRIDF, parser_name);

}

/* edma_ingridf_add_parser
 *    Registers a new IDF parser in the system
 */

ESint32 EDMAPROC 
edma_ingridf_add_parser (EPChar name, EPChar class_name) 
{
  return edma_subsystem_add_item (SS_INGRIDF, name, class_name);
}

/*
 * IDF Parser developing support functions
 */


/* edma_idf_get_free_class_id
 *   Provides an available class identifier for registering new
 *   classes in run-time
 *   Type allows to choose the kind of repository to use (SHARED/LOCAL)
 */

/* Get a valid class identifier 
 * FIXME:
 * This function should be executed atomic, that is, within a MUTEX
 * or almost the functions called inside.
*/
CLASSID EDMAPROC
edma_idf_get_free_class_id (ESint32 type)
{
  CLASSID     cid;

  switch (type)
    {
    case EDMA_SHARED_CLASS:
      {
	cid = _edma_get_shared_class_id ();
	break;
      }
    case EDMA_LOCAL_CLASS:
      {
	cid = _edma_get_local_class_id ();
	break;
      }
    }

  return cid;
}

/* FIXME: Check this functions against the ones contained in classrt.c 
          Most of then are equivalent and could be mixed
	  Note that most of this functions deal with shared memory instead
	  of per-process class pool as classrt.c does
	  
	  Also, check them against idf.c some pieces of code in that
	  file maybe changed for calls to this file.
*/

/* _edma_get_shared_class_id
 *   Provides an available class identifier for a SHARED class
 */

CLASSID EDMAPROC 
_edma_get_shared_class_id ()
{
  EUint32		i, ret;
  
  for (i = 0; i < GVar->nMaxClases; i++)
    if ((gClass[i] == 0) || (ProcMapTable[i] == CLASS_FREE))
      break;
  
  
  if (i == GVar->nMaxClases) 
    {
      edma_printf_err ("Can´t alloc class identifier");
      return -1;
    }  
  else 
    {
      /* Map new class to Shared Class entry*/
      gClass[i] = &SharedClass[i];
      gClass[i]->Status = CLASS_TEMP;
      gClass[i]->repo_type = EDMA_SHARED_REPO;

      ProcMapTable[i] = CLASS_TEMP;

      /* Allocate per-process class block*/
      if ((ret = _edma_class_alloc_priv_data (i)) < 0)
	{
	  /* XXX: (check this) Undo transaction */
	  gClass[i]->Status = CLASS_FREE;
	  ProcMapTable[i] = CLASS_FREE;
	  return ret;
	}

      return i;
    }
}



/* edma_idf_set_general
 *   Sets class general information 
 */

/* FIXME: Better split this function on several, each one managing 
 *        one of the parameters 
 *        Also note that NameSpace field in class struct is missing
 */

ESint32 EDMAPROC 
edma_idf_set_general (CLASSID Id, EPChar Name,
		      EPChar SO, EPChar Maq, EPChar Module) 
{
  if ((Id < 0) || (Id > (GVar->nMaxClases + nMaxLocalClasses)))
    {
      edma_printf_err ("[edma_idf_set_general] Class identifier %d invalid", 
		       Id);
      return -1;
    }

  if (ProcMapTable[Id] != CLASS_TEMP)
    {
      edma_printf_err ("[edma_idf_set_general] Class %d not in "
		       "definition status", Id);
      return -1;
    }

  if (Name == NULL)
    {
      edma_printf_err ("[%s]", "[edma_idf_set_general] Invalid Class Name"
		       " (NULL)");
      return -1;
    }
  
  if (SO == NULL)
    {
      edma_printf_err ("[%s]", "edma_idf_set_general] Invalid Operating System "
		       "Name (NULL)");
      return -1;
    }
  
  if (Maq == NULL)
    {
      edma_printf_err ("[%s]", "[edma_idf_set_general] Invalid Machine Name"
		       " (NULL)");
      return -1;
    }
  
  if (Module == NULL)
    {
      edma_printf_err ("[%s]", "[edma_idf_set_general] Invalid Implementation "
		       "Module Name (NULL)");
      return -1;
    }
  
  if (edma_get_class_id (Name) == -1)
    return -1;
  
  strncpy (gClass[Id]->ClassName, Name, EDMA_CLASS_NAME_LEN);
  if ((gClass[Id]->SOId = edma_get_so_id (SO)) == -1)
    {
      edma_printf_err ("[edma_idf_set_general] Bad Operating System: %s", SO);
      return -1;
    }

  if ((gClass[Id]->MaqId = edma_get_arch_id (Maq)) == -1)
    {
      edma_printf_err ("[edma_idf_set_general] Bad Architecture: %s", Maq);
    }
  strncpy (gClass[Id]->SysClass.ModuleName, Module, EDMA_CLASS_MODULE_LEN);
  
  return 0;
}


/* edma_idf_set_class_name
 *   Allows to define the class name of a given class being defined
 */

ESint32 EDMAPROC 
edma_idf_set_class_name (CLASSID class_id, EPChar class_name)
{
  if ((edma_check_class_id (class_id, "edma_get_class_module")) == -1)
    return -1;

  if (class_name == NULL)
    return -1;

  edma_log ("Setting name '%s'to class %d (%p)", 
	    class_name, class_id, gClass[class_id]->ClassName); 
  if (gClass[class_id] != 0)
    if (ProcMapTable[class_id] == CLASS_TEMP)
      {
	strncpy (gClass[class_id]->ClassName, class_name, 
		 EDMA_CLASS_NAME_LEN - 1);
	return 0;
      }
  
  return -1;
}

/* edma_idf_set_class_namespace
 *   Allows to define the class namespace for a given class being defined
 */

ESint32 EDMAPROC 
edma_idf_set_class_namespace (CLASSID class_id, EPChar name_space)
{
  if (gClass[class_id] == 0)
    {
      edma_printf_err ("[edma_local_class_add_method] Invalid Class "
		       "Identifier [%d]", class_id);
      return -1;
    }

  if (name_space == NULL)
    return -1;

  if (gClass[class_id] != 0)
    if (ProcMapTable[class_id] == CLASS_TEMP)
      {
	strncpy (gClass[class_id]->NameSpace, name_space, 
		 EDMA_CLASS_NAMESPACE_LEN);
	return 0;
      }

  return -1;
}


/* edma_idf_set_class_version
 *   Allows to define the class version for a given class being defined
 */

ESint32 EDMAPROC 
edma_idf_set_class_version (CLASSID class_id, ESint32 major, ESint32 minor)
{
  if (gClass[class_id] == 0)
    {
      edma_printf_err ("[edma_local_class_add_method] Invalid Class "
		       "Identifier [%d]", class_id);
      return -1;
    }

  if (gClass[class_id] != 0)
    if (ProcMapTable[class_id] == CLASS_TEMP)
      {
	/* FIXME: Check that the provided version do not exists */
	gClass[class_id]->MajorVer = major;
	gClass[class_id]->MinorVer = minor;
	gClass[class_id]->CurrentVer = class_id;
	return 0;
      }
  return -1;
}
 

/* edma_idf_set_class_attribs
 *   Allows to define class attributes for a given class being defined
 */

ESint32 EDMAPROC 
edma_idf_set_class_attribs (CLASSID class_id, 
			    ESint32 is_siu, ESint32 is_idf, ESint32 is_emi)
{
  if (gClass[class_id] == 0)
    {
      edma_printf_err ("[edma_local_class_add_method] Invalid Class "
		       "Identifier [%d]", class_id);
      return -1;
    }

  if (gClass[class_id] != 0)
    if (ProcMapTable[class_id] == CLASS_TEMP)
      {
	/* FIXME: Check that the provided version do not exists */
	gClass[class_id]->IsSIU = is_siu;
	gClass[class_id]->IsIDF = is_idf;
	gClass[class_id]->IsEMI = is_emi;
	return 0;
      }

  return -1;
}

/* edma_idf_set_class_arch
 *    Allows to define class target architecture for a given class being defined
 */

ESint32 EDMAPROC 
edma_idf_set_class_arch (CLASSID class_id, ESint32 os, ESint32 machine)
{
  if ((edma_check_class_id (class_id, "edma_get_class_module")) == -1)
    return -1;

  /* FIXME: CHeck if 'os' and 'machine' are valid values */

  if (gClass[class_id] != 0)
    if (ProcMapTable[class_id] == CLASS_TEMP)
      {
	gClass[class_id]->SOId = os;
	gClass[class_id]->MaqId = machine;
	return 0;
      }
  
  return -1;
}

/* edma_idf_set_class_impl
 *   Allows to set class implementation file for a given class being defined
 */

ESint32 EDMAPROC 
edma_idf_set_class_impl (CLASSID class_id, EPChar impl)
{
  if ((edma_check_class_id (class_id, "edma_get_class_module")) == -1)
    return -1;

  if (impl == NULL)
    return -1;

  if (gClass[class_id] != 0)
    if (ProcMapTable[class_id] == CLASS_TEMP)
      {
	strncpy (gClass[class_id]->SysClass.ModuleName, impl, 
		 EDMA_CLASS_IMPL_LEN);
	return 0;
      }
  
  return -1;
}

/* edma_idf_set_def
 *   Allows to define the number of properties and methods required 
 *   by the class being defined.
 *   This function allocates space to hold the number of methods and 
 *   properties for the class and is used for SHARED classes to simplify 
 *   memory management with shared memory
 */

ESint32 EDMAPROC 
edma_idf_set_def (CLASSID CId,
		  EUint32 nProp, EUint32 nMet, EUint32 nNot) 
{
  HMEM		h;
  EChar		Aux[1024];

  if ((CId < 0) || (CId > (GVar->nMaxClases + nMaxLocalClasses)))
    {
      edma_printf_err ("[edma_idf_set_def] Bad class identifier: %d", CId);
      return -1;
    }

  if (ProcMapTable[CId] != CLASS_TEMP)
    {
      edma_printf_err ("[edma_idf_set_def] Class %d isn't been defined", CId);
      return -1;
    }

  gClass[CId]->nProp = nProp;
  gClass[CId]->nMet = nMet;
  gClass[CId]->nNot = nNot;
  

  if (gClass[CId]->repo_type == EDMA_SHARED_REPO)
    {
      if(nProp != 0) 
	{
	  snprintf (Aux, 1024, "%ldProp", CId);
	  
	  h = edma_salloc (nProp * sizeof (PROP), Aux);
	  if (h == (HMEM)0) 
	    {
	      edma_printf_err ("[edma_idf_set] Can't allocate propieties "
			       "table for class %ld", CId);
	      return -1;
	    }
	  
	  pClass[CId]->Prop = (PROP*) edma_sget (h);
	  pClass[CId]->SysClass.hProp = h;
	}
      
      if (nMet != 0) 
	{
	  snprintf (Aux, 1024, "%ldMet", CId);
	  h = edma_salloc (nMet * sizeof (MET), Aux);
	  if (h == (HMEM)0) 
	    {
	      edma_printf_err("Can't allocate method table");
	      return -1;
	    }
	  
	  pClass[CId]->Met = (MET *) edma_sget (h);
	  pClass[CId]->SysClass.hMet=h;
	  
	  /* Per-process code mapping */      
	  pClass[CId]->SysClass.hMetFunc = edma_palloc (sizeof (PROCMET) 
							* nMet);
	  if (pClass[CId]->SysClass.hMetFunc == 0) 
	    {
	      edma_printf_err ("Can´t allocate process method table");
	      return -1;
	    }
	  pClass[CId]->met_func = 
	    (PROCMET *) edma_pget (pClass[CId]->SysClass.hMetFunc);
	  
	}

      if (nNot != 0) 
	{
	  snprintf (Aux, 1024, "%ldNot", CId);
	  h = edma_salloc (nNot * sizeof (NOT), Aux);
	  if (h == (HMEM)0) 
	    {
	      edma_printf_err ("Can't allocate notification table");
	      return -1;
	    }
	  
	  pClass[CId]->Not = (NOT*) edma_sget (h);
	  pClass[CId]->SysClass.hNot = h;
	}
    }
  else /* Local Repository no shared memory required */
    {
      if(nProp != 0) 
	{
	  h = edma_palloc (nProp * sizeof (PROP));
	  if (h == (HMEM)0) 
	    {
	      edma_printf_err ("[edma_idf_set] Can't allocate properties "
			       "table for class %ld", CId);
	      return -1;
	    }
	  
	  pClass[CId]->Prop = (PROP*) edma_pget (h);
	  pClass[CId]->SysClass.hProp = h;
	}
      
      if (nMet != 0) 
	{
	  h = edma_palloc (nMet * sizeof (MET));
	  if (h == (HMEM)0) 
	    {
	      edma_printf_err("Can't allocate method table");
	      return -1;
	    }
	  
	  pClass[CId]->Met = (MET *) edma_pget (h);
	  pClass[CId]->SysClass.hMet=h;
	  
	  /* Per-process code mapping */      
	  pClass[CId]->SysClass.hMetFunc = edma_palloc (sizeof (PROCMET) 
							* nMet);
	  if (pClass[CId]->SysClass.hMetFunc == 0) 
	    {
	      edma_printf_err ("Can´t allocate process method table");
	      return -1;
	    }
	  pClass[CId]->met_func = 
	    (PROCMET *) edma_pget (pClass[CId]->SysClass.hMetFunc);
	  
	}
      if (nNot != 0) 
	{
	  h = edma_palloc (nNot * sizeof (NOT));
	  if (h == (HMEM)0) 
	    {
	      edma_printf_err ("Can't allocate notification table");
	      return -1;
	    }
	  
	  pClass[CId]->Not = (NOT*) edma_pget (h);
	  pClass[CId]->SysClass.hNot = h;
	}      
    }
  
  return 0;
}

/* edma_idf_set_prop
 *  Allows to define a class property for a class being defined
 */

ESint32 EDMAPROC 
edma_idf_set_prop (CLASSID iC, EUint32 iP,
		   EPChar Name, EPChar pType,
		   EPChar pAc, EUint32 nElem, EPChar user_data)
{
  ESint32       size;
  EUint32	i;
  CLASSID       type_class;

  if ((iC < 0) || (iC > (GVar->nMaxClases + nMaxLocalClasses)))
    {
      edma_printf_err ("[edma_idf_set_prop] Bad Class Identifier: %d", iC);
      return -1;
    }

  if (ProcMapTable[iC] != CLASS_TEMP)
    {
      edma_printf_err ("[edma_idf_set_prop] Class %d esn't been defined", iC);
      return -1;
    }

  if (iP > gClass[iC]->nProp) 
    {
      edma_printf_err ("[edma_idf_set_prop] Method index out of order: "
		       "%d of %d", iP, gClass[iC]->nProp);
      return -1;
    }

  if (Name == NULL)
    {
      edma_printf_err ("%s", "[edma_idf_set_prop] Invalid Property "
		       "Name (NULL)");
      return -1;
    }

  if (pType == NULL)
    {
      edma_printf_err ("%s", "[edma_idf_set_prop] Invalid Property "
		       "Type (NULL)");
      return -1;
    }

  if (pAc == NULL)
    {
      edma_printf_err ("%s", "[edma_idf_set_prop] Invalid Property "
		       "Access Modifier (NULL)");
      return -1;
    }
  
  strncpy (pClass[iC]->Prop[iP].IdProp, Name, EDMA_PROP_NAME_LEN);
  
  if ((i = edma_get_type_id (pType)) < 0)
    {
      edma_printf_err ("[edma_idf_set_prop] Type %s is not valid", pType);
      return -1;
    }

  pClass[iC]->Prop[iP].Tipo = i;

 if ((i == DT_EUSER) && (user_data != NULL)) 
    {
      type_class = edma_get_class_id (user_data);
      if (type_class != -1)
	pClass[iC]->Prop[iP].UserInfo = (HMEM) type_class;
      else 
	{
	  edma_printf_err ("[edma_idf_set_prop] Property %s declare '%s' "
			   "user type not defined",
			   pClass[iC]->Prop[iP].IdProp, user_data);
	  pClass[iC]->Prop[iP].UserInfo = -1;
	}
    }
  
  pClass[iC]->Prop[iP].nElem = nElem;
  size = tipo[i].tam;
  if (nElem != 0)
    size *= nElem;
  
  /* FIXME: Convert to function ???? */
  pClass[iC]->Prop[iP].ioTipo = E_L;  /* Make read/write and then modify */
  if (pAc[0] == 'R') pClass[iC]->Prop[iP].ioTipo = L;
  if (pAc[0] == 'W') pClass[iC]->Prop[iP].ioTipo = E;
  if (strcmp (pAc, "READ") == 0)
    pClass[iC]->Prop[iP].ioTipo = L;
  if (strcmp (pAc, "WRITE") == 0)
    pClass[iC]->Prop[iP].ioTipo = E;
  if (strcmp (pAc, "READ/WRITE") == 0)
    pClass[iC]->Prop[iP].ioTipo = E_L;

  if (strcmp (pAc, "RW") == 0)
    pClass[iC]->Prop[iP].ioTipo = E_L;

  
  return size;
}

/* edma_idf_set_met
 *   Allows to define a class method for a class being defined
 */

ESint32 EDMAPROC 
edma_idf_set_met (CLASSID iC,EUint32 iP,EPChar mName,EPChar Sig,
		  EByte vFlag, EByte aFlag, EByte sFlag) 
{
  if ((iC < 0) || (iC > (GVar->nMaxClases + nMaxLocalClasses)))
    {
      edma_printf_err ("[edma_idf_set_prop] Bad Class Identifier: %d", iC);
      return -1;
    }

  if (ProcMapTable[iC] != CLASS_TEMP)
    {
      edma_printf_err ("[edma_idf_set_prop] Class %d esn't been defined", iC);
      return -1;
    }

  if (iP > gClass[iC]->nMet) 
    {
      edma_printf_err ("[edma_idf_set_met] Method index out of order: "
		       "%d of %d", iP, gClass[iC]->nMet);
      return -1;
    }

  if (mName == NULL)
    {
      edma_printf_err ("%s", "[edma_idf_set_method] Invalid Method Name "
		       "(NULL)");
      return -1;
    }
  
  if (Sig == NULL)
    {
      edma_printf_err ("%s", "[edma_idf_set_method] Invalid Method Signature "
		       "(NULL)");
      return -1;
    }

  strncpy (pClass[iC]->Met[iP].IdMet, mName, EDMA_MET_NAME_LEN);
  strncpy (pClass[iC]->Met[iP].Sign, Sig, EDMA_MET_SIG_LEN);
  pClass[iC]->Met[iP].Virtual = vFlag;
  pClass[iC]->Met[iP].Abstract = aFlag;
  pClass[iC]->Met[iP].Static = sFlag;
  
  if (vFlag)
    return 1;
  else
    return 0;
}

/* edma_idf_set_sclist
 *   Allows to set a class list of superclass for a class being defined
 */

ESint32 EDMAPROC 
edma_idf_set_sclist (CLASSID Id,EPChar *IdSC, EPChar *IdDownAp, EPChar *IdUpAp) 
{
  /* Let edma_derive_class check parameters sanity */
  if (IdSC != NULL)
    return edma_derive_class (gClass[Id]->ClassName, IdSC, IdDownAp, IdUpAp);
  return 0;
}

/* _edma_idf_set_subsystems
 *   This function registers a given class in the subsystems indicated by
 *   its class attributes
 */

ESint32 EDMAPROC
_edma_idf_set_subsystems (CLASSID class_id)
{
  OBJID  idEMI;

  if ((class_id < 0) || (class_id > (GVar->nMaxClases + nMaxLocalClasses)))
    {
      edma_printf_err ("[edma_idf_set_subsystems] Bad Class Identifier: %d", 
		       class_id);
      return -1;
    }

  /* Subsystem registration requires the class to be DEFINED */
  if ((ProcMapTable[class_id] < CLASS_DEF) 
      && (ProcMapTable[class_id] < CLASS_LOCKED))
    {
      edma_printf_err ("[edma_idf_set_subsystems] Class %d not defined", 
		       class_id);
      return -1;
    }
  
  if (gClass[class_id]->IsIDF == 1) 
    {
      _edma_subsystem_add_item_with_classid (SS_INGRIDF, 
					     gClass[class_id]->ClassName, 
					     class_id);
      edma_log ("        %s is an IDF Parser ...", gClass[class_id]->ClassName);
    }
  if (gClass[class_id]->IsSIU == 1) 
    {
      _edma_subsystem_add_item_with_classid (SS_SIU, 
					     gClass[class_id]->ClassName, 
					     class_id);
      edma_log ("        %s is a SIU Proxy ...", gClass[class_id]->ClassName);
    }	
  if (gClass[class_id]->IsEMI == 1) 
    {
      _edma_subsystem_add_item_with_classid (SS_EMI, 
					     gClass[class_id]->ClassName, 
					     class_id);
      edma_log ("        %s is an EMI Extension ...", 
		gClass[class_id]->ClassName);
      idEMI = edma_new_obj (gClass[class_id]->ClassName, NULL);
      edma_met3 (idEMI, "Init");
      edma_free_obj (idEMI);
    }
  
  return 0;
}

/* edma_idf_set_class_id
 *   This function performs last stage on class creation to make the class
 *   being defined available to clietns
 */

/********* FIXME: Atomic execution for this function ***************/
ESint32 EDMAPROC 
edma_idf_set_class_id (CLASSID class_id) 
{
  EUint32		i,size,size1;

  printf ("%s. Prop: %p Met:%p\n", __FUNCTION__, pClass[class_id]->Prop , pClass[class_id]->Met);
  if (class_id < MAX_CLASE)  /* If it is a Shared class */
    {
      if ((class_id < 0) || (class_id > GVar->nClases))
	{
	  edma_printf_err ("[edma_idf_set_class_id] Bad Shared Class "
			   "Identifier: %d", class_id);
	  return -1;
	}
    }
  else
    {
      if (class_id > MAX_CLASE + nMaxLocalClasses)
	{
	  edma_printf_err ("[edma_idf_set_class_id] Bad Local Class "
			   "Identifier: %d", class_id);
	  return -1;
	}

    }

  if (ProcMapTable[class_id] != CLASS_TEMP)
    {
      edma_printf_err ("[edma_idf_set_class_id] Class %d isn't been "
		       "defined", class_id);
      return -1;
    }


  /* If we are defining also the interface...*/
  if (pClass[class_id]->Prop != NULL)
    {
      /* We calculate data block size*/
      size = 0;
      for (i = 0; i < gClass[class_id]->nProp; i++) 
	{
	  size1 = tipo[pClass[class_id]->Prop[i].Tipo].tam;
	  if (pClass[class_id]->Prop[i].nElem != 0)
	    size1 *= pClass[class_id]->Prop[i].nElem;
	  size += size1;
	}
      gClass[class_id]->TamDatos = size;

      /* Create property dictionary */
      pClass[class_id]->prop_dict = edma_dict_new (gClass[class_id]->nProp);
      for (i = 0; i < gClass[class_id]->nProp; i++)
	edma_dict_add_entry (pClass[class_id]->prop_dict, 
			     pClass[class_id]->Prop[i].IdProp, i);
   
    }

  if (pClass[class_id]->Met != NULL)
    {
      size = 0;
      /* We count virtual methods*/
      for (i = 0;i < gClass[class_id]->nMet; i++)
	size += pClass[class_id]->Met[i].Virtual;
      gClass[class_id]->nMetVir = size;


      /* Create method dictionary */
      pClass[class_id]->met_dict = edma_dict_new (gClass[class_id]->nMet);
      for (i = 0; i < gClass[class_id]->nMet; i++)
	edma_dict_add_entry (pClass[class_id]->met_dict, 
			     pClass[class_id]->Met[i].IdMet, i);
     

      
    }

  edma_dict_add_entry (edma_class_dict, gClass[class_id]->ClassName, class_id);
  /* FIXME: Update version information */
  pClass[class_id]->actual_version = class_id;
  gClass[class_id]->CurrentVer = class_id;
  //_edma_class_update_versions (class_id);

  if (pClass[class_id]->Prop == NULL)
    {
      gClass[class_id]->Status = CLASS_DEF;
      ProcMapTable[class_id] = CLASS_DEF;
      if (class_id > MAX_CLASE) 
	{
	  nLocalClasses++;
	}
    }
  else
    {
      if (class_id < MAX_CLASE)
	{
	  gClass[class_id]->Status = CLASS_ILOADED;
	  ProcMapTable[class_id] = CLASS_IMAPPED;
	  //GVar->nClases++;
	}
      else
	{
	  /* Local classes always provide code in-process so they 
	     get automatically LOADED */
	  gClass[class_id]->Status = CLASS_LOADED;
	  ProcMapTable[class_id] = CLASS_LOADED;
	  nLocalClasses++;
	}
    }

  /* Last operation on classes. Register SIU, Parser, EMI....*/
  _edma_idf_set_subsystems (class_id);

  return 0;
}

/**** TEST TEST TEST TEST TEST TEST TEST ******/
ESint32 EDMAPROC 
edma_idf_set_class_id1 (CLASSID class_id) 
{
  EUint32		i,size,size1;

  printf ("%s\n", __FUNCTION__);
  if (class_id < MAX_CLASE)  /* If it is a Shared class */
    {
      if ((class_id < 0) || (class_id > GVar->nClases))
	{
	  edma_printf_err ("[edma_idf_set_class_id] Bad Shared Class "
			   "Identifier: %d", class_id);
	  return -1;
	}
    }
  else
    {
      if (class_id > MAX_CLASE + nMaxLocalClasses)
	{
	  edma_printf_err ("[edma_idf_set_class_id] Bad Local "
			   "Class Identifier: %d", class_id);
	  return -1;
	}

    }

  if (ProcMapTable[class_id] != CLASS_TEMP)
    {
      edma_printf_err ("[edma_idf_set_class_id] Class %d isn't been defined", 
		       class_id);
      return -1;
    }
 
  /* We calculate data block size*/
  size = 0;
  for (i = 0; i < gClass[class_id]->nProp; i++) 
    {
      pClass[class_id]->Prop[i].Off = size;
      size1 = tipo[pClass[class_id]->Prop[i].Tipo].tam;
      if (pClass[class_id]->Prop[i].nElem != 0)
	size1 *= pClass[class_id]->Prop[i].nElem;
      size += size1;
    }

  gClass[class_id]->TamDatos = size;
  edma_log ("Data Size for Class '%s':%d is %d.", 
	       gClass[class_id]->ClassName, class_id, size);
  
  /* We count virtual methods*/
  size = 0;
  for (i = 0;i < gClass[class_id]->nMet; i++)
    size += pClass[class_id]->Met[i].Virtual;
  gClass[class_id]->nMetVir = size;



  /* Create method dictionary */
  pClass[class_id]->met_dict = edma_dict_new (gClass[class_id]->nMet);
  for (i = 0; i < gClass[class_id]->nMet; i++)
    edma_dict_add_entry (pClass[class_id]->met_dict, 
			 pClass[class_id]->Met[i].IdMet, i);

  /* Create property dictionary */
  pClass[class_id]->prop_dict = edma_dict_new (gClass[class_id]->nProp);
  for (i = 0; i < gClass[class_id]->nProp; i++)
    edma_dict_add_entry (pClass[class_id]->prop_dict, 
			 pClass[class_id]->Prop[i].IdProp, i);

edma_dict_add_entry (edma_class_dict, gClass[class_id]->ClassName, class_id);
  /* FIXME: Update version information */
  _edma_class_update_versions (class_id);

  if (class_id < MAX_CLASE)
    {
      gClass[class_id]->Status = CLASS_ILOADED;
      ProcMapTable[class_id] = CLASS_IMAPPED;
      GVar->nClases++;
    }
  else
    {
      /* Local classes always provide code in-process so they 
       * get automatically LOADED */
      gClass[class_id]->Status = CLASS_LOADED;
      ProcMapTable[class_id] = CLASS_LOADED;
      nLocalClasses++;
    }

  /* Last operation on classes. Register SIU, Parser, EMI....*/
  _edma_idf_set_subsystems (class_id);

  return 0;
}
