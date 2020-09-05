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
 * Modulo de Query de Clases
 * Revisiones: ---------------------------------------------------------
 * 15 de Mayo de 1997
 * -------------------
 * 27/11/1997
 *   Modificaciones menores. Tipos de parámetros consistentes con lo
 * que representan, y gestión de errores
 * -------------------------------
 * 19/12/1997
 *   Añadimos más funciones de query, necesarias para FAYE
 * ----------------
 * 8/1/1998
 *   Más funciones de query requeridas por FAYE. GetPropId3
 * -----------------------
 * 12/1/1998
 *   Vamos a modificar la primitiva GetPropId3 para que nos devuelva
 * el identificador de la clase y el indice de la propiedad especificado
 * en un camino de clase.
 * 
 *   La nueva implementación no utilizará el objeto para localizar los
 * datos, simplemente troceará la cadena y se quedará con los datos del
 * final. Será la invocación de WProp3 o RProp3 la que determine que el
 * camino es incorrecto.
 * 
 *   Hacemos esto así para la implementación de WProp3 en FAYEPROXY ya
 * que en el sistema local tenemos solo una representación parcial de la
 * tabla de superclases de los objetos.
 * ---------------------------------------------------
 * January, 31th, 2001
 * Some cosmetic changes in the code, erasing non used code
 * ----------------------------------------------------------
 * November, 11th,2001
 * Added IsMetAbstract and IsMetStatic primitives
 * ------------------------------------------------------------
 * November, 24th, 2001
 * Adding IsISFParser, IsSIUProxy, IsEMIHandler primitives
 * ----------------------------------------------------------------------
 * March, 2nd, 2002
 * Code clean, -Wall warnning remove
 * -------------------------------------------------------------------
 * July, 13th-14th, 2002
 * Added quering functions for multiversioned classes
 * --------------------------------------------------------------------
 * May, 10th, 2003
 * Modification to support changes to internal class structures
 * ---------------------------------------------------------------------
 * May, 17th, 2003
 * Modification to support per-process class pool.
 * Class lookup process updated. For now, we mantain global and local 
 * classes separately
 * ------------------------------------------------------------------------
 * July, 2nd, 2003
 * Cosmetic changes
 * ----------------------------------------------------------------------------
 * October, 29th, 2003
 * Updated edma_get_class_id to deal with stock classes installed by other processes
 * If class is not found in current class dictionary, a linear search is initiated
 * through the shared class structure to look for the class.
 * 
 * If found, the class is added to the process class dictionary, and marked as defined
 * in ProcTableMap
 * ---------------------------------------------------------------------------------------
 * Febraury, 7th, 2004
 * Updated functions to work with new, per-process, 'actual_version' field
 */

#include <stdlib.h>
#include <string.h>

#include "const.h"
#include "portable.h"
#include "vglobal.h"
#include "tclass.h"
#include "ttypes.h"
#include "shmem.h"

#include "obj.h"
#include "clas.h"
#include "pri3.h"

#include "misc.h"
#include "helper.h"

#include "repo.h"

/* edma_get_class_name
 * Stores in 'name' the name of the class 'Id'
 */
ESint32 EDMAPROC 
edma_get_class_name (CLASSID class_id, EPChar class_name) 
{
  if ((edma_check_class_id (class_id, "edma_get_class_name")) == -1)
    return -1;

  if (class_name == NULL)
    return -1;

  strncpy (class_name, gClass[class_id]->ClassName, EDMA_CLASS_NAME_LEN);
  return 0;
}

/* edma_get_class_so_id
 * Returns the operating system identifier for class 'class_id'
 */
SOID EDMAPROC 
edma_get_class_so_id (CLASSID class_id) 
{ 
  if ((edma_check_class_id (class_id, "edma_get_class_so_id")) == -1)
    return -1;

  return gClass[class_id]->SOId;
}

/* edma_get_class_arch_id
 * Returns the architecture identifier for class 'class_id'
 */
MAQID EDMAPROC 
edma_get_class_arch_id (CLASSID class_id) 
{
  if ((edma_check_class_id (class_id, "edma_get_class_arch_id")) == -1)
    return -1;

  return gClass[class_id]->MaqId;
}

/* edma_get_class_module
 * Returns the physical module name for class 'class_id' implementation
 */
ESint32 EDMAPROC 
edma_get_class_module (CLASSID class_id, EPChar module_name) 
{
  if ((edma_check_class_id (class_id, "edma_get_class_module")) == -1)
    return -1;

  if (module_name == NULL)
    return -1;

  strncpy (module_name, gClass[class_id]->SysClass.ModuleName, 
	   EDMA_CLASS_MODULE_LEN);

  return 0;
}

/* edma_get_class_namespace
 * Returns the namespace where the class sits on
 */
ESint32 EDMAPROC 
edma_get_class_namespace (CLASSID class_id, EPChar name_space) 
{
  if ((edma_check_class_id (class_id, "edma_get_class_namespace")) == -1)
    return -1;

  if (name_space == NULL)
    return -1;

  strncpy (name_space, gClass[class_id]->NameSpace, EDMA_CLASS_NAMESPACE_LEN);

  return 0;
}

/* edma_get_class_major_version
 * Returns the major version for this class
 */
ESint32 EDMAPROC 
edma_get_class_major_version (CLASSID class_id) 
{
  if ((edma_check_class_id (class_id, "edma_get_class_major_version")) == -1)
    return -1;

  return gClass[class_id]->MajorVer;
}

/* edma_get_class_minor_version
 * Returns the minor version for this class
 */
ESint32 EDMAPROC 
edma_get_class_minor_version (CLASSID class_id) 
{
  if ((edma_check_class_id (class_id, "edma_get_class_minor_version")) == -1)
    return -1;

  return gClass[class_id]->MinorVer;
}

/* edma_get_class_current_version
 * Returns the current version for this class 
 */ 
ESint32 EDMAPROC 
edma_get_class_current_version (CLASSID class_id) 
{
  if ((edma_check_class_id (class_id, "edma_get_class_current_version")) == -1)
    return -1;

  //return gClass[class_id]->CurrentVer;
  if (!pClass[class_id]) return -1;
  return pClass[class_id]->actual_version;
}

EPChar EDMAPROC
edma_get_class_repo_dir (CLASSID cid)
{
  if ((edma_check_class_id (cid, "edma_get_class_repo_dir")) == -1)
    return NULL;

  //return sys_dir[SClass[cid]->repo_type];
  return edma_repo_manager_get_repo_dir (gClass[cid]->repo_id);
}

EPChar EDMAPROC
edma_get_class_repo_name (CLASSID cid)
{
  if ((edma_check_class_id (cid, "edma_get_class_repo_name")) == -1)
    return NULL;

  //return sys_dir[SClass[cid]->repo_type];
  return edma_repo_manager_get_repo_name (gClass[cid]->repo_id);
}

ESint32 EDMAPROC
edma_get_class_repo_type (CLASSID cid)
{
  if ((edma_check_class_id (cid, "edma_get_class_repo_type")) == -1)
    return -1;

  return SClass[cid]->repo_type;
}



/* edma_get_num_reg_classes
 * Returns the number of registered classes in the system
 */
EUint32 EDMAPROC 
edma_get_num_reg_classes (void)
{
  return GVar->nClases + nLocalClasses;
}

/* edma_get_real_class_id 
 * Returns the class id for the class which name is 'class_name'
 * begining the search from 'begin' and without version
 * translation
 */

/* NOTE: THis function only works on SYSTEM Classes not in LOCAL classes*/
CLASSID EDMAPROC
edma_get_real_id (EPChar class_name, CLASSID begin)
{
  EUint32     i;

  if ((edma_check_class_id (begin, "edma_get_real_id")) == -1)
    return -1;

  if (class_name == NULL)
    return -1;

  for (i = begin; i < GVar->nClases; i++)
    if (gClass[i] != 0)
      if (ProcMapTable[i] != CLASS_FREE)
	if ((strncmp (gClass[i]->ClassName, class_name, 
		      EDMA_CLASS_NAME_LEN)) == 0)
	  break;

  if (i == GVar->nClases)
    return -1;
  else
    return i;

}


/* FIXME: Missing function to deal with NameSpaces */
/* edma_get_class_id_with_version
 * Returns the class id for class which name is 'class_name'
 * and fits the version number indicated
 */
CLASSID EDMAPROC
edma_get_class_id_with_version (EPChar class_name, 
				ESint32 major_ver, ESint32 minor_ver)
{
  ESint32       i, j;
  EUint32	r;
  OBJID		idEMI;

  if (class_name == NULL)
    return -1;

  j = -1;
  i = edma_dict_get_next_index (edma_class_dict, class_name, &j);

  /* Try to locate a class matching name and version information */
  while (i != -1)
    {
      if ((gClass[i]->MajorVer == major_ver) 
	  && (gClass[i]->MinorVer == minor_ver))
	break;
      else
	{
	  i = edma_dict_get_next_index (edma_class_dict, class_name, &j);
	}
    }
  
  if (i < 0) 
    {
      /*****************************************
       * FIXME:
       * If not found there are three possibilities
       *   1. Stock class registered by another process
       *      This process need to map the class and add it to the system
       *      dictionary. Look for the class manually in the shared memory
       *      region and add it to local process dictionary
       *   2. Launch EMI Handler if any, to try to get the class from other
       *      sources
       *   3. The class really doesn't exist. Return Error
       */
      j = 0;
      /* First look for class in the Global Class Set*/
      for (i = 0; j < GVar->nClases; i++) 
	if (gClass[i] != 0)
	  if (gClass[i]->Status != CLASS_FREE)
	    {
	      if ((strncmp (gClass[i]->ClassName, class_name, 
			    EDMA_CLASS_NAME_LEN)) == 0)
		if ((gClass[i]->MajorVer == major_ver) 
		    && (gClass[i]->MinorVer == minor_ver))
		break;
	      j++;
	    }
      
      if (i == GVar->nClases) 
	{
	  /* If the class was not found in the global class set...*/
	  /* ... try an EMI handler, if any*/
	  r = -1;
	  if (GVar->GetClassEMI != 0) 
	    {
	      idEMI = edma_new_obj (gClass[GVar->GetClassEMI]->ClassName);
	      if (idEMI == -1) 
		{
		  edma_printf_err ("[edma_get_class_id] Can't create "
				   "EMI handler object [%s]", 
				   gClass[GVar->GetClassEMI]->ClassName);
		  return -1;
		}
	      r = (CLASSID) edma_met3 (idEMI, "GetClass", class_name);
	      edma_free_obj (idEMI);
	    }
	  /*return r == -1 ? -1 : pClass[r]->actual_version; */
	  return r;
	} 
      else
	{
	  /* Add class to this process system class dictionary */
	  ProcMapTable[i] = CLASS_DEF;
	  edma_dict_add_entry (edma_class_dict, gClass[i]->ClassName, i);
	  return i;
	}
      return -1; 
    }
  else 
    {
      if ((gClass[i] != 0) && (ProcMapTable[i] != CLASS_FREE))
	return i;
      else 
	return -1;
    }

  edma_printf ("%s", "(edma_get_class_id_with_version) Unreacheable code");
  return -1;
}

/* edma_get_class_id
 * Returns the class identifier for class which name is 'class_name'
 */
CLASSID EDMAPROC 
edma_get_class_id (EPChar class_name) 
{
  ESint32       i;
  EUint32	r, j;
  OBJID		idEMI;

  if (class_name == NULL)
    return -1;

  /* Check for version updates */
  if (last_checked_versions < GVar->time_last_version_update)
    _edma_class_update_all_versions ();

  if ((i = edma_dict_get_index (edma_class_dict, class_name)) < 0) 
    {
      /*****************************************
       * FIXME:
       * If not found there are three possibilities
       *   1. Stock class registered by another process
       *      This process need to map the class and add it to the system
       *      dictionary. Look for the class manually in the shared memory
       *      region and add it to local process dictionary
       *   2. Launch EMI Handler if any, to try to get the class from other
       *      sources
       *   3. The class really doesn't exist. Return Error
       */
      j = 0;
      /* First look for class in the Global Class Set*/
      for (i = 0; j < GVar->nClases; i++) 
	if (gClass[i] != 0)
	  if (gClass[i]->Status != CLASS_FREE)
	    {
	      if ((strncmp (gClass[i]->ClassName, class_name, 
			    EDMA_CLASS_NAME_LEN)) == 0)
		break;
	      j++;
	    }
      
      if (i == GVar->nClases) 
	{
	  /* If the class was not found in the global class set...*/
	  /* ... try an EMI handler, if any*/
	  r = -1;
	  if (GVar->GetClassEMI != 0) 
	    {
	      idEMI = edma_new_obj (gClass[GVar->GetClassEMI]->ClassName);
	      if (idEMI == -1) 
		{
		  edma_printf_err ("[edma_get_class_id] Can't create "
				   "EMI handler object [%s]", 
				   gClass[GVar->GetClassEMI]->ClassName);
		  return -1;
		}
	      r = (CLASSID) edma_met3 (idEMI, "GetClass", class_name);
	      edma_free_obj (idEMI);
	    }
	  /*return r == -1 ? -1 : gClass[r]->CurrentVer;*/
	  return r == -1 ? -1 : pClass[r]->actual_version;
	  //return r == -1 ? -1 : gClass[r]->CurrentVer;
	} 
      else
	{
	  /* Add class to this process system class dictionary */
	  ProcMapTable[i] = CLASS_DEF;
	  edma_log ("[%s] Registering class %i'%s'", __FUNCTION__, 
		       i, gClass[i]->ClassName);
	  edma_dict_add_entry (edma_class_dict, gClass[i]->ClassName, i);

	  return pClass[i]->actual_version;
	  //return gClass[i]->CurrentVer;
	  //return gClass[i]->CurrentVer;
	}
      return -1; 
    }
  else 
    {
      CLASSID sel = pClass[i]->actual_version;

      edma_log ("Looking for class '%s' I found '%s' v%d.%d\n",
		class_name, gClass[sel]->ClassName,
		gClass[sel]->MajorVer, gClass[sel]->MinorVer);
      if ((gClass[i] != 0) && (ProcMapTable[i] != CLASS_FREE))
	//return gClass[i]->CurrentVer;
	return pClass[i]->actual_version;
      else 
	return -1;
    }

  edma_printf ("%s", "(edma_get_class_id) Unreacheable code");
  return -1;
}


CLASSID EDMAPROC 
edma_get_next_class (CLASSID cid)
{
  int  i, base;

  base = cid;
  if (cid < GVar->nClases)
    {
      edma_printf ("Looking in shared repository from index: %d", cid);
      // We are looking in the global repository
      for (i = cid; i < GVar->nClases; i++)
	if (gClass[i]->Status != CLASS_FREE) return i;
      base = MAX_CLASE;
    }
  /* If we reach this point we are searching in the local repositories */
  for (i = base; i < MAX_CLASE + nMaxLocalClasses; i++)
    if (gClass[i]->Status != CLASS_FREE) return i;

  return -1;
}


EPChar EDMAPROC
edma_get_idf_file_path (CLASSID cid)
{
  EPChar  aux;

  if ((edma_check_class_id (cid, "edma_get_idf_file_path")) == -1)
    return NULL;
  
  if ((aux = malloc (1024)) == NULL)
    {
      edma_printf_err ("Cannot alocate memory...\n");
      return NULL;
    }

  if (edma_repo_manager_get_repo_type (gClass[cid]->repo_id) == 
      EDMA_SHARED_REPO)
    {
      if ((gClass[cid]->MajorVer == 0) && (gClass[cid]->MinorVer == 0))
	snprintf (aux, 1024, "%s/%s/%s/%s.idf", 
		  edma_repo_manager_get_repo_dir (gClass[cid]->repo_id),
		  IDFDIR, gClass[cid]->NameSpace, 
		  gClass[cid]->ClassName);
      else
	snprintf (aux, 1024, "%s/%s/%s/%s_%d_%d.idf", 
		  edma_repo_manager_get_repo_dir (gClass[cid]->repo_id),
		  IDFDIR, gClass[cid]->NameSpace, 
		  gClass[cid]->ClassName, gClass[cid]->MajorVer, gClass[cid]->MinorVer);
    }
  else
    {
      if ((gClass[cid]->MajorVer == 0) && (gClass[cid]->MinorVer == 0))
	snprintf (aux, 1024, "%s/%s/idf/%s/%s.idf",  
		  edma_repo_manager_get_repo_dir (gClass[cid]->repo_id),
		  edma_repo_manager_get_repo_name (gClass[cid]->repo_id), 
		  gClass[cid]->NameSpace, 
		  gClass[cid]->ClassName);
      else
	snprintf (aux, 1024, "%s/%s/idf/%s/%s_%d_%d.idf", 
		  edma_repo_manager_get_repo_dir (gClass[cid]->repo_id),
		  edma_repo_manager_get_repo_name (gClass[cid]->repo_id), 
		  gClass[cid]->NameSpace, 
		  gClass[cid]->ClassName, gClass[cid]->MajorVer, gClass[cid]->MinorVer);
    }


  return aux;
}

EPChar EDMAPROC
edma_get_impl_file_path (CLASSID cid)
{
  EPChar aux;

  if ((edma_check_class_id (cid, "edma_is_class_IDF_parser")) == -1) 
    return NULL;

  if ((aux = malloc (1024)) == NULL)
    {
      edma_printf_err ("Cannot alocate memory...\n");
      return NULL;
    }

#ifdef LINUX
      /* We add version information lo load shared library*/
      if (edma_repo_manager_get_repo_type (gClass[cid]->repo_id) == EDMA_SHARED_REPO)
	{
	  snprintf (aux, 1024, "%s/lib/edma/%s/%s.so.%d.%d.0", 
		    edma_repo_manager_get_repo_dir (gClass[cid]->repo_id),
		    gClass[cid]->NameSpace, 
		    gClass[cid]->SysClass.ModuleName,
		    gClass[cid]->MajorVer,
		    gClass[cid]->MinorVer);
	}
      else
	{
	  char *rn = edma_repo_manager_get_repo_name (gClass[cid]->repo_id);

	  snprintf (aux, 1024, "%s/%s/lib/%s/%s.so.%d.%d.0", 
		    edma_repo_manager_get_repo_dir (gClass[cid]->repo_id),
		    rn,
		    gClass[cid]->NameSpace, 
		    gClass[cid]->SysClass.ModuleName,
		    gClass[cid]->MajorVer,
		    gClass[cid]->MinorVer);
	  free (rn);

	}
	  
#endif

#ifdef DARWIN
      /* We add version information lo load shared library*/
      if (edma_repo_manager_get_repo_type (gClass[cid]->repo_id) == EDMA_SHARED_REPO)
	{
	  snprintf (aux, 1024, "%s/lib/edma/%s/%s.%d.%d.0.dylib", 
		    edma_repo_manager_get_repo_dir (gClass[cid]->repo_id),
		    gClass[cid]->NameSpace, 
		    gClass[cid]->SysClass.ModuleName,
		    gClass[cid]->MajorVer,
		    gClass[cid]->MinorVer);
	}
      else
	{
	  char *rn = edma_repo_manager_get_repo_name (gClass[cid]->repo_id);

	  snprintf (aux, 1024, "%s/%s/lib/%s/%s.%d.%d.0.dylib", 
		    edma_repo_manager_get_repo_dir (gClass[cid]->repo_id),
		    rn,
		    gClass[cid]->NameSpace, 
		    gClass[cid]->SysClass.ModuleName,
		    gClass[cid]->MajorVer,
		    gClass[cid]->MinorVer);
	  free (rn);
	}
#endif



  return aux;
}




/* edma_is_class_IDF_parser
 * returns 1 if class 'id' is an IDF parser, 
 *         0 if class 'id' isn't an IDF parser
 *        -1 if the class identifier is wrong
*/
ESint32 EDMAPROC  
edma_is_class_IDF_parser (CLASSID class_id) 
{
  if ((edma_check_class_id (class_id, "edma_is_class_IDF_parser")) == -1)
    return -1;

  return gClass[class_id]->IsIDF;
}

/* edma_is_class_SIU_proxy
 * return 1 if class 'id' is a SIU proxy
 *        0 if class 'id' isn't a SIU proxy
 *       -1 on error
 */
ESint32 EDMAPROC 
edma_is_class_SIU_proxy (CLASSID class_id) 
{
  if ((edma_check_class_id (class_id, "edma_is_class_SIU_proxy")) == -1)
    return -1;

  return gClass[class_id]->IsSIU;  
}

/* edma_is_class_EMI_handler
 * Returns 1 if class 'class_id' is an EMI handler
 *         0 if class 'class_id' isn't an EMI handler
 *        -1 on error
 */
ESint32 EDMAPROC 
edma_is_class_EMI_handler (CLASSID class_id) 
{
  if ((edma_check_class_id (class_id, "edma_is_class_EMI_handler")) == -1)
    return -1;

  return gClass[class_id]->IsEMI;
}

/* edma_get_prop_num
 * Returns the number of properties of class 'class_id'
*/
ESint32 EDMAPROC 
edma_get_prop_num (CLASSID class_id) 
{
  if ((edma_check_class_id (class_id, "edma_get_prop_num")) == -1)
    return -1;

  return gClass[class_id]->nProp;
}

/* edma_get_prop_name
 * Return the name of the propery number 'indx' of class 'class_id'
 */
ESint32 EDMAPROC 
edma_get_prop_name (CLASSID class_id, EUint32 indx, EPChar prop_name) 
{
  if ((edma_check_class_id (class_id, "edma_get_prop_name")) == -1)
    return -1;

  if (prop_name == NULL)
    return -1;

  if (indx > gClass[class_id]->nProp) {
    edma_printf_err ("[edma_get_prop_name] Property %d doesn't exist "
		     "for class %s[%ld]",
		     indx, gClass[class_id]->ClassName, class_id);
    return -1;
  }

  strncpy (prop_name, pClass[class_id]->Prop[indx].IdProp, EDMA_PROP_NAME_LEN);
  return 0;
}

/* edma_get_class_num_superclasses
 * Returns the number of superclasses for class 'class_id'
 */
ESint32 EDMAPROC
edma_get_class_num_superclasses (CLASSID class_id)
{
  if ((edma_check_class_id (class_id, "edma_get_class_num_superclasses")) == -1)
    return -1;

  return gClass[class_id]->Derived;
}

/* edma_get_class_superclass
 *   Get the indx-th superclass of class class_id
 */

CLASSID EDMAPROC
edma_get_class_superclass (CLASSID class_id, ESint32 indx)
{
  if ((edma_check_class_id (class_id, "edma_get_class_superclass")) == -1)
    return -1;

  if ((indx < 0) || (indx > gClass[class_id]->Derived))
    {
      edma_printf_err ("[edma_get_class_superclass] Index %d out of range for"
		       "class %s [%ld]", indx, 
		       gClass[class_id]->ClassName, class_id);
      return -1;
    }
  
  return pClass[class_id]->SCList[indx];
}


/* edma_get_class_superclass_apoints
 *   Gets the associated anchor points of indx-th superclass of class class_id
 */

ESint32 EDMAPROC
edma_get_class_superclass_apoints (CLASSID class_id, ESint32 indx, 
				   EPChar ap1, EPChar ap2)
{
  if ((edma_check_class_id (class_id, "edma_get_class_superclass")) == -1)
    return -1;

  if ((indx < 0) || (indx > gClass[class_id]->Derived))
    {
      edma_printf_err ("[edma_get_class_superclass] Index %d out of range for"
		       "class %s [%ld]", indx, 
		       gClass[class_id]->ClassName, class_id);
      return -1;
    }

  if ((ap1 == NULL) || (ap2 ==NULL))
    return -1;

  strncpy (ap1, pClass[class_id]->SCIdList[indx], EDMA_GENERAL_ID_LEN);
  strncpy (ap1, pClass[class_id]->SubCIdList[indx], EDMA_GENERAL_ID_LEN);

  return 0;
}

/* edma_get_prop_indx
 * Returns the index of the property 'class_name' in class 'class_id'
 */
ESint32 EDMAPROC 
edma_get_prop_indx (CLASSID class_id, EPChar prop_name) 
{
  EUint32		i;

  if ((edma_check_class_id (class_id, "edma_get_prop_indx")) == -1)
    return -1;

  if (prop_name == NULL)
    return -1;

  /* FIXME: Should call '_edma_look4_prop_single@locators.c' --> 
   * Inlined if possible */
  for (i = 0; i < gClass[class_id]->nProp; i++)
    if (strncmp (pClass[class_id]->Prop[i].IdProp, prop_name, 
		 EDMA_PROP_NAME_LEN) == 0)
      break;

  if (i == gClass[class_id]->nProp)
    return -1;
  else
    return i;
}

/* edma_get_prop_type
 * Returns the datatype for property number 'indx' in class 'class_name'
 */
ESint32 EDMAPROC 
edma_get_prop_type (CLASSID class_id, EUint32 indx, EPChar type_name) 
{
  if ((edma_check_class_id (class_id, "edma_get_prop_type")) == -1)
    return -1;

  if (type_name == NULL)
    return -1;

  if (indx > gClass[class_id]->nProp) {
    edma_printf_err ("[edma_get_prop_type] Property %d doesn't exist "
		     "for class %s[%ld]",
		     indx, gClass[class_id]->ClassName, class_id);
    return -1;
  }

  strncpy (type_name, tipo[pClass[class_id]->Prop[indx].Tipo].Id, 
	   EDMA_TYPE_NAME_LEN);
  return 0;
}

/* edma_get_met_num
 * Returns the number of method defined by class 'class_id'
 */
ESint32 EDMAPROC 
edma_get_met_num (CLASSID class_id) 
{
  if ((edma_check_class_id (class_id, "edma_get_met_num")) == -1)
    return -1;

  return gClass[class_id]->nMet;
}

/* edma_get_met_name
 * Returns the method number 'Ind' name
 */
ESint32 EDMAPROC 
edma_get_met_name (CLASSID class_id, EUint32 indx, EPChar met_name) 
{
  if ((edma_check_class_id (class_id, "edma_get_met_name")) == -1)
    return -1;

  if (met_name == NULL)
    return -1;

  if (indx > gClass[class_id]->nMet) 
    {
      edma_printf_err ("[edma_get_met_name] Method %d doesn't exist "
		       "for class %s[%ld]",
		       indx, gClass[class_id]->ClassName, class_id);
      return -1;
    }

  strncpy (met_name, pClass[class_id]->Met[indx].IdMet, EDMA_MET_NAME_LEN);

  return 0;
}

/* edma_get_met_sig
 * Returns the signature of method number 'indx' in class 'class_id'
 */
ESint32 EDMAPROC 
edma_get_met_sig (CLASSID class_id, EUint32 indx, EPChar met_sig) 
{
  if ((edma_check_class_id (class_id, "edma_get_met_sig")) == -1)
    return -1;

  if (met_sig == NULL)
    return -1;

  if (indx > gClass[class_id]->nMet) 
    {
      edma_printf_err ("[edma_get_met_sig] Method %d doesn't exist "
		       "for class %s[%ld]",
		       indx, gClass[class_id]->ClassName, class_id);
      return -1;
    }
  
  strncpy (met_sig, pClass[class_id]->Met[indx].Sign, EDMA_MET_SIG_LEN);
  return 0;
}

/* edma_is_met_virtual
 * Returns 1 if the method number 'indx' in class 'class_id' is virtual
 *         0 if don't
 *        -1 on error
*/
ESint32 EDMAPROC 
edma_is_met_virtual (CLASSID class_id, EUint32 indx) 
{
  if ((edma_check_class_id (class_id, "edma_is_met_virtual")) == -1)
    return -1;

  if (indx > gClass[class_id]->nMet) 
    {
      edma_printf_err ("[edma_is_met_virtual] Method %d doesn't exist "
		       "for class %s[%ld]",
		       indx, gClass[class_id]->ClassName, class_id);
      return -1;
    }
  
  if (pClass[class_id]->Met[indx].Virtual)
    return 1;
  else
    return 0;
}

/* edma_is_met_abstract
 * Returns 1 id method number 'indx' in class 'indx' is abstract
 *         0 if don't
 *        -1 on error
 */
ESint32 EDMAPROC 
edma_is_met_abstract (CLASSID class_id, EUint32 indx) 
{
  if ((edma_check_class_id (class_id, "edma_is_met_abstract")) == -1)
    return -1;
  
  if (indx > gClass[class_id]->nMet) 
    {
      edma_printf_err ("[edma_is_met_abstract] Method %d doesn't exist "
		       "for class %s[%ld]",
		       indx, gClass[class_id]->ClassName, class_id);
      return -1;
    }
  if (pClass[class_id]->Met[indx].Abstract)
    return 1;
  else
    return 0;
}

/* edma_is_met_static
 * Returns 1 if the method number 'indx' in class 'class_id' is static
 *         0 if don't
 *        -1 on error
 */
ESint32 EDMAPROC 
edma_is_met_static (CLASSID class_id, EUint32 indx) 
{
  if ((edma_check_class_id (class_id, "edma_is_met_static")) == -1)
    return -1;

  if (indx > gClass[class_id]->nMet) 
    {
      edma_printf_err ("[edma_is_met_static] Method %d doesn't exist "
		       "for class %s[%ld]",
		       indx, gClass[class_id]->ClassName, class_id);
      return -1;
    }
  
  if (pClass[class_id]->Met[indx].Static)
    return 1;
  else
    return 0;
}

/* edma_get_met_indx
 * Returns method number for method 'Name' in class 'class_id'
 */
ESint32 EDMAPROC 
edma_get_met_indx (CLASSID class_id, EPChar met_name) 
{
  EUint32		i;

  if ((edma_check_class_id (class_id, "edma_get_method_indx")) == -1)
    return -1;
  
  if (met_name == NULL)
    return -1;
  
  /* FIXME: Should call '_edma_look4_met_single@locators.c' --> 
   * Inlined if possible */
  for (i = 0; i < gClass[class_id]->nMet; i++)
    if (strncmp (pClass[class_id]->Met[i].IdMet, met_name, 
		 EDMA_MET_NAME_LEN) == 0)
      break;
  
  if (i == gClass[class_id]->nMet)
    return -1;
  else
    return i;
}


/* edma_get_met_func
 *   Returns the pointer to the function which implementes met_name 
 * in class class_id
 */

PPROC *EDMAPROC
edma_get_met_func (CLASSID class_id, EPChar met_name)
{
  EUint32		i;

   if ((edma_check_class_id (class_id, "edma_get_method_indx")) == -1)
    return 0;

   if (met_name == NULL)
     return 0;
 
  /* FIXME: Should call '_edma_look4_met_single@locators.c' --> 
     Inlined if possible */
  for (i = 0; i < gClass[class_id]->nMet; i++)
    if (strncmp (pClass[class_id]->Met[i].IdMet, met_name, 
		 EDMA_MET_NAME_LEN) == 0)
      break;

  if (i == gClass[class_id]->nMet)
    return 0;
  else
    return pClass[class_id]->met_func[i].Func;
}


/* edma_set_met_func
 *   Set method function, for local classes 
 */

ESint32 EDMAPROC
edma_set_met_func (CLASSID class_id, EPChar met_name, PPROC *func)
{
  EUint32		i;
  
  if ((edma_check_class_id (class_id, "edma_get_method_indx")) == -1)
    return 0;
  
  if (met_name == NULL)
    return -1;

  if (gClass[class_id]->repo_type != EDMA_LOCAL_REPO)
    return -1;
 
  /* FIXME: Should call '_edma_look4_met_single@locators.c' --> 
   * Inlined if possible */
  for (i = 0; i < gClass[class_id]->nMet; i++)
    if (strncmp (pClass[class_id]->Met[i].IdMet, met_name, 
		 EDMA_MET_NAME_LEN) == 0)
      break;
  
  if (i == gClass[class_id]->nMet)
    return -1;

  pClass[class_id]->met_func[i].Func = func;
  return 0;
}




/* edma_get_all_met_func
 *  Returns all the pointer to functions which implements every method 
 * for object IdObj
 */

ESint32 EDMAPROC
edma_get_all_met_func (OBJID IdObj, PPROC **list)
{
  CLASSID   class_id;
  ESint32   i, n;

  if ((edma_check_obj_id (IdObj, "edma_get_all_met_func")) == -1)
    return -1;
    
  class_id = gObj[IdObj]->IdClass;
  n = gClass[class_id]->nMet;

  /* Allocate function array */
  if ((*list = (PPROC*) edma_palloc (sizeof(PPROC*) * n)) == NULL)
    {
      edma_printf_err ("[edma_get_all_met_func] Can't alloc space for "
		       "method table from object %ld", IdObj);
      return -1;
    }

  for (i = 0; i < gClass[class_id]->nMet; i++)
    {
      *(*list + i) = (PPROC)pClass[class_id]->met_func[i].Func; 
    }
  
  return n;
}

/*********************************************************
 * New Query Class Functions
 * 10/12/1997
 *********************************************************/

/* edma_get_prop_type_id
 *   Returns data type of indx-th property for class class_id
 */

ESint32 EDMAPROC 
edma_get_prop_type_id (CLASSID class_id, EUint32 indx) 
{
  if ((edma_check_class_id (class_id, "edma_get_prop_type_id")) == -1)
    return -1;
  
  if (indx > gClass[class_id]->nProp) {
    edma_printf_err ("[edma_get_prop_type_id] Property %d doesn't exist "
		     "for class %s[%ld]",
		     indx, gClass[class_id]->ClassName, class_id);
    return -1;
  }
  return pClass[class_id]->Prop[indx].Tipo;
}


/* edma_get_prop_type_sig
 *   Returns data type signature for indx-th property of class class_id
 */

ESint32 EDMAPROC 
edma_get_prop_type_sig (CLASSID class_id, EUint32 indx, EPChar type_sig) 
{
  if ((edma_check_class_id (class_id, "edma_get_prop_type_sig")) == -1)
    return -1;

  if (type_sig == NULL)
    return -1;

  if (indx > gClass[class_id]->nProp) 
    {
      edma_printf_err ("[edma_get_prop_type_sig] Property %d doesn't exist "
		       "for class %s[%ld]",
		       indx, gClass[class_id]->ClassName, class_id);
      return -1;
    }

  strncpy (type_sig, tipo[pClass[class_id]->Prop[indx].Tipo].Sig, 
	   EDMA_TYPE_SIG_LEN);

  return 0;
}

/* edma_get_prop_num_elements
 *   Returns the number of elements for the indx-th property of class_id
 *   Will only be not zero for array properties
 */

ESint32 EDMAPROC 
edma_get_prop_num_elements (CLASSID class_id, EUint32 indx) 
{
  if ((edma_check_class_id (class_id, "edma_get_prop_num_elements")) == -1)
    return -1;
  
  if (indx > gClass[class_id]->nProp) 
    {
      edma_printf_err ("[edma_get_prop_num_elements] Property %d doesn't exist"
		       " for class %s[%ld]",
		       indx, gClass[class_id]->ClassName, class_id);
      return -1;
    }

  return pClass[class_id]->Prop[indx].nElem;
}


/* FIXME: Copy&paste artifact???
 */

ESint32 EDMAPROC 
edma_get_prop_strlen (CLASSID class_id, EUint32 indx) 
{
  if ((edma_check_class_id (class_id, "edma_get_prop_num_elements")) == -1)
    return -1;
  
  if (indx > gClass[class_id]->nProp) 
    {
      edma_printf_err ("[edma_get_prop_num_elements] Property %d doesn't exist"
		       " for class %s[%ld]",
		       indx, gClass[class_id]->ClassName, class_id);
      return -1;
    }
  
  return pClass[class_id]->Prop[indx].nElem;
}

/* edma_get_type_size
 *    Returns the requires storage size for a given type
 */

EUint32 EDMAPROC 
edma_get_type_size (EUint32 type_id) 
{

  if ((type_id < 0) || (type_id > MAX_TIPOS)) 
    {
      edma_printf_err ("[edma_Get_type_size] Type identifier %d "
		       "isn't valid", type_id);
      return -1;
    }
  
  return tipo[type_id].tam;
}

/* edma_get_type_sig
 *   Returns system type signature for the indicated type
 */

EUint32 EDMAPROC 
edma_get_type_sig (EUint32 type_id, EPChar type_name) 
{
  if ((type_id < 0) || (type_id > MAX_TIPOS)) 
    {
      edma_printf_err ("[edma_get_type_sig] Type identifier %d isn't valid", 
		       type_id);
      return -1;
    }

  if (type_name == NULL)
    return -1;

  strncpy (type_name, tipo[type_id].Sig, EDMA_TYPE_SIG_LEN);
  return 0;
}

/* edma_get_type_id
 *   Returns system type id for the given string
 */

EUint32 EDMAPROC 
edma_get_type_id (EPChar pType) 
{
  int    i;

  for (i = 0; i < MAX_TIPOS; i++)
    if (strncasecmp (tipo[i].Id, pType, EDMA_TYPE_SIG_LEN) == 0) break;
  
  if (i == MAX_TIPOS) 
    return -1;
  
  return i;
}
