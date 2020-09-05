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
 * (c) David Martínez Oliveira 
 * Versión Beta 0.3r1
 * 15 de Junio de 1997
 *
 * Gestión de Clases
 *
 * REVISIONES:-------------------------------------------------------------
 * 15 de Junio de 1997
 * -----------------------
 * 4 de Agosto de 1997
 * Añadimos soporte para signaturas.
 * El nombre de la función nativa en la DLL será la concatenación
 * del identificador de la función y de la signatura.
 * -----------------------------------------
 * 8 de AGosto de 1997
 * Modificación para mapeo de código por proceso
 * Lectura de los punteros a las funciones e instancia de DLL en pClase
 * -----------------------------
 * 6 de Octubre de 1997
 * Añadimos función para registrar clases de STOCK
 * ---------------------------------------------------------
 * 3 de Enero de 1999
 * Añadimos funciones para carga y descarga de interfaces de clases
 * Por ahora no vamos a considerar el caso de la herencia estática. 
 * SUpuestamente este hecho se gestionará de forma autinática desde las llamadas a FreeObj
 * -------------------------------------------------------------------
 * 10 de Mayo de 1999
 * Modificación para control de estado de clases por proceso.
 * ------------------------------------------------------------------
 * 19 de Febrero de 2000
 * Se elimina la carga de interface durante el registro de clases. Ahora la
 * carga se realiza bajo demanda
 * -----------------------------------------------
 * January, 31th, 2001
 * Code cleanup and comment translation
 * -------------------------------------------------------
 * July, 4th, 2001
 * First modifications to allow interface definitions. We're trying
 * to allow class loading for classes which have pure virtual methods,
 * that is, class with no definition for some methods
 *
 * Fixed problem loading class implementation for pure abstract class
 * which haven't implementation associated.
 *
 * This code still has not been tested... I must to sleep :P
 *
 * FIXME: AddStockClass. We need to update the CLASSINFO struct used
 * for this functions.
 * ------------------------------------------------------------------
 * October, 19th,2001
 * Removing warnnings
 * Changes AddStock class. Now don't prepend system path. This was
 * causing ines_class_register to introduce wrong information in the
 * registry
 * -----------------------------------------------------------------------
 * March, 1st, 2002
 * Code cleanup 
 * ----------------------------------------------------------------------
 * July, 13th, 2002
 * Support for multiversiones classes. Deletion still not implemented
 * -------------------------------------------------------------------------
 * May, 10th, 2003
 * Modification to support changes to global class strctures
 * --------------------------------------------------------------------------
 * August, 9th, 2003
 * Hotswap. edma_add_stock_class, updated to store updating information
 * -------------------------------------------------------------------------
 * August, 22th, 2003, September, 8th, 2003
 * Added Function for allocate and free private class memory
 * Cleanup testing code
 * ---------------------------------------------------------------------------
 * January, 6th, 2004
 * Modifications related to GNU/EDMA Subsystem management API unification 
 * -----------------------------------------------------------------------------
 * Febraury, 7th, 2004
 * Added function to modify per-process actual_version field
 */

#include <stdio.h>
#include <stdlib.h>
#include <unistd.h>
#include "portable.h"
#include "const.h"
#include "vglobal.h"
#include "shmem.h"
#include "dynlink.h"

#include "subsystems.h"
#include "idf.h"
#include "multiidf.h"
#include "siu.h"
#include "misc.h"

#include "classq.h"
#include "clas.h"
#include "obj.h"
#include "pri3.h"
#include "spri3.h"

#include "helper.h"
#include "hotswap.h"
#include "repo.h"

#include "error.h"

extern EUint32 EDMAEnd();

/* Local Prototypes */
ESint32 EDMAPROC void_func (OBJID id);

ESint32 EDMAPROC 
edma_load_class_imp (CLASSID IdC) 
{
  EUint32	i,n;
  HMEM	        h;
  //EChar		fname[128];
  EChar		fname[1024];
  PPROC		func;
  EByte		flag,HasOnLoad;
  
  /* Checks if the class is already loaded for this process 
     This is private information for each process */
  if (ProcMapTable[IdC] == CLASS_LOADED) 
    {
      edma_printf_dbg (8, -1, "[edma_load_class_imp] Implementation for Class %d "
		       "already in memory", IdC);
      return 0;
    }
  /* Initialize shared library handler for PureAbstract classes*/
  h = (HMEM)NULL;    
  /* If the class isn't a Pure Abstract class nor an Interface*/
  if (gClass[IdC]->PureAbstract == 0)  
    { 
      /* First of all, we load the code module */
#ifdef LINUX
      /* We add version information lo load shared library*/
      if (edma_repo_manager_get_repo_type (gClass[IdC]->repo_id) == EDMA_SHARED_REPO)
	{
	  snprintf (fname, 1024, "%s/lib/edma/%s/%s.so.%d.%d.0", 
		    edma_repo_manager_get_repo_dir (gClass[IdC]->repo_id),
		    gClass[IdC]->NameSpace, 
		    gClass[IdC]->SysClass.ModuleName,
		    gClass[IdC]->MajorVer,
		    gClass[IdC]->MinorVer);
	}
      else
	{
	  char *rn = edma_repo_manager_get_repo_name (gClass[IdC]->repo_id);

	  snprintf (fname, 1024, "%s/%s/lib/%s/%s.so.%d.%d.0", 
		    edma_repo_manager_get_repo_dir (gClass[IdC]->repo_id),
		    rn,
		    gClass[IdC]->NameSpace, 
		    gClass[IdC]->SysClass.ModuleName,
		    gClass[IdC]->MajorVer,
		    gClass[IdC]->MinorVer);
	  free (rn);

	}
	  
#endif

#ifdef DARWIN
      /* We add version information lo load shared library*/
      if (edma_repo_manager_get_repo_type (gClass[IdC]->repo_id) == EDMA_SHARED_REPO)
	{
	  snprintf (fname, 1024, "%s/lib/edma/%s/%s.%d.%d.0.dylib", 
		    edma_repo_manager_get_repo_dir (gClass[IdC]->repo_id),
		    gClass[IdC]->NameSpace, 
		    gClass[IdC]->SysClass.ModuleName,
		    gClass[IdC]->MajorVer,
		    gClass[IdC]->MinorVer);
	}
      else
	{
	  char *rn = edma_repo_manager_get_repo_name (gClass[IdC]->repo_id);

	  snprintf (fname, 1024, "%s/%s/lib/%s/%s.%d.%d.0.dylib", 
		    edma_repo_manager_get_repo_dir (gClass[IdC]->repo_id),
		    rn,
		    gClass[IdC]->NameSpace, 
		    gClass[IdC]->SysClass.ModuleName,
		    gClass[IdC]->MajorVer,
		    gClass[IdC]->MinorVer);
	  free (rn);
	}
#endif



      edma_printf_dbg (8, -1, "[edma_load_class_imp] Trying to load %s module", 
		       fname);
      h = LoadLib (fname);  
      if (h == (HMEM)NULL) 
	{
	  edma_printf_err ("[edma_load_class_imp] Can't load module %s "
			   "for class %s",
			   gClass[IdC]->SysClass.ModuleName, 
			   gClass[IdC]->ClassName);
	  return -1;
	}
    }
  
  /* Checks if the intertace is already loaded and mapped for this process */
  if (ProcMapTable[IdC] < CLASS_IMAPPED) 
    {
      edma_printf_dbg (8, -1, "[edma_load_class_imp] Interface for class %d " 
		       "isn't loaded (%d)....", IdC, gClass[IdC]->Status);
      /* if it isn't, we hace to load it */
      if ((edma_load_class_int (IdC)) == -1)
	return -1;
    }
  
  pClass[IdC]->hLib = h;
  
  /* We read the function pointers for class methods */
  n = gClass[IdC]->nMet;
  flag = 0;
  HasOnLoad = 0;
  for (i = 0; i < n; i++) 
    {
      memset (fname, 0, 80);
#ifdef WINAPI_32	   
      strcpy (fname, "_");
#endif	   
      /* Test for OnLoad static method while mapping class*/
      if (!HasOnLoad)
	if (strncmp (pClass[IdC]->Met[i].IdMet, "OnLoad", EDMA_MET_NAME_LEN) == 0)
	  HasOnLoad = 1;
      
      strncat (fname, gClass[IdC]->ClassName, 128);
      strncat (fname, pClass[IdC]->Met[i].IdMet, 128);
      /* We add the method's signature */
      strncat (fname, pClass[IdC]->Met[i].Sign, 128);
      if (pClass[IdC]->Met[i].Abstract) 
	func = (PPROC)void_func;
      else
	func = GetAddress (h, fname);   
      
      if (func == NULL) 
	{
	  /* If we can locate implementation, we assign void_func*/
	  flag = 1;
	  edma_printf_err ("[edma_load_class_imp] Can´t locate method %s "
			   "for class %s "
			   "in module %s, and method isn't virtual",
			   fname, gClass[IdC]->ClassName, 
			   gClass[IdC]->SysClass.ModuleName);
	}
      pClass[IdC]->met_func[i].Func = (PPROC *) func;
    }
  
  /* The loaded status only makes sense for each process, 
     so we only update ProcMapTable */
  ProcMapTable[IdC] = CLASS_LOADED;
  edma_printf_dbg (8, -1, "[edma_load_class_imp] (%s)Class Implementation  "
		   "succesfully mapped",
		   gClass[IdC]->ClassName);
  
  /* Now, we look for a CLASEOnload function.*/
  if (HasOnLoad) 
    edma_smet3 (gClass[IdC]->ClassName, "OnLoad");
  
  return flag; 
}

ESint32 EDMAPROC 
edma_free_class_imp (CLASSID IdC) 
{
  ESint32       i,n;
  
  if (ProcMapTable[IdC] == CLASS_LOADED) 
    {
      /* Look for available OnUnload Method*/
      n = gClass[IdC]->nMet;
      for (i = 0; i < n; i++)
	if (strncmp (pClass[IdC]->Met[i].IdMet, "OnUnload", 8) == 0)  
	  break;
      if (i != n)
	edma_smet3 (gClass[IdC]->ClassName, "OnUnload");
      
      /* We only must to unload if we've something to unload*/
      if (gClass[IdC]->PureAbstract == 0) 
	UnloadLib (pClass[IdC]->hLib);   
      /* Actually we only change the interface's status */
      ProcMapTable[IdC] = CLASS_IMAPPED;
      /* The Interface is still defined and mapped for the process after
	 the class code is unloaded from memory */
    }
  
  return 0;
}

ESint32 EDMAPROC 
edma_add_stock_class (CLASS_INFO ci,EPChar IDFName,EPChar IMPName) 
{
  CLASSID		cId;
  CLASSID               old_version;
  EDMA_UPDATE           update;
  
  /* Checks if class id is avaliable */
  /* Thread safe. Lock class table during upate */
  cId = edma_get_class_id (ci.ClassName);
  if (cId != -1) 
    {
      edma_printf ("[edma_add_stock_class] Current version: %ld.%ld. To install %ld.%ld",
		   gClass[cId]->MajorVer, gClass[cId]->MinorVer,
		   ci.MajorVer, ci.MinorVer);
      /* The class already exists, so we must check version information */
      if ((ci.MajorVer > gClass[cId]->MajorVer) || 
	  ((ci.MajorVer == gClass[cId]->MajorVer) && (ci.MinorVer > gClass[cId]->MinorVer)))
	old_version = cId;
      else
	{
	  /* // Thread-safe code is still incomplete
	  pthread_mutex_unlock (&GVar->class_mutex);
	  edma_printf ("[DEBUG][%s] Class mutex section left", __FUNCTION__);
	  */
	  edma_printf_err ("[edma_add_stock_class] This ClassName already exists");
	  return -1;
	}
    }
  edma_printf ("[edma_add_stock_class] About to install class");
  /* FIXME: edma_idf_get_class_id must run inside a MUTEX*/
  cId = edma_idf_get_free_class_id (EDMA_SHARED_CLASS);	/* Gets a class ID */
  if (cId == -1) 
    {
      /* // Thread-safe code is still incomplete
      pthread_mutex_unlock (&GVar->class_mutex);
      edma_printf ("[DEBUG][%s] Class mutex section left", __FUNCTION__);
      */
      edma_printf_err("[edma_add_stock_class] Can´t get class identifier");
      return -1;
    }
  
  gClass[cId] = &SharedClass[cId];
  /* Inits general data for the class */
  edma_idf_set_class_name (cId, ci.ClassName);
  edma_idf_set_class_namespace (cId, ci.NameSpace);
  edma_idf_set_class_impl (cId, IMPName);
  edma_idf_set_class_arch (cId, edma_get_arch_id (ci.MaqName), edma_get_so_id (ci.SOName));

  /* Sets default parser (EDMAIDF) for this class */
  gClass[cId]->IDFParser = -1;		
  if (strncmp (ci.IDFName, "EDMAIDF", 7) != 0) 
    gClass[cId]->IDFParser = edma_ingridf_get_parser (ci.IDFName);

  /* By default, the class is native... so no SIU proxy is defined*/
  gClass[cId]->SIUProxy = -1;
  if (ci.SIUName != 0)
    if (strlen (ci.SIUName) != 0) 
      gClass[cId]->SIUProxy = edma_subsystem_get_item (SS_SIU, ci.SIUName);

  edma_idf_set_class_attribs (cId, ci.IsSIUProxy, ci.IsIDFParser, ci.IsEMIComp);

  edma_idf_set_class_version (cId, ci.MajorVer, ci.MinorVer);

  gClass[cId]->repo_type = EDMA_SHARED_REPO;
 
 /* At this point the class is totally defined */
  edma_printf_dbg (8, -1, "[edma_add_stock_class] Class %s added...", ci.ClassName);
  edma_printf_dbg (8, -1, "[edma_add_stock_class] Ready for getting interface");
  
  /* Mark the class for adding to the EDMA registry on shutdown*/
  gClass[cId]->IsStock = 1;

  edma_idf_set_class_id (cId);

  gClass[cId]->Status = CLASS_DEF;
  ProcMapTable[cId] = CLASS_DEF;
  edma_printf_dbg (8, -1, "[edma_add_stock_class] Class added with id %d", cId);


  /* Now we must update current field for old versions of this class*/
  _edma_class_update_versions (cId);

  /* First hotswap approach. We introduce an update if an update script is provided */
  if (ci.UpdateScript != NULL)
    {
      edma_printf ("[edma_add_stock_class] Class %d provides update script."
		   "Adding update to UPDATE QUEUE", cId);
      update.IdClass = cId;
      update.timestamp = time (NULL);
      update.update_script[0] = 0; /* Empty string by default */
      strncpy (update.update_script, ci.UpdateScript, EDMA_PATH_LEN);
      
      edma_add_update (update);
    }

  /* // Thread-safe code is still incomplete
  pthread_mutex_unlock (&GVar->class_mutex);
  edma_printf ("[DEBUG][%s] Class mutex section left", __FUNCTION__);
  */

  return cId;
}

/*************************************************************************/


ESint32 EDMAPROC 
edma_add_stock_class2 (EDMA_REPO repo, CLASS_INFO ci,
		       EPChar IDFName,EPChar IMPName) 
{
  CLASSID		cId, last_version_id;
  CLASSID               old_version;
  EDMA_UPDATE           update;
  
  /* Checks if class id is avaliable */
  /* Thread safe. Lock class table during upate */
  
  cId = edma_get_class_id_with_version (ci.ClassName, ci.MajorVer, ci.MinorVer);
  last_version_id = edma_get_class_id (ci.ClassName);

  if (cId != -1) 
    {
      cId = pClass[cId]->actual_version;
      edma_printf ("[edma_add_stock_class] Current version: (class %d) "
		   "%ld.%ld. To install %ld.%ld",
		   cId, gClass[cId]->MajorVer, gClass[cId]->MinorVer,
		   ci.MajorVer, ci.MinorVer);
      /* The class already exists, so we must check version information */
      if ((ci.MajorVer > gClass[cId]->MajorVer) 
	  || ((ci.MajorVer == gClass[cId]->MajorVer) 
	      && (ci.MinorVer > gClass[cId]->MinorVer)))
	old_version = cId;
      else
	{
	  /* // Thread-safe code is still incomplete
	  pthread_mutex_unlock (&GVar->class_mutex);
	  edma_printf ("[DEBUG][%s] Class mutex section left", __FUNCTION__);
	  */
	  edma_printf_err ("[edma_add_stock_class] This ClassName and version "
			   "already exists");
	  return -1;
	}
    }
  edma_log ("[edma_add_stock_class2] About to install class");

  /* FIXME: edma_idf_get_class_id must run inside a MUTEX*/
  if (edma_repo_get_type (repo) == EDMA_LOCAL_REPO)
    cId = edma_idf_get_free_class_id (EDMA_LOCAL_CLASS);
  else
    cId = edma_idf_get_free_class_id (EDMA_SHARED_CLASS); 

  edma_log ("[%s] Class id: %d", __FUNCTION__, cId);

  if (cId == -1) 
    {
      /* // Thread-safe code is still incomplete
      pthread_mutex_unlock (&GVar->class_mutex);
      edma_printf ("[DEBUG][%s] Class mutex section left", __FUNCTION__);
      */
      edma_printf_err ("[edma_add_stock_class] Can´t get class identifier");
      return -1;
    }
  
  if (edma_repo_get_type (repo) != EDMA_LOCAL_REPO)
    gClass[cId] = &SharedClass[cId]; 

  /* Inits general data for the class */
  edma_log ("[%s] Inits general data for class: %d", __FUNCTION__, cId);
  edma_log ("[%s] Class Name: '%s'", __FUNCTION__, ci.ClassName);
  edma_idf_set_class_name (cId, ci.ClassName);
  edma_log ("[%s] -----------------------------------------------", 
	    __FUNCTION__);
  edma_log ("[%s] Class NameSpace: %s", __FUNCTION__, ci.NameSpace);
  edma_idf_set_class_namespace (cId, ci.NameSpace);

  edma_log ("[%s] Implementation Name: %s", __FUNCTION__, IMPName);
  edma_idf_set_class_impl (cId, IMPName);

  edma_log ("[%s] Class Arch: %s:%s", __FUNCTION__, ci.MaqName, ci.SOName);
  edma_idf_set_class_arch (cId, edma_get_so_id (ci.SOName),
			   edma_get_arch_id (ci.MaqName));

  edma_log ("[%s] Setting IDF Parser: %d", __FUNCTION__, cId);
  /* Sets default parser (EDMAIDF) for this class */
  gClass[cId]->IDFParser = -1;		
  if (strncmp (ci.IDFName, "EDMAIDF", 7) != 0) 
    gClass[cId]->IDFParser = edma_ingridf_get_parser (ci.IDFName);

  edma_log ("[%s] Setting SIU Proxy: %d", __FUNCTION__, cId);

  /* By default, the class is native... so no SIU proxy is defined*/
  gClass[cId]->SIUProxy = -1;
  if (ci.SIUName != 0)
    if (strlen (ci.SIUName) != 0) 
      gClass[cId]->SIUProxy = edma_subsystem_get_item (SS_SIU, ci.SIUName);
  
  edma_log ("[%s] Set Class Attribs: %d", __FUNCTION__, cId);
  edma_idf_set_class_attribs (cId, ci.IsSIUProxy, ci.IsIDFParser, ci.IsEMIComp);
  
  edma_log ("[%s] Setting Versionr: %d.%d", __FUNCTION__, 
	    ci.MajorVer, ci.MinorVer);
  edma_idf_set_class_version (cId, ci.MajorVer, ci.MinorVer);
  
  
  gClass[cId]->repo_type = edma_repo_get_type (repo);
  gClass[cId]->repo_id = edma_repo_get_id (repo);

  edma_repo_add_class (repo);

 /* At this point the class is totally defined */
  edma_printf_dbg (8, -1, "[edma_add_stock_class] Class %s added...", 
		   ci.ClassName);
  edma_printf_dbg (8, -1, "[edma_add_stock_class] Ready for getting interface");
  
  /* Mark the class for adding to the EDMA registry on shutdown*/
  gClass[cId]->IsStock = 1;

  edma_idf_set_class_id (cId);

  gClass[cId]->Status = CLASS_DEF;
  ProcMapTable[cId] = CLASS_DEF;
  edma_printf_dbg (8, -1, "[edma_add_stock_class] Class added with id %d", cId);


  /* Now we must update current field for old versions of this class*/
  GVar->time_last_version_update = time (NULL);

  /* In case we restote and old version, we need to update version for 
     current class */

  /* First hotswap approach. We introduce an update 
     if an update script is provided */
  if (ci.UpdateScript != NULL)
    {
      edma_printf ("[edma_add_stock_class] Class %d provides update script."
		   "Adding update to UPDATE QUEUE", cId);
      update.IdClass = cId;
      update.timestamp = time (NULL);
      update.update_script[0] = 0; /* Empty string by default */
      strncpy (update.update_script, ci.UpdateScript, EDMA_PATH_LEN);
      
      edma_add_update (update);
    }

  /* // Thread-safe code is still incomplete
  pthread_mutex_unlock (&GVar->class_mutex);
  edma_printf ("[DEBUG][%s] Class mutex section left", __FUNCTION__);
  */

  /* If the new class has been successfully installed. Mark repo as persistent
   */
  edma_repo_set_persistent (repo, 1);
  return cId;
}


/****************************************************************************/


ESint32 EDMAPROC 
edma_del_stock_class (EPChar Name) 
{
  CLASSID		IdC;

  /* // THread-safe code is still incomplete
  edma_printf ("[DEBUG][%s] Waiting to enter class mutex section", __FUNCTION__);
  pthread_mutex_lock (&GVar->class_mutex);
  edma_printf ("[DEBUG][%s] On class mutex section", __FUNCTION__);
  */
  IdC = edma_get_class_id(Name);
  if (IdC == -1) 
    {
      /* // THread-safe code is still incomplete
      pthread_mutex_unlock (&GVar->class_mutex);
      edma_printf ("[DEBUG][%s] Class mutex section left", __FUNCTION__);
      */
      edma_printf_err ("[edma_del_stock_class] Class %s doesn't exist", Name);
      return -1;
    }
  return edma_del_stock_class_id (IdC);

}




ESint32 EDMAPROC 
edma_del_stock_class_id (CLASSID IdC) 
{
  /* We only can delete a class if it isn been used */
  if (gClass[IdC]->Ocurrences <= 0) 
    {
      edma_repo_del_class (edma_repo_manager_get_repo (gClass[IdC]->repo_id));
      edma_log ("Freeing class %d '%s'\n", IdC, gClass[IdC]->ClassName);
      /* We mark it like free and we free associated memory */
      gClass[IdC]->Status = CLASS_FREE;
      ProcMapTable[IdC] = CLASS_FREE;
      edma_sfree (pClass[IdC]->SysClass.hSCList, pClass[IdC]->SCList);
      edma_sfree (pClass[IdC]->SysClass.hProp, pClass[IdC]->Prop);
      edma_sfree (pClass[IdC]->SysClass.hMet, pClass[IdC]->Met);
      edma_sfree (pClass[IdC]->SysClass.hNot, pClass[IdC]->Not);	   
    }
  //GVar->nClases--;
  if (GVar->nClases < 0) 
    {
      /* // Thread-safe code is still incomplete
      pthread_mutex_unlock (&GVar->class_mutex);
      edma_printf ("[DEBUG][%s] Class mutex section left", __FUNCTION__);
      */
      edma_printf_err ("[edma_del_stock_class] **** Number of clases less than zero !!! ****");
      /* This shouldn't ocurr so we abort application execution */
      EDMAEnd();
    }
  /* // THread-safe code is still incomplete
  pthread_mutex_unlock (&GVar->class_mutex);
  edma_printf ("[DEBUG][%s] Class mutex section left", __FUNCTION__);
  */
  GVar->time_last_version_update = time (NULL);
  _edma_class_update_all_versions();

  edma_repo_set_persistent (edma_repo_manager_get_repo (gClass[IdC]->repo_id), 
			    1);


  return 0;
}


/***************************************************************
 * Class Interface load and unload Functions
 ****************************************************************/

ESint32 EDMAPROC 
edma_load_class_int (CLASSID i) 
{
  OBJID   idParser;
  ESint32 ret;
  
  if (i < 0) return -1;
  edma_printf_dbg (8, -1, "[edma_load_class_int] Loading interface for class %d", i);
  /* If the interface is already loaded we do nothing */
  if (ProcMapTable[i] >= CLASS_ILOADED) 
    {
      edma_printf_dbg (8, -1, "[edma_load_class_int] Class interface %d already in memory", i);
      return 0;
    }
  
  if ((ret = _edma_class_alloc_priv_data (i)) < 0)
    return ret;

  /* If a new class is installed dynamically it gets defined, but each process must realize
   * that the new class is there */
  if (gClass[i]->Status == CLASS_DEF)
    ProcMapTable[i] = CLASS_DEF;

  if (gClass[i]->IDFParser == -1) 
    {
      if ((_edma_read_class_interface (i, gClass[i]->ClassName)) == -1)
	{
	  return -1;
	}
    }
  else 
    {
      edma_printf_dbg (40, -1, "      [edma_load_class_int] Invoking IDF Parser %s",
		       edma_subsystem_get_item_id (SS_INGRIDF, gClass[i]->IDFParser));

      ProcMapTable[i] = CLASS_TEMP;
      idParser = edma_new_obj (edma_subsystem_get_item_id (SS_INGRIDF, gClass[i]->IDFParser));
      if (idParser == -1) 
	{
	  edma_printf_err ("[edma_load_class_int] Can't create parser object [%s] "
			   "to parse [%s] interface", 
			   edma_subsystem_get_item_id (SS_INGRIDF, gClass[i]->IDFParser), gClass[i]->ClassName);
	  return -1;
	}
      edma_printf_dbg (40, -1, "      [edma_load_class_int] Parser created: %d:'%s' for loading class %d", 
		       idParser, gClass[gObj[idParser]->IdClass]->ClassName, i);
      edma_met3 (idParser, "Parse", i);
      edma_printf_dbg (40, -1, "       [edma_load_class_int] Parssing done");
      edma_free_obj (idParser);

  }
  edma_printf_dbg (8, -1, "[edma_load_class_int] Interface succesfully loaded");
  /* The class's interface is created in shared memory so it can be
     acceded by all the process*/
  gClass[i]->Status = CLASS_ILOADED;
  /* For the current process executing the function, it is mapped too*/
  ProcMapTable[i] = CLASS_IMAPPED;


  return 0;
}

ESint32 EDMAPROC 
edma_unload_class_int (CLASSID IdC) 
{
  if ((edma_check_class_id (IdC,"edma_unload_class_int")) == -1)
    return -1;

  edma_printf_dbg (8, -1, "[edma_load_class_int] Unloading interface for class %d", IdC);
  /* FIXME: Do we really need to check if the class interface is loaded*/
  _edma_free_class_interface (IdC, gClass[IdC]->ClassName);
  ProcMapTable[IdC] = CLASS_DEF;




  return 0;
}

/* pClass Structure functions 
 * 
 * XXX: 
 * Here, we don't check index sanity. This is an internal function
 * which is only called from points where class index was previously
 * checked
 */
ESint32 EDMAPROC
_edma_class_alloc_priv_data (CLASSID i)
{
  if (pClass[i] == NULL)
    {
      /* Allocate per-process class block*/
      if ((pClass[i] = (PCLASE*) edma_palloc (sizeof(PCLASE))) == NULL)
	{
	  edma_printf_err ("[_edma_class_alloc_priv_data] Can't alloc per-process struct for class %s[%d]", 
			   gClass[i]->ClassName, i);
	  return -1;
	}
      /* XXXX: We don't call edma_pget at this point....*/
      memset (pClass[i], 0, sizeof (PCLASE));
      pClass[i]->hLib = 0;
      pClass[i]->nRef = 0;

      //pClass[i]->actual_version = gClass[i]->CurrentVer;
      pClass[i]->actual_version = i;
    }
  return 0;
}

/* XXX: All the data structures that depends on pClass must be released before calling this function */
ESint32 EDMAPROC
_edma_class_free_priv_data (CLASSID i)
{
  if (pClass[i] != NULL)
    edma_pfree ((HMEM)pClass[i], pClass[i]);

  return 0;
}

ESint32 EDMAPROC 
_edma_class_update_all_versions ()
{
  int   i;

  edma_log ("Updating class versions... (%d classes to update)", 
	       GVar->nClases);

  /* Reset version information */
  for (i = 0; i< GVar->nClases; i++)
    if (pClass[i]) pClass[i]->actual_version = i;

  for (i = MAX_CLASE; i < MAX_CLASE + nLocalClasses; i++)
    {
      if (gClass[i] == NULL)  continue;

      pClass[i]->actual_version = i; // Reset version
    }

  /* Update Shared classes */
  for (i = 0; i< GVar->nClases; i++)
    {
      _edma_class_update_versions (i);
    }
  /* Then update rest of the classes */
  edma_log ("Updating class versions... (%d local classes to update)", 
	    nLocalClasses);

  for (i = MAX_CLASE; i < MAX_CLASE + nLocalClasses; i++)
    {
      if (gClass[i] == NULL)  continue;

      _edma_class_update_versions (i);
    }

  last_checked_versions = time (NULL);
  return 0;
}

ESint32 EDMAPROC
_edma_class_update_versions (CLASSID class_id)
{
  ESint32   i, j, current_id;

#if 0
  edma_printf ("---> Updating version for class: '%s': %d (%d registered classes)", 
	       gClass[class_id]->ClassName, class_id, GVar->nClases);
#endif
  if (gClass[class_id]->Status == CLASS_FREE) return 0;
  if (!pClass[class_id]) _edma_class_alloc_priv_data (class_id);
  if (pClass[class_id]->actual_version == -1) pClass[class_id]->actual_version = class_id;
  //if (gClass[class_id]->CurrentVer == -1) gClass[class_id]->CurrentVer = class_id;
  /****** THIS MUST BE IN A MUTEX *********/
  /* First update Shared classes if any*/
  for (i = 0; i< GVar->nClases; i++)
    if (ProcMapTable[i] != CLASS_FREE)
      {

	if ((strncmp (gClass[i]->ClassName, gClass[class_id]->ClassName, 
		      EDMA_CLASS_NAME_LEN)) == 0)
	  {
	    current_id = pClass[i]->actual_version;	    
	    edma_log ("Updating class %d %s (old:%d (%d.%d) effective (%d.%d)) new:%d (%d.%d))", 
			 i, gClass[i]->ClassName,
		      current_id, 
		      gClass[i]->MajorVer, gClass[i]->MinorVer,
		      gClass[current_id]->MajorVer, gClass[current_id]->MinorVer,
		      class_id,
		      gClass[class_id]->MajorVer, gClass[class_id]->MinorVer
		      );
	    
	    /* FIXME: We should also check version numbers */

	    if ((gClass[class_id]->MajorVer > gClass[current_id]->MajorVer) || 
		((gClass[class_id]->MajorVer == gClass[current_id]->MajorVer) && 
		 (gClass[class_id]->MinorVer > gClass[current_id]->MinorVer)))
	      if (i != class_id)
	      {

		edma_log ("  **INFO** [edma_class_update_versions] %s[id:%d] "
			  "is an old version... updating", 
			  gClass[i]->ClassName, i);
		//gClass[i]->CurrentVer = class_id;
		pClass[i]->actual_version = class_id;
	      }
	  }
      }

  /* If class_id is a local class also update local class entries ??? */
  /* FIXME:
   * For now, we keep this, which is as current implementation is working 
   * We must define how version updating process will work at the end */

  //edma_printf ("[%s] Testing local classes from %d to %d", __FUNCTION__, MAX_CLASE, nMaxLocalClasses + nLocalClasses);
  /* FIXME: Update this to work with new repository management system*/
  //if (class_id >= MAX_CLASE) 
    {
      /* Then update local classes if any */
      j = 0;
      for (i = MAX_CLASE; j < nLocalClasses; i++)
      //for (i = MAX_CLASE; i < nMaxLocalClasses; i++)
	{
	  if (gClass[i] == NULL)
	    {
	      edma_log ("[%s] NULL local class found with id %d. %d (%d) local classes ", 
			__FUNCTION__, i, j, nLocalClasses);
	      j++;
	      continue;
	    }

	  if (ProcMapTable[i] != CLASS_FREE)
	    {
	      //printf ("%p:%ld - %p:%ld\n", gClass[i]->ClassName, i, gClass[class_id]->ClassName, class_id);
	      //printf ("%s - %s\n", gClass[i]->ClassName, gClass[class_id]->ClassName);
	      if ((strncmp (gClass[i]->ClassName, gClass[class_id]->ClassName, 
			    EDMA_CLASS_NAME_LEN)) == 0)
		{
		  /* FIXME: we should check version numbers*/
		  current_id = pClass[i]->actual_version;
		  edma_log ("++ Testing class '%s' v %d.%d against %d.%d\n",
			       gClass[class_id]->ClassName, 
			       gClass[class_id]->MajorVer, gClass[class_id]->MinorVer,
			       gClass[current_id]->MajorVer, gClass[current_id]->MinorVer
			       );
		  if ((gClass[class_id]->MajorVer > gClass[current_id]->MajorVer) || 
		      ((gClass[class_id]->MajorVer == gClass[current_id]->MajorVer) && 
		       (gClass[class_id]->MinorVer > gClass[current_id]->MinorVer)))
		    if (i != class_id)
		      {
			edma_log ("  **INFO** [edma_local_class_finish] %s[id:%d] "
				  "is an old version... updating to (%d)",  
				  gClass[i]->ClassName, i, class_id);
			//gClass[i]->CurrentVer = class_id;
		      pClass[i]->actual_version = class_id;
		    }
		}
	      j++;
	    }
	}
    }
  
  return 0;
}

ESint32 EDMAPROC
_edma_class_update_actual_versions (CLASSID class_id)
{
  ESint32   i, j;

  edma_printf ("---> Updating version for class: '%s'", gClass[class_id]->ClassName);
  /****** THIS MUST BE IN A MUTEX *********/
  /* First update Shared classes if any*/
  for (i = 0; i< GVar->nClases; i++)
    if (ProcMapTable[i] != CLASS_FREE)
      {
	if ((strncmp (gClass[i]->ClassName, gClass[class_id]->ClassName, 
		      EDMA_CLASS_NAME_LEN)) == 0)
	  {
	    /* FIXME: We should also check version numbers */
	    if (i != class_id)
	      {
		edma_log ("**INFO** [edma_class_update_actual_versions] %s[id:%d] "
			     "being updated to %d", gClass[i]->ClassName, i, class_id);
		pClass[i]->actual_version = class_id;
		//gClass[i]->CurrentVer = class_id;
	      }
	  }
      }

  /* If class_id is a local class also update local class entries ??? */
  /* FIXME:
   * For now, we keep this, which is as current implementation is working 
   * We must define how version updating process will work at the end */
  if (class_id >= MAX_CLASE) 
    {
      /* Then update local classes if any */
      j = 0;
      for (i = MAX_CLASE; j < nLocalClasses; i++)
	{
	  edma_log ("---> Testing local class '%s' %d.%d", 
		       gClass[i]->ClassName, gClass[i]->MajorVer, gClass[i]->MinorVer);
	  if (ProcMapTable[i] != CLASS_FREE)
	    {
	      if ((strncmp (gClass[i]->ClassName, gClass[class_id]->ClassName, 
			    EDMA_CLASS_NAME_LEN)) == 0)
		{
		  /* FIXME: we should check version numbers*/
		  if (i != class_id)
		    {
		      edma_log ("**INFO** [edma_class_update_actual_versions] %s[id:%d] "
			   "being updated to %d", gClass[i]->ClassName, i, class_id);
		      pClass[i]->actual_version = class_id;
		      //gClass[i]->CurrentVer = class_id;
		    }
		}
	      j++;
	    }
	}
    }
  
  return 0;
}

ESint32 EDMAPROC
edma_class_set_actual_version (EPChar class_name, ESint32 major, ESint32 minor)
{
  CLASSID    cid2;

  if (class_name == NULL)
    return _edma_system_exception ("[edma_class_set_actual_version] Invalid Class Name '%s'", 
				   class_name);

  if ((cid2 = edma_get_class_id_with_version (class_name, major, minor)) < 0)
    return _edma_system_exception ("[edma_class_set_actual_version] Class '%s' version %d.%d not found",
				   class_name, major, minor);

  pClass[cid2]->actual_version = cid2;
  //gClass[cid2]->CurrentVer = cid2;

  _edma_class_update_actual_versions (cid2);

  return 0;
}

/*******************************************************
 * void_func for interface definition
 *******************************************************/

ESint32 EDMAPROC 
void_func (OBJID id) 
{
  edma_printf_obj (id, "[SYSTEM_WARNNING] Trying to execute abstract method '%s' on instance %d of class '%s'",
		   pClass[gObj[id]->IdClass]->Met[gObj[id]->last_met].IdMet, id,
		   gClass[gObj[id]->IdClass]->ClassName);
  return -1;
}
