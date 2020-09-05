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

/****************************************************************************
 * Entorno de Desarrollo Modular y Abierto
 * Versión Beta 0.3r1
 * (c) David Martínez Oliveira
 * 
 * Modulo de Gestión de ficheros de interface
 * Revisiones: ---------------------------------------------------------
 * 28 de Mayo de 1997
 * ---------------------
 * 24 de Julio de 1997
 * Añadimos soporte para matrices. En el IDF se añade el campo ELEM que indica
 * el número de elementos del tipo indicado. Si es 0, el tipo es simple, sino es una
 * matriz.
 *
 * Los errores se envían a la consola con ECout. Habrá que modificarlos
 * para utilizar una función específica para mostrar errores del sistema.
 * --------------------------------
 * 4 de Agosto de 1997
 * Modificamos LeeMetodo para que lea la signatura asociada al método
 * ---------------------------------
 * 8 de Agosto de 1997
 * Modificacion para mapeo de código por proceso. Creación de la tabla pClase
 * ----------------------------------------
 * 6 de Octubre de 1997
 * Modificación para poder leer definiciones de interface de distintos
 * directorios.
 * -----------------------
 * 18 de Noviembre de 1997
 *   Modificación para el soporte de tipos definidos por el usuario.
 * Un tipo definido por el usuario es un clase y debe ser declarada con
 * anterioridad en EDMA32.CFG.
 * 
 *   Almacenamos en el campo UserData de la estructura PROP el identificador
 * de la clase a la que referencia el tipo
 * ----------------------------------------------
 * 14 de Mayo de 1999
 * Correctión de las evoluciones de estados de las clases.
 * ------------------------------------------
 * January, 5th, 2001
 * We continue cleaning up the sources
 * ------------------------------------------------------------------
 * April, 15th, 2001
 * Added support for reading static method metainformation
 * -------------------------------------------------------------------
 * July, 4th, 2001
 * LeeMetodo modified to read Abstract field in order to not try to resolve
 * function names for abstract methods in shared libraries
 *
 * Added suport to read information about Pure Abstract/Interface classes
 * This code still has not been tested
 * -------------------------------------------------------------------
 * July, 6th, 2001
 * Instead of adding a new field in the IDF file to mark pure abstract
 * classes, we simply, don't provide implementation information.
 * 
 * This processing is now done in ini1.c
 * -----------------------------------------------------------------
 * November, 10th, 2001
 * Changes default idf file to SystemPath/share/idf
 * TODO: Generalize this to work with namespaces
 * ---------------------------------------------------------------------
 * December, 3rd, 2001
 * GetInterface now gets preferer anchor points from SuperClass lists in
 * IDF's
 * ------------------------------------------------------------------
 * March, 2nd, 2002
 * Code cleanup
 * -------------------------------------------------------------------
 * July, 13th, 2002
 * Modification to manage interface for multiversioned classes
 * ----------------------------------------------------------------------
 * May, 10th, 2003
 * Modification to support changes to internal class structures
 * ----------------------------------------------------------------------
 * July, 3rd, 2003
 * Cosmetic changes + input parameters sanity checks
 * -------------------------------------------------------------------------
 * July, 6th, 2003
 * Changes to use functions in multiidf... code was duplicated in idf and multiidf
 * ------------------------------------------------------------------------------
 * January, 7th, 2004
 * Added support for interfaces
 */
 
#include <stdio.h>
#include "vglobal.h"
#include "portable.h"
#include "const.h"
#include "shmem.h"

#include "idf.h"
#include "multiidf.h"
#include "misc.h"
#include "iniman.h"

#include "clas.h"
#include "classq.h"
#include "inh.h"

#include "helper.h"

#include "repo.h"

/* _edma_read_class_interface
 *   Reads and parses the EDMAIDF interface file 'Name' storing the retrieved
 *   information in class table entry 'ClassId'
 */

/* FIXME This function is too large.... Split it*/
ESint32 EDMAPROC 
_edma_read_class_interface (EUint32 ClassId, EPChar Name) 
{
  CLASSID	CId, aux_cid;
  EChar		FilePath[256];
  static EChar	Buffer[200];
  EChar         IFaceKey[256];
  EChar         IFaceValue[256];
  ESint32       *IFaceList;
  EChar		Ver[100];
  EDWord	Result;
  FILE		*f;
  EUint32	nProp, nMet, nNot, i, j, ii, indx;
  EUint32	nTotalProp, nTotalMet, nTotalNot;
  ESint32       nIFaces;
  ESint32       n_vmets;
  HMEM		h, h1, h2;
  EUint32	DataSize;
  EPChar	*clist;
  EPChar        *idlist, *idlist1, aux, aux1;
  PINIFILE      pi;   

  if (Name == 0)
    {
      edma_printf_err ("%s", "[_edma_read_class_interface] Interface name "
		       "invalid");
      return -1;
    }

  /* Get Class identifier*/
  if (ClassId == -1)
    CId = edma_get_class_id (Name);
  else
    CId = ClassId;

  if ((edma_check_class_id (ClassId, "_edma_read_class_interface")) == -1)
    return -1;

  /* If interface is already loadeded or implementation is mapped in memory */
  if (ProcMapTable[CId] >= CLASS_ILOADED) 
    {
      edma_printf ("[_edma_read_class_interface] Interface for Class %d is "
		   "already loaded", CId);
      /* We check out implementation mapping for this process*/
      if (ProcMapTable[CId] >= CLASS_IMAPPED)
	return 0;   /* if it's mapped there's nothing to do */
    }

  /* If not we are defining an interface class so mark the class*/
  ProcMapTable[CId] = CLASS_TEMP;

  strcpy (Ver, "");
  if (strchr (Name, ':') == 0) 
    {
      strncpy (FilePath, 
	       edma_repo_manager_get_repo_dir (gClass[CId]->repo_id), 256);
#ifdef WINAPI_32	   
      /* FIXME: Probably this location'll change when we rebuild the
	 system for windows OSes. We must try to use it with cygwin and maybe
	 we can use the same directory tree on windows system */
      strcat (FilePath, "idf\\");
#endif
#ifdef LINUX
  if (edma_repo_manager_get_repo_type (gClass[CId]->repo_id) == 
      EDMA_SHARED_REPO)
    {
      strncat (FilePath, IDFDIR, 256);
    }
  else
    {
      strncat (FilePath, 
	       edma_repo_manager_get_repo_name (gClass[CId]->repo_id), 256);
      strncat (FilePath, "/idf/", 256);
    }
#endif	   
      /* FIXME: change to snprintf */
      strncat (FilePath, gClass[CId]->NameSpace, 256);
      strncat (FilePath, "/", 256);
      strncat (FilePath, Name, 256);
      if ((gClass[CId]->MajorVer == 0) && (gClass[CId]->MinorVer == 0))
	strncat (FilePath, ".idf", 256);
      else
	{
	  sprintf (Ver, "_%d_%d", 
		   gClass[CId]->MajorVer,
		   gClass[CId]->MinorVer);
	  strncat (FilePath, Ver, 256);
	  strncat (FilePath, ".idf", 256);
	}
    } 
  else
    strncpy (FilePath, Name, 256);
  
  /* We open the file */
  /* FIXME: We can change the code bellow for an stat... 
   * which won't work on WIN32*/
  if ((f = fopen (FilePath, "rt")) == NULL)
    {
      edma_printf_err ("[_edma_read_class_interface] Interface definition "
		       "file %s not found"
		       , FilePath);
      return -1;
    }
  fclose (f);
  
  /* Get General Information. FIXME: Check for errors */
  pi = edma_open_ini(FilePath);
  
  /* Get Definitions */
  nProp = edma_get_ini_int (pi, "Definition", "PropertiesNum", 0);
  nMet  = edma_get_ini_int (pi, "Definition", "MethodsNum", 0);
  nNot  = edma_get_ini_int (pi, "Definition", "NotificationsNum", 0);
  nIFaces  = edma_get_ini_int (pi, "Definition", "InterfacesNum", 0);

  nTotalProp = nProp;
  nTotalMet = nMet;
  nTotalNot = nNot;

  /**********************************************************************/
  /* Process IFaces */
  if (nIFaces)
    {
      if ((h = edma_palloc (sizeof(ESint32) * nIFaces)) == (HMEM)0)
	{
	  edma_printf_err ("[_edma_read_class_interface] Can't allocate memory"
			   " for interface processing on Class %d", CId);
	  /* Let it go on...*/
	}
      else
	{
	  IFaceList = (ESint32*) edma_pget (h);

	  /* Build interface table */
	  for (i = 0; i < nIFaces; i++)
	    {
	      /* Build interface key on INI file */
	      snprintf (IFaceKey, 256, "Interface%ld", i);
	      edma_get_ini_string (pi, "Interface", IFaceKey, NULL, 
				   IFaceValue, 256);
	      IFaceList[i] = edma_get_class_id (IFaceValue);
	      if (IFaceList[i] == -1)
		{
		  edma_printf_err ("[_edma_read_class_interface] Interface "
				   "%d:'%s' does not exists", i, IFaceValue);
		  continue;
		}

	      if (ProcMapTable[IFaceList[i]] < CLASS_ILOADED) 
		if ((edma_load_class_int (IFaceList[i])) == -1)
		  {
		    IFaceList[i] = -1;
		    continue;
		  }

	      nTotalProp += gClass[IFaceList[i]]->nProp;
	      nTotalMet += gClass[IFaceList[i]]->nMet;
	    }
	}
    }

  /************************************************************************/
  /* We build properties tables */
  if ((edma_idf_set_def (CId, nTotalProp, nTotalMet, nNot)) == -1)
    {
      edma_printf_err ("[_edma_read_class_interface] Build Property class "
		       "failed for class %d", CId);
      edma_printf_err ("[_edma_read_class_interface] Oops!... "
		       "this shouldn't happen");
    }

  /* Set Data properties information */
  DataSize = 0;
  for (i = 0; i < nProp; i++) 
    {
      pClass[CId]->Prop[i].Off = DataSize;
      DataSize += _edma_read_edmaidf_prop (pi, CId, i);      
    }

  /************************************************************************/
  /* Add properties from IFaces */
  indx = nProp;
  for (i = 0; i < nIFaces; i++)
    if (IFaceList[i] != -1)
      {
	aux_cid = IFaceList[i];
	for (j = 0; j < gClass[aux_cid]->nProp; j++, indx++)
	  {
	    /* Copy Property Definition and calculate DataSize*/
	    strncpy (pClass[CId]->Prop[indx].IdProp, 
		     pClass[aux_cid]->Prop[j].IdProp, EDMA_PROP_NAME_LEN);
	    pClass[CId]->Prop[indx].Tipo =  pClass[aux_cid]->Prop[j].Tipo;
	    pClass[CId]->Prop[indx].ioTipo =  pClass[aux_cid]->Prop[j].ioTipo;
	    pClass[CId]->Prop[indx].nElem =  pClass[aux_cid]->Prop[j].nElem;
	    pClass[CId]->Prop[indx].Off = DataSize;
	    DataSize += (tipo[pClass[aux_cid]->Prop[j].Tipo].tam 
			 * pClass[aux_cid]->Prop[j].nElem);
	  }
      }
  /**************************************************************************/

  gClass[CId]->TamDatos = DataSize;
  gClass[CId]->nProp = indx;
  
  /* Set Method information*/
  n_vmets = 0;
  for (i = 0; i < nMet; i++)
    n_vmets += _edma_read_edmaidf_met (pi, CId, i);

  /*************************************************************************/
  /* Add Methods from IFaces */
  indx = nMet;
  for (i = 0; i < nIFaces; i++)
    if (IFaceList[i] != -1)
      {
	aux_cid = IFaceList[i];
	for (j = 0; j < gClass[aux_cid]->nMet; j++, indx++)
	  {
	    strncpy (pClass[CId]->Met[indx].IdMet, 
		     pClass[aux_cid]->Met[j].IdMet, EDMA_MET_NAME_LEN);
	    strncpy (pClass[CId]->Met[indx].Sign, pClass[aux_cid]->Met[j].Sign,
		     EDMA_MET_SIG_LEN);
	    pClass[CId]->Met[indx].Virtual = pClass[aux_cid]->Met[j].Virtual;
	    pClass[CId]->Met[indx].Abstract = pClass[aux_cid]->Met[j].Abstract;
	    pClass[CId]->Met[indx].Static = pClass[aux_cid]->Met[j].Static;
	    n_vmets += pClass[aux_cid]->Met[j].Virtual;
	  }
      }
  gClass[CId]->nMet = indx;
  gClass[CId]->nMetVir = n_vmets;

  /* Free IFace Table */
  if (nIFaces)
    edma_pfree (h, IFaceList);

  /************************************************************************/
  /* We process static inheritance information in interface definition file */
  /* FIXME: We must change the piece of code bellow which builds the string */
  /* arrays to call edma_derive_class with proper parameters*/
  /********************** Begin Processins of SCList ***********************/
  Buffer[0] = 0;   
  Result = 0;
  
  /**** FIXME: The code below is messy.... *******/
  Result = edma_get_ini_string (pi, "Definition", "SCList", NULL, Buffer, 200);
  if (Result != 0)  
    { /* If exists some information */
      /* We count commas to know how many classes to process */
      j = 0;
      for (i = 0; i < Result; i++)
	if (Buffer[i] == ',') j++;
      j++;
      edma_printf_dbg (12, -1, "[_edma_read_class_interface] %d superclases "
		       "located :", j);
      gClass[CId]->Derived = j;
      
      /* Alloc memory for SuperClass List*/
      if ((h = edma_palloc (sizeof(EPChar) * (j + 1))) == 0)
	{
	  edma_printf_err ("[_edma_read_class_interface] Can't create "
			   "inheritance info");
	  return -1;
	}
      clist = (EPChar*) edma_pget (h);
      clist[0] = Buffer;
      
      /* Allocate memory for SuperClass prefered anchor points*/
      if ((h1 = edma_palloc (sizeof(EPChar) * (j + 1))) == 0) 
	{
	  edma_printf_err ("[_edma_read_class_interface] Can't create "
			   "anchor point info...");
	  return -1;
	}
      idlist = (EPChar*) edma_pget (h1);
      
      /* Allocate memory for SubClass prefered anchor points*/
      if ((h2 = edma_palloc (sizeof(EPChar) * (j + 1))) == 0) 
	{
	  edma_printf_err ("[_edma_read_class_interface] Can't create "
			   "anchor point info...");
	  return -1;
	}
      idlist1 = (EPChar*) edma_pget (h2);
      
      ii = j = 0;
      
      for (i = 0; i < Result + 1; i++)
	if ((Buffer[i] == ',') || (Buffer[i] == 0)) 
	  {
	    clist[j] = (Buffer + ii);
	    Buffer[i] = 0; /* Finish string at comma*/
	    if ((aux = strchr (clist[j], ':')) != NULL) 
	      {
		idlist[j] = aux + 1;
		*aux = 0;
		
		if ((aux1 = strchr (idlist[j], '|')) != NULL)
		  {
		    idlist1[j] = aux1 + 1;
		    *aux1 = 0;
		  }
		else
		  idlist1[j] = NULL;
	      } 
	    else 
	      idlist[j] = NULL;	
	    j++;
	    ii = i + 1;
	  }
      clist[j] = 0;
      idlist[j] = 0;
      idlist1[j] = 0;
      /**************** FINISH Processing of SCList ***********************/
      
      gClass[CId]->Status = CLASS_ILOADED;
      /*
       * At this point, interface is loaded and mapped for this process
       * but the shared seccion is only loaded
       */
      ProcMapTable[CId] = CLASS_IMAPPED;
      
      /* Attach Static Inheritance Information*/
      if ((edma_derive_class (gClass[CId]->ClassName, clist, idlist, idlist1)) 
	  == -1)
	{
	  edma_printf_err ("Can't load interfaz for class %s", 
			   gClass[CId]->ClassName);
	  
	  edma_pfree (h, clist);
	  edma_pfree (h1, idlist);
	  edma_pfree (h2, idlist1);
	  
	  /* FIXME: At this point we should cleanup allocated memory*/
	  /*_edma_free_class_interface (CId, NULL);*/
	  edma_close_ini (pi);
	  return -1;
	}
      
      /* Freee temporal memory*/
      edma_pfree (h, clist);
      edma_pfree (h1, idlist);
      edma_pfree (h2, idlist1);
    }
  /* Create method dictionary */
  pClass[CId]->met_dict = edma_dict_new (gClass[CId]->nMet);
  for (i = 0; i < gClass[CId]->nMet; i++)
    edma_dict_add_entry (pClass[CId]->met_dict, pClass[CId]->Met[i].IdMet, i);
  
  pClass[CId]->prop_dict = edma_dict_new (gClass[CId]->nProp);
  for (i = 0; i < gClass[CId]->nProp; i++)
    edma_dict_add_entry (pClass[CId]->prop_dict, 
			 pClass[CId]->Prop[i].IdProp, i);
  
  edma_close_ini (pi);
  
  return 0;
}

/* _edma_free_class_interface
 *    This functions frees all the dynamic memory related to an interface
 */

ESint32 EDMAPROC 
_edma_free_class_interface (EUint32 ClassId, EPChar Name) 
{
  CLASSID		CId;

  /* FIXME: This is an internal function. 
   * Check if parameters are checked from it is called
   */
  if ((ClassId == -1) && (Name == NULL))
    {
      edma_printf_err ("%s", "[_edma_free_class_interface] Not enough "
		       "information to locate class");
    }
  
  /* We get the class identifier */
  if (ClassId == -1)
    CId = edma_get_class_id (Name);
  else
    CId = ClassId;

  if ((edma_check_class_id (ClassId, "_edma_free_class_interface")) == -1)
    return -1;

  /**** XXXX:  Check if this is correct. Today I sleep just a little *****/
  if (ProcMapTable[CId] >= CLASS_ILOADED)
    {
      /* For all the process we must to deattach the shared memory block */
      if (gClass[CId]->Derived)
	{
	  edma_sunget (pClass[CId]->SCList);
	  edma_sunget (pClass[CId]->SCIdList);
	}
      if (gClass[CId]->nProp)
	edma_sunget (pClass[CId]->Prop);
      if (gClass[CId]->nMet)	
	edma_sunget (pClass[CId]->Met);
      if (gClass[CId]->nNot)
	edma_sunget (pClass[CId]->Not);	   
      
      /* If no object uses this class we can free associated shared resources */
      ProcMapTable[CId] = CLASS_ILOADED;
      if (gClass[CId]->Ocurrences == 0) 
	{
	  /* Only free memory if it was allocated */
	  if (gClass[CId]->Derived)
	    {
	      edma_sfree (pClass[CId]->SysClass.hSCList, pClass[CId]->SCList);
	      edma_sfree (pClass[CId]->SysClass.hSCIdList, 
			  pClass[CId]->SCIdList);
	    }

	  if (gClass[CId]->nProp)
	    edma_sfree (pClass[CId]->SysClass.hProp, pClass[CId]->Prop);

	  if (gClass[CId]->nMet)
	    edma_sfree (pClass[CId]->SysClass.hMet, pClass[CId]->Met);

	  if (gClass[CId]->nNot)
	    edma_sfree (pClass[CId]->SysClass.hNot, pClass[CId]->Not);	 
	  
	  ProcMapTable[CId] = CLASS_DEF;
	  gClass[CId]->Status = CLASS_DEF;
	}
    }
  
  return 0;
}


/* _edma_read_edmaidf_prop
 *   Reads a property definition from an EDMAIDF file
 */
EUint32 
_edma_read_edmaidf_prop (PINIFILE pi, EUint32 class_id, EUint32 property_indx) 
{
  EChar         prop_name[EDMA_PROP_NAME_LEN];
  EChar         prop_type[EDMA_GENERAL_ID_LEN];
  EChar         prop_access[EDMA_GENERAL_ID_LEN];
  EChar         user_data[EDMA_CLASS_NAME_LEN];
  EChar		Aux[100];
  EDWord	Result;
  EUint32       n_elems, size;
  
  /* FIXME: This is an internal function. Get sure it is always called with
            correct parameters
  */
  size = 0;
  /* Build INI Key to access property*/
  sprintf (Aux, "Prop%ld", property_indx);
  Result = edma_get_ini_string (pi, Aux, "Name", NULL, prop_name, 
				EDMA_PROP_NAME_LEN);
  Result = edma_get_ini_string (pi, Aux, "Type", NULL, prop_type, 
				EDMA_GENERAL_ID_LEN);
  Result = edma_get_ini_string (pi, Aux, "UserType", NULL, user_data, 
				EDMA_CLASS_NAME_LEN);
  Result = edma_get_ini_string (pi, Aux, "Access", NULL, prop_access, 
				EDMA_GENERAL_ID_LEN);
  n_elems = edma_get_ini_int (pi, Aux, "ArrayElems", 0);

  size = edma_idf_set_prop (class_id, property_indx, prop_name, 
			    prop_type, prop_access,
			    n_elems, user_data);
  if (size < 0)
    {
      edma_printf_err ("[_edma_read_edmaidf_prop] Error reading property "
		       "%s on class %s:%d",
		       prop_name, gClass[class_id]->ClassName, class_id);
      return -1;
    }
  return size;
}

/* _edma_read_edmaidf_met
 *   Reads information about a method from an EDMAIDF file
 */

EUint32 
_edma_read_edmaidf_met (PINIFILE pi, EUint32 class_id, EUint32 method_indx) 
{
  EChar         method_name[EDMA_MET_NAME_LEN];
  EChar         method_sig[EDMA_MET_SIG_LEN];
  EChar		Aux[100];
  EDWord	Result;
  ESint32       m_abstract, m_static, m_virtual;

  /* FIXME: This is an internal function. Be sure it is called with 
            correct parameters 
  */
  
  /* Build Section Key */
  sprintf (Aux, "Met%ld", method_indx);
  
  /* Read Data*/
  Result = edma_get_ini_string (pi, Aux, "Name", NULL, method_name, 
				EDMA_MET_NAME_LEN);
  Result = edma_get_ini_string (pi, Aux, "Signature", NULL, method_sig, 
				EDMA_MET_SIG_LEN);
  m_static = edma_get_ini_int (pi, Aux, "Static", 0);
  m_abstract = edma_get_ini_int (pi, Aux, "Abstract", 0);
  m_virtual = edma_get_ini_int (pi, Aux, "Virtual", 0);

  if (m_abstract)
    if (m_virtual == 0)
      {
	edma_log ("[_edma_read_edmaidf_met] WARNNING. Method '%s' declared "
		  "abstract but not virtual. Setting virtual flag",
		  method_name);
	m_virtual = 1; /* Abstract Methid must be virtual */
      }

  edma_idf_set_met (class_id, method_indx, method_name, method_sig,
		    m_virtual, m_abstract, m_static);

  return m_virtual;
}


