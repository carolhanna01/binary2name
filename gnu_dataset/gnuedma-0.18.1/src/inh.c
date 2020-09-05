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
 * Vigo 22 de Junio de 1997
 * ---------------------------------------------------------
 * Módulo de funciones para herencia
 * REVISIONES:------------------------------------------------
 * 2 de Julio de 1997
 * Intentamos añadir herencia dinámica...
 * Parece que va
 * -----------------------------------
 * 19 de Julio de 1997
 * Añadimos soporte para el mapeo de implementaciones de clases para
 * cada proceso utilizando las.
 * -----------------------------------------------------
 * 22 de Agosoto de 1997
 * Reincorporamos las funciones MergeObj y FreeObj. Vamos a testearlas con la
 * clase CONTAINER.
 * ------------------------------
 * 18 de Septiembre de 1997
 * Vamos a intentar añadir el soporte para SIU a las funciones de herencia dinámica
 * (...)
 * Sigo con la modificación, y acabo de hacer un churro a ver que sale.
 * 
 * ------------------------------------------------------------
 * January, 5th, 2001
 * Source cleanup
 * ---------------------------------------------------------------------
 * November, 17th, 2001
 * More code cleanup and compile warnning removal
 * ------------------------------------------------------------------------
 * November, 28th,2001
 * DeriveClass Modfication. Now we must pass a string array with the
 * prefered anchor points for each superclass in a static inheritance
 * relationship
 * --------------------------------------------------------------------------
 * December, 3rd, 2001
 * Finished The preferer anchor point patch 
 * ---------------------------------------------------------------
 * March, 2nd,2002
 * Code Cleanup
 * --------------------------------------------------------------
 * March, 11th, 2002
 * Modified AddSuperClassObj to create an entry in each superclass object
 * added by the function, in order to maintain all the subobjects linked
 * -------------------------------------------------------------------------
 * March, 16th, 2002
 * Added functions add just one superclass or superobject to running object
 * ----------------------------------------------------------------------
 * May, 12th, 2002
 * Added functions to vertically inserting classes/objects in 
 * class hierarchies
 * ----------------------------------------------------------------------------
 * May, 10th, 2003
 * Modification to support changes to internal class structures
 * ---------------------------------------------------------------------
 * July, 3rd, 2003
 * Cosmetic changes + input parameters sanity checks
 * ---------------------------------------------------------------------------
 * July, 11th, 2003
 * Now using locator functions for anchor points 
 * ------------------------------------------------------------------------------
 * January, 7th, 2004
 * Code cleanup using new anchor point locator functions
 */
 
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include "portable.h"
#include "classq.h"
#include "shmem.h"
#include "vglobal.h"
#include "clas.h"
#include "obj.h"
#include "pri3.h"
#include "misc.h"
#include "cast_obj.h"
#include "helper.h"

#include "anchor_points.h"
#include "locators.h"

#include "inh1.h"

/**************************************************************
** Static inheritance function
** it uses dynamic data structures anyway
************************************************************/

/* edma_derive_class
 *   Adds a set of superclasses to a given class allowing to indicate the list 
 *   of related anchor points
 */

ESint32 EDMAPROC 
edma_derive_class (EPChar nombre,EPChar *c,EPChar *ilist, EPChar *ilist1) 
{
  EUint32	Id,IdSC;
  EUint32	i;	        /* General counter for loops */
  EUint32	nSC;		/* SuperClass count */
  EChar		error[80];
  HMEM		h;
  
  edma_printf_dbg( 12, -1, "[edma_derive_class] Build "
		   "inheritance information for class %s", nombre);
  Id = edma_get_class_id (nombre);
  edma_log ("Call '%s' id: '%d' (%d)\n", nombre, Id, 
	    GVar->nMaxClases + nMaxLocalClasses);

  if (Id >= GVar->nMaxClases + nMaxLocalClasses) 
    {
      edma_printf_err ("[edma_derive_class] Class %s not found", nombre);
      return -1;
    }
  
  /* We create the superclass table for this derived class */
  nSC = gClass[Id]->Derived;
  sprintf (error, "%ldSCLIST", Id);
  h = edma_salloc (sizeof (CLASSID) * nSC,error);
  if (h == (HMEM)0) 
    {
      edma_printf_err ("[edma_derive_class] Can´t allocate SuperClass Table");
      return -1;
    }
  pClass[Id]->SysClass.hSCList = h;
  pClass[Id]->SCList = edma_sget (h);
  
  /* Now we create the preferer anchor point list*/
  sprintf (error, "%ldIDSCLIST", Id);
  if ((h = edma_salloc (sizeof (ID) * nSC, error)) == (HMEM)0) 
    {
      edma_printf_err ("[edma_derive_class] Can't allocate SuperClass "
		       "Preferer Anchor point table");
      return -1;
    }
  pClass[Id]->SysClass.hSCIdList = h;
  pClass[Id]->SCIdList = edma_sget (h);

  /* Now we create the preferer anchor point list*/
  sprintf (error, "%ldIDSUBCLIST", Id);
  if ((h = edma_salloc (sizeof (ID) * nSC, error)) == (HMEM)0) 
    {
      edma_printf_err ("[edma_derive_class] Can't allocate SuperClass "
		       "Preferer Anchor point table");
      return -1;
    }
  pClass[Id]->SysClass.hSubCIdList = h;
  pClass[Id]->SubCIdList = edma_sget (h);

  /* ... and fullfill it */
  edma_printf_dbg (12, -1, "[edma_derive_class] Getting "
		   "Inheritence information for %d classes", nSC);
  for (i = 0; i < nSC; i++) 
    {
      edma_printf_dbg(12, -1, "[edma_derive_class] Getting "
		      "identifier for class : %s", c[i]);
      IdSC = edma_get_class_id (c[i]);
      if (IdSC == -1) 
	{
	  edma_printf_err ("[edma_derive_class] Superclass %s not found", c[i]);
	  edma_printf_err ("[edma_derive_class] => Check SCList entry "
			   "in IDF file for class %s",
			   gClass[Id]->ClassName);
	  edma_sfree (pClass[Id]->SysClass.hSCList, pClass[Id]->SCList);
	  edma_sfree (pClass[Id]->SysClass.hSCIdList, pClass[Id]->SCIdList);
	  return -1;
	}
      
      if ((ilist) && (ilist[i]) && (strlen (ilist[i]) > 0)) 
	strncpy (pClass[Id]->SCIdList[i], ilist[i], sizeof (ID));
      else
	strncpy (pClass[Id]->SCIdList[i], gClass[Id]->ClassName, 
		 EDMA_GENERAL_ID_LEN);
      
      if ((ilist1) && (ilist1[i]) && (strlen (ilist1[i]) > 0)) 
	strncpy (pClass[Id]->SubCIdList[i], ilist1[i], sizeof (ID));
      else
	strncpy (pClass[Id]->SubCIdList[i], c[i], EDMA_GENERAL_ID_LEN);
      
      edma_printf_dbg(12, -1, "(edma_derive_class)    Identifier : %d", IdSC); 
      /* if the superclass isn't mapped... we mapped it */
      if (ProcMapTable[IdSC] >= CLASS_IMAPPED)
	edma_load_class_int (IdSC);
      pClass[Id]->SCList[i] = IdSC;
    }
  return 0;
}

/****************************************************************
** Dynamic Inheritance Functions
*******************************************************************/

/*
** Add super classes to an OBJECT
*/
ESint32 EDMAPROC 
edma_add_superclasses_obj (POBJ pObj,EPChar *c,EPChar *cId) 
{
  EUint32	Id,IdSC,tam;
  EUint32	i,j;		/* General Counters for loops */
  EUint32	nSC;		/* Superclass Count */
  EUint32	nSCOld;		/* Old Superclass Count */
  EPChar	*cTemp;
  SC_ID		*NUpTable;	/* For table clonation */
  HMEM		hNUpTable;
  EChar		ClassName[EDMA_CLASS_NAME_LEN];
  EChar		SubSys[EDMA_CLASS_NAME_LEN];
  EPChar	Aux,Aux1;
  EChar		isSIU;
  OBJID         new_obj;

  if (pObj == NULL)
    {
      edma_printf_err ("%s", "[edma_mutate_obj] Invalid object pointer");
      return -1;
    }

  if ((c == NULL) || (cId == NULL))
    {
      edma_printf_err ("%s", "[edma_mutate_obj] Invalid class "
		       "or class identifier list");
      return -1;
    }

  Id = pObj->IdClass;
  /* If the class isn't in memory we load it first */
  if (ProcMapTable[Id] != CLASS_LOADED)
    edma_load_class_imp (Id);
  
  /* We count the superclasses to add */
  nSC = 0;
  cTemp = c;
  while (cTemp[nSC] != NULL)
    nSC++;
  
  /* We update superclass table and we clone the table */
  nSCOld = pObj->nUpTable;
  tam = sizeof (SC_ID) * (nSC + nSCOld);
  hNUpTable = edma_palloc (tam);
  if (hNUpTable == 0) 
    {
      edma_printf_err ("Dynamical inheritance memory alloc errror");
      return -1;
    }
  
  NUpTable = edma_pget (hNUpTable);
  if (nSCOld)	/* if there some old superclase we copy it */
    memcpy (NUpTable, pObj->UpTable, nSCOld * sizeof (SC_ID));
  /* We delete the old one */
  edma_pfree (pObj->SysObj.hUpTable, pObj->UpTable);
  pObj->UpTable = NUpTable;
  pObj->SysObj.hUpTable = hNUpTable;
  pObj->nUpTable += nSC;
  /* ... and fullfill it */
  for (i = 0; i < nSC; i++) 
    {
      /* We get the class' name */
      Aux = strchr (c[i], ')');
      if (Aux != NULL) 
	{
	  strncpy (SubSys, &c[i][0]+1, Aux-&c[i][0]-1);
	  SubSys[Aux-&c[i][0]] = 0;
	  strncpy (ClassName, Aux + 1, EDMA_CLASS_NAME_LEN);
	  Aux++;
	} 
      else 
	{ /* by default, subsystem is EDMA */
	  strncpy (SubSys, "EDMA", 4);
	  strncpy (ClassName, c[i], EDMA_CLASS_NAME_LEN);
	  Aux = c[i];
	}
      edma_printf_dbg(4, -1, "(AddSC) Adding class %s in subsystem : %s", 
		      ClassName, SubSys);
      if (strncmp (SubSys, "EDMA", 4) == 0) 
	{
	  isSIU = 0;
	  Aux1 = strchr (c[i], ':');
	  if (Aux1 != NULL) 
	    {
	      Aux1++;
	      isSIU = 1;
	    } 
	  else 
	    Aux1 = Aux;
	  
	  IdSC = edma_get_class_id (Aux);
	  if ((IdSC == GVar->nMaxClases) && (isSIU == 0)) 
	    {
	      edma_printf_err ("Class %s not found", c[i]);
	      /* FIXME: !!! Here, we should rollback transaction */
	      return -1;
	    }
	  
	  if ((ProcMapTable[IdSC] != CLASS_LOADED) && (isSIU == 0))
	    edma_load_class_imp (IdSC);
	  
	  if (isSIU == 0)
	    j = IdSC;
	  else
	    j = -1;

	  pObj->UpTable[nSCOld + i].IdClass = j;	/* Class Identifier */
      
	  strncpy (pObj->UpTable[nSCOld + i].Id, cId[i], EDMA_GENERAL_ID_LEN); 
	  /* User Identifier */ 
	  pObj->UpTable[nSCOld + i].Obj = (new_obj = edma_new_obj (ClassName,
								   NULL));

	  gObj[pObj->UpTable[nSCOld + i].Obj]->Father = pObj->IdObj;
	  gObj[pObj->UpTable[nSCOld + i].Obj]->PseudiFather = pObj->IdObj;
	  
	  /* Now link the new created object with us*/
	  /* First we add an entry in the subclasses table 
	     for the new created object*/
	  if ((hNUpTable = edma_palloc (sizeof (SC_ID))) == 0)
	    {
	      edma_printf_err ("Dynamical inheritance memory alloc errror");
	      return -1;
	    }
	  
	  NUpTable = edma_pget (hNUpTable);
	  gObj[new_obj]->DownTable = NUpTable;
	  gObj[new_obj]->SysObj.hDownTable = hNUpTable;
	  gObj[new_obj]->nDownTable = 1;
	  /* Last we full fill the new created subclass entry*/
	  gObj[new_obj]->DownTable[0].Obj = pObj->IdObj;
	  gObj[new_obj]->DownTable[0].IdClass = pObj->IdClass;
	  strncpy (gObj[new_obj]->DownTable[0].Id, c[i], EDMA_GENERAL_ID_LEN);
	  /* The new created object has been linked */
	} 
      else 
	{ /* if it's SIU, superclass identifier is setted to -1*/
	  IdSC = -1;
	  if (pObj->IdSIU == -1) 
	    {	/* else we haven a SIU Proxy*/ 
	      edma_printf_err ("Can´t inherit on SIU subsystem. "
			       "Object %d isn´t a SIU PROXY", pObj->IdObj);
	      continue;
	    }
	  if (isSIU == 0)
	    j = IdSC;
	  else
	    j = -1;
	  pObj->UpTable[nSCOld + i].IdClass = j;	/*Class Identifier*/
	  strncpy (pObj->UpTable[nSCOld + i].Id, cId[i], EDMA_GENERAL_ID_LEN); 
	  /* User Identifier*/
	  pObj->UpTable[nSCOld + i].Obj = pObj->IdObj;
	  /* The subobject'll be created by the SIU proxy */
	  edma_met3 (pObj->IdObj, "AddSC", ClassName, cId[i]);	
	}
    }
  return 0;
}

/*
** Changes a class in an object
** This function changes superclasses alredy defined in the object
*/

ESint32 EDMAPROC 
edma_over_superclasses_obj (POBJ pObj, EUint32 i, EPChar c, EPChar cId) 
{
  EUint32	IdSC;
  EUint32	j;		/*General counters for loops*/
  POBJ		OldObj,NObj;
  EChar		ClassName[EDMA_CLASS_NAME_LEN];
  EChar		SubSys[EDMA_CLASS_NAME_LEN];
  EPChar	Aux,Aux1;
  EChar		isSIU;

  if (pObj == NULL)
    {
      edma_printf_err ("%s", "[edma_mutate_obj] Invalid object pointer");
      return -1;
    }

  if ((c == NULL) || (cId == NULL))
    {
      edma_printf_err ("%s", "[edma_mutate_obj] Invalid class "
		       "or class identifier list");
      return -1;
    }

  Aux = strchr (c, ')');
  if (Aux != NULL) 
    {
      strncpy (SubSys, c, Aux - c - 1);
      SubSys[Aux-c] = 0;
      strncpy (ClassName, Aux + 1, EDMA_CLASS_NAME_LEN);
  }
  else 
    { /* by default, EDMA is assumed */
      strncpy (SubSys, "EDMA", 4);
      strncpy (ClassName, c, EDMA_CLASS_NAME_LEN);
    }

  edma_printf_dbg(4, -1, "(OverSC) Adding class %s in subsystem : %s", 
		  ClassName, SubSys);
  
  if (strncmp (SubSys, "EDMA", 4) == 0) 
    {
      isSIU = 0;
      Aux1 = strchr (c, ':');
      if (Aux1 != NULL) 
	{
	  Aux1++;
	  isSIU = 1;
	} 
      else 
	Aux1 = Aux;

      IdSC = edma_get_class_id (ClassName);
      if ((IdSC == GVar->nMaxClases) && (isSIU == 0)) 
	{
	  edma_printf_err ("Class %s not found", c);
	  return 0;
	}
      if ((IdSC == pObj->UpTable[i].IdClass) && (isSIU == 0))
	return 1;

      OldObj = gObj[pObj->UpTable[i].Obj];
    
      if ((ProcMapTable[IdSC] != CLASS_LOADED) && (isSIU ==0))
	edma_load_class_imp (IdSC);
    
      j = IdSC;
      NObj = gObj[edma_new_obj (gClass[IdSC]->ClassName, NULL)];
      if (NObj == NULL) 
	{
	  edma_printf_err("%s", "(OverSC)Can´t create subobject. "
			  "Override SuperClass Error");
	  return -1;
	}
      
      edma_free_obj (pObj->UpTable[i].Obj);
      pObj->UpTable[i].IdClass = j;		/* Class Identifier */
      strncpy (pObj->UpTable[i].Id, cId, EDMA_GENERAL_ID_LEN); 
      /* User IDentifier*/ 
      pObj->UpTable[i].Obj = NObj->IdObj;

      gObj[pObj->UpTable[i].Obj]->Father = pObj->IdObj;
      gObj[pObj->UpTable[i].Obj]->PseudiFather = pObj->IdObj;
    } 
  else 
    { /* if we override in a SIU object*/
      edma_met3 (pObj->IdObj, "OverSC", i, c, cId);
    }
  return 0;
}

/*
** General function for dynamic inheritance
*/

ESint32 EDMAPROC 
edma_mutate_obj (POBJ pObj, EPChar *c, EPChar *cId) 
{
  EUint32	nSC;	/* Superclasses counter*/
  EUint32	nOldSC;	/* Old superclasses counter*/
  EUint32	nNewSC;	/* New superclasses counter*/
  EUint32	i,j;	/* General counters for loops*/
  EUint32	nUpTable;	/* Entries counter in object class table*/
  EPChar	*cTemp;
  EPUint32	IndUpTable;
  HMEM		hIndUpTable;

  if (pObj == NULL)
    {
      edma_printf_err ("%s", "[edma_mutate_obj] Invalid object pointer");
      return -1;
    }

  if ((c == NULL) || (cId == NULL))
    {
      edma_printf_err ("%s", "[edma_mutate_obj] Invalid class or "
		       "class identifier list");
      return -1;
    }

  /* We count superclasses */
  nSC = 0;
  cTemp = c;
  while (cTemp[nSC] != NULL)
    nSC++;
  nUpTable = pObj->nUpTable;
  hIndUpTable = edma_palloc (sizeof (EUint32) * nSC);
  if (hIndUpTable == (HMEM)0) 
    {
      edma_printf_err("[edma_mutate_obj] Can't create "
		      "SuperClass table for object %d", pObj->IdObj);
      return -1;
    }
  IndUpTable = edma_pget (hIndUpTable);
  /* FIXME: Check error on memory allocation */
  /* We locate old classes first of all */
  nOldSC = 0;
  
  for (i = 0; i < nSC; i++) 
    {
      IndUpTable[i] = -1;
    
      for (j = 0; j < nUpTable; j++) 
	{
	  if (strncmp (pObj->UpTable[j].Id, cId[i], EDMA_GENERAL_ID_LEN) == 0) 
	    {
	      IndUpTable[i] = j;
	      nOldSC++;
	      break;
	    }
	}
    }
  /* We update the old ones */
  for (i = 0; i < nSC; i++) 
    {
      j = IndUpTable[i];
      if (j != -1)
	edma_over_superclasses_obj (pObj, j, c[i], cId[i]);
    }
  /* We build the table for the new ones */
  nNewSC = nSC - nOldSC;
  if (nNewSC)	
    { /* If there is someone new*/
      j = 0;
      for (i = 0; i < nSC; i++)
	if (IndUpTable[i] == -1) 
	  {
	    c[j] = c[i];
	    cId[j] = cId[i];
	    j++;
	  }
      
      edma_pfree (hIndUpTable, IndUpTable);
      c[j] = NULL;
      cId[j] = NULL;
      edma_add_superclasses_obj (pObj, c, cId);
    }
  return 0;
}

/************************************************************************
** Object Merging functions *********************************************
**************************************************************************/

/* edma_merge_superclass_obj
 *   Allows to associate a new object to a given anchor point
 *   THis function will be removed in future and operation managed 
 * by anchor points API
 */
ESint32 EDMAPROC 
edma_merge_superclass_obj (OBJID IdObj, EPChar Id, OBJID IdObj1) 
{
  POBJ		        pObj,pObj1;
  EUint32		i;
  POBJ			ObjIni;

  if ((edma_check_obj_id (IdObj, "edma_merge_superclass_obj")) == -1)
    return -1;

  if ((edma_check_obj_id (IdObj1, "edma_merge_superclass_obj")) == -1)
    return -1;

  if (Id == NULL) 
    {
      edma_printf_err ("%s", "[edma_merge_superclass_obj] Invalid "
		       "Anchor Point");
      return -1;
    }

  if (IdObj1 == 0)
    edma_printf ("(MergeObj) Merging object 0 in object %d. Id %s", 
		 IdObj, Id);

  pObj = gObj[IdObj];
  pObj1 = gObj[IdObj1];
  ObjIni = pObj;

  i = _edma_locate_uplink_by_name (IdObj, Id);
  if (i == -1)	
    { /* if there is no anchor point*/
      edma_printf_err ("(MergeObj) Anchor point %s doesn't exist", Id);
      return -1;
    }
  edma_printf_dbg(4, -1, "Meging Object %d of class %s in "
		  "table entry %d, Id %s",
		  pObj1->IdObj, gClass[pObj1->IdClass]->ClassName,i, Id);

  ObjIni = gObj[pObj->UpTable[i].Obj];
  ObjIni->Father = -1;
  pObj->UpTable[i].Obj = pObj1->IdObj;
  pObj->UpTable[i].IdClass = pObj1->IdClass;
  pObj1->Father = pObj->IdObj;
  pObj1->PseudiFather = pObj->IdObj;
  
  return 0;
}

/* edma_free_superclass_obj
 *    Free entries in the superclass table */
ESint32 EDMAPROC 
edma_free_superclass_obj (OBJID IdObj, EPChar Id) 
{
  POBJ		Obj;
  EUint32	i,j;
  
  if ((edma_check_obj_id (IdObj, "edma_free_superclass_obj")) == -1)
    return -1;

  if (Id == NULL)
    {
      edma_printf_err ("[edma_free_superclass_obj] Invalid anchor point");
      return -1;
    }

  Obj = gObj[IdObj];
  /* We locate the superclass*/
  if ((i = _edma_locate_uplink_by_name (IdObj, Id)) == -1)
    {
      edma_printf_err ("Anchor point %d doesn't exist", Id);
      return 1;
    }

  /* if it exists, we remove it */
  for (j = i; j < Obj->nUpTable - 1; j++)
    memcpy (&Obj->UpTable[j], &Obj->UpTable[j + 1], sizeof (SC_ID));
  
  Obj->nUpTable--;
  
  if (Obj->nUpTable <= 0) 
    {
      edma_pfree (Obj->SysObj.hUpTable, Obj->UpTable);
      Obj->nUpTable = 0;
      Obj->SysObj.hUpTable = 0;
    }
  
  return 0;
}

/**************************************************************************/
/**************************************************************************/
/**************************************************************************/
/**************************************************************************/
/* Inheritance support classes               
 * This functions allows to work on just one sub/superclass and provides a more
 * eficient interface to use internally */


/* edma_add_superclass
 *   Adds a new superclass to a given object. Functions creates a new object
 */

OBJID EDMAPROC
edma_add_superclass (OBJID id, CLASSID cid, EPChar apoint1, EPChar apoint2)
{
  OBJID      superobj;

  if ((edma_check_obj_id (id, "edma_add_superclass")) == -1)
    return -1;

  if ((edma_check_class_id (cid, "edma_add_superclass")) == -1)
    return -1;

  /* FIXME */
  if ((apoint1 == NULL) || (apoint2 == NULL))
    {
      edma_log ("%s", "[edma_add_superclass] WARNNIG:: Invalid Anchor Point");
      /*return -1;*/
    }
 
  /* We create the new superobject and correctly link it.*/  
  superobj = edma_new_obj (gClass[cid]->ClassName);
  if (superobj == -1) 
    {
      edma_printf_err ("[edma_add_superclass] Can't create "
		       "superobject of class %s",
		       gClass[cid]->ClassName);
      /* FIXME rollback operation*/
      return -1;
    }

  if ((_edma_add_super_ap (id, (apoint1 != NULL) ? (EPChar) apoint1 
			   : (EPChar) gClass[cid]->ClassName,
			   cid, superobj, 0)) < 0)
    return -1;
  
  /* Set father information */
  gObj[superobj]->Father = id;
  gObj[superobj]->PseudiFather = id;

  /* Finally we downlink the new superobject*/
  if ((edma_add_subobject (superobj, id, apoint2)) == -1) 
    {
      edma_printf_err ("[edma_add_superclass] Can't downlink new superobject");
      /* FIXME: rollback operation */
      return -1;
    }
  
  return superobj;
}

/* edma_add_superobject:
 *   links object superid up to object id with the anchor point apoint
 */

ESint32 EDMAPROC 
edma_add_superobject (OBJID id, OBJID superid, EPChar apoint)
{
  CLASSID    cid;

  if ((edma_check_obj_id (id, "edma_add_superobject")) == -1)
    return -1;

  if ((edma_check_obj_id (superid, "edma_add_superobject")) == -1)
    return -1;

  /* FIXME */
  if (apoint == NULL)
    {
      edma_log ("%s", "[edma_add_superobject] WARNNING:: Invalid Anchor point");
      /*return -1;*/
    }

  /********************************************************
   * Test if the object we want to add already exists
   * This models the case arising when we are adding a superobject in the
   * case of static inheritance. In this case the new_obj primitive
   * adds the superclasses and we don't want to have two downlinks
   * in fact that will make the program crash
   ********************************************************/
  if ((edma_upcast_obj (id, (apoint != (EPChar)0) ? apoint : 
			(EPChar)gClass[gObj[superid]->IdClass]->ClassName)) 
      == superid)
    {
      edma_log ("[%s] Object %d already linked to superid %d through %s", 
		__FUNCTION__, 
		superid, id, apoint);
      return 0;
    }
  
  /* We don't check for OBJID valid values. 
     This functions are used internally and
     * it's supposed the upper level functions using it has did this checks */

  cid = gObj[superid]->IdClass;  

  return _edma_add_super_ap (id, apoint ? (EPChar) apoint 
			     : (EPChar) gClass[cid]->ClassName,
			     cid, superid, 0);

}


/* edma_insert_superclass
 *   Inserts a new superclass on an existing anchor point
 *   This functions creates an object of class cid
 */

OBJID EDMAPROC
edma_insert_superclass (OBJID id, CLASSID cid, EPChar apoint1, EPChar apoint2)
{
  ESint32    i1, i2;  
  OBJID      superobj, id1;

  if ((edma_check_obj_id (id, "edma_insert_superclass")) == -1)
    return -1;

  if ((edma_check_class_id (cid, "edma_insert_superclass")) == -1)
    return -1;

  /* FIXME */
  if ((apoint1 == NULL) || (apoint2 == NULL))
    {
      edma_log ("%s", "[edma_insert_superclass] WARNNING:: Invalid "
		"Anchor Point");
      /*return -1;*/
    }
  
  /* Lookup anchor point in current object*/
  if ((i1 = _edma_locate_uplink_by_name (id, apoint1)) == -1)
    { /* We have found the anchor point */
      edma_printf_err ("[edma_link_superclass] Anchor point %s already exists"
		       " on object %ld [%s]", 
		       apoint1, id, gClass[gObj[id]->IdClass]->ClassName);
      return -1;
    }
  
  id1 = gObj[id]->UpTable[i1].Obj;
  
  /* Locate current object from superobject */
  if ((i2 = _edma_locate_downlink_by_obj (id1, id)) == -1)
    { /* We have found the anchor point */
      edma_printf_err ("[edma_insert_superclass] Object %ld not found "
		       "in subobject list on object %ld [%s]", 
		       id, id1, gClass[gObj[id1]->IdClass]->ClassName);
      return -1;
    }
  
  /* We have found the anchor points. Now, we create the object to insert */
  if ((superobj = _edma_new_obj (gClass[cid]->ClassName, id, NULL)) == -1)
    {
      edma_printf ("[edma_link_superclass] Can't create object of class [%s]." 
		   "Can't link object", gClass[cid]->ClassName);
      return -1;
    }
  /* Now we can relink the objects */
  /* First we link the inserted superobject */
  edma_add_superobject (superobj, id1, gObj[id]->UpTable[i1].Id);
  edma_add_subobject (superobj, id, gObj[id1]->DownTable[i2].Id);

  /* Update object tables*/
  gObj[id]->UpTable[i1].Obj = superobj;
  gObj[id]->UpTable[i1].IdClass = cid;
  gObj[id1]->DownTable[i2].Obj = superobj;
  gObj[id1]->DownTable[i2].IdClass = cid;
  /* Should we fill Father and PseudiFather fields???*/
  return superobj;
}


/* edma_insert_superobject
 *   Inserts the object superobj in the anchor point apoint1
 *   identical to edma_insert_superclass but no object is created
 */

OBJID EDMAPROC
edma_insert_superobject (OBJID id, OBJID superobj, 
			 EPChar apoint1, EPChar apoint2)
{
  ESint32    i1, i2;  
  OBJID      id1;

  /* FIXME: Maybe Parameter-checker wrapper and internal function */
  if ((edma_check_obj_id (id, "edma_insert_superobject")) == -1)
    return -1;

  if ((edma_check_obj_id (superobj, "edma_insert_superobject")) == -1)
    return -1;

  /* FIXME:*/
  if ((apoint1 == NULL) || (apoint2 == NULL))
    {
      edma_log ("%s", "[edma_insert_superobject] WARNNING:: Invalid Anchor "
		"point");
      /*return -1;*/
    }

  /* Lookup anchor point in current object*/
  if (( i1 = _edma_locate_uplink_by_name (id, apoint1)) == -1)
    { /* We have found the anchor point */
      edma_printf_err ("[edma_link_superclass] Anchor point %s already exists"
		       " on object %ld [%s]", 
		       apoint1, id, gClass[gObj[id]->IdClass]->ClassName);
      return -1;
    }

  id1 = gObj[id]->UpTable[i1].Obj;

  /* Lookup current object from superobject */
  if ((i2 = _edma_locate_uplink_by_obj (id1, id)) == -1)
    { /* We have found the anchor point */
      edma_printf_err ("[edma_insert_superobject] Object %ld not found "
		       "in subobject list on object %ld [%s]", 
		       id, id1, gClass[gObj[id1]->IdClass]->ClassName);
      return -1;
    }

  /* Now we can relink the objects */
  /* First we link the inserted superobject */
  edma_add_superobject (superobj, id1, gObj[id]->UpTable[i1].Id);
  edma_add_subobject (superobj, id, gObj[id1]->DownTable[i2].Id);

  /* Update object tables*/
  gObj[id]->UpTable[i1].Obj = superobj;
  gObj[id]->UpTable[i1].IdClass = gObj[superobj]->IdClass;
  gObj[id1]->DownTable[i2].Obj = superobj;
  gObj[id1]->DownTable[i2].IdClass = gObj[superobj]->IdClass;

  /* Should we fill Father and PseudiFather fields????*/

  return superobj;
}
