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
 * 25 de Abril de 1999
 * Añadimos las funciones para el manejo de la tabla de subclases
 * Reestructuración del sistema de herencia dinámico.
 ------------------------------------------------
 * November, 18th, 2001
 * Code clean up
 * March, 2nd, 2002
 * ------------------------------------------------------------
 * May, 13th, 2002
 * Added functions to vertically inserting classes/objects in 
 * class hierarchies
 * --------------------------------------------------------------
 * April, 2nd, 2003
 * Modification to support changes to OBJ struct
 * ---------------------------------------------------------------------
 * April, 16th, 2003
 * Updated edma_add_subclass to correctly set anchor points when edma_new_obj
 * links the new subclass automatically
 * ---------------------------------------------------------------------------
 * May, 10th, 2003
 * Modification to support changes to internal class structures
 * ---------------------------------------------------------------------
 * July, 3ed, 2003
 * Cosmetic changes + input parameters sanity checks
 * -------------------------------------------------------------------------
 * July, 11th, 2003
 * Now using locate anchor points functions
 * ---------------------------------------------------------------------------
 * January, 7th, 2004
 * Code cleanup using new anchor point locator functions
 */
 
#include <stdio.h>
#include <string.h>
#include "portable.h"
#include "classq.h"
#include "shmem.h"
#include "vglobal.h"
#include "clas.h"
#include "obj.h"
#include "pri3.h"
#include "misc.h"
#include "anchor_points.h"
#include "cast_obj.h"
#include "helper.h"
#include "locators.h"

#include "inh.h"
/****************************************************************
** Dynamic Inheritance Functions
*******************************************************************/

/*
 * edma_add_subclasses_obj
** Add sub classes to an OBJECT
*/

ESint32 EDMAPROC 
edma_add_subclasses_obj (POBJ pObj, EPChar *c, EPChar *cId) 
{
  EUint32	Id,IdSC,tam;
  EUint32	i,j,k;		/* General counters */ 
  EUint32	nSC;		/* Superclasses number*/
  EUint32	nSCOld;		/* Old Superclasses number*/
  EPChar	*cTemp;
  SC_ID		*NTabla;	/* To table clonning*/
  HMEM		hNTabla;
  EChar		ClassName[EDMA_CLASS_NAME_LEN];
  EChar		SubSys[EDMA_GENERAL_ID_LEN];
  EPChar	Aux,Aux1;
  EChar		isSIU;
  OBJID         new_obj;

  if (pObj == NULL)
    {
      edma_printf_err ("%s", "[edma_add_subclasses_obj] Invalid "
		       "object pointer");
      return -1;
    }

  if (c == NULL)
    {
      edma_printf_err ("%s", "[edma_add_subclass_obj] Invalid class list");
      return -1;
    }
  
  if (cId == NULL)
    {
      edma_printf_err ("%s", "[edma_add_subclass_obj] Invalid "
		       "class identifiers list");
      return -1;
    }
  
  Id = pObj->IdClass;
  /* If class isn't loaded we load it*/
  if (ProcMapTable[Id] != CLASS_LOADED)
    edma_load_class_imp (Id);
  
  /* We count superclasses */
  nSC = 0;
  cTemp = c;
  while (cTemp[nSC] != NULL)
    nSC++;
  
  /* We update superclass table and clone it */
  nSCOld = pObj->nDownTable;
  tam = sizeof (SC_ID) * (nSC + nSCOld);
  hNTabla = edma_palloc (tam);
  if (hNTabla == 0)  
    {
      edma_printf_err ("Dynamical inheritance memory alloc errror");
      return -1;
    }
  
  NTabla = edma_pget (hNTabla);
  if (nSCOld)	/* Copy old classes if any*/
    memcpy (NTabla, pObj->DownTable, nSCOld * sizeof(SC_ID));
  
  /* We delete old table*/
  edma_pfree (pObj->SysObj.hDownTable, pObj->DownTable);
  pObj->DownTable = NTabla;
  pObj->SysObj.hDownTable = hNTabla;
  pObj->nDownTable += nSC;
  /*.. and we fill in it*/
  for (i = 0; i < nSC; i++) 
    {
      /* Get class name */
      Aux = strchr(c[i], ')');
      if (Aux != NULL) 
	{
	  strncpy (SubSys, &c[i][0] + 1, Aux - &c[i][0] - 1);
	  SubSys[Aux-&c[i][0]] = 0;
	  strncpy (ClassName, Aux + 1, EDMA_CLASS_NAME_LEN);
	  Aux++;
	}  
      else  
	{ /* default system is EDMA*/
	  strncpy (SubSys, "EDMA", 4);
	  strncpy (ClassName, c[i], EDMA_CLASS_NAME_LEN);
	  Aux = c[i];
	}

      edma_printf_dbg (4,-1,"(AddSC) Adding class %s in subsystem : %s",
		       ClassName,SubSys);

      if (strncmp (SubSys, "EDMA", 4) == 0) 
	{
	  isSIU = 0;
	  Aux1 = strchr(c[i], ':');
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
	    edma_printf_err ("[AddSC1] Class %s not found", c[i]);
	    /* FIXME: We need to rollback transaction*/
	    return -1;
	  }
	  if ((ProcMapTable[IdSC] != CLASS_LOADED) && (isSIU == 0))
	    edma_load_class_imp (IdSC);
	  
	  if (isSIU == 0)
	    j = IdSC;
	  else
	    j = -1;
	  pObj->DownTable[nSCOld+i].IdClass = j; /* Class Identifier */
	  
	  strncpy (pObj->DownTable[nSCOld+i].Id, cId[i], EDMA_GENERAL_ID_LEN); 
	  /* User identifier */
	  /* we create a new subobject*/
	  /* Modification building a new subobject. XXX: 
	     FIXME: Check for errors*/
	  /* FIXME: This'd be moved to a new function*/

	  /*----------------------------------------*/
	  new_obj = _edma_new_obj (ClassName, pObj->IdObj, NULL);

	  pObj->DownTable[nSCOld+i].Obj = new_obj;
	  /*----------------------------------------*/
	  
	  pObj->DownTable[nSCOld+i].IdClass = j;    /* Class identidfier*/
	  if (strncmp (c[i], cId[i], EDMA_GENERAL_ID_LEN) == 0) 
	    {    
	      /* We should look in SCIdList for the new created object*/
	      j = gObj[new_obj]->IdClass;
	      /* First test if there is a SCIdList for the object*/
	      if (pClass[j]->SCIdList) 
		{ 
		  for (k = 0; k < gClass[j]->Derived; k++)
		    if (pClass[j]->SCList[k] == pObj->IdClass) 
		      break;
		  if ((k == gClass[j]->Derived) || (!pClass[j]->SCIdList[k]))
		    strncpy (pObj->DownTable[nSCOld+i].Id, 
			     cId[i], EDMA_GENERAL_ID_LEN); /* User identifier*/
		  else
		    strncpy (pObj->DownTable[nSCOld+i].Id,
			     pClass[j]->SCIdList[k], EDMA_GENERAL_ID_LEN);
		}
	    } 
	  else 
	    {
	      strncpy (pObj->DownTable[nSCOld+i].Id,
		       cId[i], EDMA_GENERAL_ID_LEN); /* User identifier*/
	    }
	  
	  gObj[pObj->DownTable[nSCOld+i].Obj]->Father = pObj->IdObj;
	  gObj[pObj->DownTable[nSCOld+i].Obj]->PseudiFather = pObj->IdObj;
	} 
      else 
	{ /* if is in SIU, superclass identifier is set to -1*/
	  IdSC = -1;
	  if (pObj->IdSIU == -1) 
	    {	/*if we don't have a SIU Proxy */
	      edma_printf_err("Can´t inherit on SIU subsystem. "
			      "Object %d isn´t a SIU PROXY",pObj->IdObj);
	      continue;
	    }
	  if (isSIU == 0)
	    j = IdSC;
	  else
	    j = -1;
	  
	  /*XXXXXXXXXXXXXXXXXXXXXXXXXx TODO XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX*/
	  /* This code is for SIU Layer 2, and it isn't tested              */
	  /******************************************************************/
#if 0
	pObj->DownTable[nSCOld+i].IdClass=j;	    /* Class identidfier*/
	edma_printf ("[AddSC1] Working on %s [%s]",c[i],cId[i]);
	/* If classname == cId -> on-demand inheritance*/
	if (strncmp (c[i], cId[i], EDMA_GENERAL_ID_LEN) == 0) {    
	  /* We should look in SCIdList for the new created object*/
	  edma_printf ("[AddSC1] Testing PAP in object %d...",new_obj);
	  j=gObj[new_obj].IdClass;
	  /* First test if there is a SCIdList for the object*/
	  if (pClass[j]->SCIdList) { 
	    for (k=0;k<gClass[j].Derived;k++) {
	      if (pClass[j].SCList[k]==pObj->IdClass) 
		break;
	    }
	    if ((k==gClass[j].Derived) || (!pClass[j]->SCIdList[k]))
	      strncpy (pObj->DownTable[nSCOld+i].Id,cId[i], 
		       EDMA_GENERAL_ID_LEN); /* User identifier*/
	    else
	      strncpy (pObj->DownTable[nSCOld+i].Id,pClass[j]->SCIdList[k], 
		       EDMA_GENERAL_ID_LEN);
	  }
	} else
	  strncpy (pObj->DownTable[nSCOld+i].Id,cId[i], 
		   EDMA_GENERAL_ID_LEN); /* User identifier*/
#endif
	/*******************************************************************/
	pObj->DownTable[nSCOld+i].Obj = pObj->IdObj;
	/* SIU Proxy will create the subobject for us*/
	edma_met3 (pObj->IdObj,"AddSC", ClassName, cId[i]); 
	}    
    }
  return 0;
}

/*
** Changes a class in an object
** This function changes superclasses alredy defined in the object
*/
/* December, 8th,2001: 
 FIXME: Test this function. We're using the same approach as in AddSC
*/
ESint32 EDMAPROC 
edma_over_subclasses_obj (POBJ pObj,EUint32 i,EPChar c,EPChar cId) 
{
  EUint32	IdSC;
  EUint32	j,k;		/* General loop counters */
  POBJ		OldObj,NObj;
  EChar		ClassName[EDMA_CLASS_NAME_LEN];
  EChar		SubSys[EDMA_CLASS_NAME_LEN];
  EPChar	Aux,Aux1;
  EChar		isSIU;
  OBJID         new_obj;

  if (pObj == NULL)
    {
      edma_printf_err ("%s", "[edma_over_subclasses_obj] Invalid "
		       "object pointer");
      return -1;
    }

  if (c == NULL)
    {
      edma_printf_err ("%s", "[edma_over_subclasses_obj] Invalid class list");
      return -1;
    }

  if (cId == NULL)
    {
      edma_printf_err ("%s", "[edma_over_subclasses_obj] Invalid "
		       "class identifier list");
      return -1;
    }
  Aux = strchr(c, ')');

  if (Aux != NULL) 
    {
      strncpy (SubSys, c, Aux - c - 1);
      SubSys[Aux-c] = 0;
      strncpy (ClassName, Aux+1, EDMA_CLASS_NAME_LEN);
    } 
  else 
    { /* by default we're in EDMA */
      strncpy (SubSys, "EDMA", 4);
      strncpy (ClassName, c, EDMA_CLASS_NAME_LEN);
    }
  edma_printf_dbg (4,-1,"(OverSC) Adding class %s in subsystem : %s",
		   ClassName,SubSys);

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
	  return -1;
	}
      /* if it is the same we do nothing */
      if ((IdSC == pObj->DownTable[i].IdClass) && (isSIU == 0))
	return 1;
      
      OldObj = gObj[pObj->DownTable[i].Obj];
      if ((ProcMapTable[IdSC] != CLASS_LOADED) && (isSIU == 0))
	edma_load_class_imp (IdSC);
      j = IdSC;

      /*----------------------------------------*/
      new_obj = _edma_new_obj (ClassName, pObj->IdObj, NULL);
      /*----------------------------------------*/
      
      NObj = gObj[new_obj];
      /* XXX: Ooops... is this correct??????*/
      if (NObj == NULL) 
	{
	  edma_printf_err("(OverSC1)Can´t create subobject",
			  "Override SuperClass Error",0);
	  return -1;
	}
      
      edma_free_one_obj (pObj->DownTable[i].Obj);
      pObj->DownTable[i].IdClass = j;	/* Class identifier*/
      /* If classname == cId -> on-demand inheritance*/
      if (strncmp (c, cId, EDMA_GENERAL_ID_LEN) == 0) 
	{    
	  /* We should look in SCIdList for the new created object*/
	  j = gObj[new_obj]->IdClass;
	  /* First test if there is a SCIdList for the object*/
	  if (pClass[j]->SCIdList) 
	    { 
	      for (k = 0; k < gClass[j]->Derived; k++)
		if (gObj[new_obj]->UpTable[k].IdClass == pObj->IdClass) 
		  break;
	      if ((k == gClass[j]->Derived) || (!pClass[j]->SCIdList[k]))
		strncpy (pObj->DownTable[i].Id, 
			 cId, EDMA_GENERAL_ID_LEN); /* User identifier*/
	      else
		strncpy (pObj->DownTable[i].Id,
			 pClass[j]->SCIdList[k], EDMA_GENERAL_ID_LEN);
	    }
	} 
      else
	strncpy (pObj->DownTable[i].Id,
		 cId, EDMA_GENERAL_ID_LEN);  /* User provided identifier*/
      
      pObj->DownTable[i].Obj = NObj->IdObj;
      gObj[pObj->DownTable[i].Obj]->Father = pObj->IdObj;
      
      gObj[pObj->DownTable[i].Obj]->PseudiFather = pObj->IdObj;
    }
  else	
    {	/* if we override within SIU */
      edma_met3 (pObj->IdObj, "OverSC", i, c, cId);
    }
  return 0;
}

/*
** General function for dynamic inheritance
*/
ESint32 EDMAPROC 
edma_mutate_obj1 (POBJ pObj,EPChar *c,EPChar *cId) 
{
  EUint32	nSC;	       /* Superclasses counter*/
  EUint32	nOldSC;	       /* Old Superclasses counter*/
  EUint32	nNewSC;	       /* New Superclasses counter*/
  EUint32	i,j,k;	       /* general counter */
  EUint32	nTabla;	       /* Number of entries in object table*/
  EPChar	*cTemp;
  EPUint32	IndTabla;
  HMEM		hIndTabla;
  POBJ		sObj;
  CLASSID       idC,thisId;

  if (pObj == NULL)
    {
      edma_printf_err ("%s", "[edma_mutate_obj1] Invalid Object Pointer");
      return -1;
    }
  
  if (c == NULL)
    {
      edma_printf_err ("%s", "[edma_mutate_obj1] Invalid class list");
      return -1;
    }
  
  if (cId == NULL)
    {
      edma_printf_err ("%s", "[edma_mutate_obj1] Invalid "
		       "class identifier list");
      return -1;
    }

  /* We count the superclasses provided by user*/
  nSC = 0;
  cTemp = c;
  while (cTemp[nSC] != NULL)
    nSC++;

  nTabla = pObj->nDownTable;
  hIndTabla = edma_palloc (sizeof (EUint32) * nSC);
  if (hIndTabla == (HMEM)0) 
    {
      edma_printf_err("[edma_mutate_obj1] Can't create SuperClass table "
		      "for object %d", pObj->IdObj);
      return -1;
    }
  IndTabla = edma_pget (hIndTabla);
  /* FIXME: Test if alloc worked*/
  /* We find what superclasses are old*/
  nOldSC = 0;
  for (i = 0; i < nSC; i++) 
    {
      IndTabla[i] = -1;
      /* If classname==anchor point look for class id*/
      if (strncmp (c[i], cId[i], EDMA_GENERAL_ID_LEN) == 0) 
	{ 
	  idC = edma_get_class_id (c[i]);
	  if (idC == -1) 
	    {
	      edma_log ("[WARNNING][edma_mutate_obj1] Class %s "
			   "doesn't exist", c[i]);
	      continue;
	    }
	  thisId = pObj->IdClass;
	  for (j = 0; j < nTabla; j++) 
	    {
	      sObj = gObj[pObj->DownTable[j].Obj];
	      for (k = 0;k < sObj->nUpTable; k++) 
		{
		  if (sObj->UpTable[k].IdClass == thisId) 
		    {
		      IndTabla[i] = j;
		      nOldSC++;
		      break;
		    }
		}
	    }
	} 
      else   /* else look for anchor point*/
	for (j = 0; j < nTabla; j++) 
	  {
	    if (strncmp (pObj->DownTable[j].Id, cId[i], 
			 EDMA_GENERAL_ID_LEN) == 0) 
	      {
		IndTabla[i] = j;
		nOldSC++;
		break;
	      }
	  }
    }
  /* We modify the old ones*/
  for (i = 0; i < nSC; i++) 
    {
      j = IndTabla[i];
      if (j != -1) 
	edma_over_subclasses_obj (pObj,j,c[i],cId[i]);
    }
  /* Build a table to store old superclasses*/
  nNewSC = nSC - nOldSC;
  if (nNewSC) 
    {	/* Are there some new superclass? */
      j = 0;
      for (i = 0; i < nSC; i++)
	if (IndTabla[i] == -1) 
	  {
	    c[j] = c[i];
	    cId[j] = cId[i];
	    j++;
	  }
      edma_pfree (hIndTabla, IndTabla);
      c[j] = NULL;
      cId[j] = NULL;
      
      edma_add_subclasses_obj (pObj, c, cId);
    }
  
  return 0;
}

/************************************************************************
** Object Merging functions *********************************************
**************************************************************************/

/* edma_merge_subclass_obj
 *   Changes the subobject associated to a subclass anchor point
 */
EUint32 EDMAPROC 
edma_merge_subclass_obj (OBJID IdObj,EPChar Id,OBJID IdObj1) 
{
  POBJ		pObj,pObj1;
  EUint32	i;
  POBJ		ObjIni;

  if (Id == NULL)
    {
      edma_printf_err ("%s", "[edma_merge_subclass_obj] Invalid anchor point");
      return -1;
    }

  if ((edma_check_obj_id (IdObj, "edma_merge_subclass_obj")) == -1)
    return -1;

  if ((edma_check_obj_id (IdObj1, "edma_merge_subclass_obj")) == -1)
    return -1;
  
  if (IdObj1 == 0)
    edma_printf ("(MergeObj1) Merging object 0 in object %d. Id %s", IdObj, Id);

  pObj = gObj[IdObj];
  pObj1 = gObj[IdObj1];
  ObjIni = pObj;

  if ((i = _edma_locate_downlink_by_name (IdObj, Id)) == -1)
    { /* If no anchor point exists */
      edma_printf_err("(MergeObj1) Anchor point %s doesn't exist",Id);
      return -1;
    }
  
  edma_printf_dbg (4,-1,"Meging Object %d of class %s in table entry %d,"
		   " Id %s", pObj1->IdObj,gClass[pObj1->IdClass]->ClassName,
		   i,Id);

  ObjIni = gObj[pObj->DownTable[i].Obj];
  /* Test */
  ObjIni->Father = -1;
  /* Test ends */
  pObj->DownTable[i].Obj = pObj1->IdObj;
  pObj->DownTable[i].IdClass = pObj1->IdClass;
  pObj1->Father = pObj->IdObj;
  pObj1->PseudiFather = pObj->IdObj;
  
  return 0;
}

/* Free entries in the superclass table */
EUint32 EDMAPROC 
edma_free_subclass_obj (OBJID IdObj, EPChar Id) 
{
  POBJ		Obj;
  EUint32	i,j;

  if (Id == NULL)
    {
      edma_printf_err ("%s", "[edma_free_subclass_obj] Invalid anchor point");
      return -1;
    }
  
  if ((edma_check_obj_id (IdObj, "edma_free_subclass_obj")) == -1)
    return -1;

  Obj = gObj[IdObj];

  /* Locate the superclass*/
  if ((i = _edma_locate_downlink_by_name (IdObj, Id)) == -1)
    {
      edma_printf_err ("[FreeSC1] Anchor point %d doesn't exist",Id);
      return -1;
    }

  for (j = i; j < Obj->nDownTable - 1; j++)
    memcpy (&Obj->DownTable[j], &Obj->DownTable[j + 1], sizeof(SC_ID));
  
  Obj->nDownTable--;

  if (Obj->nDownTable <= 0) 
    {
      edma_pfree (Obj->SysObj.hDownTable, Obj->DownTable);
      Obj->nDownTable = 0;
      Obj->SysObj.hDownTable = 0;
    }

  return 0;
}

/*** FUNCTONS ABOVE WILL BECOME DEPRECATED IN FUTURE VERSIONS          ***/
/*** DO NOT USE                                                        ***/
/*************************************************************************/


/**************************************************************************/
/* Dynamic inheritance support functions
 * This classes allows to work on just one sub/superclass and provides a more
 * eficient interface to use internally */

/* edma_add_subclass
 *   Adds a new subclass to a given object
 *   This function will create a new object for the new subclass added
 */

/* FIXME: Big Function... it must be splitted */
/* edma_add_subclass
 *   Adds the specified subclass to given object.
 *   A new object will be created for the subclass subobject
 */
OBJID EDMAPROC
edma_add_subclass (OBJID id, CLASSID cid, EPChar apoint1, EPChar apoint2)
{
  ESint32    n, i;  
  OBJID      subobj, superobj;

  /* FIXME: We should write a parameter-checker wrapper and 
   * make this function internal */
  if ((edma_check_obj_id (id, "edma_add_subclass")) == -1)
    return -1;

  if ((edma_check_class_id (cid, "edma_add_subclass")) == -1)
    return -1;

  if ((apoint1 == NULL) || (apoint2 == NULL))
    {
      edma_log ("%s", "[edma_add_subclass] WARNNING:: Invalid Anchor point");
      /*return -1;*/
    }

  /************************************************ 
   * When adding a subclass we let the edma_new_obj to link the object
   * if necesary.
   * Then, we must check if the new have the right object linked
   * -------------------------------------------------------------------
   * FIXME:
   * We should ensure that the added anchor points are the same
   * that we have in the parameters
   *
   * In the code bellow we are only ensuring that the required class already
   * exists in the class hierarchy.
   *
   * Does it make sense deal with repeated inheritance????
   */
  subobj = _edma_new_obj1 (gClass[cid]->ClassName, id, NULL);
  
  if ((superobj = edma_upcast_obj (subobj, 
				   gClass[gObj[id]->IdClass]->ClassName)) != -1)
    {
      /* If the new_obj primitive has linked the objects, 
       * we must set right anchor points*/
      /* First rename uplink. We must look for the right downlink */
      n = gObj[id]->nDownTable;
      for (i = 0; i < n; i++)
	if (gObj[id]->DownTable[i].Obj == subobj)
	  break;

      /******
       * This is a hack to avoid Segmentations fault at run-time.
       * In some case we can find subobj in the downtable of object id. 
       * We need some extra checkings to do that.
       */
      if (i != n) /* Added subobject is related to base object 
		   * through static inheritance*/
	{
	  edma_rename_subclass_ap (id, gObj[id]->DownTable[i].Id, apoint1);
	  /* Now we must update the uplink from the new subobject just created*/
	  n = gObj[subobj]->nUpTable;
	  for (i = 0; i < n; i++)
	    if (gObj[subobj]->UpTable[i].Obj == superobj)
	      break;
	  edma_rename_superclass_ap (subobj, gObj[subobj]->UpTable[i].Id, 
				     apoint2);
	  
	  return subobj;
	}
    }

  if (subobj == -1) 
    {
      edma_printf_err ("[edma_add_subclass] Can't create "
		       "subobject of class %s", gClass[cid]->ClassName);
      /* FIXME rollback operation*/
      return -1;
    }

  if ((_edma_add_sub_ap (id, apoint1 ? apoint1 
			 : (EPChar)gClass[cid]->ClassName, 
			 cid, subobj, 0)) < 0 )
    return -1;

  /* Set father information */
  gObj[subobj]->Father = id;
  gObj[subobj]->PseudiFather = id;
  /* Finally we downlink the new superobject*/
  if ((edma_add_superobject (subobj, id, apoint2)) == -1) 
    {
      edma_printf_err ("[edma_add_subclass] Can't uplink new superobject");
      /* FIXME: rollback operation 
	 Not necessary it can happen that new_obj links the current superobject
      */

      return -1;
    }
  return subobj;
}


/* edma_add_subobjet:
 *  links object subid down object id with the anchor point apoint
 */

ESint32 EDMAPROC 
edma_add_subobject (OBJID id, OBJID subid, EPChar apoint)
{
  CLASSID    cid;
  
  /* FIXME: Maybe we should write a Parameter-checker wrapper and make 
   * this an internal function*/
  if (apoint == NULL)
    {
      edma_log ("%s", "[edma_add_subobject] WARNNING:: Invalid Anchor point");
      /*return -1;*/
    }

  if ((edma_check_obj_id (id, "edma_add_subobject")) == -1)
    return -1;

  if ((edma_check_obj_id (subid, "edma_add_subobject")) == -1)
    return -1;

  /* We don't check for OBJID valid values. 
     This functions are used internally and
     it's supposed the upper level functions using it has did this checks */
  /* Sanity check. Test, anchor point doesn't exists */

  /*************************************************************************
   * We must check that the subobject we want to attach doesn't exist
   * This happen when edma_add_subobject is called from edma_add_superobject
   * after creating an object with static inheritance relationships. 
   * THis operation build the up and downlink and the we don't need 
   * to build the link, because it already exists.
   */
  if ((edma_downcast_obj (id, apoint)) == subid)
    {
      edma_log ("[%s] Object %d already linked to subid %d through %s", 
		   __FUNCTION__, subid, id, apoint);
      return 0;
    }

  cid = gObj[subid]->IdClass;
  return _edma_add_sub_ap (id, apoint ? apoint 
			   : (EPChar) gClass[cid]->ClassName, cid, subid, 0);
}

/* edma_insert_subclass
 *    Inserts a new subclass at anchor point apoint1
 */

OBJID EDMAPROC
edma_insert_subclass (OBJID id, CLASSID cid, EPChar apoint1, EPChar apoint2)
{
  ESint32    i1, i2;  
  OBJID      subobj, id1;

  /* FIXME: Maybe Parameter-cheker wrapper */
  if ((edma_check_obj_id (id, "edma_insert_subclass")) == -1)
    return -1;

  if ((edma_check_class_id (id, "edma_insert_subclass")) == -1)
    return -1;
  
  if ((apoint1 == NULL)  || (apoint2 == NULL))
    {
      edma_log ("%s", "[edma_insert_subclass] WARNNING:: Invalid anchor name");
      /*return -1;*/
    }

  /* Lookup anchor point in current object*/
  if ((i1 = _edma_locate_downlink_by_name (id, apoint1)) == -1)
    { /* We have found the anchor point */
      edma_printf_err ("[edma_link_subclass] Anchor point %s already exists"
		       " on object %ld [%s]", 
		       apoint1, id, gClass[gObj[id]->IdClass]->ClassName);
      return -1;
    }
  
  id1 = gObj[id]->DownTable[i1].Obj;

  /* Lookup current object from superobject */
  if ((i2 = _edma_locate_uplink_by_obj (id1, id)) == -1)
    {
      edma_printf_err ("[edma_link_superclass] Object %ld not found "
		       "in subobject list on object %ld [%s]", 
		       id, id1, gClass[gObj[id1]->IdClass]->ClassName);
      return -1;
    }

  /* We have found the anchor points. Now, we create the object to insert */
  if ((subobj = _edma_new_obj (gClass[cid]->ClassName, id, NULL)) == -1)
    {
      edma_printf ("[edma_link_superclass] Can't create object of class [%s]." 
		   "Can't link object", gClass[cid]->ClassName);
      return -1;
    }
  /* Now we can relink the objects */
  /* First we link the inserted superobject */
  edma_add_superobject (subobj, id, gObj[id1]->UpTable[i2].Id);
  edma_add_subobject (subobj, id1, gObj[id]->DownTable[i1].Id);
  
  /* Update object tables*/
  gObj[id]->DownTable[i1].Obj = subobj;
  gObj[id]->DownTable[i1].IdClass = cid;
  gObj[id1]->UpTable[i2].Obj = subobj;
  gObj[id1]->UpTable[i2].IdClass = cid;
  
  return subobj;
}


/* edma_inser_subobject
 *   Inserts a new subobject at anchor point aponint1
 */

OBJID EDMAPROC
edma_insert_subobject (OBJID id, OBJID subobj, EPChar apoint1, EPChar apoint2)
{
  ESint32    i1, i2;  
  OBJID      id1;

  /* FIXME: Maybe parameter-checker wrapper */
  if ((edma_check_obj_id (id, "edma_insert_subobject")) == -1)
    return -1;

  if ((edma_check_obj_id (subobj, "edma_insert_subobject")) == -1)
    return -1;

  if ((apoint1 == NULL) || (apoint2 == NULL))
    {
      edma_log ("%s", "[edma_insert_subobject] WARNNING:: "
		"Invalid Anchor Point");
      /*return -1;*/
    }

  /* Lookup anchor point in current object*/
  if ((i1 = _edma_locate_downlink_by_name (id, apoint1)) == -1)
    { /* We have found the anchor point */
      edma_printf_err ("[edma_link_subclass] Anchor point %s already exists"
		       " on object %ld [%s]", 
		       apoint1, id, gClass[gObj[id]->IdClass]->ClassName);
      return -1;
    }
  
  id1 = gObj[id]->DownTable[i1].Obj;

  /* Lookup current object from superobject */
  if ((i2 = _edma_locate_uplink_by_obj (id1, id)) == -1)
    {
      edma_printf_err ("[edma_link_superclass] Object %ld not found "
		       "in subobject list on object %ld [%s]", 
		       id, id1, gClass[gObj[id1]->IdClass]->ClassName);
      return -1;
    }

  /* Now we can relink the objects */
  /* First we link the inserted superobject */
  edma_add_superobject (subobj, id, gObj[id1]->UpTable[i2].Id);
  edma_add_subobject (subobj, id1, gObj[id]->DownTable[i1].Id);
  
  /* Update object tables*/
  gObj[id]->DownTable[i1].Obj = subobj;
  gObj[id]->DownTable[i1].IdClass = gObj[subobj]->IdClass;
  gObj[id1]->UpTable[i2].Obj = subobj;
  gObj[id1]->UpTable[i2].IdClass = gObj[subobj]->IdClass;
  
  return subobj;
}
/************************************************************************/
