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
 * REVISIONES:-------------------------------------------------------------
 * 15 de Junio de 1997
 * Este módulo incorpora las funciones básicas para la gestión de
 * objetos.
 *
 * Tenemos que mapear las DLL´s que contienen la implementación de las clases
 * en el espacio de direccionamiento del proceso que invoca las primitivas de
 * creación de objetos. A la fecha de hoy simplemente hacemos un "LoadLibrary"
 * cada vez que se crea un objeto, pero esto sólo tenemos que hacerlo por cada
 * proceso que se mapea en EDMA32.DLL.
 *
 * Con las clases de STOCK no debería haber problema, puesto que es el módulo
 * de inicialización principal el que las carga en memoria y las mapea a cada
 * proceso cada vez que carga EDMA.DLL. Una clase definida dinámicamente solo
 * sería accesible al proceso que la creó, aunque todos puedan verla (tendrían
 * que realizar el mapeo a mano)...
 *-----------------------------------------------------------------------------
 * 7 de Julio de 1997
 * Añadimos soporte para herencia estática si se han modificado las
 * funciones de creación de objetos para construir las estructuras adecuadas.
 * Se siguen utilizando punteros a los objetos internamente en algunas funciones
 * por comodidad y rapidez, pero ahora los subobjetos se crean en la matriz del sistema
 * permitiendo mayor control para el "Garbaje Collection"
 * ----------------------------------------
 * 13 de Julio de 1997
 * Movemos GetPObj al módulo de QUery de objetos
 * ----------------------------------
 * 19 de Julio de 1997
 * Añadimos soporte para el mapeo de las clases para cada proceso que lo necesite
 * y solo cuando lo necesite
 * --------------------------------------------------
 * 28 de Julio de 1997
 * Modificación para soporte de carga dinámica de implementación de clases
 * bajo de manda
 * -------------------------------------
 * 7 de Agosto de 1997
 * Añadimos soporte para ejecución de constructores y destructores.
 * Los contructores todavía no soportan polimorfismo.
 * --------------------------------------
 * 2 de Agosto de 1997
 * Hemos añadido el modificador final a los objetos que impide la herencia bajo
 * demanda.
 * Vamos a intentar modificar NewObj para indicar el subsistema del objeto
 * en el nombre de la clase.
 * -----------------------
 * 18 de Noviembre de 1997
 *    Modificaciones para soporte de tipos definidos por el usuario
 * Durante la creación del objeto, debemos crear un objeto por cada tipo
 * definido por el usuario
 * -------------------------------------
 * 10th May 1999
 *    Añadimos información de subclases para conocer el camino de vuelta 
 * en las relaciones de herencia estática.
 *-----------------------------------------------------------
 * Febraury, 7th, 2001
 * Code cleanup and comment translation
 * ---------------------------------------------------------------
 * November, 17th, 2001
 * Compile warnnings removal
 * -----------------------------------------------------------------
 * December, 3rd, 2001
 * Modification to use the preferer anchor points for static inheritance
 * when creating an object
 * -----------------------------------------------------------------------
 * December, 6th, 2001
 * Modification to make build objects process a mutistage procedure
 * We need this to allow dynamic inheritance functions build class
 * hierarchies correctly
 * -----------------------------------------------------------------------------
 * December, 8th, 2001
 * Updated NewObj_VMStage to autoimagically override homonym virtual methods
 * ----------------------------------------------------------------------------
 * January, 22th, 2002
 * Fixed problem with SIU Proxys and multistage NewObj. Now, it seems to work
 * properly. Extra testing needed
 * -----------------------------------------------------------------------------
 * March, 1st, 2002
 * Modifying NewObj to accept a variable parameter name in order to allow
 * to pass parameters to the constructor method if any
 * 
 * Code cleanup
 * ----------------------------------------------------------------------
 * April, 1st, 2003
 * Changes to support new object table structure
 * --------------------------------------------------------------------------
 * April, 17th, 2003
 * Casting and Clonning primitives were moved to other files
 * Added edma_swap_obj primitive
 * -------------------------------------------------------------------------------
 * April, 18th, 2003
 * Now object table grows dynamically. If there is no room to create a new object
 * the object table is increased (if there is available memory)
 * --------------------------------------------------------------------------------
 * May, 10th, 2003
 * Modification to support changes to internal class structures
 * ----------------------------------------------------------------------------------
 * July, 4th, 2003
 * Input Parameter sanity check
 * -------------------------------------------
 * Febraury, 7th, 2004
 * Rework object creation functions. Extension to work with class version information
 * Code cleanup
 * --------------------------------------------------------------------------------------
 * Febraury, 24th, 2004
 * Fixed super-object table initialization on object creation inheritance stage
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

/* For now the amount used to increment object table is fixed
 * Maybe we should calculate it as a percent of current object table size???
 */
#define OBJ_INC 2048

/* _edma_new_obj_basic_stage
 *   This function implements the basic stage in object creation.
 *   Basically allocates memory for object data structures and initialices
 *   general object properties
 */

OBJID EDMAPROC 
_edma_newobj_basic_stage (CLASSID IdC, CLASSID isSIU, EPVoid ConstPar) 
{
  EUint32	i;
  OBJID		IdO;
  HMEM		h;
  
  /* We map the library which creates the object for this process */
  if (ProcMapTable[IdC] != CLASS_LOADED) 
    if ((edma_load_class_imp (IdC)) == -1) 
      return _edma_system_exception ("[new_obj_basic_stage] Can't load "
				     "class '%s' implementation", 
				     gClass[IdC]->ClassName);

  /* We get a object identifier*/
  /* Update. Now we begin to look for a free identifier from 
   * the last free known */
  /* XXXX: There is a problem freeing objects. It seems that some 
   *       identifiers are not used*/

  pthread_mutex_lock (&obj_mutex); 
  for (i = 0; i < nMaxObj; i++)
    if (gObj[(nObj + i) % nMaxObj] == 0)
      break;
  
  if (i == nMaxObj) 
    {
      /* Here, we try to make object table bigger */
      if ((hSObj = edma_prealloc (hSObj, sizeof(OBJ*) 
				  * (nMaxObj + OBJ_INC))) == (HMEM)0)
	{
	  pthread_mutex_unlock (&obj_mutex);
	  return _edma_system_exception ("%s", "[edma_new_obj_basic_stage] "
					 "Can't create more objects");
	}
      SObject = edma_pget (hSObj);
      memset ((SObject + nMaxObj), 0, OBJ_INC * sizeof(OBJ*));
      /* We get as valid object identifier the first item in the 
       * new allocated table*/
      i = nMaxObj;
      nMaxObj = nMaxObj + OBJ_INC;
      edma_printf ("[INFO] Object table extended to %d Objects", nMaxObj);
    }

  IdO = (nObj + i) % nMaxObj;

  if ((gObj[IdO] = (OBJ*) malloc (sizeof(OBJ))) == NULL)
    {
      pthread_mutex_unlock (&obj_mutex);
      return _edma_system_exception ("%s", "[edma_new_obj_basic_stage}"
				     "Can't alloc memory for object struct");
    }

  pthread_mutex_unlock (&obj_mutex);
  memset (gObj[IdO], 0, sizeof(OBJ));
  gObj[IdO]->Flag = OBJ_LOCKED; 

  /* We create a data block for this object*/
  h = edma_palloc (gClass[IdC]->TamDatos);
  if (h == (HMEM)0) 
    {
      free (gObj[IdO]);
      gObj[IdO] = NULL;
      return _edma_system_exception ("%s", "[edma_new_obj_basic_stage "
				     "Can't create local storage for object");
    }
  
  gObj[IdO]->SysObj.hData = h;
  gObj[IdO]->Data = (EPVoid) edma_pget (h);
  memset (gObj[IdO]->Data, 0, gClass[IdC]->TamDatos);

  edma_log ("Allocated %d bytes for object of class '%s':%d at %p", 
	    gClass[IdC]->TamDatos, gClass[IdC]->ClassName, 
	    IdC, gObj[IdO]->Data);

  
  /* Here, the object has been physically created */
  /* We setup general information for it */
  gObj[IdO]->IdObj = IdO;
  gObj[IdO]->IdClass = IdC;
  gObj[IdO]->IdApp = AppId;
  gObj[IdO]->Father = -1;
  gObj[IdO]->PseudiFather = -1;
  gObj[IdO]->Flag = OBJ_EXIST;
  gObj[IdO]->IdSIU = isSIU;
  gObj[IdO]->Final = 0;
 
  nObj++;
  /* The two initialization bellow shouldn't be necessary. Check it*/
  gObj[IdO]->nUpTable = 0;
  gObj[IdO]->nDownTable = 0;

  gObj[IdO]->SysObj.hUpTable = 0;
  gObj[IdO]->SysObj.hDownTable = 0;

  return IdO;
}


/**************************************************************************/

/*----------------------------------------------------------------------------*/
/* _edma_new_obj_vm_stage
 *   This functions implements the Virtual Method stage on object creation
 *   Basically constructs the vtbl for the object
 */

OBJID EDMAPROC 
_edma_newobj_vm_stage (OBJID IdO) 
{
  CLASSID	IdC;
  EUint32	i,j;

  IdC = gObj[IdO]->IdClass;
  /* First we must to alloc memory for virtual methods */
  if (gClass[IdC]->nMetVir) 
    {
      gObj[IdO]->SysObj.hvTable = 
	edma_palloc (gClass[IdC]->nMetVir * sizeof (PMetVir));

      gObj[IdO]->vTable = (PMetVir*) edma_pget (gObj[IdO]->SysObj.hvTable);
      if (gObj[IdO]->SysObj.hvTable == 0) 
	return _edma_system_exception ("%s", "[edma_new_obj_vm_stage] "
				       "Can't allocate virtual table");
      j = 0;
      /* Initialize Virtual Method table for Object IdO*/
      for (i = 0; i < gClass[IdC]->nMet; i++) 
	{
	  if (pClass[IdC]->Met[i].Virtual) 
	    {
	      gObj[IdO]->vTable[j].Ind = i;
	      strncpy ((char*) gObj[IdO]->vTable[j].Id, 
		       pClass[IdC]->Met[i].IdMet,
		       EDMA_GENERAL_ID_LEN);

	      gObj[IdO]->vTable[j].Func = (PPROC)pClass[IdC]->met_func[i].Func;
	      gObj[IdO]->vTable[j].Obj  = gObj[IdO];
	      gObj[IdO]->vTable[j].next = NULL;
	      j++;
	    }
	}
    }
  return 0;
}

/*---------------------------------------------------------------------------*/

/* _edma_new_obj_inh_stage
 *   This function implements the inheritance stage in object creation
 *   Basically process static inheritance information from the object's class
 *   and creates the required subobjects to setup the class hierarchy
 */

/* FIXME: Modify this code to link current object with the Father one passed 
 * as a parameter. This will be used from dynamic inheritance prmitives.
 *
 * Probably we'll need to pass a OBJID vector for this function              */

OBJID EDMAPROC 
_edma_newobj_inh_stage (OBJID IdO, OBJID Father, EPVoid ConstPar) 
{
  EUint32	i, must_link, j;
  CLASSID	IdC;
  OBJID         id;
  EPChar        ap_id;
  ESint32       flag=0;

  IdC = gObj[IdO]->IdClass;
  /* If the object's class is derived... we create subobjects */
  if (gClass[IdC]->Derived) 
    {
      /* We build a table for storing subobject information */
      gObj[IdO]->nUpTable = gClass[IdC]->Derived;
      gObj[IdO]->SysObj.hUpTable = 
	edma_palloc (gClass[IdC]->Derived * sizeof (SC_ID));
      gObj[IdO]->UpTable = (SC_ID*) edma_pget (gObj[IdO]->SysObj.hUpTable);
      if (gObj[IdO]->SysObj.hUpTable == 0) 
	return _edma_system_exception ("%s", "[edma_new_obj_inh_stage] "
				       "Can't create superclass table");

      for (i = 0; i < gClass[IdC]->Derived; i++) gObj[IdO]->UpTable[i].Obj = -1;
      for (i = 0; i < gClass[IdC]->Derived; i++) 
	{
	  id = -1;
	  gObj[IdO]->UpTable[i].Obj = -1;
	  /* We create the subobject*/
	  edma_printf_dbg (4, -1, "[_edma_newobj_inh_stage] Creating "
			   "superobject of class %s",
			   gClass[pClass[IdC]->SCList[i]]->ClassName);
	  /* If father parameter is in the object's class SCList, 
	   * use the father parameter instead of create a new object */
	  must_link = 0;
	  if ((Father != -1) 
	      && (gObj[Father]->IdClass == pClass[IdC]->SCList[i])) 
	    {
	      id = Father;
	      flag = 1;
	      must_link = 1;
	    } 
	  else 
	    {
	      /* If no father provide, check if the superclass already exists.
	       * If exists, use that object as superclass subobject, 
	       * if not create a new object*/
	      /* First check from current object to test already 
	       * added superclasses */
	      if ((id = edma_upcast_obj (IdO, 
					 pClass[IdC]->SubCIdList[i])) == -1)
		{
		  must_link = 1;
		  /* if not found repeat from current root object */
		  if ((id = edma_upcast_obj (gObj[IdO]->root_obj, 
					     pClass[IdC]->SubCIdList[i])) == -1)
		    {
		      if ((id = _edma_new_obj (gClass[pClass[IdC]->SCList[i]]->ClassName, IdO, NULL)) == -1)
			{
			  return _edma_system_exception ("[%s] Can't create superobject of class %s", __FUNCTION__, gClass[pClass[IdC]->SCList[i]]->ClassName);
			}
		      must_link = 1;
		    }
		}
	    }

	  if (id == -1)
	    {
	      return _edma_system_exception ("[%s] Can't link superclass %s to object %d", __FUNCTION__, pClass[IdC]->SubCIdList[i]);
	    }

	  gObj[IdO]->UpTable[i].Obj = id;
	  if (pClass[IdC]->SubCIdList[i][0])
	    strncpy (gObj[IdO]->UpTable[i].Id, pClass[IdC]->SubCIdList[i], 
		     EDMA_GENERAL_ID_LEN);
	  else
	    strncpy (gObj[IdO]->UpTable[i].Id, 
		     gClass[pClass[IdC]->SCList[i]]->ClassName,
		     EDMA_GENERAL_ID_LEN);

	  gObj[IdO]->UpTable[i].IdClass = pClass[IdC]->SCList[i];

	  if (must_link) 
	    {
	      /* Check if the anchor point already exists*/
	      ap_id =  pClass[IdC]->SCIdList[i] 
		? pClass[IdC]->SCIdList[i] : gClass[IdC]->ClassName;
	      for (j = 0; j < gObj[id]->nDownTable; j++)
		if (strncmp (gObj[id]->DownTable[j].Id, ap_id, 
			     EDMA_GENERAL_ID_LEN) == 0)
		  break;
	      if (j == gObj[id]->nDownTable)
		edma_add_subobject (id, IdO, ap_id);
	      else
		edma_add_subobject (id, IdO, gClass[IdC]->ClassName);
	    }
	  
	}
    }
  
  return IdO;
}

/*-------------------------------------------------------------------------*/

/* _edma_new_obj_final_stage
 *    This function implements the final stage of object creation
 *    Basically creates user defined datatypes and executes object's 
 *    constructor if any
 */

OBJID EDMAPROC 
_edma_newobj_final_stage (OBJID IdO, EPChar ClassName, EPVoid ConstPar) 
{
  EUint32	i,j;
  CLASSID	IdC,IdC1;
  ESint32	isSIU;
  
  IdC = gObj[IdO]->IdClass;
  isSIU = gObj[IdO]->IdSIU;
  
  /* Here, we execute the constructor */
  for (i = 0; i < gClass[IdC]->nMet; i++)
    if (strncmp (pClass[IdC]->Met[i].IdMet, "born", 4) == 0)
      break;
  
  if (i != gClass[IdC]->nMet)
    {	/* if we have got one */
      edma_met1 (IdO, i, 0, NULL);
    }
  
  /* If the object is external using SIU, we invoke NewObj primitive
   * in the SIU proxy object
   */
  if (isSIU != -1) 
    {
      edma_met3 (IdO, "NewObj", ClassName);
    }
  
  gClass[gObj[IdO]->IdClass]->Used = 1;
  /*************************************************************************/
  /*** User Defined Datatypes ===> UNSTABLE FEATURE  --- NOT TESTED --- ****/
  /*************************************************************************/
  /* User datatypes extension... not fully tested --> 
   * !!!!!!! In fact this should be prior to constructor execution !!!!! */

  for (i = 0; i < gClass[IdC]->nProp; i++)
    if (pClass[IdC]->Prop[i].Tipo == DT_EUSER) 
      {
	IdC1 = pClass[IdC]->Prop[i].UserInfo;  /* Get datatype class*/
	/* Create user data type object*/
	j = (OBJID) edma_new_obj (gClass[IdC1]->ClassName, NULL); 
	/* We introduce the new object in the right place*/
	*((OBJID *) ((EPByte) gObj[IdO]->Data + pClass[IdC]->Prop[i].Off)) = j;
      }
  return IdO;
}


/* edma_new_obj
 *     The top-level NewObj Primitive. It doesn't change in order 
 * to don't modify availables apps
 */

/* FIXME: Do a cleanup on error:
 *        Free object identifier in object table
 *        Free object associated memory  
 */

OBJID EDMAPROC 
edma_new_obj (EPChar ClassNam1,...) 
{
  OBJID              id;
  CLASSID            cid, siu_cid;
  ESint32            off;
  va_list            ConstPar;

  /* Locate lates class version */
  if (( off = _edma_parse_class_name (ClassNam1, &cid, &siu_cid, -1, -1)) == -1)
    {
      edma_printf_err ("[edma_new_obj] Invalid Class Name '%s'", ClassNam1);
      return -1;
    }

  va_start(ConstPar,ClassNam1);
  id = _edma_new_obj_internal (ClassNam1 + off, cid, siu_cid, -1, ConstPar);
  va_end(ConstPar);

  return id;
}

OBJID EDMAPROC 
edma_new_obj_with_version (EPChar ClassNam1, 
			   ESint32 vmajor, ESint32 vminor,...) 
{
  OBJID              id;
  CLASSID            cid, siu_cid;
  ESint32            off;
  va_list            ConstPar;

  if (( off = _edma_parse_class_name (ClassNam1, &cid, &siu_cid, 
				      vmajor, vminor)) == -1)
    {
      edma_printf_err ("[edma_new_obj_with_version] Invalid Class Name '%s' "
		       "version %ld.%ld", ClassNam1, vmajor, vminor);
      return -1;
    }

  va_start(ConstPar,vminor);
  id = _edma_new_obj_internal (ClassNam1 + off, cid, siu_cid, -1, ConstPar);
  va_end(ConstPar);

  return id;
}


/* _edma_new_obj
 *    Object creation primitive for subobject creation. 
 *    Object is linked to the father parameter
 */

OBJID EDMAPROC 
_edma_new_obj (EPChar ClassNam1, OBJID father, EPVoid ConstPar) 
{
  OBJID              id;
  CLASSID            cid, siu_cid;
  ESint32            off;

  if (( off = _edma_parse_class_name (ClassNam1, &cid, &siu_cid, -1, -1)) == -1)
    {
      edma_printf_err ("[edma_new_obj] Invalid Class Name '%s'", ClassNam1);
      return -1;
    }

  id = _edma_new_obj_internal (ClassNam1 + off, cid, siu_cid, father, ConstPar);

  return id;
}


/* _edma_new_obj_internal
 *   Function to effectively create an object. Goes through the different 
 * object creation stages
 */

OBJID EDMAPROC
_edma_new_obj_internal (EPChar ClassNam1, CLASSID cid, CLASSID siu_cid, 
			OBJID idFather, EPVoid ConstPar)
{
  OBJID            id;

  /* First the basic stage. This allocs minimum memory requiremnts
     as well as, it does basic initialization*/
  if ((id = _edma_newobj_basic_stage (cid, siu_cid, ConstPar)) == -1)
    return _edma_system_exception ("[edma_new_obj] Basic Stage Failed."
				   "Can't create object of class '%s'", 
				   ClassNam1);

  gObj[id]->root_obj = ((idFather == -1) ? id : idFather);
  /* Then, process the static inheritance information */
  if ((_edma_newobj_inh_stage (id, idFather, ConstPar)) == -1) 
    return _edma_system_exception ("[edma_new_obj] Inheritance Stage Failed. "
				   " Can't create object of class '%s'", 
				   ClassNam1);

  /* Then the virtual method management*/
  if ((_edma_newobj_vm_stage (id)) == -1) 
    return _edma_system_exception ("[edma_new_obj] Virtual Method Stage Failed."
				   " Can't create object of class '%s'", 
				   ClassNam1);

  /* Last, run constructor if any*/
  if ((_edma_newobj_final_stage (id, ClassNam1, ConstPar)) == -1)
    return _edma_system_exception ("[edma_new_obj] Final Stage Failed. "
				   "can't create object of class '%s'", 
				   ClassNam1);

  return id;
}

/* This function is used by MULTISERIALIZATION Code ... 
 * Check up if it can be replaced by a combination of other functions 
 * FIXME: Rework MULTISERIALIZATION class, and add new primitives if required*/

OBJID EDMAPROC
edma_new_simple_obj (EPChar ClassNam1, EPVoid ConstPar)
  {
    OBJID            id;
    CLASSID          cid, siu_cid;
    ESint32          off;

  if (ClassNam1 == NULL)
    {
      edma_printf_err ("[_edma_new_simple_obj] Invalid Class Name (NULL)");
      return -1;
    }

  if (( off = _edma_parse_class_name (ClassNam1, &cid, &siu_cid, -1, -1)) == -1)
    {
      edma_printf_err ("[edma_new_simple_obj] Invalid Class Name '%s'", 
		       ClassNam1);
      return -1;
    }

  /* First the basic stage. This allocs minimum memory requiremnts
     as well as, it does basic initialization*/
  if ((id = _edma_newobj_basic_stage (cid, siu_cid, ConstPar)) == -1)
    return _edma_system_exception ("[edma_new_obj] Basic Stage Failed."
				   "Can't create object of class '%s'", ClassNam1);

  /* Then the virtual method management*/
  if ((_edma_newobj_vm_stage (id)) == -1) 
    return _edma_system_exception ("[edma_new_obj] Virtual Method Stage Failed."
				   " Can't create object of class '%s'", 
				   ClassNam1);

  /* Last, mark class as used. We don't want the contructor to be executed*/
  gClass[gObj[id]->IdClass]->Used = 1;

  return id;
}

/*****************************************************************************/
/*****************************************************************************/
/*****************************************************************************/
/*****************************************************************************/
/*****************************************************************************/

/* _edma_free_superobject
 *   Frees the superobjects associated to a given object
 */

ESint32 EDMAPROC
_edma_free_superobjects (OBJID IdObj, OBJID old, ESint32 remove)
{
  int i, n;
  POBJ          aux;

  /* THis is an internal function. We suppose parameters are ok at this point */
  /* FIXME: We'd add some asserts at this point */
  n = gObj[IdObj]->nUpTable;
  for (i = 0; i < n; i++) 
    {
      if (gObj[IdObj]->UpTable[0].IdClass != -1)
	{
	  /* Unlink superobject */
	  aux = gObj[gObj[IdObj]->UpTable[0].Obj];
	  edma_remove_superclass_ap (IdObj, gObj[IdObj]->UpTable[0].Id);

	  /* If superobject has no more subobjects remove it.*/
	  if ((aux->nDownTable == 0))
	      _edma_internal_free_obj (((POBJ)aux)->IdObj, IdObj, remove);
	}
      else
	edma_met3 (gObj[IdObj]->UpTable[0].Obj, "FreeObj");
    }
  
  /* We free table of subobjects*/
  if (gObj[IdObj]->nUpTable)
    edma_pfree (gObj[IdObj]->SysObj.hUpTable, gObj[IdObj]->UpTable);
  
  /* Delete internal object structs*/
  gObj[IdObj]->nUpTable = 0;
  gObj[IdObj]->UpTable = NULL;

  return 0;
}
/*****************************************************************************/

/* edma_fre_obj
 *     User entry point for freeing objects
 */

EUint32 EDMAPROC
edma_free_obj (OBJID IdObj)
{
  if ((edma_check_obj_id (IdObj, "edma_free_obj")) == -1)
    return -1;
  else
    return _edma_internal_free_obj (IdObj, IdObj, 0);
}

/* _edma_internal_free_obj
 *     Internal entry point for freeing objects
 */

EUint32 EDMAPROC 
_edma_internal_free_obj (OBJID IdObj, OBJID old, ESint32 flag) 
{
  EUint32	i, j, n;
  EUint32	IdClass;

  /* FIXME: Find out if this check is required....*/
  if ((edma_check_obj_id (IdObj, "_edma_internal_free_obj")) == -1)
    {
      return -1;
    }

  /* If the object is being removed, just return */
  if (gObj[IdObj]->Flag == OBJ_FREE)
    return 0;

  /* Mark the object as being removed. This should be thread safe*/
  gObj[IdObj]->Flag = OBJ_FREE;
  IdClass = gObj[IdObj]->IdClass;
  if (gObj[IdObj]->IdSIU != -1)	/*if we have a SIU proxy*/
    edma_met3 (IdObj, "FreeObj");
  
  /* Run destructor:
   * We run destructor at the begining in order to ensure
   * the whole object struct is available.*/

  /* We look for destructor method */
  for (i = 0; i < gClass[IdClass]->nMet; i++)
    if (strncmp (pClass[IdClass]->Met[i].IdMet, "rip", 3) == 0)
      break;
  
  if (i != gClass[IdClass]->nMet)	/* if we have got one*/
    edma_met1 (IdObj, i, 0, NULL);
  

  /****************************************************
   * Should we do this after freeing subobjects ?... I think YES!
   *******************************************************/
  /* User datatypes support... not fully tested*/
  for (i = 0;i < gClass[IdClass]->nProp; i++)
    if (pClass[IdClass]->Prop[i].Tipo == DT_EUSER) 
      {
	/*Get the object identifier form object struct*/
	j = * ((OBJID *) ((EPByte) gObj[IdObj]->Data + 
			  pClass[IdClass]->Prop[i].Off));
	edma_free_obj (j); /* and free it */
      }
  
  nObj--;
  if (nObj < 0)
    edma_print ("**** Ooops!! **** ---> (SYSTEM ERROR) "
		"Negative number of objects in system...");
  
  /* We decrement object's class ocurrences field */
  /* FIXME: This is not being used for now */
  //gClass[gObj[IdObj]->IdClass]->Ocurrences--;
  /**************************************************************************/

  /********************* TODO ****************************************
   * Here, we'll test if we've an stock class and we will free
   * class resources id necesary
   */
  
  /**************************************************************************/
  /* ===> We free subobjects */
  /* NOTE:
   *   When freeing subobject we pass as parameter the object in the top level
   *   edma_free_obj call in order to do not allow subobject to remove 
   *   superobjects above the top level object
   *
   *   This superobjects must be free from the top level object, 
   *   because they only can be destroyed if the top level object 
   *   is the only subobjects they are linked  to.... 
   *   Superobjects can be shared parents so they can be used by other subobject
   *   in another hierarchy branch
   */

  n = gObj[IdObj]->nDownTable;
#if 0
  for (i = 0; i < n; i++) 
    {
      if (gObj[IdObj]->DownTable[0].IdClass != -1) 
	_edma_internal_free_obj (gObj[IdObj]->DownTable[0].Obj, old, 1);
      else
	edma_met3 (gObj[IdObj]->DownTable[0].Obj, "FreeObj");
    }
#endif

  for (i = 0; i < n; i++) 
    {
      if (gObj[IdObj]->DownTable[i].IdClass != -1) 
	_edma_internal_free_obj (gObj[IdObj]->DownTable[i].Obj, old, 1);
      else
	edma_met3 (gObj[IdObj]->DownTable[i].Obj, "FreeObj");
      /* XXX: Reference counting not properly implemented
       *      At this point the subobject table of the current object
       *      can be deleted when freeing the subobject, in the case of
       *      shared parents are available in the hierarchy
       */
      if (gObj[IdObj]->nDownTable == 0) break;
    }

  /* We free table of subobjects*/
  if (gObj[IdObj]->nDownTable)
    edma_pfree (gObj[IdObj]->SysObj.hDownTable, gObj[IdObj]->DownTable);
  
  gObj[IdObj]->nDownTable = 0;
  gObj[IdObj]->DownTable = NULL;

  /* Here we are done with subobjects*/
  /**************************************************************************/
  /* Now, we go for the superobjects */
  if (flag)
    _edma_free_superobjects (IdObj, old, 1);
  else
    _edma_free_superobjects (IdObj, old, 0);
  /**************************************************************************/
 
  /*We free object data memory block*/
  edma_pfree (gObj[IdObj]->SysObj.hData, gObj[IdObj]->Data);
  /*We free virtual table information*/
  edma_pfree (gObj[IdObj]->SysObj.hvTable, gObj[IdObj]->vTable);
  gObj[IdObj]->vTable = NULL;
  
  /* Free IdSubSIU strdup*/
  if (gObj[IdObj]->IdSubSIU) 
    {
      free (gObj[IdObj]->IdSubSIU);
    }

  free (gObj[IdObj]);
  gObj[IdObj] = NULL;

  edma_printf_dbg (4, -1, "(%s) Rips Object %u of Class %s", AppName,
		   IdObj, gClass[IdClass]->ClassName);

  return 0;
}

/***************************************************************************/
/***************************************************************************/
/***************************************************************************/
/***************************************************************************/

/* edma_free_one_obj
 *   Fress a single object don't touching object hierarchy
 */

EUint32 EDMAPROC 
edma_free_one_obj (OBJID IdObj)
{
  EUint32	i,j;
  EUint32	IdClass;

  if ((edma_check_obj_id (IdObj, "edma_free_one_obj")) == -1)
    return -1;

  IdClass = gObj[IdObj]->IdClass;
  
  if (gObj[IdObj]->IdSIU != -1)	/*if we have a SIU proxy*/
    edma_met3 (IdObj, "FreeObj");
  
  /*We look for destructor method*/
  for (i = 0; i < gClass[IdClass]->nMet; i++)
    if (strncmp (pClass[IdClass]->Met[i].IdMet, "rip", 3) == 0)
      break;
  
  if (i != gClass[IdClass]->nMet)	/* if we have got one*/
    edma_met1 (IdObj, i, 0, NULL);
  
  /* User datatypes support... not fully tested*/
  for (i = 0; i < gClass[IdClass]->nProp; i++)
    if (pClass[IdClass]->Prop[i].Tipo == DT_EUSER) 
      {
	/*Get the object identifier form object struct*/
	j = *((OBJID *) ((EPByte) gObj[IdObj]->Data + 
			 pClass[IdClass]->Prop[i].Off));
	edma_free_obj (j); /* and free it */
      }
  
  /* Modificación temporal 14 de Agosot*/
  nObj--;
  if (nObj < 0)
    edma_print ("(SYSTEM ERROR) Negative number of objects in system...");
  /* FIn modificacion */
  
  /* We decrement object's class ocurrences field */
  /* FIXME: This is not being used for now */
  //gClass[gObj[IdObj]->IdClass]->Ocurrences--;
  /*
   * Here, we'll test if we've an stock class and we will free
   * class resources id necesary
   */
  
  /* We free subobjects. This function leaves  the related object  */
  /* We free table of subobjects*/
  edma_pfree (gObj[IdObj]->SysObj.hUpTable, gObj[IdObj]->UpTable);
  
  /*We free data memory block*/
  edma_pfree (gObj[IdObj]->SysObj.hData, gObj[IdObj]->Data);
  /*We free virtual method information*/
  edma_pfree (gObj[IdObj]->SysObj.hvTable, gObj[IdObj]->vTable);
  
  free (gObj[IdObj]);
  gObj[IdObj] = NULL;
  
  edma_printf_dbg (4,- 1, "(%s) Rips Object %u of Class %s", AppName,
		   IdObj, gClass[IdClass]->ClassName);

  return 0;
}

/**************************************************************
 ** Virtual Objects Support 
 ******************************************************************/

/* edma_make_obj_virtual
 *   DEPRECATED: Activates virtual flag on object
 */

EDWord EDMAPROC 
edma_make_obj_virtual (OBJID IdObj)
{
  if ((edma_check_obj_id (IdObj, "edma_make_obj_virtual")) == -1)
    return -1;

  gObj[IdObj]->Flag = VIRTUAL_OBJECT;
  return 0;
}

/* edma_set_obj_final
 *   Activates the final flag on object. 
 *   Prevents On-Demand inheritance to be applied on it
 */ 

EUint32 EDMAPROC 
edma_set_obj_final (OBJID IdObj,EUint32 i)
{
  EUint32	old;

  if ((edma_check_obj_id (IdObj, "edma_set_obj_final")) == -1)
    return -1;  

  old = gObj[IdObj]->Final;
  gObj[IdObj]->Final = i;

  return old;
}

/* edma_swap_obj
 *   Interchanges two objects
 */

ESint32 EDMAPROC
edma_swap_obj (OBJID id, OBJID new_id)
{
  POBJ     current;
  OBJID    aux;
  ESint32  i, j;
  
  if ((edma_check_obj_id (id, "edma_swap_obj")) == -1)
    return -1;

  if ((edma_check_obj_id (new_id, "edma_swap_obj")) == -1)
    return -1;

  /* FIXME: This should be inside a mutex */
  current = gObj[id];
  gObj[id] = gObj[new_id];
  gObj[new_id] = current;

  gObj[id]->IdObj = id;
  gObj[new_id]->IdObj = new_id;

  /* Now update subobject back links */
  for (i = 0; i < gObj[id]->nUpTable; i++)
    {
      aux = gObj[id]->UpTable[i].Obj;
      for (j = 0; j < gObj[aux]->nDownTable; j++)
	if (gObj[aux]->DownTable[j].Obj == new_id)
	  gObj[aux]->DownTable[j].Obj = id;
    }

  for (i = 0; i < gObj[id]->nDownTable; i++)
    {
      aux = gObj[id]->DownTable[i].Obj;
      for (j = 0; j < gObj[aux]->nUpTable; j++)
	if (gObj[aux]->UpTable[j].Obj == new_id)
	  gObj[aux]->UpTable[j].Obj = id;
    }

  /* Update the other object */
  for (i = 0; i < gObj[new_id]->nUpTable; i++)
    {
      aux = gObj[new_id]->UpTable[i].Obj;
      for (j = 0; j < gObj[aux]->nDownTable; j++)
	if (gObj[aux]->DownTable[j].Obj == id)
	  gObj[aux]->DownTable[j].Obj = new_id;
    }

  for (i = 0; i < gObj[new_id]->nDownTable; i++)
    {
      aux = gObj[new_id]->DownTable[i].Obj;
      for (j = 0; j < gObj[aux]->nUpTable; j++)
	if (gObj[aux]->UpTable[j].Obj == id)
	  gObj[aux]->UpTable[j].Obj = new_id;
    }

  return 0;
}




/***************************************************************************/
OBJID EDMAPROC 
_edma_newobj_inh_stage1 (OBJID IdO, OBJID Father, EPVoid ConstPar) 
{
  EUint32	i, must_link, j;
  CLASSID	IdC;
  OBJID         id;
  EPChar        ap_id;
  ESint32       flag=0;

  IdC = gObj[IdO]->IdClass;
  /* If the object's class is derived... we create subobjects */
  if (gClass[IdC]->Derived) 
    {
      /* We build a table for storing subobject information */
      gObj[IdO]->nUpTable = gClass[IdC]->Derived;
      gObj[IdO]->SysObj.hUpTable = 
	edma_palloc (gClass[IdC]->Derived * sizeof (SC_ID));
      gObj[IdO]->UpTable = (SC_ID*) edma_pget (gObj[IdO]->SysObj.hUpTable);
      if (gObj[IdO]->SysObj.hUpTable == 0) 
	return _edma_system_exception ("%s", "[edma_new_obj_inh_stage] "
				       "Can't create superclass table");

      for (i = 0; i < gClass[IdC]->Derived; i++) gObj[IdO]->UpTable[i].Obj = -1;
      for (i = 0; i < gClass[IdC]->Derived; i++) 
	{
	  id = -1;
	  gObj[IdO]->UpTable[i].Obj = -1;
	  /* We create the subobject*/

	  edma_printf_dbg (4, -1, "[_edma_newobj_inh_stage] Creating "
			   "superobject of class %s",
			   gClass[pClass[IdC]->SCList[i]]->ClassName);

	  /* If father parameter is in the object's class SCList, 
	   * use the father parameter instead of create a new object */
	  edma_printf_dbg (4, -1, "SuperClass %d: '%s'(%d) Father: %d: '%s'%d "
			   ":: IdO: %d",
			   i, gClass[pClass[IdC]->SCList[i]]->ClassName, 
			   pClass[IdC]->SCList[i],
			   Father, gClass[gObj[Father]->IdClass]->ClassName, 
			   gObj[Father]->IdClass, IdO);

	  must_link = 0;
	  if ((Father != -1) 
	      && (gObj[Father]->IdClass == pClass[IdC]->SCList[i])) 
	    {
	      
	      edma_printf_dbg (4, -1, "**SuperClass %d: '%s'(%d) Father: %d: "
			       "'%s'%d ",
			       i, gClass[pClass[IdC]->SCList[i]]->ClassName, 
			       pClass[IdC]->SCList[i], Father, 
			       gClass[gObj[Father]->IdClass]->ClassName, 
			       gObj[Father]->IdClass);

	      id = Father;
	      flag = 1;
	      must_link = 1;
	    } 
	  else 
	    {
	      /* If no father provide, check if the superclass already exists.
	         If exists, use that object as superclass subobject, 
                 if not create a new object*/
	      /* First check from current object to test already added 
		 superclasses */
	      if ((id = edma_upcast_obj (IdO, 
					 pClass[IdC]->SubCIdList[i])) == -1)
		{
		  must_link = 1;
		  /* if not found repeat from current root object */
		  if ((id = edma_upcast_obj (gObj[IdO]->root_obj, 
					     pClass[IdC]->SubCIdList[i])) == -1)
		    {
		      if ((id = _edma_new_obj1 (gClass[pClass[IdC]->SCList[i]]->ClassName, Father, NULL)) == -1)
			{
			  return _edma_system_exception ("[%s] Can't create superobject of class %s", __FUNCTION__, gClass[pClass[IdC]->SCList[i]]->ClassName);
			}
		      must_link = 1;
		    }
		}
	    }
	  
	  if (id == -1)
	    {
	      return _edma_system_exception ("[%s] Can't link superclass %s "
					     "to object %d", __FUNCTION__,
					     pClass[IdC]->SubCIdList[i]);
	    }
	  gObj[IdO]->UpTable[i].Obj = id;
	  if (pClass[IdC]->SubCIdList[i][0])
	    strncpy (gObj[IdO]->UpTable[i].Id, pClass[IdC]->SubCIdList[i], 
		     EDMA_GENERAL_ID_LEN);
	  else
	    strncpy (gObj[IdO]->UpTable[i].Id, 
		     gClass[pClass[IdC]->SCList[i]]->ClassName,
		     EDMA_GENERAL_ID_LEN);

	  gObj[IdO]->UpTable[i].IdClass = pClass[IdC]->SCList[i];

	  if (must_link) 
	    {
	      /* Check if the anchor point already exists*/
	      ap_id =  pClass[IdC]->SCIdList[i] ? pClass[IdC]->SCIdList[i] 
		: gClass[IdC]->ClassName;
	      for (j = 0; j < gObj[id]->nDownTable; j++)
		if (strncmp (gObj[id]->DownTable[j].Id, ap_id, 
			     EDMA_GENERAL_ID_LEN) == 0)
		  break;
	      if (j == gObj[id]->nDownTable)
		edma_add_subobject (id, IdO, ap_id);
	      else
		edma_add_subobject (id, IdO, gClass[IdC]->ClassName);
	    }
	  
	}
    }
  
  return IdO;
}



OBJID EDMAPROC 
_edma_new_obj1 (EPChar ClassNam1, OBJID father, EPVoid ConstPar) 
{
  OBJID              id;
  CLASSID            cid, siu_cid;
  ESint32            off;

  if (( off = _edma_parse_class_name (ClassNam1, &cid, &siu_cid, -1, -1)) == -1)
    {
      edma_printf_err ("[edma_new_obj] Invalid Class Name '%s'", ClassNam1);
      return -1;
    }

  id = _edma_new_obj_internal1 (ClassNam1 + off, cid, siu_cid, 
				father, ConstPar);
  return id;
}

/* _edma_new_obj_internal
 *   Function to effectively create an object. 
 *   Goes through the different object creation stages
 */

OBJID EDMAPROC
_edma_new_obj_internal1 (EPChar ClassNam1, CLASSID cid, CLASSID siu_cid, 
			 OBJID idFather, EPVoid ConstPar)
{
  OBJID            id;

  /* First the basic stage. This allocs minimum memory requiremnts
     as well as, it does basic initialization*/
  if ((id = _edma_newobj_basic_stage (cid, siu_cid, ConstPar)) == -1)
    return _edma_system_exception ("[edma_new_obj] Basic Stage Failed."
				   "Can't create object of class '%s'", 
				   ClassNam1);

  gObj[id]->root_obj = ((idFather == -1) ? id : idFather);
  /* Then, process the static inheritance information */
  if ((_edma_newobj_inh_stage1 (id, idFather, ConstPar)) == -1) 
    return _edma_system_exception ("[edma_new_obj] Inheritance Stage Failed. "
				   " Can't create object of class '%s'", 
				   ClassNam1);

  /* Then the virtual method management*/
  if ((_edma_newobj_vm_stage (id)) == -1) 
    return _edma_system_exception ("[edma_new_obj] Virtual Method Stage Failed."
				   " Can't create object of class '%s'", 
				   ClassNam1);

  /* Last, run constructor if any*/
  if ((_edma_newobj_final_stage (id, ClassNam1, ConstPar)) == -1)
    return _edma_system_exception ("[edma_new_obj] Final Stage Failed. "
				   "can't create object of class '%s'", 
				   ClassNam1);
  return id;
}

ESint32 EDMAPROC
edma_obj_commit_suicide (OBJID id)
{
  //if ((edma_check_obj_id1 (id, "edma_met3", id)) == -1) 
  if ((edma_check_obj_id (id, "edma_met3")) == -1) 
    return -1;

  gObj[id]->Flag = OBJ_DIE;
  return 0;
}
