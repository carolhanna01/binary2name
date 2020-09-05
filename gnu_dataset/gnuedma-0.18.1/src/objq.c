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
 * Modulo de Query de oOnjetos
 * Revisiones: ---------------------------------------------------------
 * 14 de Agosto de 1997
 * ---------------------
 * 22 de Septiembre de 1997
 * Añadimos funciones para el Object INspector
 * -----------------------------------
 * 4 de Enero de 1998
 * Eliminamos las funciones de objetos nombrados
 * ------------------------------------------
 * 10 de Mayo de 1999
 * Modificamos ObjReport para que muestre información de subobjetos y superobjetos
 *------------------------------------------
 * Febraury, 7th, 2001
 * Code cleanup and comment translation
 * ------------------------------------------------
 * November, 17th, 2001
 * Compile warnnings removal
 * --------------------------------------------------------------
 * December, 8th, 2001
 * Minor bug fixed in ObjReport about virtual method information showed
 * ---------------------------------------------------------------
 * march, 2nd, 2002
 * Code cleanup
 * ----------------------------------------------------------------
 * March 17th, 2002
 * Using new helper function to check object identifiers
 * New report functions to show run-time object's class hierarchy
 * -------------------------------------------------------------------------
 * April, 2nd, 2003
 * Changes to support the new OBJ struct
 * ---------------------------------------------------------------------
 * May, 10th, 2003
 * Modification to supportr changes to internal class structures
 * -------------------------------------------------------------------
 * July, 4th, 2003
 * Input parameters sanity checks
 */
 
#include <string.h>
#include "portable.h"
#include "vglobal.h"
#include "const.h"
#include "tobj.h"
#include "obj.h"
#include "sys31.h"
#include "misc.h"
#include "helper.h"

/* edma_get_obj_pobj
 *   Get the pointer to a given object
 */

POBJ EDMAPROC 
edma_get_obj_pobj (OBJID IdObj)
{
  if ((edma_check_obj_id (IdObj, "GetPObj")) == -1)
    return NULL;

  return (gObj[IdObj]);
}

/* edma_get_obj_father_id
 *   Returns the father object of the given object
 */

OBJID EDMAPROC 
edma_get_obj_father_id (OBJID IdObj)
{
  if ((edma_check_obj_id (IdObj, "edma_get_obj_father_id")) == -1)
    return -1;

  return gObj[IdObj]->Father;
}

/* edma_get_obj_pseudofather_id
 *   Returns the pseudofather object of the given object
 */

OBJID EDMAPROC 
edma_get_obj_pseudofather_id (OBJID IdObj)
{
  if ((edma_check_obj_id (IdObj, "edma_get_obj_pseudofather_id")) == -1)
    return -1;

  return gObj[IdObj]->PseudiFather;
}

/* edma_get_obj_class_name
 *    Returns the class name of the given object
 *    Calling function must provide storage space for the class name string
 */

EPChar EDMAPROC 
edma_get_obj_class_name (OBJID IdObj, EPChar *name)
{
  if ((edma_check_obj_id (IdObj, "edma_get_obj_class_name")) == -1)
    return NULL;

  if ((name == NULL) || (*name == NULL))
    {
      edma_printf_err ("%s", "[edma_get_obj_class_name] Invalid target buffer "
		       "(NULL)");
      return NULL;
    }
  strncpy (*name, gClass[gObj[IdObj]->IdClass]->ClassName, EDMA_CLASS_NAME_LEN);
  return *name;
}

/* edma_get_obj_class_id
 *   Returns the class identifier of the given object
 */

CLASSID EDMAPROC 
edma_get_obj_class_id (OBJID IdObj)
{
  if ((edma_check_obj_id (IdObj, "edma_get_obj_class_id")) == -1)
    return -1;

  return gObj[IdObj]->IdClass;
}

/* edma_get_obj_status
 *   Returns status of a given object
 */

ESint32 EDMAPROC 
edma_get_obj_status (OBJID Id)
{
  if ((edma_check_obj_id (Id, "edma_get_obj_status")) == -1)
    return -1;

  return gObj[Id]->Flag;
}

/* edma_get_num_objects
 *   Returns the number of current object in the system
 */

EUint32 EDMAPROC 
edma_get_num_objects (void)
{
  return nObj;
}

/* edma_get_obj_num_superobjects
 *    Returns the number of superobject for a given object
 */

ESint32 EDMAPROC 
edma_get_obj_num_superobjects (OBJID Id)
{
  if ((edma_check_obj_id (Id, "edma_get_obj_num_superobjects")) == -1)
    return -1;

  return gObj[Id]->nUpTable;
}

/* edma_get_obj_superobject
 *   Returns the i-th superobject linked to a given object
 */

OBJID EDMAPROC 
edma_get_obj_superobject (OBJID Id, ESint32 i)
{
  if ((edma_check_obj_id (Id, "edma_get_obj_superobject")) == -1)
    return -1;
  
  if ((i < 0) && (i > gObj[Id]->nUpTable)) 
    {
      edma_printf_err ("[GetObjSC] Invalid superobject index: "
		       "%d of %d in object %d",
		       i, gObj[Id]->nUpTable, Id);
      return -1;
		
    }

  return (gObj[Id]->UpTable[i].Obj);
}

/* edma_get_obj_superobject_ap
 *    Returns the anchor point associated to the i-th superobject 
 * of a given object
 */

ESint32 EDMAPROC 
edma_get_obj_superobject_ap (OBJID Id, ESint32 i, EPChar ap)
{
  if ((edma_check_obj_id (Id, "edma_get_obj_superobject")) == -1)
    return -1;

  if (ap == NULL)
    {
      edma_printf_err ("%s", "[edma_get_obj_superobject_ap] Invalid "
		       "Anchor Point (NULL)");
      return -1;
    }
  
  if ((i < 0) && (i > gObj[Id]->nUpTable)) 
    {
      edma_printf_err ("[GetObjSC] Invalid superobject index: "
		       "%d of %d in object %d",
		       i, gObj[Id]->nUpTable, Id);
      return -1;
		
    }
  strcpy (ap, gObj[Id]->UpTable[i].Id);

  return 0;
}

/* edma_get_obj_num_subobject
 *    Returns the number of subobjects linked to a gicen object
 */

ESint32 EDMAPROC 
edma_get_obj_num_subobjects (OBJID Id)
{
  if ((edma_check_obj_id (Id, "edma_get_obj_num_subobjects")) == -1)
    return -1;

  return gObj[Id]->nDownTable;
}

/* edma_get_obj_subobject
 *    Returns the i-th subobject associated to a given object
 */

OBJID EDMAPROC 
edma_get_obj_subobject (OBJID Id, ESint32 i)
{
  if ((edma_check_obj_id (Id, "edma_get_obj_subobject")) == -1)
    return -1;
  
  if ((i < 0) && (i > gObj[Id]->nDownTable)) 
    {
      edma_printf_err ("[GetObjSC] Invalid superobject index: "
		       "%d of %d in object %d",
		       i, gObj[Id]->nDownTable, Id);
      return -1;
		
    }
  return (gObj[Id]->DownTable[i].Obj);
}

/* edma_get_obj_subobject_ap
 *   Returns the associated anchor point to the i-th subobject of a given object
 */

ESint32 EDMAPROC 
edma_get_obj_subobject_ap (OBJID Id, ESint32 i, EPChar ap)
{
  if ((edma_check_obj_id (Id, "edma_get_obj_subobject")) == -1)
    return -1;

  if (ap == NULL)
    {
      edma_printf_err ("%s", "[edma_get_obj_subobject_ap] Invalid "
		       "Anchor Point (NULL)");
      return -1;
    }
  
  if ((i < 0) && (i > gObj[Id]->nDownTable)) 
    {
      edma_printf_err ("[GetObjSC] Invalid superobject index: "
		       "%d of %d in object %d",
		       i, gObj[Id]->nDownTable, Id);
      return -1;
		
    }
  strcpy (ap, gObj[Id]->DownTable[i].Id);
  return 0;
}

/*
 * DEPRECATED
 */

OBJID EDMAPROC 
edma_get_obj_app (OBJID Id)
{
  if ((edma_check_obj_id (Id, "edma_get_obj_app")) == -1)
    return -1;

  return gObj[Id]->IdApp;
}

/*
 * DEPRECATED
 */

EUint32 EDMAPROC 
edma_get_app_id (void)
{
  return AppId;
}

/*******************************************************************************/
/* Report and Debug helper functions 
********************************************************************************/

/* edma_obj_report
 *   Dumps to stdout all the information related to a given object
 *   Debug Function
 */

ESint32 EDMAPROC 
edma_obj_report (OBJID IdObj)
{
  EUint32	i;
  OBJID		Padre;
  
  if ((edma_check_obj_id (IdObj, "edma_obj_report")) == -1)
    return -1;
  
  edma_printf ("==OBJECT REPORT==============================================");
  if (gObj[IdObj] == NULL) 
    {
      edma_printf ("Object is free");
      edma_printf ("==END OBJECT REPORT======================================");
      return 0;
    }
  
  if (gObj[IdObj]->Flag == VIRTUAL_OBJECT)
    edma_printf ("Object is virtual");
  
  edma_printf ("Object (%d) of class (%s)", IdObj,
	       gClass[gObj[IdObj]->IdClass]->ClassName);
  Padre = gObj[IdObj]->Father;

  if (Padre == -1)
    edma_printf ("Top Level Object. Object haven't father");
  else
    edma_printf ("Object Father (%d) of class (%s)", Padre,
		 gClass[gObj[Padre]->IdClass]->ClassName);
  
  Padre = gObj[IdObj]->PseudiFather;
  if (Padre == -1)
    edma_printf ("No PseudoFather information");
  else
    edma_printf ("Object PseudoFather (%d) of class (%s)", Padre,
		 gClass[gObj[Padre]->IdClass]->ClassName);
  
  edma_printf ("Object has %d superobjects", gObj[IdObj]->nUpTable);

  for (i = 0; i < gObj[IdObj]->nUpTable; i++)
    edma_printf("    Id : (%s) Class : (%s) Class attached Object: (%s)",
		gObj[IdObj]->UpTable[i].Id,
		gClass[gObj[IdObj]->UpTable[i].IdClass]->ClassName,
		gClass[gObj[gObj[IdObj]->UpTable[i].Obj]->IdClass]->ClassName);
  
  edma_printf ("Object has %d subobjects", gObj[IdObj]->nDownTable);
  for (i = 0; i < gObj[IdObj]->nDownTable; i++)
    edma_printf("    Id : (%s) Class : (%s) Class attached Object: (%s)",
		gObj[IdObj]->DownTable[i].Id,
		gClass[gObj[IdObj]->DownTable[i].IdClass]->ClassName,
		gClass[gObj[gObj[IdObj]->DownTable[i].Obj]->IdClass]->ClassName);
  
  edma_printf ("Virtual Methods in object");
  for (i = 0; i < gClass[gObj[IdObj]->IdClass]->nMetVir; i++)
    if (gObj[IdObj]->vTable[i].Flag) 
      {
	if (gObj[IdObj]->vTable[i].Flag == 1)
	  edma_printf ("    Object [%03d] of Class [%s] overrides "
		       "Method [%s] with Method [%s]",
		       ((POBJ)gObj[IdObj]->vTable[i].Obj)->IdObj,
		       gClass[((POBJ)gObj[IdObj]->vTable[i].Obj)->IdClass]->ClassName,
		       pClass[gObj[IdObj]->IdClass]->Met[gObj[IdObj]->vTable[i].Ind].IdMet,
		       gObj[IdObj]->vTable[i].Id);
	else
	  edma_printf ("    Main Application overrides Method [%s]",
		       gObj[IdObj]->vTable[i].Id);
      }
    else
      edma_printf ("    Nobody overrides Method [%s]", 
		   gObj[IdObj]->vTable[i].Id);     
  
  edma_printf ("==END OBJECT REPORT==========================================");
  return 0;
}

/* edma_show_subobjects_up
 *   Dumps to stdout the superobject hierarchy tree
 */

ESint32 EDMAPROC
edma_show_subobjects_up (OBJID IdObj,EPChar Id, ESint32 level) 
{
  ESint32    i;
  EChar      sep[40];

  if ((edma_check_obj_id (IdObj, "edma_show_subobjects_up")) == -1)
    return -1;

  if (Id == NULL)
    {
      edma_printf_err ("%s", "[edma_show_subobjects_up] Invalid "
		       "Root Identifier (NULL)");
      return -1;
    }

  memset (sep,0,40);
  memset (sep,' ',2 * level);
  edma_printf ("%s%s ==> %s [%d]", sep,
	       Id, gClass[gObj[IdObj]->IdClass]->ClassName,
	       IdObj);

  for (i = 0; i < gObj[IdObj]->nUpTable; i++) {
    edma_show_subobjects_up (gObj[IdObj]->UpTable[i].Obj, 
			     gObj[IdObj]->UpTable[i].Id, 
			     level + 1);
  }
  
  return 0;
}

/* edma_show_subobjects_up
 *   Dumps to stdout the subobject hierarchy tree
 */

ESint32 EDMAPROC
edma_show_subobjects_down (OBJID IdObj,EPChar Id, ESint32 level) 
{
  ESint32    i;
  EChar      sep[40];

  if ((edma_check_obj_id (IdObj, "edma_show_subobjects_down")) == -1)
    return -1;

  if (Id == NULL)
    {
      edma_printf_err ("%s", "[edma_show_subobjects_up] Invalid "
		       "Root Identifier (NULL)");
      return -1;
    }

  memset (sep,0,40);
  memset (sep,' ',2 * level);

  for (i = 0; i < gObj[IdObj]->nDownTable; i++) {
    edma_show_subobjects_down (gObj[IdObj]->DownTable[i].Obj, 
			       gObj[IdObj]->DownTable[i].Id, 
			       level + 1);
  }
  edma_printf ("%s%s ==> %s [%d]", sep 
	       , Id, gClass[gObj[IdObj]->IdClass]->ClassName
	       , IdObj);  
  return 0;
}

/* edma_show_object_interfaces
 *   Dumps to stdout the interfaces implemented by a given object
 *   DEBUG Function
 */

ESint32 EDMAPROC
edma_show_object_interface (OBJID IdObj) 
{
  ESint32    i, n;
  CLASSID    idc;
  EChar      modifiers[80];

  if ((edma_check_obj_id (IdObj, "edma_show_subobjects_down")) == -1)
    return -1;

  /* Write properties */
  idc = gObj[IdObj]->IdClass;
  n = gClass[idc]->nProp;

  /* FIXME: Show Property value*/
  for (i = 0; i < n; i++)
    edma_printf ("%s \t\t %s::%s", 
		 tipo[pClass[idc]->Prop[i].Tipo].Id,
		 gClass[idc]->ClassName,
		 pClass[idc]->Prop[i].IdProp);



  n = gClass[idc]->nMet;
  for (i = 0; i < n; i++)
    {
      memset (modifiers, 0, 80);
      if (pClass[idc]->Met[i].Virtual) 
	strcat (modifiers, "virtual ");
      else
	strcat (modifiers, "\t");
      if (pClass[idc]->Met[i].Abstract) 
	strcat (modifiers, "abstract ");
      else
	strcat (modifiers, "\t");
      if (pClass[idc]->Met[i].Static) 
	strcat (modifiers, "static "); 
      else
	strcat (modifiers, "\t");

      edma_printf ("%s %s::%s (%s)", 
		   modifiers,
		   gClass[idc]->ClassName,
		   pClass[idc]->Met[i].IdMet,
		   pClass[idc]->Met[i].Sign);
    }
  for (i = 0; i < gObj[IdObj]->nUpTable; i++) {
    edma_show_object_interface (gObj[IdObj]->UpTable[i].Obj);
  }
  return 0;
}
