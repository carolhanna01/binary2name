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
 * April, 12th, 2003
 * Method/property lookup algorithm changed to avoid lookup loops
 * -------------------------------------------------------------------
 * May, 10th, 2003
 * Modification to support changes to internal class structure
 * ---------------------------------------------------------------------------
 * August, 15th 2003
 * Update to take into account signature on method lookup
 * -------------------------------------------------------------------
 * August, 23th, 2003
 * Method lookup using dictionaries. First probe
 * -----------------------------------------------------------------------------
 * August, 20th, 2003
 * Changing all internal functions to work with POBJ instead of IdObj. Only high level
 * functions keeps IDOBJ as parameter (_edma_locate_met/prop)
 * This should be faster
 * -----------------------------------------------------------------------------------
 * September, 6th, 2003
 * Finished changes to locator functions.
 * -------------------------------------------------------------------------------------
 * January, 7th, 2004
 * Added missing anchor point locators
 */ 
#include <strings.h>
#include <stdlib.h>
#include "portable.h"
#include "vglobal.h"

#include "locators.h"
#include "misc.h"
#include "anchor_points.h"
#include "error.h"

ESint32 EDMAPROC _edma_look4_met_single (POBJ IdObj, EPChar MetName, 
					 EPChar Signature);
ESint32 EDMAPROC _edma_look4_met_ext_down (POBJ *Obj1, POBJ  ObjFrom, 
					   EPChar MetName, EPChar Signature);
ESint32 EDMAPROC _edma_look4_closer_met_up (POBJ *Obj1, POBJ  ObjFrom, 
					    EPChar MetName, EPChar Signature, 
					    ESint32 down);
ESint32 EDMAPROC _edma_look4_closer_met_down (POBJ *Obj1, POBJ  ObjFrom, 
					      EPChar MetName, EPChar Signature);


ESint32 EDMAPROC _edma_look4_prop_single (POBJ IdObj, EPChar PropName);
ESint32 EDMAPROC _edma_look4_prop_ext_down (POBJ *Obj1, POBJ  ObjFrom, 
					    EPChar MetName);
ESint32 EDMAPROC _edma_look4_closer_prop_up (POBJ *Obj1, POBJ  ObjFrom, 
					     EPChar MetName, ESint32 down);
ESint32 EDMAPROC _edma_look4_closer_prop_down (POBJ *Obj1, POBJ  ObjFrom, 
					       EPChar MetName);

#if 0
ESint32 EDMAPROC _edma_look4_prop_single (OBJID IdObj, EPChar PropName);
ESint32 EDMAPROC _edma_look4_prop_ext_down (OBJID *Obj1, OBJID  ObjFrom, 
					    EPChar MetName);
ESint32 EDMAPROC _edma_look4_closer_prop_up (OBJID *Obj1, OBJID  ObjFrom, 
					     EPChar MetName, ESint32 down);
ESint32 EDMAPROC _edma_look4_closer_prop_down (OBJID *Obj1, OBJID  ObjFrom, 
					       EPChar MetName);
#endif

/***************************************************************************/
/***************************************************************************
 * Helper functions to locate methods and properties in a single object
 ***************************************************************************/

/* _edma_look4_met_single
 *   Looks for the indicated method only in the given object
 */

ESint32 EDMAPROC
_edma_look4_met_single (POBJ pObj, EPChar MetName, EPChar Signature)
{

  ESint32  indx, key;

  key = -1;
  indx = edma_dict_get_next_index (pClass[pObj->IdClass]->met_dict, 
				   MetName, &key);
  if (Signature)
    do 
      {
	if ((strncmp (pClass[pObj->IdClass]->Met[indx].Sign, Signature, 
		      EDMA_MET_SIG_LEN)) == 0)
	  break;
	indx = edma_dict_get_next_index (pClass[pObj->IdClass]->met_dict, 
					 MetName, &key);
      } while (indx >= 0);
  
  return indx;
}

/* _edma_look4_prop_single
 *  Looks for the indicated property only in the given object
*/

ESint32 EDMAPROC
_edma_look4_prop_single (POBJ pObj, EPChar PropName)
{
  return edma_dict_get_index (pClass[pObj->IdClass]->prop_dict, PropName); 
}
/***************************************************************************/
/***************************************************************************/
/***************************************************************************/

/* edma_look4_met
 *  Looks for a given method processing choosing the lookup algorithm to use
 */

ESint32 EDMAPROC 
edma_look4_met (POBJ *pObj1, EPChar MetName1, EPChar Signature) 
{
  EPChar  MetName;
  ESint32 indx;
  POBJ    orig;

  /* Determine how to look for Method*/
  if (MetName1[0] == '.' )
    {
      MetName = MetName1 + 1;
      indx = _edma_look4_closer_met_down (pObj1, NULL, MetName, Signature);
      if (indx == -1)
	{
	  indx = _edma_look4_closer_met_up (pObj1, NULL, MetName, Signature, 1);
	}
      return indx;      
    }
  else
    {
      orig = *pObj1;
      indx = _edma_look4_met_single (orig, MetName1, Signature);
      if (indx == -1)
	indx = _edma_look4_closer_met_up (pObj1, orig, MetName1, Signature, 0);
      return indx;
    }
}

/**************************************************************************/
/**************************************************************************/
/**************************************************************************/
/**************************************************************************/
/**************************************************************************/

/*  edma_look4_met_ext
 *     Additional version of method search lookup algorithm 
 */
ESint32 EDMAPROC 
edma_look4_met_ext (POBJ *pObj, EPChar MetName1, EPChar Signature) 
{
  EPChar  MetName;
  ESint32 indx;
  POBJ    orig;

  /* Determine how to look for Method*/
  if (MetName1[0] == '.' )
    {
      MetName = MetName1 + 1;
      indx = _edma_look4_closer_met_down (pObj, NULL, MetName, Signature);
      if (indx == -1)
	{
	  indx = _edma_look4_closer_met_up (pObj, NULL, MetName, Signature, 1);
	}
      return indx;
    }
  else
    {
      orig = *pObj;
      indx = _edma_look4_met_ext_down (pObj, NULL, MetName1, Signature);
      if (indx == -1)
	{
	  indx = _edma_look4_closer_met_up (pObj, orig, MetName1, Signature, 1);
	}
      return indx;
    }
}


/* _edma_look4_met_ext_down
 *    Look for most general method in subobjects 
 */

ESint32 EDMAPROC 
_edma_look4_met_ext_down (POBJ *pObj1, POBJ pObjFrom, EPChar MetName, 
			  EPChar Signature) 
{
  EUint32     j, n;
  ESint32     Flag;
  POBJ        pObj, pObj_temp;
  SC_ID       *down;

  pObj = *pObj1;
  n = pObj->nDownTable;

  if (n)
    {
      down = pObj->DownTable;

      /* First we look in the subclasses for the most concret object*/
      for (j = 0; j < n; j++) 
	{
	  pObj_temp = gObj[down[j].Obj];
	  if (pObj_temp == pObjFrom)
	    continue;

	  Flag = _edma_look4_met_ext_down (&pObj_temp, pObj, 
					   MetName, Signature);
	  if (Flag != -1) 
	    {
	      *pObj1 = pObj_temp;
	      return Flag;
	    }
	}
    }

  /* Then we look for at level 0*/
  return _edma_look4_met_single (pObj, MetName, Signature);
}

/* _edma_look4_closer_met_up
 *     Look for closer matching method in superobject set
 */

ESint32 EDMAPROC 
_edma_look4_closer_met_up (POBJ *pObj1, POBJ pObjFrom, EPChar MetName, 
			   EPChar Signature, ESint32 down) 
{
  EUint32     j, n;
  ESint32     indx = -1;
  ESint32     Flag;
  POBJ        pObj;
  SC_ID       *up;
  
  pObj = *pObj1;
  n = pObj->nUpTable;

  if (n) /* If there is some superclass  */
    {  
      up = pObj->UpTable;
      indx = -1;
      for (j = 0; j < n; j++)
	{
	  *pObj1 = gObj[up[j].Obj];
	  if (*pObj1 == pObjFrom)
	    continue;

	  indx = _edma_look4_met_single (*pObj1, MetName, Signature);
	  if (indx != -1)
	    return indx;

	  Flag = _edma_look4_closer_met_up (pObj1, pObjFrom, 
					    MetName, Signature, down);
	  if (Flag != -1)
	    {
	      return Flag;
	    }
	}
    }
  else
    {
      if (down == 1)
	return _edma_look4_met_ext_down (pObj1, pObjFrom, MetName, Signature);
    }
  return indx;
}

/* _edma_look4_closer_met_down
 *    Look for closer matching methiod in subobject set
 */

ESint32 EDMAPROC 
_edma_look4_closer_met_down (POBJ *pObj1, POBJ pObjFrom, EPChar MetName, EPChar Signature) 
{
  EUint32     j, n;
  ESint32     Flag, indx;
  POBJ        pObj;
  SC_ID       *down;
  
  pObj = *pObj1;
  indx = _edma_look4_met_single (pObj, MetName, Signature);

  if (indx == -1) /* Method still not found */
    {
      n = pObj->nDownTable;
      down = pObj->DownTable;

      for (j = 0; j < n; j++)
	{
	  *pObj1 = gObj[down[j].Obj];
	  if (*pObj1 == pObjFrom)
	    continue;
	  Flag = _edma_look4_closer_met_down (pObj1, pObj, MetName, Signature);
	  if (Flag != -1)
	    {
	      return Flag;
	    }
	}
    }
  return indx;
}

/************************************************************************/
/************************************************************************/
/************************************************************************/

/* edma_look4_prop
 *   Process property name and choose appropriated lookup algorithm
 */

ESint32 EDMAPROC 
edma_look4_prop (POBJ *pObj1, EPChar PropName1) 
{
  EPChar  PropName;
  ESint32 indx;
  POBJ    orig;

  /* Determine how to look for Method*/
  if (PropName1[0] == '.' )
    {
      PropName = PropName1 + 1;
      indx = _edma_look4_closer_prop_down (pObj1, NULL, PropName);
      if (indx == -1)
	{
	  indx = _edma_look4_closer_prop_up (pObj1, NULL, PropName, 1);
	}
      return indx;      
    }
  else
    {

      orig = *pObj1;
      indx = _edma_look4_prop_single (orig, PropName1);
      if (indx == -1)
	indx = _edma_look4_closer_prop_up (pObj1, orig, PropName1, 0);
      return indx;
    }
}

/* edma_look4_prop_ext
 *   Extended version of edma_look4_prop
 */

ESint32 EDMAPROC 
edma_look4_prop_ext (POBJ *pObj, EPChar PropName1) 
{
  EPChar  PropName;
  ESint32 indx;
  POBJ    orig;

  /* Determine how to look for Method*/
  if (PropName1[0] == '.' )
    {
      PropName = PropName1 + 1;
      indx = _edma_look4_closer_prop_down (pObj, NULL, PropName);
      if (indx == -1)
	{
	  indx = _edma_look4_closer_prop_up (pObj, NULL, PropName, 1);
	}
      return indx;
    }
  else
    {
      orig = *pObj;
      indx = _edma_look4_prop_ext_down (pObj, NULL, PropName1);
      if (indx == -1)
	{
	  indx = _edma_look4_closer_prop_up (pObj, orig, PropName1, 1);
	}
      return indx;
    }
}



/* _edma_look4_prop_ext_down
 *    looks for most general property
 */

ESint32 EDMAPROC 
_edma_look4_prop_ext_down (POBJ *pObj1, POBJ pObjFrom, EPChar PropName) 
{
  EUint32     j, n;
  ESint32     Flag;
  POBJ        pObj, pObj_temp;
  SC_ID       *down;

  pObj = *pObj1;
  n = pObj->nDownTable;

  if (n)
    {
      down = pObj->DownTable;

      /* First we look in the subclasses for the most concret object*/
      for (j = 0; j < n; j++) 
	{
	  pObj_temp = gObj[down[j].Obj];
	  if (pObj_temp == pObjFrom)
	    continue;

	  Flag = _edma_look4_prop_ext_down (&pObj_temp, pObj, PropName);
	  if (Flag != -1) 
	    {
	      *pObj1 = pObj_temp;
	      return Flag;
	    }
	}
    }

  /* Then we look for at level 0*/
  return _edma_look4_prop_single (pObj, PropName);
}


/* _edma_look4_closer_prop_up
 *     Looks for closer matching property in superobject set
 */

ESint32 EDMAPROC 
_edma_look4_closer_prop_up (POBJ *pObj1, POBJ pObjFrom, 
			    EPChar PropName, ESint32 down) 
{
  EUint32     j, n;
  ESint32     indx = -1;
  ESint32     Flag;
  POBJ        pObj;
  SC_ID       *up;
  
  pObj = *pObj1;
  n = pObj->nUpTable;

  if (n) /* If there is some superclass  */
    {  

      up = pObj->UpTable;
      indx = -1;
      for (j = 0; j < n; j++)
	{
	  *pObj1 = gObj[up[j].Obj];
	  if (*pObj1 == pObjFrom)
	    continue;

	  indx = _edma_look4_prop_single (*pObj1, PropName);
	  if (indx != -1)
	    return indx;

	  Flag = _edma_look4_closer_prop_up (pObj1, pObjFrom, PropName, down);
	  if (Flag != -1)
	    {
	      return Flag;
	    }
	}
    }
  else
    {
      if (down == 1)
	return _edma_look4_prop_ext_down (pObj1, pObjFrom, PropName);
    }
  return indx;
}

/* _edma_look4_closer_prop_down
 *   Looks for closer matching property in subobject set
 */

ESint32 EDMAPROC 
_edma_look4_closer_prop_down (POBJ *pObj1, POBJ pObjFrom, EPChar PropName) 
{
  EUint32     j, n;
  ESint32     Flag, indx;
  POBJ        pObj;
  SC_ID       *down;
  
  pObj = *pObj1;
  indx = _edma_look4_prop_single (pObj, PropName);

  if (indx == -1) /* Method still not found */
    {
      n = pObj->nDownTable;
      down = pObj->DownTable;

      for (j = 0; j < n; j++)
	{
	  *pObj1 = gObj[down[j].Obj];
	  if (*pObj1 == pObjFrom)
	    continue;
	  Flag = _edma_look4_closer_prop_down (pObj1, pObj, PropName);
	  if (Flag != -1)
	    {
	      return Flag;
	    }
	}
    }
  return indx;
}



/***********************************************************************/

/* _edma_locate_method
 *   Main entry point for method lookup. Processes classpaths and 
 *   locates implementation subobject
 */

OBJID EDMAPROC
_edma_locate_method (OBJID IdObj, EPChar Id1, EPChar Signature, 
		     EPSint32 has_cp, EPSint32 met_indx)
{
  EPChar     MetName;
  OBJID      aux_obj;
  POBJ       pObj;
  ESint32    pos;

  pos = strlen (Id1);
  while ((Id1[pos] != '>') && (Id1[pos] != '<') && (pos > 0)) 
    --pos;
  
  /* Check if we have classpath*/
  if (pos != 0) 
    {
      /* Make a copy of the identifier to work on it*/
      MetName = Id1 + pos + 1;

      //aux_obj = edma_parse_classpath (IdObj, Id1, pos);
      aux_obj = edma_parse_classpath (IdObj, Id1, pos, 1);
      if (aux_obj != -1)
      {
	pObj = gObj[aux_obj];

	if ((*met_indx = edma_look4_met (&pObj, MetName, Signature)) == -1 ) 
	  {
	    edma_printf_err ("[%s] Method '%s' not Found from classpath '%s'"
			     , __FUNCTION__, MetName, Id1);
	  }
      }
      else
	return -1;

    }
  else  
    { 
      /* No ClassPath*/
      pObj = gObj[IdObj];

      /* Just look for the method the usual way*/
      if (((*met_indx = edma_look4_met_ext (&pObj, Id1, Signature)) == -1) 
	  && (gObj[IdObj]->IdSIU == -1)) 
	{
	  return _edma_system_exception ("[_edma_locate_method] Method '%s' "
					 "not found in object '%ld' "
					 "through classpath: '%s'",
					 Id1, IdObj, Id1);
	}
    }

  *has_cp = pos;

  return pObj->IdObj;
}


/* _edma_locate_property
 *   Main entry point for property lookup. Processes classpaths and 
 *   locates implementation subobject
 */

OBJID EDMAPROC
_edma_locate_property (OBJID IdObj, EPChar Id1, EPSint32 has_cp, 
		       EPSint32 prop_indx)
{
  EPChar     PropName;
  OBJID      aux_obj;
  POBJ       pObj;
  ESint32    pos;

  pos = strlen (Id1);
  while ((Id1[pos] != '>') && (Id1[pos] != '<') && (pos > 0)) 
    --pos;
  
  /* Check if we have classpath*/
  if (pos != 0) 
    {
      /* Make a copy of the identifier to work on it*/
      PropName = Id1 + pos + 1;

      //aux_obj = edma_parse_classpath (IdObj, Id1, pos);
      aux_obj = edma_parse_classpath (IdObj, Id1, pos, 1);
      if (aux_obj != -1)
      {
	pObj = gObj[aux_obj];

	if ((*prop_indx = edma_look4_prop (&pObj, PropName)) == -1 ) 
	  {
	    edma_printf_err ("[%s] Property '%s' not Found from classpath '%s'"
			     , __FUNCTION__, PropName, Id1);
	  }
      }
      else
	return -1;

    }
  else  
    { 
      /* No ClassPath*/
      pObj = gObj[IdObj];

      /* Just look for the method the usual way*/
      if (((*prop_indx = edma_look4_prop_ext (&pObj, Id1)) == -1) 
	  && (gObj[IdObj]->IdSIU == -1)) 
	{
	  return _edma_system_exception ("[_edma_locate_property] "
					 "Property '%s' "
					 "not found in object '%ld' "
					 "through classpath: '%s'",
					 Id1, IdObj, Id1);
	}
    }

  *has_cp = pos;

  return pObj->IdObj;
}

/**************************************************************************/
/* Anchor point locator function */

/* FIXME: Should we add a dictionary for  Anchor point access ???? */
/*        See edma_look4_single_method function for alternative implementation*/
/*        This will require to update all the code in anchor_points.c */

/* Internal functions do not checks input parameters... it's suposed 
 * to be done by calling function */
/* XXXX: Change implementation to use dictionaries */

/* _edma_locate_uplink_by_name
 *   Locates an uplink anchor point using its name
 */

ESint32 EDMAPROC
_edma_locate_uplink_by_name (OBJID IdObj, EPChar anchor_point)
{
  POBJ    pObj;
  ESint32 i, n;

  pObj = gObj[IdObj];
  n = pObj->nUpTable;
  
  if (n == 0)
    return -1;

  for (i = 0; i < n; i++)
    if (strncmp (pObj->UpTable[i].Id, anchor_point, EDMA_GENERAL_ID_LEN) == 0)
      break;

  if (i == n)
    return -1;

  return i;
}

/* _edma_locate_downlink_by_name
 *   Locates a downlink anchor point using its name
 */

ESint32 EDMAPROC
_edma_locate_downlink_by_name (OBJID IdObj, EPChar anchor_point)
{
  POBJ    pObj;
  ESint32 i, n;

  pObj = gObj[IdObj];
  n = pObj->nDownTable;
  
  if (n == 0)
    return -1;

  for (i = 0; i < n; i++)
    if (strncmp (pObj->DownTable[i].Id, anchor_point, EDMA_GENERAL_ID_LEN) == 0)
      break;

  if (i == n)
    return -1;

  return i;
}

/* _edma_locate_uplink_by_pobj
 *   Locates an uplink anchor point using the pointer to the object associated
 */

ESint32 EDMAPROC
_edma_locate_uplink_by_pobj (POBJ pObj_father, POBJ pObj)
{
  ESint32   i, n;

  n = pObj_father->nUpTable;

  for (i = 0; i < n; i++)
    if (gObj[pObj_father->UpTable[i].Obj] == pObj) 
      break;

  /* if not found --> ERROR */
  if (i == n) 
    return -1;

  return i;
}

/* _edma_locate_downlink_by_pobj
 *   Locates a downlink anchor point using the pointer to the object associated
 */

ESint32 EDMAPROC
_edma_locate_downlink_by_pobj (POBJ pObj_father, POBJ pObj)
{
  ESint32   i, n;

  n = pObj_father->nDownTable;

  for (i = 0; i < n; i++)
    if (gObj[pObj_father->DownTable[i].Obj] == pObj) 
      break;

  /* if not found --> ERROR */
  if (i == n) 
    return -1;

  return i;
}

/* _edma_locate_uplink_by_obj
 *   Locates an uplink anchor point using the object associated
 */

ESint32 EDMAPROC
_edma_locate_uplink_by_obj (OBJID id, OBJID Obj)
{
  ESint32   i, n;
  POBJ      pObj;

  pObj = gObj[id];
  n = pObj->nUpTable;

  for (i = 0; i < n; i++)
    if (pObj->UpTable[i].Obj == Obj) 
      break;

  /* if not found --> ERROR */
  if (i == n) 
    return -1;

  return i;
}


/* _edma_locate_downlink_by_obj
 *   Locates a downlink anchor point using the object associated
 */

ESint32 EDMAPROC
_edma_locate_downlink_by_obj (OBJID id, OBJID Obj)
{
  ESint32   i, n;
  POBJ      pObj;

  pObj = gObj[id];
  n = pObj->nDownTable;

  for (i = 0; i < n; i++)
    if (pObj->DownTable[i].Obj == Obj) 
      break;

  /* if not found --> ERROR */
  if (i == n) 
    return -1;

  return i;
}




