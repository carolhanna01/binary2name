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

/* Revisions:
 * ---------------------------------------------------
 * March, 2nd, 2002
 * File Creation
 * ---------------------------------------------------------
 * March, 10th, 2002
 * Added function LookUpClassPath
 * -----------------------------------------------------------
 * March, 16th-17th, 2002
 * Added anchor point query functions
 * Finished first phase LookUpClassPath. TODO: SIU support
 * -----------------------------------------------------------------------
 * April, 2nd, 2003
 * Modification to support changes to OBJ structure 
 * ----------------------------------------------------------------------
 * May, 10th, 2003
 * Modifications to support changes to CLASE structure
 * -------------------------------------------------------------------------
 * July, 3rd, 2003
 * Cosmetic changes
 * -------------------------------------------------------------------
 * July, 11th, 2003
 * Now using anchor_point locator functions
 * ---------------------------------------------------------------------
 * January, 7th, 2004
 * Code cleanup using new anchor point locator functions
 * ----------------------------------------------------------------------
 * January, 14th, 2004
 * Added some missing functions to manage anchor points
 * ----------------------------------------------------------------------
 * October, 16th, 2004
 * Code Cleanup
*/
#include <stdlib.h>
#include <string.h>

#include "portable.h"
#include "vglobal.h"
#include "const.h"
#include "shmem.h"
#include "tobj.h"
#include "misc.h"
#include "obj.h"
#include "objq.h"
#include "classq.h"

#include "anchor_points.h" 
#include "inh.h"
#include "inh1.h"
#include "helper.h"
#include "error.h"
#include "cast_obj.h"

#include "locators.h"

#define EDMA_CP_OP_ADD               0
#define EDMA_CP_OP_VERTICAL_INSERT   1
#define EDMA_CP_OP_HORIZONTAL_INSERT 2
#define EDMA_CP_OP_OVERWRITE         3



OBJID EDMAPROC
edma_apply_classpath (OBJID IdObj, EPChar classpath)
{

  return edma_parse_classpath (IdObj, classpath, strlen (classpath), 0);
  
}


/***************************************************************************/
/***************************************************************************/
/* FIXME: If no '<' or '>' character is present, this function returns IdObj, 
 *        instead of -1*/ 
/* -----------------------------------*/
/* Note: this function should not be invoked directly. 
 * For now, it is responsibility of
 * upper layers to make this check */
/***********************************************************************/
/***************************************************************************/



/* edma_parse_classpath
 *    Parses a given classpath returning the subobject referenced by it
 *    Also implements on-demand inheritance action embedded in classpaths
 */

OBJID EDMAPROC
edma_parse_classpath (OBJID IdObj, EPChar eclass_path, ESint32 len, ESint32 ex) 
{
  OBJID      id, casted_id;
  OBJID      aux_id;
  ESint32    l,pos;
  ESint32    op;
  EPChar     current,next;
  EPChar     iclass_path;
  /* Used for on-demand inheritance */
  EPChar     dyn_class_id;
  EPChar     apoint1, apoint2;
  CLASSID    temp_id; 
  EChar      ex_msg[1024];
  
  /* Test for valid object reference*/
  if (edma_check_obj_id (IdObj, "edma_parse_classpath") == -1)
    return -1;

  if (eclass_path == NULL)
    return -1;

  l = len + 2;

  current = next = iclass_path = strdup (eclass_path);
  pos = 0;
  op = EDMA_CP_OP_ADD;
  id = IdObj;

  while (pos < l) 
    {
      /* If it's the last classpath item go here */
      /* --------------------------------------------------------*/
      if ((*next == 0) && (current != next))
	{

	  if ((casted_id = edma_cast_obj (id,current)) == -1)
	    {
	      snprintf (ex_msg, 1024, "[edma_parse_classpath] '%s' "
			"Anchor Point/ClassName"
			" not found in classpath", current);
	      free (iclass_path);
	      if (ex)
		return _edma_system_exception ("%s", ex_msg);
	      else
		return -1;
	    }
	  id = casted_id;
	  next ++;
	  current = next;
	  pos++;
	  continue;
	}
      /* If the classpath item says us to go up the class hierarchy */
      /* ---------------------------------------------------------------*/
      if (*next == '>') 
	{
	  *next = 0; /* Truncate the string we only want current item*/
	  _edma_parse_classpath_items (current, &dyn_class_id, 
				       &apoint1, &apoint2, &op);
	  
	  /* We look up class hierarchy for first anchor point if it exists
	   * If it doesn't exists we look up for provided classname*/
	  if ((casted_id = edma_upcast_obj (id, 
					    (apoint1 == NULL) ? dyn_class_id 
					    : apoint1)) == -1) 
	    {
	      if ((gObj[id]->Final) || (gObj[IdObj]->Final))
		{
		  snprintf (ex_msg, 1024, "[edma_parse_classpath] "
			    "Object %ld is final. "
			    "Can't add superclass %s", id, current);
		  free (iclass_path);
		  return _edma_system_exception ("%s", ex_msg);
		}
	      
	      /* The classpath item doesn't exist in current class hierarchy */
	      /* First we check if classname names a valid classname 
	       * in the system */
	      
	      if ((temp_id = edma_get_class_id (dyn_class_id)) == -1) 
		{
		  snprintf (ex_msg, 1024,"[edma_parse_classpath] Class '%s' "
			    "don't exist."
			    "'%s' Anchor Point/Class not found in classpath",
			    dyn_class_id, current); 
		  free (iclass_path);
		  return _edma_system_exception ("%s", ex_msg);
		}
	      
	      /* At this point we have a valid class id so we attach 
	       * it to the class hierarchy */
	      /* We ignore operation. Operations only makes sense when 
	       * the anchor point exists.
	       * By default we allway add the new class */
	      if ((casted_id = edma_add_superclass (id, temp_id, 
						    apoint1, apoint2)) ==-1) 
		{
		  snprintf (ex_msg, 1024, "[edma_parse_classpath] Can't add "
			    "superclass "
			    "for '%s' classpath item", current);
		  free (iclass_path);
		  return _edma_system_exception ("%s", ex_msg);
		}
	    } 
	  else 
	    { /* If anchor point exists we must check the subobject's class */
	      if (apoint1 != NULL) 
		{
		  if ((temp_id = edma_get_class_id (dyn_class_id)) != -1) 
		    {
		      /* The class indicated in classpath exists. We proceed*/
		      switch (op)
			{
			case EDMA_CP_OP_ADD:
			  {
			    if (gObj[casted_id]->IdClass != temp_id) 
			      {
				snprintf (ex_msg, 1024, "[edma_parse_classpath] Can't override existing"
					  "subobject %ld at '%s'. Superobject at %ld of class"
					  "'%s' not '%s", casted_id, apoint1, casted_id,
					  gClass[gObj[casted_id]->IdClass]->ClassName,
					  gClass[temp_id]->ClassName);
				free (iclass_path);
				return _edma_system_exception ("%s", ex_msg);
			      }
			  }
			case EDMA_CP_OP_VERTICAL_INSERT:
			  {
			    if ((casted_id = edma_insert_superclass (id, temp_id, apoint1, apoint2)) == -1)
			      {
				snprintf (ex_msg, 1024, "[edma_parse_classpath] Can't insert superclass"
					  " %s at anchor point %s", dyn_class_id, apoint1);
				free (iclass_path);
				return _edma_system_exception ("%s", ex_msg);
			      }
			    break;
			  }
			case  EDMA_CP_OP_OVERWRITE:
			  {
			    edma_remove_superclass_ap (id, apoint1);
			    edma_free_obj (casted_id);
			    
			    if ((casted_id = edma_add_superclass (id, temp_id, apoint1, apoint2)) == -1)
			      {
				snprintf (ex_msg, 1024,"[edma_parse_classpath] Can't overwrite superclass"
					  " %s at anchor point %s", dyn_class_id, apoint1); 
				free (iclass_path);
				return _edma_system_exception ("%s", ex_msg);
			      }
			    break;
			  }
			default:
			  {
			    snprintf (ex_msg, 1024, "[edma_parse_classpath] Opertation %ld still"
				      " not implemented. Classpath: %s", op, current); 
			    free (iclass_path);
			    return _edma_system_exception ("%s", ex_msg);
			  }
			}

		    } 
		  else 
		    {
		      snprintf (ex_msg, 1024, "[edma_parse_class_path] Class '%s' "
				"in classpath '%s' doesn't exists",
				dyn_class_id, eclass_path);
		      free (iclass_path);
		      return _edma_system_exception ("%s", ex_msg);
		    }
		  
		}
	    }
	  id = casted_id;
	  next ++;
	  current = next;
	  pos++;
	  continue;
	}
      /* --------------------------------------------------------------*/
      if (*next == '<') /* Go down class hierarchy*/
	{
	  /* First, truncate string */
	  *next=0;
	  /* Then we parse elements in the identifier*/
	  _edma_parse_classpath_items (current, &dyn_class_id, 
				       &apoint1, &apoint2, &op);
	  if ((casted_id = edma_downcast_obj (id, (apoint1 == NULL) 
					      ? dyn_class_id : apoint1)) == -1) 
	    {
	      if ((gObj[id]->Final) || (gObj[IdObj]->Final))
		{
		  snprintf (ex_msg, 1024,"[edma_parse_classpath] Object %ld "
			    "is final. "
			    "Can't add subclass %s", id, current); 
		  free (iclass_path);
		  return _edma_system_exception ("%s", ex_msg);
		}
	      /* First we check if classname names a valid classname 
	       * in the system */
	      if ((temp_id = edma_get_class_id (dyn_class_id)) == -1) 
		{
		  snprintf (ex_msg, 1024, "[edma_parse_classpath] Class '%s' "
			    "don't exist."
			    "'%s' Anchor Point/Class not found in classpath",
			    dyn_class_id, current);
		  free (iclass_path);
		  return _edma_system_exception ("%s", ex_msg);
		}
	      
	      /* At this point we have a valid class id so we attach it 
	       * to the class hierarchy */
	      if ((casted_id = edma_add_subclass (id, temp_id, 
						  apoint1, apoint2)) == -1) 
		{
		  snprintf (ex_msg, 1024, "[edma_parse_class_path] Can't add "
			    "subclass for '%s' "
			    "classpath item", current); 
		  free (iclass_path);
		  return _edma_system_exception ("%s", ex_msg);
		}
	    } 
	  else 
	    { /* If anchor point exists we must check the subobject's class */
	      if (apoint1 != NULL) 
		{
		  if ((temp_id = edma_get_class_id (dyn_class_id)) != -1) 
		    {
		      /* The class indicated in classpath exists. We proceed*/
		      switch (op)
			{
			case EDMA_CP_OP_ADD:
			  {
			    if (gObj[casted_id]->IdClass != temp_id) 
			      {
				snprintf (ex_msg, 1024, "[edma_parse_classpath] Can't override existing"
					  "subobject %ld at '%s'. Superobject at %ld of class"
					  "'%s' not '%s", casted_id, apoint1, casted_id,
					  gClass[gObj[casted_id]->IdClass]->ClassName,
					  gClass[temp_id]->ClassName); 
				free (iclass_path);
				return _edma_system_exception ("%s", ex_msg);
			      }
			  }
			case EDMA_CP_OP_VERTICAL_INSERT:
			  {
			    edma_printf ("[%s] Inserting up: %s @ %s | %s", __FUNCTION__,
					 gClass[temp_id]->ClassName, apoint1, apoint2);
			    if ((casted_id = edma_insert_subclass (id, temp_id, apoint1, apoint2)) == -1)
			      {
				snprintf (ex_msg, 1024, "[edma_parse_classpath] Can't insert superclass"
					  " %s at anchor point %s", dyn_class_id, apoint1);
				free (iclass_path);
				return _edma_system_exception ("%s", ex_msg);
			      }
			    break;
			  }
			case  EDMA_CP_OP_OVERWRITE:
			  {
			    edma_remove_subclass_ap (id, apoint1);
			    edma_free_obj (casted_id);
			    /****************************************************************************
			     * =========================================================================
			     * Note too, that the current code doesn't set correctly the Father field
			     * in the new created object, and we set it here
			     *
			     * I don't know where this information should be set.... _edma_new_obj should
			     * set it..... we must revise its code.
			     ****************************************************************************/

			    aux_id = _edma_new_obj (dyn_class_id,id, NULL);
			    if ((casted_id = edma_add_subobject (id, aux_id, apoint1)) == -1)
			      {
				snprintf (ex_msg, 1024, "[edma_parse_classpath] Can't overwrite superclass"
					  " %s at anchor point %s", dyn_class_id, apoint1); 
				free (iclass_path);
				return _edma_system_exception ("%s", ex_msg);
			      }
			    edma_add_superobject (aux_id, id, apoint2);

			    gObj[aux_id]->Father = id;
			    gObj[aux_id]->PseudiFather = id;

			    casted_id = aux_id;
			    break;
			  }
			default:
			  {
			    snprintf (ex_msg, 1024, "[edma_parse_classpath] Opertation %ld still"
				      " not implemented. Classpath: %s", op, current); 
			    free (iclass_path);
			    return _edma_system_exception ("%s", ex_msg);
			  }
			}
		    }
		  else 
		    {
		      snprintf (ex_msg, 1024, "[edma_parse_classpath] Class '%s' in "
						     "classpath '%s' doesn't exists",
						     dyn_class_id, eclass_path);
		      free (iclass_path);
		      return _edma_system_exception ("%s", ex_msg);
		    }
		}
	    }
	  id = casted_id;
	  next++;
	  current = next;
	  pos++;
	  continue;
	}
      pos++;
      next++;
    }
  
  free (iclass_path);
  return id;
}


/* _edma_parse_classpath_items
 *    Parses a class path item, extracting the indicated class name, upper anchor point,
 *    lower anchor point and the embedded operation
 */

ESint32 EDMAPROC 
_edma_parse_classpath_items (EPChar id, EPChar *classname, EPChar *apoint1, EPChar *apoint2, EPSint32 op) 
{
  EPChar     p_ap, p_ap_sep; 

  /* Set default values*/
  *classname=id;
  *apoint1 = *apoint2 = NULL;
  *op = EDMA_CP_OP_ADD;
  /* Test if we have ap identifiers */
  if ((p_ap = strchr (id, '@')) != NULL) 
    {
      *p_ap = 0;
      p_ap++;
      /* We test next character to determine operation*/
      switch (p_ap[0])
	{
	case '!': /* Override */
	  {
	    *op = EDMA_CP_OP_OVERWRITE;
	    *p_ap = 0;
	    p_ap ++;
	    break;
	  }
	case '*': /* Insert Vertical */
	  {
	    *op = EDMA_CP_OP_VERTICAL_INSERT;
	    *p_ap = 0;
	    p_ap ++;
	    break;
	  }
	case '-': /* Insert Horizontallu. Not implemented */
	  {
	    *op = EDMA_CP_OP_HORIZONTAL_INSERT;
	    *p_ap = 0;
	    p_ap ++;
	    break;
	  }
	}
      /* Test if we have up and down identifiers */
      if ((p_ap_sep = strchr (p_ap, '|')) != NULL)
	{
	  *apoint2 = strdup (p_ap_sep + 1);
	  *p_ap_sep = 0;
	}
      *apoint1 = strdup(p_ap);
    }
  return 0;
}

/************************************************************************************
 * Anchor point query functions
 ************************************************************************************/

/* edma_query_subclass_ap
 *   Parameter checking wrapper for _edma_query_subclass_ap
 */

ESint32 EDMAPROC
edma_query_subclass_ap (OBJID id, EPChar anchor_point, OBJID *obj, CLASSID *cid)
{
  if (! edma_check_obj_id (id, "edma_query_subclass_ap"))
    return -1; /* Error message showed by edma_check_obj_id */

  if ( anchor_point == NULL)
    {
      edma_printf ("%s","[edma_query_subclass_ap] No anchot point provided");
      return -1;
    }

  return _edma_query_subclass_ap (id, anchor_point, obj, cid);
  
}

/* _edma_query_subclass_ap
 *   From a given object (id) tries to locate anchor_point within the subclass set
 *   Returns the subobject associated to the given classpath and its class identifier
 *   -1 on error
 */

ESint32 EDMAPROC
_edma_query_subclass_ap (OBJID id, EPChar anchor_point, OBJID *obj, CLASSID *cid)
{
  ESint32    i,n;
  
  /* Internal FUnction. Do not check parameters. It is supposed they are ok before call*/
  n = gObj[id]->nDownTable;
  if (n == 0) 
    {
      *obj = -1;
      *cid = -1;
      return _edma_system_exception ("[_edma_query_subclass_ap] Object %d hasn't subobjects", id);
    }

  if ((i = _edma_locate_downlink_by_name (id, anchor_point)) == -1)
    {
      *obj = -1;
      *cid = -1;
      return -1;
    }
  
  /* If anchor_point was found */  
  *obj = gObj[id]->DownTable[i].Obj; /* Returns object identifier*/
  *cid = gObj[gObj[id]->DownTable[i].Obj]->IdClass; 

  return i;
}


/* edma_query_superclass_ap
 *   Parameter checking wrapper to _edma_query_superclass_ap
 */

ESint32 EDMAPROC
edma_query_superclass_ap (OBJID id, EPChar anchor_point, OBJID *obj, CLASSID *cid)
{
  if (! edma_check_obj_id (id, "edma_query_superclass_ap"))
    return -1; /* Error message showed by edma_check_obj_id */
  
  if ( anchor_point == NULL)
    {
      edma_printf ("%s","[edma_query_superclass_ap] No anchot point provided");
      return -1;
    }

  return _edma_query_superclass_ap (id, anchor_point, obj, cid);
  
}


/* _edma_query_syperclass_ap
 *   From a given object (id) tries to locate anchor_point within the superclass set
 *   Returns the subobject associated to the given classpath and its class identifier
 *   -1 on error
 */

ESint32 EDMAPROC
_edma_query_superclass_ap (OBJID id, EPChar anchor_point, OBJID *obj, CLASSID *cid)
{
  ESint32    i,n;
  
  n = gObj[id]->nUpTable;
  if (n == 0) 
    {
      *obj = -1;
      *cid = -1;
      return _edma_system_exception ("[_edma_query_superclass_ap] Object %d hasn't subobjects", id);
    }

  if ((i = _edma_locate_uplink_by_name (id, anchor_point)) == -1)
    { /* Anchor point not found in object id */
      *obj = -1;
      *cid = -1;
      return -1;
    }
  /* If anchor_point was found */
  *obj = gObj[id]->UpTable[i].Obj; /* Returns object identifier*/
  *cid = gObj[gObj[id]->UpTable[i].Obj]->IdClass; 

  return i;
}


/* edma_rename_super_class_ap
 *   Renames anchor points in one object's superclass table
 *
 */
ESint32 EDMAPROC 
edma_rename_superclass_ap (OBJID IdObj, EPChar old_name, EPChar new_name) 
{
  POBJ       pObj;
  ESint32    i;
  
  /* Test for valid object reference*/
  if (edma_check_obj_id (IdObj, "edma_rename_superclass_ap") == -1)
    return -1;

  if (old_name == 0)
    return _edma_system_exception ("[edma_rename_superclass_ap] No 'OldName' provided for"
				   " object %ld", IdObj);
  
  if (new_name == NULL)
    return _edma_system_exception ("[edma_rename_superclass_ap] No 'NewName' provided for"
				   " object %ld", IdObj);
  pObj = gObj[IdObj];
  
  if ((i = _edma_locate_uplink_by_name (IdObj, old_name)) == -1)
    { /* if anchor point doesn't exists*/
      return _edma_system_exception ("[edma_rename_superclass_ap] '%s' anchor point doesn't exist "
				     "in object %ld", old_name, IdObj);
    }
  
  strncpy (pObj->UpTable[i].Id, new_name, EDMA_GENERAL_ID_LEN);
  return 0;
}

/* edma_rename_subclass_ap
 *   Renames anchor points in one object's subclass table
 */

ESint32 EDMAPROC 
edma_rename_subclass_ap (OBJID IdObj, EPChar old_name, EPChar new_name) 
{
  POBJ       pObj;
  ESint32    i;
  
  /* Test for valid object reference*/
  if (edma_check_obj_id (IdObj, "edma_rename_subclass_ap") == -1)
    return -1;

  if (old_name == 0)
    return _edma_system_exception ("[edma_rename_subclass_ap] No OldName provided for"
				   " object %ld", IdObj);
  
  if (new_name == 0)
    return _edma_system_exception ("[edma_rename_subclass_ap] No NewName provided for"
				   " object %ld", IdObj);
  pObj = gObj[IdObj];
  
  if ((i = _edma_locate_downlink_by_name (IdObj, old_name)) == -1)
    { /* if anchor point doesn't exists*/
      return _edma_system_exception ("[edma_rename_subclass_ap] %s anchor point "
				     "doesn't exist in object %ld", old_name, IdObj);
    }
  strncpy (pObj->DownTable[i].Id, new_name, EDMA_GENERAL_ID_LEN);
  return 0;
}



/* Anchor point remove functions */

/* edma_remove_superclass_ap
 *   Removes the indicated superclass anchor point from the given object
 */

ESint32 EDMAPROC
edma_remove_superclass_ap (OBJID IdObj, EPChar anchor_point)
{
  POBJ       pObj, pObj_father;
  ESint32    i, indx1, indx2;
  
  /* Test for valid object reference*/
  if (edma_check_obj_id (IdObj, "edma_remove_superclass_ap") == -1)
    return -1;

  if ( anchor_point == NULL)
    {
      edma_printf ("%s", "[edma_remove_superclass_ap] No anchot point provided");
      return -1;
    }

  /* Locate specified anchor point in current object*/
  pObj = gObj[IdObj];

  if ((i = _edma_locate_uplink_by_name (IdObj, anchor_point)) == -1)
    { /* if anchor point doesn't exists*/
      return _edma_system_exception ("[edma_remove_superclass_ap] %s anchor point "
				     "doesn't exist in object %ld", 
				     anchor_point, IdObj);
    }
  indx1 = i;

  /* If the anchor point exists we remove it. */
  pObj_father = gObj[pObj->UpTable[indx1].Obj];

  
  /* Locate the back link in superobject using object identifier*/
  /* ***************************************************************
   * FIXME: 
   * This method can be ambiguous if more than one link exists
   * between the two objects. We just link the first available one 
   * ----------------------------------------------------------------
   * We can add a counter in the SC_ID struct that can be used as a unique
   * identifier to relate amchor point entries unambiguously
   *
   * For doing this we must revise all the anchor point code.... 
   *******************************************************************/

  if ((indx2 = _edma_locate_downlink_by_pobj (pObj_father, pObj)) == -1)
      return _edma_system_exception ("[edma_remove_superclass_ap] %s anchor point "
				     "doesn't exist in superobject %ld", 
				     anchor_point, (POBJ)pObj_father->IdObj);    

  memset (pObj->UpTable + indx1, 0, sizeof (SC_ID));
  memset (pObj_father->DownTable + indx2, 0, sizeof (SC_ID));
  /* Remove entries in both object tables*/
  pObj->nUpTable --;
  if (pObj->nUpTable == 0)
    {
      /* No more entries in table.... remove it*/
      edma_pfree (gObj[IdObj]->SysObj.hUpTable, gObj[IdObj]->UpTable);
      gObj[IdObj]->UpTable = NULL;
      gObj[IdObj]->SysObj.hUpTable = 0;
    }
  else /* else... compact the table*/
    memcpy (pObj->UpTable + indx1, pObj->UpTable + (indx1 + 1), 
	    sizeof (SC_ID) * (pObj->nUpTable - indx1));
  

  pObj_father->nDownTable --;
  if (pObj_father->nDownTable < 0)
    {
      /* No more entries in table.... remove it*/
      edma_pfree (pObj_father->SysObj.hDownTable, pObj_father->DownTable);
      gObj[IdObj]->DownTable = NULL;
      gObj[IdObj]->SysObj.hDownTable = 0;
    }
  else /* else... compact the table*/
    memcpy (pObj_father->DownTable + indx2, pObj_father->DownTable + (indx2 + 1), 
	    sizeof (SC_ID) * (pObj_father->nDownTable - indx2));

  return 0;
}


/* edma_remove_subclass_ap
 *    Removes the indicated subclass anchor point from the given object
 */

ESint32 EDMAPROC
edma_remove_subclass_ap (OBJID IdObj, EPChar anchor_point)
{
  POBJ       pObj, pObj_father;
  ESint32    i, indx1, indx2;
  
  /* Test for valid object reference*/
  if (edma_check_obj_id (IdObj, "edma_remove_subclass_ap") == -1)
    return -1;

  if ( anchor_point == NULL)
    {
      edma_printf ("%s","[edma_remove_subclass_ap] No anchot point provided");
      return -1;
    }  

  indx1 = indx2 = 0;
  /* Locate specified anchor point in current object*/
  pObj = gObj[IdObj];

  if ((i = _edma_locate_downlink_by_name (IdObj, anchor_point)) == -1)
    { /* if anchor point doesn't exists*/
      return _edma_system_exception ("[edma_remove_subclass_ap] %s anchor point "
				     "doesn't exist in object %ld", 
				     anchor_point, IdObj);
    }
  indx1 = i;

  /* If the anchor point exists we remove it. */
  pObj_father = gObj[pObj->DownTable[indx1].Obj];
  
  /* Locate the back link in subobject using object identifier*/
  /* ***************************************************************
   * FIXME: 
   * This method can be ambiguous if more than one link exists
   * between the two objects. We just link the first available one 
   * ----------------------------------------------------------------
   * We can add a counter in the SC_ID struct that can be used as a unique
   * identifier to relate amchor point entries unambiguously
   *
   * For doing this we must revise all the anchor point code.... 
   *******************************************************************/

  if ((indx2 = _edma_locate_uplink_by_pobj (pObj_father, pObj)) == -1)
    return _edma_system_exception ("[edma_remove_subclass_ap] %s anchor point "
				   "doesn't exist in superobject %ld", 
				   anchor_point, (POBJ)pObj_father->IdObj);    

  memset (pObj->DownTable + indx1, 0, sizeof (SC_ID));
  memset (pObj_father->UpTable + indx2, 0, sizeof (SC_ID));
  /* Remove entries in both object tables*/
  pObj->nDownTable --;
  if (pObj->nDownTable == 0)
    {
      /* No more entries in table.... remove it*/
      edma_pfree (gObj[IdObj]->SysObj.hDownTable, gObj[IdObj]->DownTable);
      gObj[IdObj]->DownTable = NULL;
      gObj[IdObj]->SysObj.hDownTable = 0;
    }
  else /* else... compact the table*/
    memcpy (pObj->DownTable + indx1, pObj->DownTable + (indx1 + 1), 
	    sizeof (SC_ID) * (pObj->nDownTable - indx1));
  

  pObj_father->nUpTable --;
  if (pObj_father->nUpTable == 0)
    {
      /* No more entries in table.... remove it*/
      edma_pfree (pObj_father->SysObj.hUpTable, pObj_father->UpTable);
      gObj[IdObj]->UpTable = NULL;
      gObj[IdObj]->SysObj.hUpTable = 0;
    }
  else /* else... compact the table*/
    memcpy (pObj_father->UpTable + indx2, pObj_father->UpTable + (indx2 + 1), 
	    sizeof (SC_ID) * (pObj_father->nUpTable - indx2));


  return 0;
}

/* Addition Functions */

/* _edma_add_super_ap
 *   Adds a new superclass anchor point to the given object
 */

ESint32 EDMAPROC
_edma_add_super_ap (OBJID id, EPChar apoint, CLASSID cid, OBJID superid, ESint32 flags)
{
  ESint32    n;

  if (apoint) 
    {
      if (_edma_locate_uplink_by_name (id, apoint) != -1)
	{ /* We have found the anchor point */
	  edma_printf_err ("[edma_add_super_ap] Anchor point %s "
			   "already exists on object %ld [%s]", 
			   apoint, id, gClass[gObj[id]->IdClass]->ClassName);
	  return -1;
	}
    }

  /* We add the new superobject entry */
  n = gObj[id]->nUpTable;  /* First, we get the number of current subclasses*/
  
  /* NOTE: 
   * We use realloc to add a new entry in the object's superclass table
   * we suppose realloc is available and it maintains all data in the buffer */
  
  if (( gObj[id]->SysObj.hUpTable = 
	edma_prealloc (gObj[id]->SysObj.hUpTable, sizeof (SC_ID) * (n + 1))) == 0) 
    {
      edma_printf_err ("%s", "[edma_add_super_ap] Can't realloc "
		       "object superclass table. Not enough memory");
      return -1;
    }

  if ((gObj[id]->UpTable = edma_pget (gObj[id]->SysObj.hUpTable)) == NULL) 
    {
      edma_printf_err ("%s", "[edma_add_superobject] Can't realloc "
		       "object superclass table. Not enough memory");
      return -1;
    }

  gObj[id]->nUpTable = n + 1;

  /* Copy data into the new entry */
  gObj[id]->UpTable[n].IdClass = cid;
  gObj[id]->UpTable[n].Obj = superid;
  strncpy (gObj[id]->UpTable[n].Id, apoint, EDMA_GENERAL_ID_LEN);

  return 0;
}


/* _edma_add_sub_ap
 *   Adds a new subclass anchor point to the given object
 */

ESint32 EDMAPROC
_edma_add_sub_ap (OBJID id, EPChar apoint, CLASSID cid, OBJID subid, ESint32 flags)
{
  ESint32    n;

  if (apoint) 
    {
      if (_edma_locate_downlink_by_name (id, apoint) != -1)
	{ /* We have found the anchor point */
	  edma_printf_err ("[edma_add_sub_ap] Anchor point %s "
			   "already exists on object %ld [%s]", 
			   apoint, id, gClass[gObj[id]->IdClass]->ClassName);
	  return -1;
	}
    }

  /* We add the new superobject entry */
  n = gObj[id]->nDownTable;  /* First, we get the number of current subclasses*/
  
  /* NOTE: 
   * We use realloc to add a new entry in the object's superclass table
   * we suppose realloc is available and it maintains all data in the buffer */
  if (n < 0)
    edma_printf_err ("[edma_add_sub_ap] Number of entries in downlink table is < 0: %d",  n);

  if (( gObj[id]->SysObj.hDownTable = 
	edma_prealloc (gObj[id]->SysObj.hDownTable, sizeof (SC_ID) * (n + 1))) == 0) 
    {
      edma_printf_err ("%s", "[edma_add_sub_ap] Can't realloc "
		       "object subclass table. Not enough memory");
      return -1;
    }

  if ((gObj[id]->DownTable = edma_pget (gObj[id]->SysObj.hDownTable)) == NULL) 
    {
      edma_printf_err ("%s", "[edma_add_superobject] Can't realloc "
		       "object subclass table. Not enough memory");
      return -1;
    }

  gObj[id]->nDownTable = n + 1;

  /* Copy data into the new entry */
  gObj[id]->DownTable[n].IdClass = cid;
  gObj[id]->DownTable[n].Obj = subid;
  strncpy (gObj[id]->DownTable[n].Id, apoint, EDMA_GENERAL_ID_LEN);

  return 0;
}

/**************************************************************************************/
/**************************************************************************************/

/****** NEED TO REWORK PHYLOSOPHY ON DELEGATION ***************************************/

/**************************************************************************************/
/**************************************************************************************/


/****** XXXX: FUNCTION NOT TESTED XXXX XXXX XXXX XXXX *********************************/

/* edma_set_super_ap
 *  Changes information associated to a given superclass anchor point.
 *  For now, only allows to change associated object
 */

OBJID EDMAPROC
edma_set_super_ap (OBJID id, EPChar apoint, OBJID new_id)
{
  ESint32  indx, temp_indx;
  OBJID    old_id;

  /* Check for valid parameters */
  if (!apoint)
    {
      edma_printf_err ("%s", "[edma_set_super_ap] Invalid anchor point (NULL)");
      return -1;
    }

#if 0
  /* Locate apoint in current object */
  if ((indx = _edma_locate_uplink_by_name (id, apoint)) != -1)
    { /* We have found the anchor point */
      edma_printf_err ("[edma_set_super_ap] Anchor point %s "
		       "already exists on object %ld [%s]", 
		       apoint, id, gClass[gObj[id]->IdClass]->ClassName);
      return -1;
    }  
#endif
  /* Locate apoint in current object */
  if ((indx = _edma_locate_uplink_by_name (id, apoint)) == -1)
    { /* We have found the anchor point */
      edma_printf_err ("[edma_set_super_ap] Anchor point %s "
		       "doesn't exist on object %ld [%s]", 
		       apoint, id, gClass[gObj[id]->IdClass]->ClassName);
      return -1;
    }  
  old_id = gObj[id]->UpTable[indx].Obj;

  /* Type checking */
  /* TODO: The code bellow will be conditionally executed when anchor points attributes 
     become implemented --> If attribute is type-safe, then run code, if not, go ahead
  */
  /* if AP_TYPE == AP_TYPE_SAFE */
  if ((edma_cast_obj (new_id, gClass[gObj[old_id]->IdClass]->ClassName)) == -1)
    {
      edma_printf ("%s", "[edma_set_super_ap] New object type doesn't match Anchor Point Type");
    }

  /* Update anchor point data */

  gObj[id]->UpTable[indx].IdClass = gObj[new_id]->IdClass;
  gObj[id]->UpTable[indx].Obj = new_id;  

  /* Remove downlink on old super object */
  if ((temp_indx = _edma_locate_downlink_by_obj (old_id, id)) == -1)
    {
      edma_printf_err ("[WARNNING] SuperObject %d bad linkage to object %d. No anchor point found", 
		       old_id, id);
      /* Do not unroll operation. Current operation effectively performed but previous state was wrong*/
      return -1;
    }

  /* XXX: We need a low level function to remove APs on a single object */
  /* For now, simply pack the table */

  /* XXX: Refactor code bellow to a new low-level function */

  if (gObj[old_id]->nDownTable > 1)
    memcpy (gObj[old_id]->DownTable + temp_indx, gObj[old_id]->DownTable + temp_indx + 1,
	    gObj[old_id]->nDownTable - temp_indx);

  gObj[old_id]->nDownTable --;
  if (gObj[old_id]->nDownTable <= 0)
    {
      edma_pfree (gObj[old_id]->SysObj.hDownTable, gObj[old_id]->DownTable);
    }
  /* Else realloc table to free memory or store a capacity value on structure */

  return old_id;
}

/****** XXXX: FUNCTION NOT TESTED XXXX XXXX XXXX XXXX *********************************/

/* edma_set_sub_ap
 *   Changes information associated to a given subclass anchor point
 *   For now only allows to change related object
 */

OBJID EDMAPROC
edma_set_sub_ap (OBJID id, EPChar apoint, OBJID new_id)
{
  ESint32  indx, temp_indx;
  OBJID    old_id;

  /* Check for valid parameters */
  if (!apoint)
    {
      edma_printf_err ("%s", "[edma_set_sub_ap] Invalid anchor point (NULL)");
      return -1;
    }

  /* Locate apoint in current object */
  if ((indx = _edma_locate_downlink_by_name (id, apoint)) == -1)
    { /* We have found the anchor point */
      edma_printf_err ("[edma_set_sub_ap] Anchor point %s "
		       "doesn't exists on object %ld [%s]", 
		       apoint, id, gClass[gObj[id]->IdClass]->ClassName);
      return -1;
    }  

  old_id = gObj[id]->DownTable[indx].Obj;

  /* Type checking */
  /* TODO: The code bellow will be conditionally executed when anchor points attributes 
     become implemented --> If attribute is type-safe, then run code, if not, go ahead
  */
  /* if AP_TYPE == AP_TYPE_SAFE*/
#if 0
  if ((edma_cast_obj (new_id, gClass[gObj[old_id]->IdClass]->ClassName)) == -1)
    {
      edma_printf ("%s", "[edma_set_sub_ap] New object type doesn't match Anchor Point Type");
    }
#endif
  /* Update anchor point data */

  gObj[id]->DownTable[indx].IdClass = gObj[new_id]->IdClass;
  gObj[id]->DownTable[indx].Obj = new_id;  

  /* Remove downlink on old super object */
  if ((temp_indx = _edma_locate_uplink_by_obj (old_id, id)) == -1)
    {
      edma_printf_err ("[edma_set_sub_ap] ** WARNNING** SuperObject %d bad linkage to object %d. "
		       "No anchor point found", old_id, id);
      /* Do not unroll operation. Current operation effectively performed but previous state was wrong*/
      return -1;
    }

  /* XXX: We need a low level function to remove APs on a single object */
  /* For now, simply pack the table */

  /* XXX: Refactor code bellow to a new low-level function */

  memcpy (gObj[old_id]->UpTable + temp_indx, gObj[old_id]->UpTable + temp_indx + 1,
	  gObj[old_id]->nUpTable - temp_indx);

  gObj[old_id]->nUpTable --;
  if (gObj[old_id]->nUpTable <= 0)
    { 
      edma_pfree (gObj[old_id]->SysObj.hUpTable, gObj[old_id]->UpTable);
    }
  /* TODO: Else realloc table to free memory or store a capacity value in structure*/

  return old_id;
}
