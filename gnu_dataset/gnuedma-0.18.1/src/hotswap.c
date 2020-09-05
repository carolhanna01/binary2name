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

/*********************************************************************
 *
 * Revisiones: ---------------------------------------------------------
 * May 24th, 2003
 * File creation
 * ----------------------------------------------------------------------
 * August, 3rd, 2003
 * Added function to encapsulate access to stack execution global vars
 * ----------------------------------------------------------------------
 * August, 10th, 2003
 * Added function to manage pending_updates list
 * --------------------------------------------------------------------------
 * May, 2nd, 2004
 * Updated stack execution stack function for multithreading
 * ----------------------------------------------------------------------------
 * August, 19th, 2004
 * Finishing Update of stack execution functions for multithreading
 */

#include <stdio.h>
#include <strings.h> /* memcpy and memset*/
#include <time.h>    /* For update timestamp*/

#include <signal.h>

#include "portable.h"
#include "const.h"
#include "obj.h"
#include "clone_obj.h"
#include "tclass.h"
#include "obj.h"
#include "multiidf.h"
#include "vglobal.h"
#include "classq.h"
#include "misc.h"
#include "inh.h"
#include "helper.h"
#include "error.h"
#include "pri3.h"

#include "shmem.h"
#include "ethread.h"
#include "hotswap.h"

#define CAPACITY_STEP  100

// FIXME: Proper interface to restore signals
void HandleEx(int);

/* edma_hotswap
 *   Performs a hotswap update of the system according to current update table
 *   Updates are introduced automatically in the system when a new version of a
 *   component is registered via ines_class_register
 *
 *   Current implementation simply copy current status. A TCC (tiny C Compiler) 
 *   based implementation is included in code but commented. 
 *   This implementations allow to provide specific scripts for object 
 *   status transformation at hotswap
 */
ESint32 EDMAPROC 
edma_hotswap (EPChar s) 
{
  ESint32    i, cnt; 
  ESint32    current_capacity;
  OBJID      oldid, newid;
  OBJID      script;
  CLASSID    cid;
  time_t     old_time;

  edma_printf ("%s", "[edma_hotswap] New update available. Proceeding...");
  edma_printf ("[edma_hotswap] Update script is: %s", 
	       GVar->pending_update[0].update_script);

  /* Prevents infinite loop when running update script*/
  old_time = last_checked_update;
  last_checked_update = GVar->time_last_update;

  script = -1;

  /* General management of scripts 
   * -------------------------------------------------
   * Scripts name on .ines deployment files should contain the
   * Interpreter class to run the script
   */
  if ((script = edma_new_obj ("EDMA_SCRIPT")) == -1)
    {
      /* FIXME: If class SCRIPT is not found. We should remove update 
	 since it won't ever apply */
      return _edma_system_exception ("%s", "[edma_hotswap] Can't find SCRIPT "
				     "component. "
				     "Can't run update script");
    }
  
  edma_met3 (script, "set_script", GVar->pending_update[0].update_script);
  /* Set default signal handlers for properly shutdown the system*/
  signal (SIGHUP   , HandleEx);
  signal (SIGINT   , HandleEx);
  signal (SIGQUIT  , HandleEx);
  signal (SIGILL   , HandleEx);
  signal (SIGABRT  , HandleEx);
  signal (SIGFPE   , HandleEx);
  signal (SIGSEGV  , HandleEx);
  signal (SIGPIPE  , HandleEx);

  /******** General Algorithm. For now are doing just a probe ********/
  /* Get all pending updates from the last checked one */
  /* Build pending object list for each new available update */
  /* While updates availables and current update success 
   *    for each object affected by update
   *        if object is not in current execution stack
   *           update object (run update script)
   *        else
   *           mark update as not complete
   */

  /* Alloc space for pending object list */
  /* FIXME: check memory error */
  pending_objects_list = (OBJID *) edma_palloc (sizeof(OBJID) * CAPACITY_STEP);
  current_capacity = CAPACITY_STEP;
  cnt = 0;

  /* For now, only an update can be applied at a time */
  /* FIXME: For this test we consider object table compacted. 
   * This might not be true */
  for (i = 0; i < nObj; i++)
    {
      /* Check if object is affected. IdClass = Class being updated */
      if (gObj[i])
	{
#if 0
	  if (gClass[gObj[i]->IdClass]->CurrentVer 
	      == GVar->pending_update[0].IdClass)
#endif
	  if (pClass[gObj[i]->IdClass]->actual_version 
	      == GVar->pending_update[0].IdClass
	      && (gObj[i]->IdClass != pClass[gObj[i]->IdClass]->actual_version)
	      )
	    {
	      /* Add object to current pending object table */
	      if (cnt > current_capacity)
		{
		  current_capacity += CAPACITY_STEP;
		  pending_objects_list = 
		    (OBJID *) edma_prealloc ((HMEM)pending_objects_list, 
					     current_capacity);
		}
	      pending_objects_list[cnt] = i;
	      cnt++;
	    }
	}
    }
  edma_log ("[hotswap] %d objects affected by current update. "
	    "Trying to update", cnt);

  /* Check what objects are in current execution stack */
  for (i = 0; i < cnt; i++)
    {
      edma_log ("[hotswap] Checking object %d/%d for safe update", 
		pending_objects_list[i], cnt);
      if ((_edma_is_obj_in_execution_stack (pending_objects_list[i])) == 1)
	continue;
      /* Object not in execution stack. It is safe to update */
      edma_log ("[edma_hotswap] Object %d not in stack execution... "
		"Proceeding", pending_objects_list[i]);
      oldid = pending_objects_list[i];

      cid = GVar->pending_update[0].IdClass;

      if ((gClass[cid] == NULL))
	{
	  edma_printf ("[edma_hotswap] Class %d not valid", cid);
	  continue;
	}

      edma_log ("[edma_hotswap] Creating new instance of class %d", cid);
      newid = edma_new_obj (gClass[cid]->ClassName);

      edma_met3 (script, "update_object", newid, oldid);
#if 0
      /* For now, we only copy object state */
      memcpy (gObj[newid]->Data, gObj[oldid]->Data, 
	      gClass[gObj[oldid]->IdClass]->TamDatos);
#endif

      edma_log ("%s", "Hotswap: Now swapping instances...");
      /* update top level reference */
      edma_swap_obj (oldid, newid);
      /* Free old object, depending on transactional strategy */
      /* For now we delete the old object */
      edma_free_obj (newid);
    }

  edma_free_obj (script);
  /* Set default signal handlers for properly shutdown the system*/
  signal (SIGHUP   , HandleEx);
  signal (SIGINT   , HandleEx);
  signal (SIGQUIT  , HandleEx);
  signal (SIGILL   , HandleEx);
  signal (SIGABRT  , HandleEx);
  signal (SIGFPE   , HandleEx);
  signal (SIGSEGV  , HandleEx);
  signal (SIGPIPE  , HandleEx);

  edma_printf ("*** Update Done **************************************");
  return 0;
}

/* edma_attach_proxy
 *   Allows to dynamically attach a SIU proxy to a running object
 *   SIU proxy must support the 'obj' property which will be updated with
 *   the target object identifier
 */

ESint32 EDMAPROC
edma_attach_proxy (OBJID IdObj, EPChar proxy)
{
  OBJID   proxy_id;
  POBJ    current;

  /* First check for valid object identifier*/
  if ((edma_check_obj_id (IdObj, "edma_attach_proxy")) == -1) 
    return -1;

  if ((proxy_id = edma_new_obj (proxy)) == -1)
    {
      return _edma_system_exception ("[edma_attach_proxy] Proxy '%s' does "
				     "not exists", proxy);
    }

  current = gObj[IdObj];
  gObj[IdObj] = gObj[proxy_id];
  gObj[proxy_id] = current;

  gObj[IdObj]->IdObj = IdObj;
  gObj[proxy_id]->IdObj = proxy_id;

  /* It is supossed that attachable proxies stores real object identifier in
   * a property named 'obj' 
   * Note that after object swap, identifiers got changed */
  edma_wprop3 (IdObj, "obj", proxy_id);

  /* Set Proxy information */
  gObj[IdObj]->IdSIU = edma_get_class_id (proxy);

  return 0;
}


/* edma_deattach_proxy
 *   Allows to dynamically deattach a SIU proxy to a running object
 *   SIU proxy must support the 'obj' property which will be updated with
 *   the target object identifier
 */

ESint32 EDMAPROC
edma_deattach_proxy (OBJID IdObj)
{
  OBJID   obj;
  POBJ    current;

  if ((edma_check_obj_id (IdObj, "edma_deattach_proxy")) == -1) 
    return -1;

  edma_rprop3 (IdObj, "obj", &obj);

  current = gObj[IdObj];
  gObj[IdObj] = gObj[obj];
  gObj[obj] = current;

  gObj[IdObj]->IdObj = IdObj;
  gObj[obj]->IdObj = obj;

  /* Remove proxy reference*/
  /*gObj[IdObj]->IdSIU = -1;*/
  /* Destroy the proxy object */
  edma_free_obj (obj);

  return 0;
}

/* _edma_stack_execution_add_obj
 *   Adds an object reference to current thread stack executions
 *   Stack execution is used to determine which objects can be safely hotswapped
 */

ESint32 EDMAPROC
_edma_stack_execution_add_obj (OBJID id)
{
  ETHREAD_DATA  *p;

  if ((p = edma_tsd_get_data (thread_stack_key)) == NULL)
    {
      edma_printf ("%s", "WARNNING:  Non registered thread accessing GNU/EDMA");
      /* Register thread to be aware of their objects */
      /* FIXME: How to know when a free thread ends???? */
      edma_thread_register ();
      p = edma_tsd_get_data (thread_stack_key);
    }

  p->current_stack_execution[p->top] = id;
  p->top ++;

  /* If stack gets full launch an exception */
  if (p->top == MAX_STACK_EXECUTION)
    return _edma_system_exception ("[_edma_add_stack_execution_obj] "
				   "Stack overflow");
  else return 0;
}

/* _edma_stack_execution_del_obj
 *    Removes an object of current thread stack execution
 *   Stack execution is used to determine which objects can be safely hotswapped
 */

ESint32 EDMAPROC
_edma_stack_execution_del_obj (OBJID id)
{
  OBJID          top; 
  ETHREAD_DATA   *p;

  if ((p = edma_tsd_get_data (thread_stack_key)) == NULL)
    {
      edma_printf ("%s", "WARNNING:  Non registered thread accessing GNU/EDMA");
      /* Register thread to be aware of their objects */
      /* FIXME: How to know when a free thread ends???? */
      edma_thread_register ();
      p = edma_tsd_get_data (thread_stack_key);
    }

  if (p->top < 1)
    {
      edma_printf_err ("[_edma_del_stack_execution_obj] %s",
		       "Stack corrupted: Stack Empty");
      return -1;
    }

  top = p->current_stack_execution[p->top - 1];

  if (top != id)
    {
      return -1;
      /* Temporally remove message, until mutithread support get fully implemented*/
      /*
      return _edma_system_exception ("[_edma_del_stack_execution_obj] Stack corrupted"
				     "Expected: %d and Found %d", id, top);
      */
    }
  p->top --;
  return 0;
}

/* _edma_is_obj_in_execution_stack
 *   Checks if a given object is currently active, that is, if that object is
 *   in the current execution stack of any active thread in the system
 */

ESint32 EDMAPROC
_edma_is_obj_in_execution_stack (OBJID id)
{
  ESint32  i, j;
  OBJID    *current_stack;

  /* For each active thread */
  for (i = 0; i < num_threads; i++)
    {
      current_stack = thread_list[i]->current_stack_execution;

      for (j = 0; j < thread_list[i]->top; i++)
	if (current_stack[j] == id)
	  return 1;
    }

  return 0;
}

/* _edma_print_stack_execution
 *   DEBUG function to dump current execution stack
 */

ESint32 EDMAPROC
_edma_print_stack_execution ()
{
  ESint32    i;
  ETHREAD_DATA   *p;

  if ((p = edma_tsd_get_data (thread_stack_key)) == NULL)
    {
      edma_printf ("%s", "WARNNING:  Non registered thread accessing GNU/EDMA");
    }

  edma_printf ("%s", "--------> Current Stack Execution <-------------------");
  for (i = p->top - 1; i >= 0; i--)
      edma_printf ("Entry %d: %d", i, p->current_stack_execution[i]);
  edma_printf ("%s", "----------------------------------");
  return 0;
}

/* edma_add_update
 *   Allows to insert a new update in the system
 */

ESint32 EDMAPROC
edma_add_update (EDMA_UPDATE update)
{
  if (GVar->num_updates < EDMA_MAX_UPDATES)
    {
      memcpy (GVar->pending_update, &update, sizeof(EDMA_UPDATE));
      GVar->num_updates++;
      GVar->time_last_update = time(NULL);
    }
  else
    {
      return _edma_system_exception ("Update queue is full. "
				     "Wait for pending update to be completed");
    }

  return 0;
}


/* edma_remove_update
 *  Removes an existing update of the system
 */

ESint32 EDMAPROC
edma_remove_update (ESint32 indx)
{
  if ((indx > 0) && (indx < GVar->num_updates))
    {
      /* Compact pending updates array */
      memcpy (&GVar->pending_update[indx], &GVar->pending_update[indx + 1], 
	      sizeof (EDMA_UPDATE) * (GVar->num_updates - indx));
      /* clear last entry in array.
       * Think if this operation can be removes for optimization purpouses */
      memset (&GVar->pending_update[GVar->num_updates], 0, sizeof(EDMA_UPDATE));
      GVar->num_updates --;
    }
  return 0;
}
