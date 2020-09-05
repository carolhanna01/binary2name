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

/* GNU/EDMA Thread Management 
 * Revisions:--------------------------------------------------------
 * August, 19th, 2004
 * Modification to add thread to the GNU/EDMA Global Thread List
 */

#include "portable.h"
#include "vglobal.h"
#include "shmem.h"
#include "linuxth.h"
#include "ethread.h"

#include "misc.h"
#include "tobj.h"

/* edma_thread_register
 *  Register current thread in the system. Sets up TLS for this thread
 *  and begins to consider it in EDMA update system
 */

ESint32 EDMAPROC
edma_thread_register (void)
{
  OBJID          *aux;
  ETHREAD_DATA   *temp;
  ETHREAD        current;
  ESint32        i;

  /* Alloc specific thread data block on Run-time TSD key*/
  current = edma_thread_self();
    
  if ((aux = edma_tsd_get_data (thread_stack_key)) == NULL)
    {
      edma_log ("[edma_thread_register] Thread %p not registered. "
		"Registering now",
		   current);
      /* Allocate Thread Execution Stack */
      /* FIXME: Go through Alloc/Get process*/
      if ((temp = (ETHREAD_DATA*) edma_palloc (sizeof (ETHREAD_DATA))) == NULL)
	{
	  edma_printf_err ("[edma_thread_register] %s",
			   "Can't alloc stack for thread. Register aborted");
	  return -1;
	}
      if ((aux = (OBJID *)edma_palloc (sizeof (OBJID) * MAX_STACK_EXECUTION)) 
	  == NULL)
	{
	  edma_pfree ((HMEM)temp, temp);
	  edma_printf_err ("[edma_thread_register] %s",
			   "Can't alloc stack for thread. Register aborted");
	  return -1;
	}
      /* Initialization */
      temp->tid = current;
      temp->top = 0;
      temp->current_stack_execution = aux;
      for (i = 0; i < MAX_STACK_EXECUTION; i++)
	temp->current_stack_execution[i] = -1;
      edma_tsd_set_data (thread_stack_key, temp);

      /* Add thread to GNU/EDMA Global Thread List */
      if ((thread_list = (ETHREAD_DATA**) 
	   edma_prealloc ((HMEM)thread_list, (num_threads + 1) 
			  * sizeof (ETHREAD_DATA*))) == NULL)
	{
	  edma_printf_err ("[edma_thread_register] %s",
			   "Can't alloc memory for thread in "
			   "GNU/EDMA Global Thread List");
	  /* Free Memory */
	  edma_pfree ((HMEM)aux, aux);
	  edma_pfree ((HMEM)temp, temp);
	  edma_tsd_set_data (thread_stack_key, NULL);

	  return -1;
	}
      /* Update Global Thread List */
      thread_list[num_threads] = (ETHREAD_DATA*) temp;
      num_threads++;
    }

  return 0;
}

/* edma_thread_unregister
 *   Allows a given thread to stop using GNU/EDMA 
 */

ESint32 EDMAPROC
edma_thread_unregister (void)
{
  ETHREAD_DATA    *aux;
  ETHREAD         current;
  ESint32         i;
  /* Free Specific Thread Data from Run-time TSD key*/

  current = edma_thread_self();

  edma_printf ("[edma_thread_unregister] Unregistering thread %p", current);
  if ((aux = edma_tsd_get_data (thread_stack_key)) != NULL)
    {
      /* Locate current thread in GNU/EDMA Global Thread List and remove it*/
      for (i = 0; i < num_threads; i++)
	  if (thread_list[i]->tid == current)
	    break;
      
      printf ("======> Index %ld of %ld", i, num_threads);
      if (i != num_threads)
	{
	  edma_printf ("[edma_thread_unregister] Thread %p found at index %d", 
		       current, i);
	  thread_list[i] = NULL;
	  /* Compact Table */
	  if (i != num_threads -1)
	    memcpy (thread_list + i, (thread_list + i + 1), 
		    sizeof (ETHREAD_DATA*) * (num_threads - i));
	  num_threads --;
	}

      edma_pfree ((HMEM)aux->current_stack_execution, 
		  aux->current_stack_execution);
      edma_pfree ((HMEM)aux, aux);

    }

  return 0;
}

/* edma_thread_list
 *  DEBUG function to list all registered threads in system 
 */
ESint32 EDMAPROC
edma_thread_list (void)
{
  ESint32    i;
  
  edma_printf ("[edma_thread_list] GNU/EDMA Global Thread List:");
  for (i = 0; i < num_threads; i++)
    edma_printf ("[edma_thread_list] Index %d: Thread 0x%p Thread Stack: %d", 
		 i, thread_list[i]->tid, thread_list[i]->top);

  return 0;
}
