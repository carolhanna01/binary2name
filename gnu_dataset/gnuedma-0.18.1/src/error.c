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

#include <stdio.h>
#include <stdarg.h>
#include <stdlib.h>
#include <setjmp.h>

#include "portable.h"
#include "vglobal.h"
#include "const.h"
#include "shmem.h"
#include "misc.h"

#include "obj.h"
#include "pri3.h"

/* Stack Exceution management -> Maybe will be moved to other file*/
#include "hotswap.h"


/* edma_error 
 *  Returns last system error
 */
ESint32 EDMAPROC
edma_error ()
{
/* FIXME. This should be stored in thread TLS */
  return edma_last_error;
}

/* FIXME: Update this for multithread support */
static jmp_buf **edma_cntx = NULL;

/* Basic support for exceptions */

/* edma_exception_try
 *   try constructor simulator in C
 */

ESint32 EDMAPROC
edma_exception_try (jmp_buf *env)
{
  edma_in_try_block++;
  if (edma_cntx == NULL)
    {
      if (( edma_cntx = 
	    (jmp_buf **) 
	    edma_palloc (sizeof (jmp_buf *) * edma_in_try_block)) == 0)
	{
	  edma_printf ("[edma_exception_try] Can't alloc context "
		       "exception table");
	  return -1;
	}
    }
  else
    {
      if (( edma_cntx = 
	    (jmp_buf **) 
	    edma_prealloc ((HMEM) edma_cntx, 
			   sizeof (jmp_buf *) * edma_in_try_block)) == 0)
	{
	  edma_printf ("[edma_exception_try] Can't realloc context "
		       "exception table");
	  return -1;
	}
    }
  edma_cntx[edma_in_try_block - 1] = env;
  return 0;
}

/* edma_exception_throw
 *  throw constructor simulator in C
 */

ESint32 EDMAPROC
edma_exception_throw (OBJID val)
{
  if (edma_in_try_block) 
    {
      /* Remove exception object from execution stack */
      _edma_stack_execution_del_obj (val);
      longjmp (*edma_cntx[edma_in_try_block - 1], val + 1);
    }
  else
    {
      /* If not in try block just show the message and destroy the
       * exception object*/
      edma_met3 (val, "brief");
      edma_free_obj (val);
      return -1;
    }
  return 0;
}


/* edma_exception_clean
 *   Manages the exit from a 'try' block
 */

ESint32 EDMAPROC
edma_exception_clean (OBJID id)
{
  if (edma_in_try_block)
    {
      edma_in_try_block--; /* Clear the exception*/
    }
  return 0;
}


/* _edma_system_exception
 *   Internal function used to launch a system exception 
 * catcheable by applications
 */
ESint32 EDMAPROC
_edma_system_exception (EPChar fmt,...)
{
  EChar     msg[1024];
  OBJID     eex;
  va_list   p;

  va_start(p,fmt);
  vsprintf (msg, fmt, p);
  eex = edma_new_obj ("EDMA_EXCEPTION");
  edma_met3 (eex, "set", 1, msg);
  edma_met3 (eex, "throw");
  /* FIXME: Here we should set edma_last_error*/
  return -1;

}

/* _edma_system_exception_clean
 *   Clean a system exception
 */

ESint32 EDMAPROC
_edma_system_exception_clean ()
{
  /* Restore top */
  edma_in_try_block = 0;
  free (edma_cntx);
  edma_cntx = 0;
  return 0;
}
