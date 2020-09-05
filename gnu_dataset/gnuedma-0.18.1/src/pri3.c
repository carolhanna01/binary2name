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

/******************************************************************************
 * Entorno de Desarrollo Modular y Abierto
 * (c) David Martínez Oliveira
 * Versión Beta 0.3r1
 * 2 de Julio de 1997
 *
 * Primitivas de Objeto de Nivel 3
 *
 * REVISIONES:-------------------------------------------------------------
 * 10 de Julio de 1997
 * Añadimos la función GetRef3 para acceso directo al bloque de datos
 * de los objetos. Se ha testeado con enteros satisfactoriamente. Falta testear
 * con otros tipos.
 * --------------------------------
 * 4 de Agosto de 1997
 *
 * Vamos a añadir el soporte para métodos polimórficos mediante signaturas
 * y a añadir modificaciones para la integración de SIU.
 * ----------------------------
 * 7 de AGosto de 1997
 *
 * Estamos modificando la función de las signaturas para trabajar con múltiples
 * parámetros.
 * Revisión del interface de las primitivas de nivel 3
 *-------------------------------
 * 8 de Agosto de 1997
 * Movemos las funciones de parssing a otro módulo
 * Modificación para mapeo de código por proceso. Leemos el puntero a la función
 * de pClase en lugar de la estructura global    
 * -----------------------------------------
 * 21 de Agosto de 1997
 * Utilizamos ECoutErr para mostrar los mensajes de ERROR
 * COrregimos el mensaje en la primitiva de éjecución de objetos
 * --------------------------------------
 * 22 de Agosto de 1997
 * Modificación en las funciones de escritura/lectura de propiedades
 * para soportar objetos virtuales
 * ----------------------------------------
 * 31 de Agosto de 1997
 * Intentamos la modificación para integración de SIU
 * --------------------------
 * 21/11/1997
 *   Modificación para el soporte de sobrecarga de métodos virtuales desde
 * la aplicación.
 * 
 *    SI el objeto asociado al metodo sobrecargado es NULL, consideramos
 * que ha sido sobrecargado por una aplicación y no obtenemos el identificador
 * del objeto del puntero, sino que se iguala a -1
 * ----------------------------
 * 10/12/1997
 * 
 *  Intentamos añadir funciones primitivas de soporte para proxys, que
 * reciban los parametros empaquetados en un bloque de memoria.
 * ---------------------------
 * 26/01/1998
 * 
 *   Intentamos añadir el interface de un nivel para la herencia
 * ------------------------------------------------------------
 * Febraury, 7th, 2001
 * Code cleanup and comment translation
 * ---------------------------------------------------------------------
 * November, 17th, 2001
 * Compile warnnings removal
 * -------------------------------------------------------------------
 * January, 22th, 2002
 * Updated Met3 to work properly with SIU Proxies
 * -----------------------------------------------------------------------
 * March, 2nd, 2002
 * Code cleanup
 * ----------------------------------------------------------------------
 * March 17th, 2002
 * Added new primitives working with the new inheritance model
 * The primitives name was chanfed to edma_primName
 * 
 * Added new function to lookup properties and methods through
 * class hierarchy
 * ------------------------------------------------------------------------------
 * April, 2nd, 2003
 * Changes to support new OBJ struct
 * --------------------------------------------------------------------------
 * May, 10th, 2003
 * Modification to support changes to internal class structures
 * -------------------------------------------------------------------------------
 * May, 31th, 2003
 * Add type-safe wrapper for property access
 * ---------------------------------------------------------------------------
 * July, 4th, 2003
 * Input Parameter Sanity checks
 * -----------------------------------------------------------------------------
 * January, 4th, 2005
 * Restored signature expansion on met3s primitive
 */

#include <stdio.h>
#include <string.h>
#include <stdlib.h>
#include <stdarg.h>
#include "portable.h"
#include "vglobal.h"
#include "const.h"
#include "pri1.h"
#include "pri3.h"
#include "misc.h"
#include "obj.h"
#include "locators.h"
#include "anchor_points.h"
#include "helper.h"

#include "pri3.h"
#include "pri3x.h"
#include "error.h"

#include "hotswap.h"

extern EUint32 EDMAEnd();
/*
** Level 3 object primitives
*/


/*************************************************************************/
/*
 * Direct object data mapping for class implementation improvement
 */

/* edma_get_data_ref
 *    Returns a pointer to private object property structure
 */

EPVoid	EDMAPROC	
edma_get_data_ref (OBJID IdObj) 
{
  if ((edma_check_obj_id (IdObj, "edma_get_data_ref")) == -1) 
    return NULL;

  return (EPVoid)gObj[IdObj]->Data;
}

static ESint32 EDMAPROC
_edma_check_suicide (OBJID IdObj)
{
  if (gObj[IdObj] && gObj[IdObj]->Flag == OBJ_DIE)
    {
      /* FIXME: This has to be a bit more complex. For now, just delete the
       *        object if its recursion level is 0
       */
      if (!gObj[IdObj]->rlevel)
	edma_free_obj (IdObj);
    }
  return 0;
}

/*************************************************************************
 * New Level 3 primitives
 */

/* edma_met3
 *   level 3 primitive to invoke a method
 */ 

ESint32 EDMAPROC
edma_met3 (OBJID IdObj, EPChar Id1,...)
{
  va_list   p;
  ESint32   ret_val;
  ESint32   i;
  
  if ((edma_check_obj_id1 (IdObj, "edma_met3", Id1)) == -1) 
    return -1;

  if (Id1 == NULL)
    {
      edma_printf_err ("%s", "[edma_met3] Invalid Method Name (NULL)");
      return -1;
    }

  /* Check for pending updates and proceed if possible */
  if (last_checked_update < GVar->time_last_update)
    edma_hotswap (NULL);
  
  va_start(p,Id1);

  gObj[IdObj]->rlevel ++;

  if (gObj[IdObj]->rlevel >= MAX_RECURSION)
    {
      va_end(p); /* XXXXXX */
      edma_printf ("FATAL ERROR :: [edma_met3] %ld recursion level achieved. "
		   "Probably, there is a cycle in your class diagram.\n"
		   "            Method %s on object %ld <%s> not executed\n"
		   "**** ABORTING execution" , gObj[IdObj]->rlevel, Id1, IdObj, 
		   gClass[gObj[IdObj]->IdClass]->ClassName);

      edma_printf ("==== DEBUG: Max Recursion Level Object List ="
		   "========================");
      for (i = 0; i < nMaxObj; i++)
	{
	  if (gObj[i] != NULL)
	    if (gObj[i]->rlevel > MAX_RECURSION - MAX_RECURSION * 0.1)
	      edma_printf ("--> Object %05ld [%05ld] <%-30s> Recursion: %05ld. "
			   "Last Method: '%s'",
			   i, gObj[i]->IdClass, 
			   gClass[gObj[i]->IdClass]->ClassName, 
			   gObj[i]->rlevel,
			   pClass[gObj[i]->IdClass]->Met[gObj[i]->last_met].IdMet);
	}
      edma_printf ("======================================"
		   "========================\n");

      EDMAEnd();
      exit (1);
      return -1;
    }

  /* Run method dealing with overriden virtual methods*/
  ret_val = edma_met3_pargs (IdObj, Id1, NULL, 1, p);

  va_end(p); /* XXXX */

  if (gObj[IdObj])
    gObj[IdObj]->rlevel --;
  
  _edma_check_suicide (IdObj);

  return ret_val;
}

/* edma_met3s
 *   Level 3 primtive to invoke a method with signature
 */

ESint32 EDMAPROC
edma_met3s (OBJID IdObj, EPChar Id1, EPChar Sig,...)
{
  va_list   p;
  ESint32   ret_val;
  ESint32   i, len, npar;
  EChar     sig_ext[EDMA_MET_SIG_LEN], *src, *dest;
  OBJID     sObj;
  
  if ((edma_check_obj_id (IdObj, "edma_met3s")) == -1) 
    return -1;

  if (Id1 == NULL)
    {
      edma_printf_err ("%s", "[edma_met3s] Invalid Method Name (NULL)");
      return -1;
    }

  /* Check for pending updates and and proceed if possible */
  if (last_checked_update < GVar->time_last_update)
    edma_hotswap (NULL);

  va_start(p,Sig);

  /* Do signature expansion. */
  /* XXX: Maybe should be refactored to a function */
  src = Sig;
  dest = sig_ext;
  npar = 0;
  /*edma_printf ("processing signature '%s'", Sig);*/

  /* FIXME: This should be moved to a set of functions as it is used 
  *         for other modules
  */
  while (*src != 0)
    {
      switch (*src)
	{
	case 'L':  /* Class Name */
	  {
	    src++;  /* Skip L*/
	    while (*src != ';' && *src != 0)
	      {
		*dest = *src;
		dest++;
		src++;
	      }
	    /* FIXME: Here should check if class exists */
	    dest--;
	    break;
	  }
	case 'X':  /* Expandable Object*/
	  {
	    /* FIXME: Change object acquire bellow for a more PORTABLE process*/
	    sObj = (OBJID)(*((EPUint32)p + npar));

	    if ((edma_check_obj_id (sObj, "edma_met3s")) == -1) 
	      return -1;

	    len = strlen (gClass[gObj[sObj]->IdClass]->ClassName);
	    strcpy (dest, gClass[gObj[sObj]->IdClass]->ClassName);	
	    len --;
	    dest += len;
	    break;
	  }
	default:
	  {
	    *dest = *src;	  
	    break;
	  }
	}
      npar++;
      dest++;
      src++;
    }
  *dest = 0;
  /*
  edma_printf ("%s", "-----------------------------");
  edma_printf ("Processed %d parameters", npar);
  edma_printf ("Original Signature: '%s'", Sig);
  edma_printf ("Expanded Signature: '%s'", sig_ext);
  edma_printf ("%s", "-----------------------------");
  */

  gObj[IdObj]->rlevel ++;

  if (gObj[IdObj]->rlevel >= MAX_RECURSION)
    {
      va_end(p); /* XXXXXX */
      edma_printf ("FATAL ERROR :: [edma_met3] %ld recursion level achieved. "
		   "Probably, there is a cycle in your class diagram.\n"
		   "            Method %s on object %ld <%s> not executed\n"
		   "**** ABORTING execution" , gObj[IdObj]->rlevel, Id1, IdObj, 
		   gClass[gObj[IdObj]->IdClass]->ClassName);

      edma_printf ("==== DEBUG: Max Recursion Level Object List ====="
		   "====================");
      for (i = 0; i < nMaxObj; i++)
	{
	  if (gObj[i] != NULL)
	    if (gObj[i]->rlevel > MAX_RECURSION - MAX_RECURSION * 0.1)
	      edma_printf ("--> Object %05ld [%05ld] <%-30s> Recursion: %05ld. "
			   "Last Method: '%s'",
			   i, gObj[i]->IdClass, 
			   gClass[gObj[i]->IdClass]->ClassName, 
			   gObj[i]->rlevel,
			   pClass[gObj[i]->IdClass]->Met[gObj[i]->last_met].IdMet);
	}
      edma_printf ("======================================================"
		   "========\n");

      EDMAEnd();
      exit (1);
      return -1;
    }

  /* Run method dealing with overriden virtual methods*/
  ret_val = edma_met3_pargs (IdObj, Id1, sig_ext, 1, p);
  va_end(p); /* XXXX */
  if (gObj[IdObj])
    gObj[IdObj]->rlevel --;

  _edma_check_suicide (IdObj);

  return ret_val;
}

/* edma_wprop3
 *    Level 3 write property primitive
 */
ESint32 EDMAPROC
edma_wprop3 (OBJID IdObj, EPChar Id1,...)
{
  va_list   p;
  ESint32   ret_val;
  
  if (Id1 == NULL)
    {
      /* FIXME: Launch exception here*/
      edma_printf_err ("%s", "[edma_wprop3] Invalid Property Name (NULL)");
      return -1;
    }
  va_start(p,Id1);
  ret_val = edma_wprop3_pargs (IdObj, Id1, p);
  va_end(p);
  return ret_val;
}

/* edma_rprop3
 *    Level 3 read property primitive
 */

ESint32 EDMAPROC
edma_rprop3 (OBJID IdObj, EPChar Id1,...)
{
  va_list   p;
  ESint32   r;

  if (Id1 == NULL)
    {
      /* FIXME: Launch exception here*/
      edma_printf_err ("%s", "[edma_wprop3] Invalid Property Name (NULL)");
      return -1;
    }  
  va_start(p,Id1);
  r = edma_rprop3_pargs (IdObj, Id1, p);
  va_end(p);
  return r;
}

/* Type-Safe wrappers, just for compile-time warnning */
/* First implementation just call edma_wprop3 and edma_rprop3. 
 * Final implementation should call edma_wprop3_pargs and edma_rprop3_pargs
 * in order to avoid a function call*/

/* edma_set_prop_sint32
 *   Type-safe wrapper to write 32bits signed integer properties
 */

ESint32 EDMAPROC
edma_set_prop_sint32 (OBJID IdObj, EPChar Id1, ESint32 val)
{
  ESint32   ret_val;

  ret_val = edma_wprop3 (IdObj, Id1, val);
  return ret_val;
}

/* edma_set_prop_uint32
 *   Type-safe wrapper to write 32bits unsigned integer properties
 */

ESint32 EDMAPROC
edma_set_prop_uint32 (OBJID IdObj, EPChar Id1, EUint32 val)
{
  ESint32   ret_val;

  ret_val = edma_wprop3 (IdObj, Id1, val);
  return ret_val;
}

/* edma_set_prop_sint16
 *   Type-safe wrapper to write 16bits signed integer properties
 */

ESint32 EDMAPROC
edma_set_prop_sint16 (OBJID IdObj, EPChar Id1, ESint16 val)
{
  ESint32   ret_val;

  ret_val = edma_wprop3 (IdObj, Id1, val);
  return ret_val;
}

/* edma_set_prop_uint16
 *   Type-safe wrapper to write 16bits unsigned integer properties
 */

ESint32 EDMAPROC
edma_set_prop_uint16 (OBJID IdObj, EPChar Id1, EUint16 val)
{
  ESint32   ret_val;

  ret_val = edma_wprop3 (IdObj, Id1, val);
  return ret_val;
}

/* edma_set_prop_sint8
 *   Type-safe wrapper to write 8bits signed integer properties
 */

ESint32 EDMAPROC
edma_set_prop_sint8 (OBJID IdObj, EPChar Id1, ESint8 val)
{
  ESint32   ret_val;

  ret_val = edma_wprop3 (IdObj, Id1, val);
  return ret_val;
}

/* edma_set_prop_uint8
 *   Type-safe wrapper to write 8bits unsigned integer properties
 */

ESint32 EDMAPROC
edma_set_prop_uint8 (OBJID IdObj, EPChar Id1, EUint8 val)
{
  ESint32   ret_val;

  ret_val = edma_wprop3 (IdObj, Id1, val);
  return ret_val;
}

/* edma_set_prop_strz
 *   Type-safe wrapper to write zero-terminated string properties
 */

ESint32 EDMAPROC
edma_set_prop_strz (OBJID IdObj, EPChar Id1, EPChar val)
{
  ESint32   ret_val;

  ret_val = edma_wprop3 (IdObj, Id1, val);
  return ret_val;
}

/* edma_set_prop_strz
 *   Type-safe wrapper to write object properties
 */

ESint32 EDMAPROC
edma_set_prop_obj (OBJID IdObj, EPChar Id1, OBJID val)
{
  ESint32   ret_val;

  ret_val = edma_wprop3 (IdObj, Id1, val);
  return ret_val;
}




/* Get accesors */

/* edma_get_prop_sint32
 *   Type-safe wrapper to read 32 bits signed properties
 */

ESint32 EDMAPROC
edma_get_prop_sint32 (OBJID IdObj, EPChar Id1)
{
  ESint32   val;
  
  edma_rprop3 (IdObj, Id1, &val);
  return val;
}

/* edma_get_prop_uint32
 *   Type-safe wrapper to read 32 bits unsigned properties
 */

EUint32 EDMAPROC
edma_get_prop_uint32 (OBJID IdObj, EPChar Id1)
{
  EUint32   val;
  
  edma_rprop3 (IdObj, Id1, &val);
  return val;
}

/* edma_get_prop_sint16
 *   Type-safe wrapper to read 16 bits signed properties
 */

ESint16 EDMAPROC
edma_get_prop_sint16 (OBJID IdObj, EPChar Id1)
{
  ESint16   val;
  
  edma_rprop3 (IdObj, Id1, &val);
  return val;
}

/* edma_get_prop_uint16
 *   Type-safe wrapper to read 16 bits unsigned properties
 */

EUint16 EDMAPROC
edma_get_prop_uint16 (OBJID IdObj, EPChar Id1)
{
  EUint16   val;
  
  edma_rprop3 (IdObj, Id1, &val);
  return val;
}

/* edma_get_prop_sint8
 *   Type-safe wrapper to read 8 bits signed properties
 */

ESint8 EDMAPROC
edma_get_prop_sint8 (OBJID IdObj, EPChar Id1)
{
  ESint8   val;
  
  edma_rprop3 (IdObj, Id1, &val);
  return val;
}

/* edma_get_prop_sint8
 *   Type-safe wrapper to read 8 bits unsigned properties
 */

EUint8 EDMAPROC
edma_get_prop_uint8 (OBJID IdObj, EPChar Id1)
{
  EUint8   val;
  
  edma_rprop3 (IdObj, Id1, &val);
  return val;
}

/* WARNNING... We need a buffer*/
/* edma_get_prop_strz
 *   Type-safe wrapper to read zero-terminated string properties
 */
EPChar EDMAPROC
edma_get_prop_strz (OBJID IdObj, EPChar Id1, EPChar val)
{
  /* Here check for size */
  edma_rprop3 (IdObj, Id1, val);
  return val;
}

/* edma_get_prop_buffer
 *   Type-safe wrapper to read EDMAT_BUFFER properties
 */

EDMAT_BUFFER *EDMAPROC
edma_get_prop_buffer (OBJID IdObj, EPChar Id1, EDMAT_BUFFER *buf)
{
  /* FIXME: This returns a copy of the struct but not of the data
   *        This breaks encapsulation principle, but is usefull for
   *        zero-buffer processing.
   *
   *        We should provide two function to deal with buffer copies
   *        or buffer references.
   *
   *        Such a change requires modification on pri1.c
   */
  edma_rprop3 (IdObj, Id1, buf);
  return buf;
}

/* edma_get_prop_obj
 *   Type-safe wrapper to read object properties
 */

OBJID EDMAPROC
edma_get_prop_obj (OBJID IdObj, EPChar Id1)
{
  OBJID   val;
  
  edma_rprop3 (IdObj, Id1, &val);
  return val;
}


