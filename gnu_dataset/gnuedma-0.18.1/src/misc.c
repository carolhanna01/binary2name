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
 * Vigo 18 de Octubre de 1996
 *
 * Versión 0.3r1
 * ---------------------------------------------------------
 * Módulo de funciones miscelánea
 * --------------------------------------------------------
 * 14 de Noviembre de 1996
 * (San Teleco 1:45 am)
 * Vamos a añadir funciones para reservar bloques de memoria
 * . Hacemos que los reserve EDMA, para probar si los problemas
 * con los tamaños de los bloques al descargar clase es debido
 * a esto
 * ----------------------------------------------------------------
 * 29 de Abril de 1997
 * Modificación de tipos para Beta WIN32
 *********************************************************
 * 8 de Agosto de 1997
 * Añadimos funciones para nombrar aplicaciones
 * -----------------------------
 * 19/11/1997
 *   Añadimos funciones para la gestion del tipo BUFFER
 *   Estas funciones serán movidas a otro fichero cuando alcancen
 * un número importante
 * ----------------------------------------------------
 * Febraury, 7th, 20001
 * Code cleanup and comment translation
 * -----------------------------------------------------------------
 * October, 14th,2001
 * Added EBufferRealloc function
 * -------------------------------------------------------------------
 * November, 18th, 2001
 * Compile Warnning removal
 * ----------------------------------------------------------------
 * March, 2nd, 2002
 * Code cleanup
 * -------------------------------------------------------
 * May, 10th, 2003
 * Modification to support changes to internal class structures
 * --------------------------------------------------------------------
 * August, 23th, 2003
 * Added simple function for dictionary management
 * ---------------------------------------------------------------------
 * August, 25th, 2003
 * Added function for multiple searchs in dictionary. 
 * Required for method + signature and multiversioned classes search
 * ------------------------------------------------------------------------
 * Febraury, 7th, 2004
 * Updated edma_dict_get_next_index to pass/update last visited position
*/
 
#include <stdarg.h>
#include "portable.h"
#include "misc.h"     
#include "error.h"
#include "vglobal.h"
#include "vserv.h"
#include "shmem.h"
#include "helper.h"
/*
** Errors **********************************************
*/
#define NUM_ERRORS 17
#define NUM_TITLES 12
#define MSG_LEN    1024

#define DEBUG_LEVEL_LOG  20

/* edma_print
 *   Prints a string to strdout
 */

EUint32 EDMAPROC 
edma_print (EPChar a) 
{
  printf ("%s\n",a);
  return 0;
}

/* edma_printf
 *   Prints with format
 */

EUint32 EDMAPROC 
edma_printf (EPChar f,...) 
{
  va_list	p;
  EChar	        msg[MSG_LEN];
  
  va_start(p,f);

  vsnprintf(msg, MSG_LEN, f, p);
  printf ("%s\n", msg);

  return 0;
}



/* edma_printf
 *   Prints with format
 */

EUint32 EDMAPROC 
edma_log (EPChar f,...) 
{
  va_list	p;
  EChar	        msg[MSG_LEN];

  if (GVar->DebugLevel > DEBUG_LEVEL_LOG) return 0;
  va_start(p,f);

  vsnprintf(msg, MSG_LEN, f, p);
  printf ("LOG: %s\n", msg);

  return 0;
}


/* edma_printf_obj
 *   Prints with format showing class of the object passed as parameter
 */

EUint32 EDMAPROC 
edma_printf_obj (OBJID IdObj,EPChar f,...) 
{
  va_list	p;
  EChar	        msg[MSG_LEN];
  EChar	        msg1[MSG_LEN];
 
  if ((edma_check_obj_id (IdObj, "edma_printf_obj")) == -1)
    return -1;

  va_start(p,f);

  snprintf (msg1, MSG_LEN, "(%s)", gClass[gObj[IdObj]->IdClass]->ClassName);
  vsnprintf (msg, MSG_LEN, f, p);
  strncat (msg1, msg, MSG_LEN);
  printf ("%s\n", msg1);

  va_end(p);
  return 0;
}

/* edma_printf_err
 *   Prints an error to stdout
 */
EUint32 EDMAPROC 
edma_printf_err (EPChar f,...) 
{
  va_list	p;
  EChar	        msg[MSG_LEN];
  EChar	        msg1[MSG_LEN];
  
  va_start(p,f);

  strncpy (msg1, "**ERROR**", MSG_LEN);
  vsnprintf(msg, MSG_LEN, f, p);
  strncat (msg1, msg, MSG_LEN);
  /*printf ("%s\n",msg1);*/
  fprintf (stderr, "%s\n", msg1);

  va_end(p);
  return 0;
}

/* edma_printf_dbg
 *   Print a formated message if the debug level is greater that the
 *   system debug level
 */

EUint32 EDMAPROC 
edma_printf_dbg (EUint32 l,OBJID IdObj,EPChar f,...) 
{
  va_list	p;
  EChar	        msg[MSG_LEN];
  EChar	        msg1[MSG_LEN];

  if (l >= GVar->DebugLevel) 
    {
      va_start(p,f);
      if (IdObj == -1)
	strncpy (msg1, "(DEBUG-System)", MSG_LEN);
      else
	snprintf (msg1, MSG_LEN, "(DEBUG-%s)",
		  gClass[gObj[IdObj]->IdClass]->ClassName);
      vsnprintf (msg, MSG_LEN, f, p);
      strncat (msg1, msg, MSG_LEN);
      printf ("%s\n",msg1);
      va_end(p);
    }
  return 0;
}

/* Various functions... App Naming....*/

/* edma_set_app_name
 *   Associates a name to current process for printing
 *   This function is currently not used anywhere. 
 *   Probably it will be removed
 */

EUint32 EDMAPROC 
edma_set_app_name (EPChar n) 
{
  if (n == NULL)
    {
      edma_printf_err ("%s", "[edma_set_app_name] Invalid Application Name");
      return -1;
    }

  strncpy (AppName, n, EDMA_GENERAL_ID_LEN);
  return 0;
}

/* edma_get_system_path
 *   Returns system path. DIrectory where all GNU/EDMA files can be found
 */
EPChar EDMAPROC 
edma_get_system_path (EPChar c) 
{
  /* FIXME: ???????*/
  return strdup (GVar->SystemPath);
}

/* edma_set_debug_level
 *    Sets current system debub level
 */

EUint32 EDMAPROC 
edma_set_debug_level (EUint32 l) 
{
  EUint32		o;
  
  o = GVar->DebugLevel;
  GVar->DebugLevel = l;
  return 0;
}

/*
 * Funciones de Gestión del tipo EDMAT_BUFFER
 * 19/11/1997
 * ------------------------
 * Estas funciones compondrán un nuevo módulo del sistema cuando
 * alcancen un número importante
 * ---------------------------------------------------------------
 * Febraury, 7th, 2001
 * EDMAT_BUFFER datatype management functions
 */

/* edma_buffer_alloc
 *    Allocates a buffer of type EDMAT_BUFFRE
 */

ESint32 EDMAPROC 
edma_buffer_alloc  (EDMAT_BUFFER *Buf, EUint32 Size) 
{
  if (Buf == NULL)
    {
      edma_printf_err ("%s", "[edma_buffer_alloc] Invalid target buffer");
      return -1;
    }
  Buf->h = edma_palloc (Size);
  if (Buf->h == 0) 
    {
      edma_printf_err ("%s", "[edma_buffer_alloc] Can't allocate buffer");
      return -1;
    }
  Buf->dat = (EPVoid) edma_pget (Buf->h);
  Buf->Size = Size;
  /*Set to zero  memory block*/
  memset (Buf->dat, 0, Size);
  return 0;
}

/* edma_buffer_free
 *   Frees a buffer of type EDMAT_BUFFER
 */

ESint32 EDMAPROC 
edma_buffer_free (EDMAT_BUFFER *Buf) 
{
  if (Buf == NULL)
    {
      edma_printf_err ("%s", "[edma_buffer_free] Invalid target buffer");
      return -1;
    }
  
  if (Buf->Size != 0)
    edma_pfree (Buf->h, Buf->dat);
  Buf->Size = 0;

  return 0;
}

/* edma_buffer_realloc
 *   Reallocates a buffer of type EDMAT_BUFFER
 */

ESint32 EDMAPROC 
edma_buffer_realloc (EDMAT_BUFFER *Buf, EUint32 Size) 
{
  if (Buf == NULL)
    {
      edma_printf_err ("%s", "[edma_buffer_realloc] Invalid target buffer");
      return -1;
    }
  /* CAUTION!!!: Realloc function had not been developed with windows
     32 compatibility in mind... that is, we don't know what's the windows
     API for this, so this interface can change in future*/
  Buf->h = edma_prealloc (Buf->h,Size);
  if (Buf->h == 0)  
    {
      edma_printf_err ("%s", "[edma_buffer_realloc] Can't reallocate buffer");
      return -1;
    }
  Buf->dat = (EPVoid) edma_pget (Buf->h);
  Buf->Size = Size;
  return 0;
}

/* Dictionary functions *************************/

/* Simplest hash function ever seen*/

/* edma_dict_map_string
 *   Generates a hash key for a given dictionary and string
 */

ESint32 EDMAINLINE
edma_dict_map_string (EDMA_DICT d, EPChar str)
{
  ESint32   i, hash = 0;

  for (i = 0; str[i] !=0; i++)
    hash += str[i];

  hash = (EUint32)(hash % d->size);

  return hash;
}

/* edma_dict_new
 *    Create a new dictionary datastruct (hash table)
 */

EDMA_DICT EDMAPROC
edma_dict_new (ESint32 size)
{
  HMEM       h;
  EDMA_DICT  the_dict;
  ESint32    i;

  if (size <= 0) return NULL;

  if ((h = edma_palloc (sizeof(EDMA_DICT_T))) == (HMEM)0)
    {
      edma_printf_err ("%s", "[edma_dict_new] Can't allocate memory "
		       "to create Dictionary");
      return NULL;
    }

  the_dict = edma_pget (h);
  the_dict->h_myself = h;

  /* Allocate data entries */
  if ((h = edma_palloc (sizeof (EDMA_DICT_ITEM) * size * 2)) == (HMEM)0)
    {
      edma_printf_err ("%s", "[edma_dict_new] Can't allocate memory "
		       "for dictionary entries");
      edma_pfree (the_dict->h_myself, the_dict);
      return NULL;
    }

  the_dict->h_entry = h;
  the_dict->entry = edma_pget(h);
  
  memset (the_dict->entry, 0, sizeof(EDMA_DICT_ITEM) * 2 * size);
  for (i = 0; i < size * 2; i++)
    the_dict->entry[i].next = -1;

  the_dict->size = size;
  the_dict->over_size = size;
  the_dict->over_indx = 0;

  return the_dict;
}


/* edma_dict_free
 *   Frees a dictionary data structure (hash table)
 */

ESint32 EDMAPROC
edma_dict_free (EDMA_DICT d)
{
  /* free entries */
  /* XXXX: Check errors freeing memory ????*/
  if (!d)  return -1;
  edma_pfree (d->h_entry, d->entry);
  edma_pfree (d->h_myself, d);

  return 0;
}

/* edma_dict_add_entry
 *   Adds a new entry to a given dictionary
 */

ESint32 EDMAPROC
edma_dict_add_entry (EDMA_DICT d, EPChar name, ESint32 indx)
{
  EUint32    pos;
  
  if (!d)    return -1;
  if (!name) return -1;

  if (edma_dict_get_index (d, name) >= 0)
    {
      edma_log ("[edma_dict_add_entry] WARNNING: Entry '%s' already exists", 
		name);
      /*return -1;*/
    }

  pos = edma_dict_map_string (d, name);
  /* If entry is empty. Store data there*/
  if (d->entry[pos].data == NULL)
    {
      d->entry[pos].data = name;
      d->entry[pos].indx = indx;
      d->entry[pos].next = -1;
    }
  else /* else, store in overflow section */
    {
      while (d->entry[pos].next >= 0) 
	pos = d->entry[pos].next;

      d->entry[d->size + d->over_indx].data = name;
      d->entry[d->size + d->over_indx].indx = indx;
      d->entry[d->size + d->over_indx].next = -1;
      
      d->entry[pos].next = d->size + d->over_indx;
      d->over_indx++;
    }

  return 0;
}

/* edma_dict_get_index
 *    Get value(index) associated to the given key (name) in the indicated 
 *    dictionary
 */

ESint32 EDMAPROC
edma_dict_get_index (EDMA_DICT d, EPChar name)
{
  ESint32   found;
  ESint32   pos;

  if (!d)    return -1;
  if (!name) return -1;

  pos = edma_dict_map_string (d, name);
  found = -1;
  
  do
    {
      if ((pos == -1) || (d->entry[pos].data == NULL))
	return -1;
	
      if (strcmp (d->entry[pos].data, name) == 0)
	{
	  found = d->entry[pos].indx;
	  break;
	}
      pos = d->entry[pos].next;
      if (pos == -1) return -1;
    } while (d->entry[pos].data != NULL);
  
  return found;
}

/* edma_dict_get_next_index
 *   Get value (index) associated to the provided key (name) when
 *   key colission happens.
 */

ESint32 EDMAPROC
edma_dict_get_next_index (EDMA_DICT d, EPChar name, ESint32 *cpos)
{
  ESint32   found;
  ESint32   pos;

  if (!d)    return -1;
  if (!name) return -1;

  if (*cpos < 0)
    pos = edma_dict_map_string (d, name);
  else
    pos = d->entry[*cpos].next;

  found = -1;
  
  do
    {
      if ((pos == -1) || (d->entry[pos].data == NULL))
	{
	  *cpos = -1;
	  return -1;
	}
	
      if (strcmp (d->entry[pos].data, name) == 0)
	{
	  found = d->entry[pos].indx;
	  break;
	}
      pos = d->entry[pos].next;
      if (pos == -1) 
	{
	  *cpos = -1;
	  return -1;
	}
    } while (d->entry[pos].data != NULL);
  
  *cpos = pos;
  return found;
}
