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

/*******************************************************************************
 * Entorno de Desarrollo Modular y Abierto
 * Versión Beta 0.3r1
 * (c) David Martínez Oliveira
 *
 * ini.C
 * Modulo de inicialización
 * Revisiones: ---------------------------------------------------------
 * 28 de Abril de 1997
 * 12 de Junio de 1997
 * Añadimos las funciones de lectura de interface sencillas (idf.C)
 * -----------------------
 * 19 de Julio de 1997
 * Añadimos la construcción de la tabla de mapeo para cada proceso
 * que carga la DLL. Esta tabla indica si una clase determinada (su implementacion)
 * está mapeada en el espacio de direccionamiento del proceso.
 * -------------------------------------------
 * 18 de Julio de 1997
 * Vamos a modificar el fichero EDMA32.CFG para soportar el almacenamiento de
 * información sobre clases del sistema IDF,SIU.
 * ---------------------------
 * 8 de Agosto de 1997
 * Modificación para soporte de información de mapeo de código por proceso
 * -----------------------------
 * 24 de Agosto de 1997
 * Modificación para soporte de múltiples parsers de interface
 * ------------------------
 * 31 de Agosto de 1997
 * Estoy intentando añadir SIU y me parece que me lo he cargado todo
 * ------------------------------
 * 17/11/1997
 *   Modificaciones para la versión 0.3r1
 *   Soporte de nuevos campos IDF
 * --------------------------------
 * 25/11/1997
 *    Modificaion para corregir bug de mapeo de estructuras compartidas
 * -----------------------------
 * 31/98/1998
 *    There is a problem freeing memory when all process unload EDMA.
 *    We clena a bit source
 *    We will try some probes:
 *      First, we make on-demand interface load for using less memory
 * 
 *---------------------------------------------------
 *2 de Enero de 1999
 * Soporte para variable de entorno EDMADIR
 * --------------------------------------------------------------
 * 3 de Enero de 1999
 * Modificación para carga dinámica de interfaces. Los interfaces
 * ya no se cargan durante el proceso de inicialziación del sistema
 * ------------------------------------------------------------------
 * 19 de Febrero de 2000
 * El registro de clases se localiza, pero no se carga si ya hay procesos
 * ejecutándose (las zonas de memoria compartida ya está mapeadas en el
 * espacio de direccionamiento del nuevo proceso en EDMA).
 * ---------------------------------------------------
 * Febraury, 7th, 2001
 * Clean up code and comments translation
 * ------------------------------------------------------------
 * July, 6th, 2001
 * Modification to manage classes with no implementation as pure abstract
 * clases
 * ----------------------------------------------------------------
 * November, 10th, 2001
 * We change the configuration files location. Now, by default, edma32.cfg
 * is at SYSTEMPATH/etc
 *
 * The edma32.ini configuration file no longer is used
 * --------------------------------------------------------------------------
 * November, 18th,2001
 * Compile warnnings removal
 * ----------------------------------------------------------------
 * March 2nd, 2002
 * Code cleanup
 * -------------------------------------------------------------------------
 * April, 1st, 2003
 * Changes to support the new object table
 * ----------------------------------------------------------------------
 * Aprill, 18th, 2003
 * Added declaration of new per-process global vars nObj, and nMaxObj
 * -----------------------------------------------------------------------
 * May, 10th, 2003
 * Modification to support changes to internal class structures
 * -----------------------------------------------------------------------
 * May, 17th, 2003
 * Modification to support per-process classes, and new MOP/MCP
 * ---------------------------------------------------------------------------
 * August, 2nd, 2003
 * Modification to support for local registry loading
 * ------------------------------------------------------------------------------
 * January, 6th, 2004
 * Modification to support GNU/EDMA Subsystem Management API Unification
 * -----------------------------------------------------------------------------
 * Febraury, 24th, 2004
 * EDMAIni split and repository load unification
 * -----------------------------------------------------------------------------
 * May, 2nd, 2004
 * Added new thread support
 * ---------------------------------------------------------------------------
 * August, 19th, 2004
 * Global Thread List structure update.
*/
 
#include <stdio.h>
#include <string.h>
#include <stdlib.h>
#include <setjmp.h>
#include <pthread.h> 
#include <time.h>

#include "portable.h"
#include "vglobal.h"
#include "const.h"
#include "shmem.h"
#include "sys31.h"
#include "ini1.h"
#include "idf.h"
#include "tobj.h"
#include "tclass.h"	
#include "ttypes.h"
#include "error.h"
#include "clas.h"
#include "misc.h"
#include "obj.h"
#include "procmap.h"
#include "iniman.h"

#include "subsystems.h"
#include "multiidf.h"
#include "siu.h"
#include "emi.h"
#include "pri3.h"

#include "classrt.h"

#include "ethread.h"

#include "sclasses.h"
#include "repo.h"

/* Local Prototypes */
ESint32 static  _edma_init_per_process_data();
ESint32 static  _edma_init_global_data();
ESint32 static  _edma_init_class_data();
ESint32 static  _edma_init_obj_data();

/************************************************************************
 ******* Global Vars
 *************************************************************************
*/
SYS_GLOBAL_VAR	*GVar;
/* System datatypes table*/
TIPOS	tipo[MAX_TIPOS]=
{
  {"EUINT8",   DT_EUINT8,   sizeof (EUint8),       "U8"},
  {"EUINT16",  DT_EUINT16,  sizeof (EUint16),      "U16"},
  {"EUINT32",  DT_EUINT32,  sizeof (EUint32),      "U32"},
  {"ESINT8",   DT_ESINT8,   sizeof (ESint8),       "S8"},
  {"ESINT16",  DT_ESINT16,  sizeof (ESint16),      "S16"},
  {"ESINT32",  DT_ESINT32,  sizeof (ESint32),      "S32"},
  {"EBYTE",    DT_EBYTE,    sizeof (EByte),        "B"},
  {"EWORD",    DT_EWORD,    sizeof (EWord),        "W"},
  {"EDWORD",   DT_EDWORD,   sizeof (EDWord),       "D"},
  {"ECHAR",    DT_ECHAR,    sizeof (EChar),        "C"},
  {"EBOOL",    DT_EBOOL,    sizeof (EByte),        "B"},
  {"EREAL32",  DT_EREAL32,  sizeof (EReal32),      "R32"},
  {"EREAL64",  DT_EREAL64,  sizeof (EReal64),      "R64"},
  {"EZSTRING", DT_EZSTRING, sizeof (EPChar),       "Z"},
  {"EBUFFER",  DT_EBUFFER,  sizeof (EDMAT_BUFFER), "A"},
  {"EOBJECT",  DT_EOBJECT,  sizeof (OBJID),        "O"},
  {"EUSER",    DT_EUSER,    sizeof (OBJID),        "X"},
  {"EPOINTER", DT_EPOINTER, sizeof (EPVoid),       "P"}
};
/* Mapped classes table for this process */
HMEM	    hProcMapTable;
EPByte	    ProcMapTable;

/* Per-process class information*/
HMEM	    hpClass;
PCLASS_REF  *pClass;

HMEM        hSObj;
POBJ        *SObject;

/* Shared Class Table*/
HMEM        hSharedClass;
CLASE       *SharedClass;

/* Global Class Ref table*/
HMEM        hSClass;
CLASS_REF   *SClass;

/* Global Class Dictionary */
EDMA_DICT  edma_class_dict;

EUint32     nObj;      /* Number of current objects*/
EUint32     nMaxObj;   /* Max number of objects in system*/

EUint32     nLocalClasses;    /* Number of current per-process classes */
EUint32     nMaxLocalClasses; /* Max number of per-process classes */

EUint32	    AppId;
EChar	    AppName[80];
PPROC	    ProcEPTable[MAX_PRIM];

/* FIXME: Convert to array for multi-threading*/
ESint32     edma_last_error = 0;
ESint32     edma_in_try_block = 0;
jmp_buf     edma_jmp;

pthread_mutex_t class_mutex = PTHREAD_MUTEX_INITIALIZER;
pthread_mutex_t obj_mutex = PTHREAD_MUTEX_INITIALIZER;

/* THis data structs must be keep per-trhead*/
OBJID       current_stack_execution[MAX_STACK_EXECUTION];
ESint32     current_stack_execution_indx;
ETKEY       thread_stack_key;

/* Global Thread Registry */
ETHREAD_DATA **thread_list = NULL;
EUint32      num_threads = 0;

/* Information about pending updates */
ESint32     current_update = -1;
OBJID       *pending_objects_list = NULL;
time_t      last_checked_update = 0;

time_t      last_checked_versions = 0;

/* EDMAIni
 *   Main GNU/EDMA data initialisation function
 */

EUint32 EDMAPROC 
EDMAIni (void) 
{
  EChar	*evar;

  /* We build the per-process mapping table */
  edma_log ("New Process Joining EDMA System. Identifier is: %d", AppId);
  /*edma_printf ("Registering main thread [%ld]", pthread_self());*/
  edma_log ("Global vars attached at [%p] [%ld bytes]", 
	    GVar, sizeof (SYS_GLOBAL_VAR));
  /* Call to edma_register_thread: Allocates private data for thread 
   *                               and creates thread execution stack*/
  
  /* Process Environmental Vars*/
  evar = getenv ("EDMA_DEBUG_LEVEL");
  if (evar == NULL) edma_set_debug_level (69);
  else edma_set_debug_level (atoi(evar));
  edma_log ("DEBUG_LEVEL: '%s': %d", evar, GVar->DebugLevel);


  GVar->SystemPath[0] = 0;
  last_checked_versions = time (NULL);
  last_checked_update = time (NULL);
  evar = getenv ("EDMA_SYSTEM_PATH");
  if (evar != NULL) strncpy (GVar->SystemPath, evar, EDMA_PATH_LEN);


  /* Also create Condition variable to stop threads on updates */  
  if ((_edma_init_per_process_data()) == -1)
    return -1;
  
  if ((_edma_init_global_data ()) == -1)
    return -1;

 
  if ((_edma_init_class_data ()) == -1)
    return -1;
  
  
  if ((_edma_init_obj_data ()) == -1)
    return -1;
  
  if ((edma_repo_manager_init ()) < 0)
    return -1;

  //edma_repo_add_system_repo ();

  /* Register system classes */
  edma_register_system_classes ();

  /* Load system repositories */
  edma_repo_add_shared_repo ();



  edma_printf ("%s", "EDMA Startup process... OK");

  /* Try to load app repositories if any */
  edma_add_app_repos ();

  /* Update versions for first time */
  _edma_class_update_all_versions ();
   
  return 0;
}


ESint32 EDMAPROC
EDMAFinish (void)
{
  edma_repo_manager_end();
  return 0;
}


/* EDMAFree
 *    Free allocated memory resources 
 */

EUint32 EDMAPROC 
EDMAFree (void)
{
  int	i;
  
  edma_log ("%s", "[EDMAFree] Freeing System classes....");
  for (i = 0; i < GVar->nClases; i++) 
    {
      if (ProcMapTable[i] >= CLASS_ILOADED) 
	{
	  /*We free class implementations */
	  edma_free_class_imp (i);
	  _edma_free_class_interface (i, NULL);
	}
      /* Free Per-process class block if any*/
      if (pClass[i] && pClass[i]->met_dict)
	edma_dict_free (pClass[i]->met_dict);
      edma_pfree ((HMEM)pClass[i], pClass[i]);
#if 0
      /* We free class-related memory */
      edma_sfree (pClase[i].SysClass.hMet, pClase[i].Met);
      edma_sfree (pClase[i].SysClass.hProp, pClase[i].Prop);
      edma_sfree (pClase[i].SysClass.hNot, pClase[i].Not);
#endif
    }
  edma_pfree (hSObj, SObject);


  /* FIXME: Remove Local classes at this point */
  edma_pfree (hSClass, SClass);

  edma_pfree (hpClass, pClass);  /* Valgrind */

  edma_sunget (SharedClass);
  edma_sfree (hSharedClass, SharedClass);

  return 0;
}

EUint32 EDMAPROC 
EDMAAppGC (void)
{
  int	i;
  EChar	msg[80];
  
  sprintf (msg, "(%s) is going down ...%ld,%ld",
	   AppName, nObj, nMaxObj);
  edma_printf (msg);

  for (i = 0; i < nMaxObj; i++)
    if ((gObj[i]->IdApp == AppId) && (gObj[i]->Flag == OBJ_EXIST)
	&& (gObj[i]->Father == -1))
      edma_free_obj (i);

  return 0;
}



/****************************************************************
 ********  STATIC FUNCTIONS GO HERE *****************************
 *****************************************************************/

/******* Data struct and global initialization functions ********/

/* _edma_init_per_process_data
 *    Data initialisation specific for each process
 */

ESint32 static
_edma_init_per_process_data ()
{
  ESint32   i;

  edma_log ("%s", "Initiating Process Mapping Table and System Tables...");

  hProcMapTable = edma_palloc (MAX_CLASE * sizeof (EByte) * 2);
  if (hProcMapTable == (HMEM)0) 
    {
      edma_printf_err ("Can't alloc per process mapping table. Aborting.");
      return -1;
    }
  ProcMapTable = edma_pget (hProcMapTable);
  
  /* We build table for indirect references to shared memory blocks 
   * per-process 
   */
  hpClass = edma_palloc (MAX_CLASE * sizeof (PCLASS_REF) * 2);
  if (hpClass == (HMEM)0) 
    {
      edma_printf_err ("Can't Alloc Process Shared Structs Mapping Table");
      return -1;
    }
  
  pClass = edma_pget (hpClass);
  for (i = 0; i < 2 * MAX_CLASE; i++) 
    {
      ProcMapTable[i] = CLASS_FREE;
    }

  /* Create Thread TSL key*/
  thread_stack_key = AppId;
  if ((edma_thread_key_create (&thread_stack_key)) != 0)
    {
      edma_printf_err ("Can't create thread specific data key");
      return -1;
    }

  edma_log ("Thread Specific Data Created [%p]", thread_stack_key);

  /* Register Main thread */
  edma_thread_register ();

  /* Init per-process stack execution data */
   current_stack_execution_indx = 0;
   for (i = 0; i < MAX_STACK_EXECUTION; i++)
     current_stack_execution[i] = -1;

   return 0;
}

/* _edma_init_global_data
 *    System Global Data initialisation 
 */

ESint32 static
_edma_init_global_data ()
{
  ESint32     i;

  nMaxObj = MAX_OBJ;
  nLocalClasses = 0;
  nMaxLocalClasses = MAX_CLASE;
  if (GVar->Running <= 1) 
    {
      GVar->nMaxClases = MAX_CLASE;
      GVar->nMaxIdf = MAX_IDF_PARSERS;
      GVar->nMaxSIU = MAX_SIU_PROXYS;
      
      GVar->nIdf = 1;
      GVar->nSIU = 1;
      GVar->nEMI = 1;
      
      GVar->GetClassEMI = 0;
      
      /* Default IDF parser init */
      strncpy (GVar->SubSystem[SS_INGRIDF][0].Id, "EDMAIDF", 7);
      GVar->SubSystem[SS_INGRIDF][0].IdClass = -1;
      
      GVar->SubSystem[SS_SIU][0].IdClass = -1;
      
      /*pthread_mutex_init (&GVar->class_mutex, NULL);*/
      edma_log ("Max Classes [%d + %d] | Max Objects [%d] | "
		"Max Parsers [%d] | Max Proxies [%d]",
		GVar->nMaxClases, nMaxLocalClasses, nMaxObj, 
		GVar->nMaxIdf, GVar->nMaxSIU);
      /* Hotswap */
      GVar->num_updates = 0;
      for (i = 0; i < EDMA_MAX_UPDATES;i++)
	GVar->pending_update[i].IdClass = -1;
    }

  if (GVar->SystemPath[0] == 0)
    strncpy (GVar->SystemPath, GNUEDMADIR, EDMA_PATH_LEN);

  /* We init system tables */
  _edma_ini_sys_table (GVar->SystemPath);
  
  return 0;
}


/* _edma_init_class_data
 *   Class related data structures initialisation 
 */

ESint32 static
_edma_init_class_data ()
{
  ESint32    i;
  CLASE	     *Clase;

  /* We init class tables */
  /* First map shared class table*/
  hSharedClass = edma_salloc (GVar->nMaxClases * sizeof (CLASE), "CLASSES");
  if (hSharedClass == (HMEM)0) 
    {
      edma_printf_err ("%s", "Can´t create shared class table");
      return -1;
    }
  
  Clase = SharedClass = edma_sget (hSharedClass);   
  if (SharedClass == NULL) 
    {
      edma_printf_err("%s", "Can´t create shared class table");
      return -1;
    }
  /* We inicialize class tables */
  if (GVar->Running <= 1) 
    {
      edma_log ("%s", "Initiating Shared Class Table");
      for (i = 0; i < GVar->nMaxClases; i++) 
	{
	  memset (&Clase[i], sizeof (CLASE), 0);
	}
    }
  
  /* Create and initialize Global Class Reference Table*/
  hSClass = 
    edma_palloc ((GVar->nMaxClases + nMaxLocalClasses) * sizeof (CLASS_REF));
  if (hSClass == (HMEM)0) 
    {
      edma_printf_err ("%s", "Can´t create Global Class Reference Table");
      return -1;
    }
  
  SClass = edma_pget (hSClass);   
  if (SClass == NULL) 
    {
      edma_printf_err("%s", "Can´t create Global Class Refernece Table");
      return -1;
    }
  
  /* Inicialization of Global Class Reference Table */
  edma_log ("%s", "Initiating Global Class Reference Table");
  /* Global classes are set to default shared memory blocks*/
  for (i = 0; i < GVar->nMaxClases; i++) 
    SClass[i] = &SharedClass[i];
  
  /* Local classes are set to NULL*/
  for (i = GVar->nMaxClases; i < nMaxLocalClasses; i++) 
    SClass[i] = 0;

  /* Create global Class Dictionary */
  edma_log ("%s", "Initiating Global Class Dictionary");
  if ((edma_class_dict = edma_dict_new (GVar->nMaxClases 
					+ nMaxLocalClasses)) == NULL)
    {
      edma_printf_err ("%s", "Can't create global class dictionary");
      return -1;
    }

   /* If shared structures are already mapped, just create per-process 
    * data structures
    */
   if (GVar->Running > 1) 
     { /* Global var block already in memory?*/
       edma_log ("%s", "  Shared Data already in-memory");
       edma_log ("  %d Classes", GVar->nClases);
       for (i = 0; i < GVar->nClases; i++)
	 {
	   SClass[i] = &SharedClass[i];

	   if ((_edma_class_alloc_priv_data (i)) < 0)
	     {
	       edma_printf_err ("Can't alloc per-process struct "
				"for class %s[%d]", gClass[i]->ClassName, i);
	       return -1;
	     }
	   ProcMapTable[i] = CLASS_DEF;

	   /* Populate local dictionary */
	   edma_dict_add_entry (edma_class_dict, gClass[i]->ClassName, i);
	 }
       /* Nothing more to do on init. Returning*/
       return 0;
     }
  
  return 0;
}

/* _edma_init_obj_data
 *   Object Data Structures specific initialisation
 */

ESint32 static
_edma_init_obj_data ()
{
  ESint32   i;

  /* Creation and Inicialization of object table */
  hSObj = edma_palloc (nMaxObj * sizeof (OBJ*));  
  if (hSObj == (HMEM)0) 
    {
      edma_printf_err ("%s", "Can´t create object table");
      return -1;
    }
  SObject = edma_pget (hSObj);
  if (GVar->Running <= 1) 
    {
      edma_log ("%s", "Initiating Global Object Refernece Table ");
      for (i = 0; i < nMaxObj; i++)
	gObj[i] = NULL;
    }
  
  return 0;
}


