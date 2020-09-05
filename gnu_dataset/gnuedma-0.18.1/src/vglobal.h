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
 * Vigo 24 de Septiembre de 1996
 *
 * Versión 0.3r1
 *---------------------------------------------------------
 *
 * Variables Globales de EDMA
 * REVISIONES:---------------------------------------------
 * 22 de Febrero de 1997
 * Modificación de variables globales como huge de acuerdo
 * a las modificaciones en el fichero INI31.C
 * ----------------------------------------------------------------
 * 29 de Abril de 1997
 * Modificaciones para la Beta WIN32
 *---------------------------------------------------------------
 * Febraury, 20th, 2001
 * Code cleanup and comment translation 
 * ---------------------------------------------------------
 * June, 30th, 2002
 * Finally we add a mutex to access class table
 * ---------------------------------------------------------
 * April, 1st, 2003
 * Global object table becomes reference table to OBJ* items
 * ----------------------------------------------------------
 * April, 18th, 2003
 * Added nObj and nMaxObj as per-process global vars
 * ------------------------------------------------------------
 * May, 17th, 2003
 * Added nLocalClasses and nMaxLocalClasses to support per-process
 * class pool
 *********************************************************
 */
#ifndef VGLOBAL_H
#define VGLOBAL_H
#include "portable.h"
#include "const.h"
#include "systyp.h"
#include "ttypes.h"
#include <setjmp.h>
#include <pthread.h>

/* FIXME: Conditional Include */
#include "ethread.h"
#include "linuxth.h"

#ifdef __cplusplus
extern "C"{
#endif
  extern	SYS_GLOBAL_VAR *GVar;
#define gClass SClass
#define gObj   SObject
  
  extern TIPOS 	    tipo[MAX_TIPOS];  /* Data type definition*/
  extern EPByte	    ProcMapTable;     /* Per-process primitive mapping table*/
  extern HMEM	    hProcMapTable;    /* Handle for ProcMapTable array*/
  extern HMEM	    ConOut;           /* Console Handler for windows version*/
  extern HMEM	    hpClass;       
  extern HMEM       hfClass;   
  //extern HMEM       hSClass,
  extern HMEM       hSObj;
  extern POBJ       *SObject;
  extern CLASE      *SharedClass;   
  extern CLASS_REF  *SClass;
  //extern PCLASE	    *pClass;
  extern PCLASS_REF *pClass;
  //extern FCLASE     *fClass;
  extern EDMA_DICT  edma_class_dict;
  extern EUint32    AppId;
  extern EChar	    AppName[EDMA_GENERAL_ID_LEN];
  extern PPROC	    ProcEPTable[MAX_PRIM];	
  extern EUint32    nObj;
  extern EUint32    nMaxObj;
  extern EUint32    nLocalClasses;
  extern EUint32    nMaxLocalClasses;

  /* Errors & Exceptions */
  extern ESint32 edma_last_error;
  extern ESint32 edma_in_try_block;
  extern jmp_buf edma_jmp;
  /* Updating */
  /*extern pthread_mutex_t class_mutex;*/
  extern pthread_mutex_t obj_mutex;

  //extern EPChar  sys_dir[2];
  extern OBJID  current_stack_execution[MAX_STACK_EXECUTION];
  extern ESint32 current_stack_execution_indx;

  //extern ETHREAD *thread_list;
  extern ETHREAD_DATA **thread_list;
  extern EUint32      num_threads;

/* Information about pending updates */
  extern ESint32     current_update;         /* Index on shared pending update list*/
  extern OBJID       *pending_objects_list;  /* Current list of objects to be updated */
  extern time_t      last_checked_update;
  extern time_t      last_checked_versions;
  extern ETKEY       thread_stack_key;
#ifdef __cplusplus
}
#endif
#endif
