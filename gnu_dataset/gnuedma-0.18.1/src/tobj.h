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
 * Versión Beta 0.3r1
 * (c) David Martínez Oliveira
 * Vigo 15 de Mayo de 1997
 *
 * Definición de tipos Objeto de EDMA
 * Revisiones:-------------------------------------------------------------
 * 23 de Septiembre de 1997
 * Añadimos el campo pseudopadre para depuración	
 * -------------------------------------------------------------------------
 * 4 de Enero de 1998
 * Modificamos la estructura OBJ. Añadimos soporte para superclases y 
 * subclases, y eliminamos campos que no utilizamos
 * --------------------------------------------------------------
 * Febraury, 20th, 2001
 * Code cleanup and comment translation
 * -----------------------------------------------------------------------
 * December, 8th, 2001
 * Updated PMetVir for virtual method stacking
 * -----------------------------------------------------------------------
 * January, 22th, 2002
 * tobj struct modification to fix up a problem with the new multistage
 * newObj code and SIU subsystem.
 * --------------------------------------------------------------------------
 * April, 1st, 2003
 * Changes to OBJ and related structures
 * -----------------------------------------------------------------------------
 * August, 3rd, 2003
 * Added hash_code for optimization tests 
 * -----------------------------------------------------------------------------
 * January, 8th, 2004
 * Update of SC_ID struct and object structure cleanup and sort
 * Currently unused fields are commented until definitively erased
 * or feature implemented
 *********************************************************
*/
#ifndef TOBJ_H
#define TOBJ_H

#include "esp_typ.h"
#include "ptypes.h"
#include "const.h"
#include "tclass.h"
#include "dict_type.h"

/*** Data Structs ***********************************************************/
#ifdef __cplusplus
extern "C"{
#endif
  typedef ESint32	OBJID;

  /* Superclass Identifier struct */
  typedef struct
  {
    CLASSID   IdClass;
    EChar     Id[EDMA_GENERAL_ID_LEN];  /* Symbolic Identifier for superclass. Upto 32 char. */
    OBJID     Obj;                      /* Subobject Identifier*/
    ESint32   Searchable;               /* Must this SC be searched on Method/Property Lookup??*/
  } SC_ID;

#ifndef PTYPES_H
  typedef EChar		IDSIM[EDMA_GENERAL_ID_LEN];
#endif

  /* Private Memory Virtual Method struct. For method override*/
  typedef struct PMetVir_t
  {
    EByte       Flag;
    EUint32	Ind;		/* Virtual Method Index in object's vtbl */
    IDSIM	Id;		/* Symbolic Identifier for this virtual Method */
    PPROC	Func;		/* Pointer to associated code */
    EPfVoid	Obj;		/* Used for referencing object data from override level */
    struct PMetVir_t *next;     /* For virtual method stacking*/
  } PMetVir;

  /*** Types for Virtual Objects **************************/
  /*   Feature Currently obsolete */
  typedef struct	v_dat
  {
    HMEM		h;	  /* Memory Handler for dynamic memory allocation */
    PPROC		Func[4];  /* Up to 4 functions to manage */
    IDSIM		LocalId;  /* Shortcuts for virtual data */
    struct v_dat	*Next;	  /* Next virtual data chunk in list */
    EPfChar		Id;	  /* Symbolic Identifier */
  } VIR_DATA;

  typedef VIR_DATA	*PVIR_DATA;

  typedef struct t_vir_data
  {
    VIR_DATA		*vMet;		/* Virtual Method list */
    VIR_DATA		*vProp;		/* Virtual Properties list */
  } VIROBJ_DATA;
  
  /** Virtual Object data structs ends ******************************/
  /*
    4 de Enero de 1999
    --------------------------------------------
    Eliminamos campos de la estructura que no están siendo utilizados
    Añadimos los campos nTabla1 y Tabla1 para separación 
    superclases y subclases
  */
  /* Virtual Objects: Feature currently OBSOLETE */
  struct	t_obj
  {
    EPfVoid	Data;           /* Pointer to object's data/properties */
    PMetVir	*vTable;        /* Virtual Method table */

    EUint32	IdObj;
    EByte  	Flag;		/* Object Modifier. For the moment marks virtual objects */
    EByte	Final;		/* Does this object allow on-demand inheritance */

        ESint32	IdApp;		/* Application identifier */
    ESint32	IdClass;	/* Object's class Identifier */
    ESint32	IdSIU;		/* SIU Class Identifier */
    EPChar      IdSubSIU;       /* SIU Delegated Class Identifier.*/
    OBJID       Father;
    OBJID       PseudiFather;
    OBJID       root_obj;

    SYS_OBJ	SysObj;		/* System Depend data */

    ESint32   rlevel;           /* Recursion level for this object */
    ESint32   last_met;         /* Last executed method*/
    ESint32   returning;        /* Flag to recover for infinite recursion*/
    ESint32   last_error;       /* Last error related to this object */

    /* Fields bellow are not currently used but are kept for future implementation */
#if 0
    //VIROBJ_DATA	VirData;	/* Virtual Object Data */
    //OBJID     robj;            /* Object on the right if any*/
    //OBJID     lobj;            /* Object on the left if any */

    //EDMA_DICT UpDict;          /* Dictionary for uplinks */
    //EDMA_DICT DownDict;        /* Dictionary for downlinks */ 
#endif

    EUint32   nUpTable;         /* Counter for superclass table entries */
    SC_ID     *UpTable;         /* Superclass table for this object */
    EUint32   nDownTable;       /* Counter for subclass table entries */
    SC_ID     *DownTable;       /* Subclass Table */


  };

  typedef struct t_obj	OBJ;
  typedef OBJ		*POBJ;
  

  
#ifdef __cplusplus
}
#endif
#endif

