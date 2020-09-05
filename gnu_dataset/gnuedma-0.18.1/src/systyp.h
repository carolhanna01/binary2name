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

/********************************************************************
 * Entorno de Desarrollo Modular y Abierto
 * Versión Beta 0.3r1
 * (c) David Martínez Oliviera
 *	
 * Tipos del sistema
 * Define un bloque con las variables globales del sistema que serán
 * compartidas por todas las instancias de la DLL
 * 29 de Abril de 1997
 * -------------------------------------------------------
 * 24 de Julio de 1997
 * Añadimos estructuras de datos para soporte de multiples IDFs y
 * proxys SIU
 * ------------------------
 * 8 de Agosto de 1997
 * Añadimos estructuras de datos para almacenar información de mapeo de código
 * por proceso.
 * ------------------------------
 * 2 de Octubre de 1997
 * Añadimos primer soporte para EMI. En primer lugar solo la función GetClassId
 * --------------------------------------------------------------------
 * Febraury, 19th, 2001
 * Code cleanup and comment translation
 * ------------------------------------------------------------------------
 * November, 28th, 2001
 * Begins modification to add implicit anchor point info for inheritance
 * ------------------------------------------------------------------------
 * April, 18th, 2003
 * nMaxObj and nObj removed from shared struct.
 * This values are process depend
 * -------------------------------------------------------------------------
 * May, 24th, 2003
 * Added Reference Use field to private class structure. Required for HotSwapping
 * ----------------------------------------------------------------------------------
 * August, 8th, 2003
 * Added structures and variables to hold updates for hotswaping system
 * ---------------------------------------------------------------------------------
 * August, 23th, 2003
 * Added dictionary structure
 * ----------------------------------------------------------------------------------
 * January, 6th, 2004
 * Modification to unify GNU/EDMA subsystems management interface
 * ----------------------------------------------------------------------
 * Febraury, 7th, 2004
 * Added per-process (PCLASE) current version for version management
*/

#ifndef SYS_TYPES_H
#define SYS_TYPES_H

#include <pthread.h>
#include <time.h>

#include "portable.h"
#include "const.h"
#include "tclass.h"
#include "tobj.h"
#include "esp_typ.h"
#include "vserv.h"
#include "dict_type.h"

#ifdef __cplusplus
extern "C"{
#endif

  typedef EChar         ID[EDMA_GENERAL_ID_LEN];

  /* IDF Parser data struct. To be removed by new Subsystem Architecture */
  typedef struct 
  {
    /* Symbolic Identifier */
    EChar		Id[EDMA_GENERAL_ID_LEN];	 
    CLASSID		IdClass; /* Class Identifier who implements IDF parssing */
  } IDF_PARSER_ITEM;
  
  /* SIU Proxy data struct To be removed by new Subsystem Architecture */
  typedef struct 
  {
    EChar		Id[EDMA_GENERAL_ID_LEN];  /* Symbolic Identifier*/
    CLASSID		IdClass; /* Class Identifier who implements proxy */
  } SIU_PROXY_ITEM;
  
  /* EMI Extension data struct. To be removed by new Subsystem Architecture  */
  typedef struct 
  {
    EChar		Id[EDMA_GENERAL_ID_LEN];  /* Symbolic Identifier */
    CLASSID		IdClass; /* Class Identifier who implements emi extension */
  } EMI_ITEM;

  /* GNU/EDMA Subsystem data struct: IngrIDF, SIU, EMI*/
  typedef struct
  {
    EChar		Id[EDMA_GENERAL_ID_LEN];  /* Symbolic Identifier */
    CLASSID		IdClass; /* Class Identifier who implements emi extension */
  } SUBSYSTEM_ITEM;

  /* Update structure. Simple one for primary tests */
  typedef struct 
  {
    CLASSID             IdClass;
    EUint32             timestamp; /* Maybe will be removed in final version*/
    EChar               update_script[EDMA_PATH_LEN]; /* Relative to base_dir/share/edma */
  } EDMA_UPDATE;
  
  /* Global variables memory chunck*/
  typedef struct 
  {
    HMEM	hMySelf;	/* Memory Handle for this block */
    ESint32	Running;	/* Is the block initialized? */
    EUint32	nClases;	/* Number of Class in system */
#if 0
    EUint32	nObj;		/* Number of Objects in system */
    EUint32	nMaxObj;	/* Maximum object table entries allowed */
#endif
    EUint32	nMaxClases;	/* Maximum class table entries allowed */

    EUint32     nMaxIdf;	/* MAximum IDF_PARSERS in system */
    EUint32     nMaxSIU;	/* Maximum SIU PROXYs in system */
    EUint32     nIdf;		/* Number of IDF_PARSERS in system */
    EUint32     nSIU;		/* Number of SIU PROXYs in system */
    EUint32	nEMI;		/* Number of EMI PROXYs in system */

    SYS_SO	SysSO[SO_NUM];	/* Operating Systems System Register */
    SYS_MAQ	SysMaq[MAQ_NUM];/* Machine System Register */

    SUBSYSTEM_ITEM SubSystem[SS_LAST][MAX_SS_ITEMS];
    EUint32        n_SubSystem[SS_LAST]; 

    EUint32	nSONum;		/* Number of Entries defined en SysSO */
    EUint32	nMaqNum;	/* Number of Entries defined en SysMaq */
    EChar	SystemPath[EDMA_PATH_LEN];
    VSERV	*vServ;		/* Console server id; */
    EUint32	AppIdGen;
    EUint32	DebugLevel;
    CLASSID	GetClassEMI;	/* Class for GetClass Extension */
    pthread_mutex_t class_mutex; /* Class mutex to update class table*/
    /* Hotswap: pending updates*/
    EDMA_UPDATE pending_update[EDMA_MAX_UPDATES];
    ESint32     num_updates;
    time_t      time_last_update;
    time_t      time_last_version_update;
  } SYS_GLOBAL_VAR;
  
  typedef struct {
    PPROC		*Func;
  } PROCMET;
  
  typedef struct {
    HMEM	    hLib;
    SYS_CLASS1      SysClass;

    MET             *Met;
    PROP            *Prop;
    NOT             *Not;
    CLASSID         *SCList;    /* SuperClass list for this class*/
    ID              *SCIdList;  /* Default anchor points for each superclass*/
    ID              *SubCIdList; /* Idem subclases*/

    PROCMET         *met_func;
    ESint32         nRef;       /* Reference Use Counter */
    EDMA_DICT       met_dict;
    EDMA_DICT       prop_dict;
    CLASSID         actual_version;
  } PCLASE;
  
  typedef PCLASE *PCLASS_REF;

#ifdef __cplusplus
}
#endif    
#endif

