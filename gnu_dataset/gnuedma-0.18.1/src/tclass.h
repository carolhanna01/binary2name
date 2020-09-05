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

/**********************************************************************
 * Entorno de Desarrollo Modular y Abierto
 * Versión Beta 0.3r1
 * (c) David Martínez Oliveira
 * 10 de Mayo de 1997
 *	
 * Definición de tipos para módulos que trabajen con clases
 * REVISIONES:------------------------------------------------------------------------
 * 28 de Julio de 1997
 * Añadimos campos para identificación de subsistemas
 * 17/11/1997
 * Modificaciones para la versión 0.3r1.
 *   Añadimos el campo Maker para el tema de la resolución de nombres
 * -----------------------------------------------------------
 * Febraury, 19th-20th, 2001
 * Code cleanup and comment translation
 * --------------------------------------------------------------------
 * July, 4th, 2001
 * Added field PureAbstract to mark pure abstract/interface classes
 * This is required in order to not try to load a class implementation
 * ---------------------------------------------------------------------
 * June, 30th, 2002
 * Added new fields to support Hot-Swapping
*/

#ifndef TCLASS_H
#define TCLASS_H
#include "portable.h"
#include "sys31.h"	
#include "ptypes.h"	
#include "esp_typ.h"	

#ifdef __cplusplus
extern "C"{
#endif
  /* General Types definition */

  typedef ESint32		CLASSID;
  typedef ESint32		VERSIONID;
  typedef ESint32		GROUPID;
  typedef ESint32		LEVELID;
  
  /* Class Type definition */
  typedef struct
  {
    EByte	Flag;
    EByte	IsIDF;			/* IDF Parser class for this class */
    EByte	IsSIU;			/* Does this Class belongs to some SIU Subsystem ? */
    EByte	IsEMI;			/* Is this class an EMI Extension? */
    EByte	IsStock;		/* Is it a stock class? */
    EUint32	IDFParser;		/* Parser for class interface */
    EUint32	SIUProxy;		/* Proxy for SIU subsystem */
    EChar	ClassName[EDMA_CLASS_NAME_LEN];		/* Class name. 50 char max. */
    //EChar       Maker[EDMA_CLASS_MAKER_LEN];              /* Developer Identifier */
    EByte       MajorVer;               /* Major Version */
    EByte       MinorVer;               /* Minor Version */
    CLASSID     CurrentVer;             /* Current Version installed. The version to use by default*/
    //EChar	Implementation[EDMA_CLASS_IMPL_LEN];	/* Class Implementation filename */
    EChar       NameSpace[EDMA_CLASS_NAMESPACE_LEN]; /* Prefix to look for class related files*/
    CLASSID	ClassId;		/* Class Identifier */
    MAQID	MaqId;			/* Machine Identifier */
    SOID	SOId;			/* Operating System Identifier */
    //LEVELID	LevelId;		/* EDMA Level Identifier. EDMA Primitives supported by the class */
    SYS_CLASS	SysClass;		/* System depended data */
    EByte	Status;			/* Class Status */
    EUint32	Ocurrences;		/* Counter for object using this class */
    EByte	Used;			/* Was this class used in this system session. Cache/autoupdate  system */
    EByte       Updated;                /* Flag to indicate that the class instances need to be updated*/
    EUint32	Derived;		/* Is this class a derived class? */
                                        /* if distinct to zero, Derived contains the superclass counter */
    EUint32	nMetVir;		/* Virtual Methods Counter */
    EUint32	TamDatos;		/* Data block Size */
    EUint16	nProp,nMet,nNot;        /* Counters for Properties, Methods and Notifications*/
    //EUint32     UserExt;                /* Extra data for user information*/
    EByte       PureAbstract;
    EByte       repo_type;              /* Type of repository used by the class. SHARED or LOCAL */
    ESint32     repo_id;
    ESint32     repo_class_id;
  } CLASE;
  
  typedef CLASE *CLASS_REF;
#ifdef __cplusplus
}
#endif
#endif

