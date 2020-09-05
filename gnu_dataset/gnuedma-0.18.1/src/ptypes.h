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

/***********************************************************
 * Entorno de Desarrollo Modular y Abierto
 * Versión BETA 0.3r1
 * (c) David Martínez Oliveira
 * 10 de Mayo de 1997
 *
 * Definición de tipos para propiedades, métodos y notificaciones
 * REVISIONES -----------------------------------------------
 * 17/11/1997
 * Modificaciones para versión 0.3r1
 *   Añadimos campo adicional a las propiedades para que el usuario pueda
 * ampliarlas.
 ***********************************************************
 * Febraury, 19th, 2001
 * Code cleanup and comment translation
 * --------------------------------------------------------------
 * April, 15th, 2001
 * Adding support for abstract and static methods
 * First we'll implement the static method support. For abstract methods
 * we only add a flag to the method struct. The abstract method implementation
 * should be trivial...
 * ----------------------------------------------------------------------------------
 * August, 3rd, 2003
 * Added hash_code field to property and method structure for optimization tests
*/

#ifndef PTYPES_H
#define PTYPES_H

#include "portable.h"
#include "const.h"

#ifdef WINAPI_32
typedef FARPROC PPROC;
#endif
#ifdef LINUX
typedef EUint32 (*PPROC)();
#endif

/* Type definition for internal items: Properties/Methods/... */

#ifdef __cplusplus
extern "C"{
#endif
/* CONSTANTS *********************************************/
#define E			0
#define L			1
#define E_L			2

/* Operations Identifiers */
#define	WRITE_PROP		0
#define READ_PROP		1
#define RUN_MET			2
#define GET_DATA		3


/* Virtual feature identifiers */
#define V_PROP			0
#define V_MET			1


/* Type Identifiers **********************************/

  typedef	char	IDSIM[EDMA_GENERAL_ID_LEN];	/* Symbolic Identifier */
 
  /* Property data struct */ 
  typedef struct
  {
    IDSIM	IdProp;		/* Property Identifier */
    EUint32	Tipo;		/* Datatype */
    EByte	ioTipo;		/* Access (read,write)*/
    EUint16	Off;		/* Offset in object data block */
    EUint32	nElem;		/* Element number if the property is an array*/
    HMEM	UserInfo;	/* Extra information. For future use*/
  } PROP, *PPROP;
  
  /* Method data struct*/
  typedef struct
  {
    IDSIM	IdMet;		/* Method Identifier */
    EChar	Sign[EDMA_MET_SIG_LEN];	/* Method Signature */
    EByte	Virtual;	/* Virtual Method Indicator*/
    HMEM	UserInfo;	/* Extra Information */
    EByte       Static;         /* Static Method Indicator*/
    EByte       Abstract;       /* Abstract Method Indicator*/
  } MET, *PMET;
  
  /* Notification Data struct. Currently not implement*/
  /* Not yet clear if it will be removed. Keep for now */
  typedef struct
  {
    IDSIM	IdNot;		/* Notification Identifier */
    EUint32	Tipo;		/* Data Type ...*/
    EPChar 	Des;		/* Description */
    EUint32	IdClase;	/* For level 3 (inheritance, polimorphism,...) */
    PPROC	Func;           /* Associated function */
    HMEM	UserInfo;	/* Extra Information*/
  } NOT,*PNOT;
  
  /* Parameters for virtual object access. To be Removed */
  typedef struct
  {
    EPVoid	a;	/* Pointer to parameters */
    EPfChar	Id;	/* Call identifier */
    EUint16	IdPrim;	/* Primitive Identifier */
    ESint32	IdObj;	/* Object which receives the primitive*/
    EUint32	Ind;	/* Property, Method index for using level 1 primitives*/
  } VIR_MET_PARAMS;
  
#ifdef __cplusplus
}
#endif

#endif
