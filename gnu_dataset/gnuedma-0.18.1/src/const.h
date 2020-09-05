/*
 * EDMA: Entorno de Desarrollo Modular y Abierto
 * Object Oriented and Componetware Framework
 * Copyright (C) 1998, 2002, 2003, 2004, 2005, 2006, 2007, 2008, 2010
 *    David Mart�nez Oliveira
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
 * (c) David Mart�nez Oliveira
 * Vigo 18 de Octubre de 1996
 * 
 * Versi�n 0.3r1
 * ---------------------------------------------------------
 *
 * Constantes EDMA
 * --------------------------------------------------------------
 * 3 de Enero de 1999
 * A�adimos la constante CLASS_ILOADED
 * ------------------------------------------------------------
 * 14 de Mayo de 1999
 * A�adimos nuevas constantes de estado de clase, para llevar control
 * del estado de la clase en cada proceso.
 * -------------------------------------------------------------
 * Febraury, 20th, 2001
 * Code cleanup and comment translation
 * -------------------------------------------------------------------
 * January, 6th, 2004
 * Added subsystem identification constants
 *********************************************************
*/

#ifndef CONST_H
#define CONST_H

#ifdef __cplusplus
extern "C"{
#endif

  /* Version Information*/
#define VERSION_MA 0
#define VERSION_MI 18
#define VERSION_BUG 0
#define VERSION_STATUS "Stable"
#define EVERSION "0.18.1"

#define TMPDIR "/tmp/EDMAtmp"
#define IDFDIR "/share/edma/idf/"

#define GNUEDMADIR "/usr/local/"

/** Maximum size for system tables ****************************/

#define MAX_PRIM	200
#define	MAX_OBJ		8192
#define MAX_APP		100
#define MAX_CLASE	256

#define MAX_IDF_PARSERS	64
#define MAX_SIU_PROXYS	64
#define MAX_EMI_COMP	16

#define MAX_SS_ITEMS    64

#define MAX_RECURSION   7000
#define MAX_STACK_EXECUTION MAX_RECURSION

  /** Class Status constants */
#define	CLASS_FREE	0	/* Free Entry */
#define CLASS_DEF	1	/* Class defined but not memory-mapped */
#define CLASS_ILOADED   2       /* Interface loaded, but implementation not*/
#define CLASS_IMAPPED   3       /* Interface mapped for this process */
#define CLASS_LOADED	4	/* Class full loaded */
#define CLASS_LOCKED    5       /* For thread safe access */
#define CLASS_TEMP	10	/* Temporal Class. For dynamic class definition */

  /** Object Status Constants */
#define OBJ_FREE	0
#define OBJ_EXIST	1
#define VIRTUAL_OBJECT	2
#define OBJ_LOCKED      3  /* For thread safe access*/
#define OBJ_DIE         4  /* Marked to die*/


  /* GNU/EDMA Subsystems Constants*/
#define SS_INGRIDF      0
#define SS_SIU          1
#define SS_EMI          2

#define SS_LAST         3

  /** Machine Types **************************************/
  /* Mantained for compatibility. Now, EDMA loads machine types from a file*/
#define MAQ_NUM		32				

#define I386		0
#define	I386_4		1
#define	I386_8		2
#define	I486		4
#define	I486_8		5
#define	I486_16		6
#define	I486_32		7
#define PENT		8
#define	PENT_8		9
#define	PENT_16		10
#define	PENT_32		11
#define	PENT_64		12

#define	VIRTUAL		13
#define	JAVA		14

/** Operating System Types **********************************/
#define	SO_NUM		8

#define	WINDOWS		0
#define	WIN311		1
#define	WIN95		2
#define	WIN_NT		3

/** Object Status ********************************************/
  /* Field sizes */
#define EDMA_CLASS_NAME_LEN     80
#define EDMA_CLASS_MAKER_LEN    80
#define EDMA_CLASS_IMPL_LEN     80
#define EDMA_CLASS_MODULE_LEN   80
#define EDMA_CLASS_NAMESPACE_LEN 80

#define EDMA_ARCH_LEN           50
#define EDMA_SO_LEN             50

#define EDMA_MET_SIG_LEN        50
#define EDMA_GENERAL_ID_LEN     80

#define EDMA_MET_NAME_LEN       EDMA_GENERAL_ID_LEN
#define EDMA_PROP_NAME_LEN      EDMA_GENERAL_ID_LEN
#define EDMA_TYPE_NAME_LEN      EDMA_GENERAL_ID_LEN

#define EDMA_TYPE_SIG_LEN       50
#define EDMA_PATH_LEN		2048


  /* Repository identifiers */
#define EDMA_SHARED_REPO        0
#define EDMA_LOCAL_REPO         1

  /* Constants related to HotSwapping System*/
#define EDMA_MAX_UPDATES       64

#define EDMA_SHARED_CLASS      1
#define EDMA_LOCAL_CLASS       2

#ifdef __cplusplus
}
#endif
#endif
