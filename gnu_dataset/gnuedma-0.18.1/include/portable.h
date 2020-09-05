/*
 * EDMA: Entorno de Desarrollo Modular y Abierto
 * Object Oriented and Componetware Framework
 * Copyright (C) 1998, 2002, 2003, 2004, 2005, 2006, 2007, 2008, 2010
 *    David Mart.nez Oliveira
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

/*
*********************************************************
   Fichero de Cabecera para Portabilidad de tipos
	 (c) David Martínez Oliveira
   Vigo, 24 de Septiembre de 1996

   Versión 0+j0
  REVISIONES:----------------------------------------------------------
  * 29 de ABril de 1997
  *	Modificación para la Beta WIN32. Tenemos conflictos con los nombres
  *     de tipos. Añadimos la letra 'E' al principio de todas las definiciones
  *     para evitarlos, esta modificación afecta a todos los módulos
  * ----------------------------
  * 21 de Octubre de 1997
  *  A¤adimos tipos para la versi¢n LINUX y compilador GNU
  * ----------------------------------------------------------------
  * August, 28th, 2003
  * Added constant EDMAINLINE for inline functions
  * ------------------------------------------------------------
  * August, 19th, 2004
  * Added thread related types
  * ---------------------------------------------------------
  * December, 22th, 2006
  * Changed EChar from unsigned char* to char* to remove warnings on
  * gcc-4
  **********************************************************
*/
#ifndef PORTABLE_H
#define PORTABLE_H

#ifdef __cplusplus
extern "C"{
#endif

#ifdef BC_31
// Tipos básicos
typedef		unsigned  char Flag;
typedef		unsigned  char EByte;
typedef		unsigned  int  EWord;
typedef		unsigned  long  EDWord;
typedef  	unsigned  char EChar;

// Enteros
typedef		unsigned  char 	EUint8;
typedef 	unsigned  int 	EUint16;
typedef 	unsigned  long 	EUint32;
typedef 	signed    char 	ESint8;
typedef 	signed    int 	ESint16;
typedef 	signed    long 	ESint32;

// Reales
typedef		float 				EReal32;
typedef  	double 				EReal64;
typedef  	long	double 	EReal80;

// Punteros

typedef   void 						*EPVoid;
typedef   unsigned  char 	*EPChar;
typedef   unsigned  char 	*EPByte;
typedef   unsigned  int 	*EPWord;
typedef   unsigned  long 	*EPDWord;

typedef   unsigned  char 	*EPUint8;
typedef   unsigned  int 	*EPUint16;
typedef   unsigned  long 	*EPUint32;
typedef   signed    char 	*EPSint8;
typedef   signed    int 	*EPSint16;
typedef   signed    long 	*EPSint32;

typedef   float 					*EPReal32;
typedef   double 					*EPReal64;
typedef		long      double *EPReal80;

// Punteros far para PC´s
typedef   void 						far *EPfVoid;
typedef   unsigned  char 	far *EPfChar;
typedef   unsigned  char 	far *EPfByte;
typedef   unsigned  int 	far *EPfWord;
typedef   unsigned  long 	far *EPfDWord;

typedef   unsigned  char 	far *EPfUint8;
typedef   unsigned  int 	far *EPfUint16;
typedef   unsigned  long 	far *EPfUint32;
typedef   signed    char 	far *EPfSint8;
typedef   signed    int 	far *EPfSint16;
typedef   signed    long 	far *EPfSint32;

typedef   float 					far *EPfReal32;
typedef   double 					far *EPfReal64;
typedef		long      double far *EPfReal80;

// funciones


#endif

#ifdef EBONY
// Tipos básicos
typedef		unsigned  char Flag;
typedef		unsigned  char EByte;
typedef     unsigned  short  EWord;
typedef		unsigned  long  EDWord;
typedef  	unsigned  char EChar;

// Enteros
typedef		unsigned  char 	EUint8;
typedef     unsigned  short   EUint16;
typedef 	unsigned  long 	EUint32;
typedef 	signed    char 	ESint8;
typedef     signed    short   ESint16;
typedef 	signed    long 	ESint32;

// Reales
typedef		float 				EReal32;
typedef  	double 				EReal64;
typedef  	long	double 	EReal80;

// Punteros

typedef   void 						*EPVoid;
typedef   char 	*EPChar;
typedef   unsigned  char 	*EPByte;
typedef   unsigned  short     *EPWord;
typedef   unsigned  long 	*EPDWord;

typedef   unsigned  char 	*EPUint8;
typedef   unsigned  short     *EPUint16;
typedef   unsigned  long 	*EPUint32;
typedef   signed    char 	*EPSint8;
typedef   signed    short     *EPSint16;
typedef   signed    long 	*EPSint32;

typedef   float 					*EPReal32;
typedef   double 					*EPReal64;
typedef		long      double *EPReal80;

// Punteros  para PC´s
typedef   void                       *EPfVoid;
typedef   unsigned  char     *EPfChar;
typedef   unsigned  char     *EPfByte;
typedef   unsigned  short      *EPfWord;
typedef   unsigned  long     *EPfDWord;

typedef   unsigned  char     *EPfUint8;
typedef   unsigned  short      *EPfUint16;
typedef   unsigned  long     *EPfUint32;
typedef   signed    char     *EPfSint8;
typedef   signed    short      *EPfSint16;
typedef   signed    long     *EPfSint32;

typedef   float                      *EPfReal32;
typedef   double                     *EPfReal64;
typedef     long      double  *EPfReal80;

// funciones


#endif

#ifdef GCC
// Tipos básicos
typedef		unsigned  char Flag;
typedef		unsigned  char EByte;
typedef     unsigned  short  EWord;
typedef		unsigned  long  EDWord;
typedef  	char EChar;
  //typedef  	unsigned  char EChar;

// Enteros
typedef		unsigned  char 	EUint8;
typedef     unsigned  short   EUint16;
typedef 	unsigned  long 	EUint32;
typedef 	signed    char 	ESint8;
typedef     signed    short   ESint16;
typedef 	signed    long 	ESint32;

// Reales
typedef		float 				EReal32;
typedef  	double 				EReal64;
typedef  	long	double 	EReal80;

// Punteros

typedef   void 						*EPVoid;
typedef   EChar 	*EPChar;
typedef   unsigned  char 	*EPByte;
typedef   unsigned  short     *EPWord;
typedef   unsigned  long 	*EPDWord;

typedef   unsigned  char 	*EPUint8;
typedef   unsigned  short     *EPUint16;
typedef   unsigned  long 	*EPUint32;
typedef   signed    char 	*EPSint8;
typedef   signed    short     *EPSint16;
typedef   signed    long 	*EPSint32;

typedef   float 					*EPReal32;
typedef   double 					*EPReal64;
typedef		long      double *EPReal80;

// Punteros  para PC´s
typedef   void                       *EPfVoid;
typedef   unsigned  char     *EPfChar;
typedef   unsigned  char     *EPfByte;
typedef   unsigned  short      *EPfWord;
typedef   unsigned  long     *EPfDWord;

typedef   unsigned  char     *EPfUint8;
typedef   unsigned  short      *EPfUint16;
typedef   unsigned  long     *EPfUint32;
typedef   signed    char     *EPfSint8;
typedef   signed    short      *EPfSint16;
typedef   signed    long     *EPfSint32;

typedef   float                      *EPfReal32;
typedef   double                     *EPfReal64;
typedef     long      double  *EPfReal80;

// funciones


#endif

#ifdef LINUX
#define EDMAPROC
#define EDMAINLINE inline
  //#define EDMAINLINE EDMAPROC
//typedef void *HMEM;
typedef EUint32 HMEM;  

/* GNU/EDMA Threading types */
#include <pthread.h>

typedef pthread_t         ETHREAD; 
typedef void              *ETHREAD_PARAMS; 
typedef pthread_mutex_t  *EMUTEX; 
typedef pthread_cond_t    *ECOND; 
typedef pthread_key_t     ETKEY; 
typedef void*             (*EPROC)(ETHREAD_PARAMS);

/* Threading type declaration ends */

#endif

#ifdef DARWIN
#define EDMAPROC
#define EDMAINLINE inline
  //#define EDMAINLINE EDMAPROC
//typedef void *HMEM;
typedef EUint32 HMEM;  

/* GNU/EDMA Threading types */
#include <pthread.h>

typedef pthread_t         ETHREAD; 
typedef void              *ETHREAD_PARAMS; 
typedef pthread_mutex_t  *EMUTEX; 
typedef pthread_cond_t    *ECOND; 
typedef pthread_key_t     ETKEY; 
typedef void*             (*EPROC)(ETHREAD_PARAMS);

/* Threading type declaration ends */

#endif


#ifdef WINAPI_32
#define EDMAPROC _export
#define EDMAINLINE inline
typedef HANDLE	HMEM;

  /* Thread-Related types missed at this point */
#endif
// Macros
#ifdef WINAPI_APP
/*
	Macro MiMalloc
		p		-> Puntero a los datos que se reservan
		s		-> Tamaño del bloque de datos
		c		-> CAST para el puntero
		f		-> Flags para el bloque de memoria
		h		-> Handle para windows
*/
#define MiMalloc(p,s,c,f,h) h=GlobalAlloc(f,s);p=(c)GlobalLock(h);

#define MiFree(p,h) GlobalUnlock(h),GlobalFree(h);

#define EDMAPROC __export FAR PASCAL

#else
#define MiMalloc(p,s,c,f,h) (p=(c)malloc(s);)
#define MiFree(p,h) free(p);
#endif

#ifdef __cplusplus
}
#endif

#endif		//PORTABLE_H
