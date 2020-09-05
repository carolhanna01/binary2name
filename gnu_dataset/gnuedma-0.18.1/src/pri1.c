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

/*****************************************************************************
 * Entorno de Desarrollo Modular y Abierto
 * (c) David Martínez Oliveira
 * Versión Beta 0.3r1
 * 15 de Junio de 1997
 *
 * Primitivas de Objeto de nivel1
 * REVISIONES:-------------------------------------------------------------
 * 15 de Junio de 1997
 * Primitivas de nivel 1. Por ahora solo vamos a implementar las de propiedades
 * Tenemos problemas con los tipos reales y la comprobación de tipos
 * del Builder
 * Los tipos enteros funcionan perfectamente
 *---------------------------------------------------------------
 * 12 de Julio de 1997
 * Vamos a modificar las primitivas para poner en formato
 * de número de parámetros variable
 *-------------------------------------------------------
 *19 de Julio de 1997
 * Añadimos el tipo STRING
 *---------------------------------------------------
 * 24 de Julio de 1997
 * Intentamos añadir soporte para matrices
 * Mandamos los errores a la consola
 *--------------------------------
 *7 de Agosto de 1997
 * Añadimos soporte para paso de multiples parámetros desde primitivas
 * de nivel 3
 *--------------------------------------
 *8 de Agosto de 1997
 * Modificacion para mapeo de código por proceso
 * Leemos el puntero a la función de pClase en lugar de la estructura global
 *---------------------------------------------
 * 14 de Noviembre de 1997
 *   Añadimos la nueva gestión de tipos a las primitivas de acceso 
 * a propiedades
 *
 * -------------------------
 * 18 de Noviembre de 1997
 *   Añadimos soporte para tipos definidos por usuario
 * 
 * -------------------------------
 * January, 30, 2001
 * Version number changing -> 0.5.1
 * pri1.c : Level 1 EDMA primitives
 *
 * Code don't compile with RH7, due to type promotion in va_args macros.
 * We'll try to fix it. We'll begin code clean up
 * --------------------------------------------------------------------------
 * October, 23th, 2001
 * Finally we check for access modifiers in read and write operations.
 *
 * This feature isn't fully tested, but it seems to work right
 * -----------------------------------------------------------------
 * November, 18th, 2001
 * Compile warnnings removal
 * ------------------------------------------------------
 * March, 2nd, 2002
 * Code cleanup
 * --------------------------------------------------------------------------------
 * April, 2nd, 2003
 * Changes to support new OBJ structure
 * -----------------------------------------------------------------
 * May, 10th, 2003
 * Modification to support changes to internal class structures
 * ---------------------------------------------------------------------------
 * July, 17th, 2003
 * Fixed va_list issue with gcc 3.x
 * Updated primitives to access values using va_arg macro instead of accesing
 * directly the pointer returned by va_start
 * XXXXXXXXXXXX CAUTION XXXXXXXXXXXXXX CAUTION XXXXXXXXXXXXXXXXXXXXX
 * This last change hasn't be extensively tested. Mainly against SIU Proxies
 */
 
#include <stdio.h>
#include <string.h>
#include <stdarg.h>
#include "portable.h"
#include "tobj.h"
#include "vglobal.h"
#include "classq.h"
#include "shmem.h"
#include "ptypes.h"
#include "ttypes.h"
#include "const.h"
#include "tclass.h"
#include "misc.h"
#include "helper.h"

/* THis shoidn't be here, we shouldn't use level 3 primitives at level one*/
#include "pri3.h" 


/* edma_wprop1
 *    Level 1 primitive to write object's properties
 */

EPVoid EDMAPROC	
edma_wprop1 (OBJID IdObj, EUint32 Ind,...) 
{
  EUint32	Tipo;
  EUint32	Off;
  EPVoid	Punt;
  va_list	p;
  EPChar	c,c1;
  HMEM		h;
  EUint32	tam;
  EDMAT_BUFFER  Buf;
  CLASSID       cid;
  
  /* Check Object Reference */
  if ((edma_check_obj_id (IdObj, "edma_wprop1")) == -1) 
    return (EPVoid)-1;

  cid = gObj[IdObj]->IdClass;

  /* Check for write permission*/
  if (pClass[cid]->Prop[Ind].ioTipo == L) 
    {
      edma_printf_err("[ERROR] Property %s is read only",
		      pClass[cid]->Prop[Ind].IdProp);
      return (EPVoid)-1;
    }

  Off = pClass[cid]->Prop[Ind].Off;
  if (Off > gClass[cid]->TamDatos)
    edma_printf_err ("(WProp1) Offset out of ranger for property %d "
		     "of object %d", Ind, IdObj);

  Tipo = pClass[cid]->Prop[Ind].Tipo;
  Punt=(EPByte)(gObj[IdObj]->Data) + Off;
  
  /* p points to first parameter */
  va_start(p,Ind);
  /* Special processing for User Defined Types */
  if (Tipo == DT_EUSER) 
    {
      if (pClass[cid]->Prop[Ind].UserInfo != -1)
	edma_met3 (*((OBJID*)Punt),"Write",p);
      else {
	edma_printf_err ("(WProp1) User type not defined for property");
	return (EPVoid)-1;
      }
    }
  /* Check array flag */
  if ((tam = pClass[cid]->Prop[Ind].nElem) != 0) 
    {
      tam *= tipo[Tipo].tam;
      strncpy (Punt, va_arg(p,EPVoid), tam);
    }
  
  switch (Tipo)
    {
    case DT_EBUFFER:
      Buf = va_arg(p,EDMAT_BUFFER);
      memcpy (Punt, &Buf, sizeof(EDMAT_BUFFER));
      break;	
    case DT_EPOINTER:
    case DT_EOBJECT:
    case DT_EUINT32:
      *((EPUint32)Punt) = va_arg(p,EUint32);
      break;
    case DT_ESINT32:
      *((EPSint32)Punt) = va_arg(p,ESint32);
      break;
    case DT_ESINT8:
    case DT_ECHAR:
      *((EPChar)Punt) = va_arg(p,int);
      break;
    case DT_EZSTRING:
      c = va_arg(p,EPChar);
      tam = strlen(c);
      edma_pfree (*((HMEM*)Punt), (EPVoid)*((HMEM*)Punt));
      h = edma_palloc (sizeof(EByte) * (tam+1));
      c1 = edma_pget (h);
      strncpy (c1, c, tam);
      c1[tam] = 0;
      *((HMEM*)Punt) = h;
      break;
    case DT_ESINT16:
      *((EPSint16)Punt) = (ESint16)va_arg(p,int);
      break;
    case DT_EUINT16:
      *((EPUint16)Punt) = va_arg(p,int);
      break;
    case DT_EUINT8:
    case DT_EBYTE:
      *((EPByte)Punt) = va_arg(p,int);
      break;
    case DT_EWORD:
      *((EPWord)Punt) = va_arg(p,int);;
      break;
    case DT_EDWORD:
      *((EPDWord)Punt) = va_arg(p,EDWord);;
      break;
    case DT_EREAL32:
      *((EPReal32)Punt) = va_arg(p,double);;
      break;
    case DT_EREAL64:
      *((EPReal64)Punt) = va_arg(p,EReal64);
      break;
    }
  
  return Punt;
}

/* edma_rprop1
 *  Level 1 primitive to read Object's properties
 */

EPVoid EDMAPROC	
edma_rprop1 (OBJID IdObj,EUint32 Ind,...) 
{
  EUint32	Tipo;
  EUint32	Off;
  EPVoid	Punt;
  va_list	p;
  EPChar	c,c1;
  EDMAT_BUFFER  *Buf;

  /* Check Object Reference */
  if ((edma_check_obj_id (IdObj, "edma_rprop1")) == -1) 
    return (EPVoid)-1;

  /* Check for write permission*/
  if (pClass[gObj[IdObj]->IdClass]->Prop[Ind].ioTipo == E) 
    {
      edma_printf_err("[ERROR] Property %s is write only... "
		      "whatever means that!"
		      ,pClass[gObj[IdObj]->IdClass]->Prop[Ind].IdProp);
      return (EPVoid)-1;
  }

  Off = pClass[gObj[IdObj]->IdClass]->Prop[Ind].Off;
  Tipo = pClass[gObj[IdObj]->IdClass]->Prop[Ind].Tipo;
  Punt = (EPByte)(gObj[IdObj]->Data)+Off;
  
  va_start(p,Ind);
  /* Special Processing for User Defined Data Types */
  if (Tipo == DT_EUSER) 
    {
      if (pClass[gObj[IdObj]->IdClass]->Prop[Ind].UserInfo!=-1)
	edma_met3 (*((OBJID*)Punt),"Read",p);
      else {
	edma_printf_err ("[RProp1] User type not defined for property");
	return (EPVoid)-1;
      }
    }   
  /* If it's an array we returns a reference */
  if (pClass[gObj[IdObj]->IdClass]->Prop[Ind].nElem!=0) 
    {
      *(va_arg(p,EPByte*)) = (EPByte)Punt;
      return Punt;
    }    
  switch (Tipo)
    {
    case DT_EBUFFER:
      Buf = va_arg(p,EDMAT_BUFFER*);
      memcpy (Buf, Punt, sizeof(EDMAT_BUFFER));
      break;
    case DT_EPOINTER:
    case DT_EOBJECT:
    case DT_EUINT32:
      *(va_arg(p,EPUint32)) = *((EPUint32)Punt);
      break;
    case DT_ESINT32:
      *(va_arg(p,EPSint32)) = *((EPSint32)Punt);
      break;
    case DT_ESINT8:
    case DT_ECHAR:
      *(va_arg(p,EPChar)) = *((EPChar)Punt);
      break;
    case DT_EZSTRING:
      c = (va_arg(p,EPChar));
      c1 = edma_pget (*((HMEM*)Punt));
      strcpy (c, c1);
      break;
    case DT_EUINT16:
      *(va_arg(p,EPUint16)) = *((EPUint16)Punt);
      break;
    case DT_ESINT16:
      *(va_arg(p,EPSint16)) = *((EPSint16)Punt);
      break;
    case DT_EUINT8:
    case DT_EBYTE:
      *(va_arg(p,EPByte)) = *((EPByte)Punt);
      break;
    case DT_EWORD:
      *(va_arg(p,EPWord)) = *((EPWord)Punt);
      break;
    case DT_EDWORD:
      *(va_arg(p,EPDWord)) = *((EPDWord)Punt);
      break;
    case DT_EREAL32:
      *(va_arg(p,EPReal32)) = *((EPReal32)Punt);
      break;
    case DT_EREAL64:
      *(va_arg(p,EPReal64)) = *((EPReal64)Punt);
      break;
    }
  /* FIXME: We must return value*/
  return Punt;
}

/* edma_met1
 *   Level 1 primitive to invoke object's methods
 */

EUint32 EDMAPROC 
edma_met1 (OBJID IdObj, EUint32 Ind, EByte Old, va_list Val) 
{
  PPROC	        Func;
  EUint32	Tipo,i;
  POBJ		Obj;
  
  /* Check Object Reference */
  if ((edma_check_obj_id (IdObj, "edma_met1")) == -1)
    return -1;

  Obj = gObj[IdObj];
  Tipo = Obj->IdClass;
  if ((pClass[Tipo]->Met[Ind].Virtual) && Old == 0) 
    {
      for (i = 0; i < gClass[Tipo]->nMetVir; i++)
	if (Obj->vTable[i].Ind == Ind)
	  break;
      Func = Obj->vTable[i].Func;
      IdObj = ((POBJ)Obj->vTable[i].Obj)->IdObj;
    } 
  else
    Func = (PPROC)pClass[Tipo]->met_func[Ind].Func;

  Tipo = Func (IdObj, Val);
  return (Tipo);
}

/* _edma_wprop1_pargs
 *   Internal Level 1 property write function used by upper levels
 */

EPVoid EDMAPROC	
_edma_wprop1_pargs (OBJID IdObj, EUint32 Ind, va_list p) 
{
  EUint32	Tipo;
  EUint32	Off;
  EPVoid	Punt;
  EPChar	c,c1;
  HMEM		h;
  EUint32	tam;
  EDMAT_BUFFER  Buf;
  CLASSID       cid;

  /* Checks object reference */
  if ((edma_check_obj_id (IdObj, "_edma_wprop1_pargs")) == -1) 
    return (EPVoid)-1;

  cid = gObj[IdObj]->IdClass;
  /* Check for write permission*/
  if (pClass[cid]->Prop[Ind].ioTipo == L) 
    {
      edma_printf_err("[ERROR] Property %s is read only",
		      pClass[cid]->Prop[Ind].IdProp);
      return (EPVoid)-1;
    }

  Off = pClass[cid]->Prop[Ind].Off;
  if (Off > gClass[cid]->TamDatos)
    edma_printf_err("[edma_wprop1_pargs] Offset out of ranger "
		    "for property %d of object %d", Ind, IdObj);

  Tipo = pClass[cid]->Prop[Ind].Tipo;
  Punt = (EPByte) (gObj[IdObj]->Data) + Off;
  
  /* Special processing for user datatypes */
  if (Tipo == DT_EUSER) 
    {
      if (pClass[cid]->Prop[Ind].UserInfo != -1)
	edma_met3 (*((OBJID*)Punt), "Write", p);
      else 
	{
	  edma_printf_err ("[edma_wprop1_pargs] User type not defined "
			   "for property");
	  return (EPVoid)-1;
	}
    }   
  
  /* If it's an array... */
  if ((tam = pClass[cid]->Prop[Ind].nElem) != 0) 
    {
      tam *= tipo[Tipo].tam;
      memcpy (Punt, va_arg(p,EPVoid), tam);
      return p;
    }
  switch (Tipo)
    {
    case DT_EBUFFER:
      Buf = va_arg(p,EDMAT_BUFFER);
      memcpy (Punt, &Buf, sizeof(EDMAT_BUFFER));
      break;	   
    case DT_EPOINTER:
    case DT_EOBJECT:
    case DT_EUINT32:
      *((EPUint32)Punt) = va_arg(p,EUint32);
      break;
    case DT_ESINT32:
      *((EPSint32)Punt) = va_arg(p,ESint32);
      break;
    case DT_ESINT8:
    case DT_ECHAR:
      *((EPChar)Punt) = va_arg(p,int);
      break;
    case DT_EZSTRING:
      c = va_arg(p,EPChar);
      tam = strlen(c);
      edma_pfree (*((HMEM*)Punt),(EPVoid)*((HMEM*)Punt));
      h = edma_palloc (sizeof (EByte) * (tam + 1));
      c1 = edma_pget (h);
      strncpy (c1, c, tam);
      c1[tam] = 0;
      *((HMEM*)Punt) = h;
      break;
    case DT_ESINT16:
      *((EPSint16)Punt) = va_arg(p,int);;
      break;
    case DT_EUINT16:
      *((EPUint16)Punt) = va_arg(p,int);;
      break;
    case DT_EUINT8:
    case DT_EBYTE:
      *((EPByte)Punt) = va_arg(p,int);;
      break;
    case DT_EWORD:
      *((EPWord)Punt) = va_arg(p,int);;
      break;
    case DT_EDWORD:
      *((EPDWord)Punt) = va_arg(p,EDWord);;
      break;
    case DT_EREAL32:
      *((EPReal32)Punt) = va_arg(p,EReal64);
      break;
    case DT_EREAL64:
      *((EPReal64)Punt) = va_arg(p,EReal64);
      break;
    }
  return Punt;
}

/* _edma_rprop1_pargs
 *     Internal Level 1 read property function used by upper levels
 */

EPVoid EDMAPROC	
_edma_rprop1_pargs (OBJID IdObj, EUint32 Ind, va_list p) 
{
  EUint32	Tipo;
  EUint32	Off;
  EPVoid	Punt;
  EPChar	c1,c2;
  EDMAT_BUFFER  *Buf;
  CLASSID       cid;

  /* Checks Object Reference */
  if ((edma_check_obj_id (IdObj, "_edma_rprop1_pargs")) == -1) 
    return (EPVoid)-1;

  cid = gObj[IdObj]->IdClass;

  if (pClass[cid]->Prop[Ind].ioTipo == E) 
    {
      edma_printf_err("[ERROR] Property %s is write only... "
		      "whatever means that!",
		      pClass[cid]->Prop[Ind].IdProp);
    return (EPVoid)-1;
  }

  Off = pClass[cid]->Prop[Ind].Off;
  Tipo = pClass[cid]->Prop[Ind].Tipo;
  Punt = (EPByte)(gObj[IdObj]->Data)+Off;
  
  /* Special processing for user datatypes */
  if (Tipo == DT_EUSER) 
    {
      if (pClass[cid]->Prop[Ind].UserInfo != -1)
	edma_met3 (*((OBJID*)Punt),"Read",p);
      else 
	{
	  edma_printf_err ("[_edma_rprop1_pargs] User type not defined "
			   "for property");
	  return (EPVoid)-1;
	}
    }   
  /* Get array reference */
  if (pClass[cid]->Prop[Ind].nElem != 0) 
    {
      *(va_arg(p,EPByte*)) = Punt;
      return Punt;
    }
  switch (Tipo)
    {
    case DT_EBUFFER:
      Buf = va_arg(p,EDMAT_BUFFER*);
      memcpy (Buf, Punt, sizeof(EDMAT_BUFFER));
      break;
    case DT_EPOINTER:
    case DT_EOBJECT:
    case DT_EUINT32:
      *(va_arg(p,EPUint32)) = *((EPUint32)Punt);
      return (EPVoid)(*((EPUint32)Punt));
      break;
    case DT_ESINT32:
      *(va_arg(p,EPSint32)) = *((EPSint32)Punt);
      return (EPVoid)(*((EPSint32)Punt));
      break;
    case DT_ESINT8:
    case DT_ECHAR:
      *(va_arg(p,EPChar)) = *((EPChar)Punt);
      return (EPVoid)(*((EPChar*)Punt));
      break;
    case DT_EZSTRING:
      c2 = (EPChar)(va_arg(p,EPChar));
      c1 = edma_pget (*((HMEM*)Punt));
      strcpy (c2, c1);
      break;
    case DT_ESINT16:
      *(va_arg(p,EPSint16)) = *((EPSint16)Punt);
      /* XXXXXXXXXXX: Check if this expression is correct.*/
      return (EPVoid)*((EPSint16*)Punt);
      break;
    case DT_EUINT16:
      *(va_arg(p,EPUint16)) = *((EPUint16)Punt);
      /* XXXXXXXXXXX: Check if this expression is correct.*/
      return (EPVoid)*((EPUint16*)Punt);
      break;
    case DT_EUINT8:
    case DT_EBYTE:
      *(va_arg(p,EPByte)) = *((EPByte)Punt);
      /* XXXXXXXXXXX: Check if this expression is correct.*/
      return (EPVoid)*((EPByte*)Punt);
      break;
    case DT_EWORD:
      *(va_arg(p,EPWord)) = *((EPWord)Punt);
      /* XXXXXXXXXXX: Check if this expression is correct.*/
      return (EPVoid)*((EPWord*)Punt);
      break;
    case DT_EDWORD:
      *(va_arg(p,EPDWord)) = *((EPDWord)Punt);
      return (EPVoid)*((EPDWord)Punt);
      break;
    case DT_EREAL32:
      *(va_arg(p,EPReal32)) = *((EPReal32)Punt);
      break;
    case DT_EREAL64:
      *(va_arg(p,EPReal64)) = *((EPReal64)Punt);
      break;
    }
  /* FIXME: We should returno value*/
  return Punt;
}

/* _edma_met1_pargs
 *    Internal Level 1 method invokation function used by upper levels
 */
 
EUint32 EDMAPROC 
_edma_met1_pargs (OBJID IdObj,EUint32 Ind,EByte Old,va_list p) 
{
  EUint32	Tipo,i;
  POBJ		Obj;
  PPROC         Func;
  
  /* Checks Object reference*/
  if ((edma_check_obj_id (IdObj, "_edma_met1_pargs")) == -1) 
    return (EUint32)-1;

  Obj = gObj[IdObj];
  Tipo = Obj->IdClass;
  if ((pClass[Tipo]->Met[Ind].Virtual) && Old == 0) 
    {
      for (i = 0; i < gClass[Tipo]->nMetVir; i++)
	if (Obj->vTable[i].Ind == Ind)
	  break;
      Func = (PPROC)Obj->vTable[i].Func;
      IdObj = ((POBJ)Obj->vTable[i].Obj)->IdObj;
    } 
  else
    Func = (PPROC)pClass[Obj->IdClass]->met_func[Ind].Func;
  
  if (Func)
    return Func (IdObj, va_arg(p,long*), va_arg(p,long*), 
		 va_arg(p,long*), va_arg(p,long*), va_arg(p,long*), 
		 va_arg(p,long*), va_arg(p,long*), va_arg(p,long*), 
		 va_arg(p,long*), va_arg(p,long*));
  else
    return 0;
}

/* edma_prop1_size
 *   Returns the size in bytes of the Ind-th property in  object IdObj
 */

EUint32 EDMAPROC	
edma_prop1_size (OBJID IdObj, EUint32 Ind) 
{
  EUint32	Tipo;
  EUint32	Off;
  EPVoid	Punt;
  EPChar	c1;
  EUint32       size;
  CLASSID       cid;

  /* Check Object Reference */
  if ((edma_check_obj_id (IdObj, "edma_prop1_size")) == -1) 
    return (EUint32)-1;

  cid = gObj[IdObj]->IdClass;
  Off = pClass[cid]->Prop[Ind].Off;
  Tipo = pClass[cid]->Prop[Ind].Tipo;
  Punt = (EPByte)(gObj[IdObj]->Data) + Off;

#if 0  
  /* Special Processing for User Defined Data Types */
  if (Tipo==DT_EUSER) {
    if (pClass[cid]->Prop[Ind].UserInfo!=-1)
      edma_met3 (*((OBJID*)Punt),"Read",p);
    else {
      edma_printf_err ("[RProp1] User type not defined for property");
      return (EPVoid)-1;
    }
  }  
#endif
 
  /* If it's an array we returns a reference */
  if (pClass[cid]->Prop[Ind].nElem != 0) 
    {
      return pClass[cid]->Prop[Ind].nElem;
    }    
  switch (Tipo)
    {
    case DT_EBUFFER:
      size = (((EDMAT_BUFFER*)Punt)->Size);
      break;
    case DT_EZSTRING:
      c1 = edma_pget (*((HMEM*)Punt));
      if (c1)
	size = strlen (c1);
      else
	size = 0;
      break;
    default:
      size = tipo[Tipo].tam;
    }
  /* FIXME: We must return value*/
  return size;
}
