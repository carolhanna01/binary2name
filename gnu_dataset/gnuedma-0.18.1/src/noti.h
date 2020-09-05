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

//---------------------------------------------------------------------------
#ifndef NOTI_H
#define NOTI_H
#include "portable.h"
#include "tobj.h"

#ifdef __cplusplus
extern "C" {
#endif
EUint32 EDMAPROC SetObjNotifyFunc(OBJID ,EPVoid);
EUint32 EDMAPROC DeactivateNotify (OBJID );
EUint32 EDMAPROC NotStub(EPVoid );
EUint32 EDMAPROC SendNotify (OBJID,OBJID,EPChar,EPVoid,EUint32);
EUint32 EDMAPROC AddQueue (OBJID,OBJID,EPVoid,EUint32);
EUint32 EDMAPROC GetQueue (OBJID,EPVoid);

#ifdef __cplusplus
}
#endif
//---------------------------------------------------------------------------
#endif
