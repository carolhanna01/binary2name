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
 * Vigo 17 de Octubre de 1996
 *
 * Versión 0.3r1
 * ---------------------------------------------------------
 * Módulo de gestión de sistemas operativos y hardware
 *********************************************************
 *-----------------------------------
 * Febraury, 19th, 2001
 * Code cleanup and comment translation
 * --------------------------------------------------
 * November, 10th, 2001
 * Look fot sysdat32.cfg at SystemPath/etc
 *
 * TODO: Change this to work on other OSes
 * ---------------------------------------------
 * March, 2nd,2002
 * Code cleanup
 */

#include <stdio.h>
#include <string.h>
#include <stdlib.h>
#include "portable.h"
#include "const.h"
#include "error.h"
#include "vglobal.h"
#include "misc.h"

/* _edma_ini_sys_table
 *   Initialice system table (OS + Arch) from system files (sysdat32.cfg)
 */

ESint32	
_edma_ini_sys_table (EPChar EdmaPath) 
{
  FILE		  *f;
  EUint16	  n,i;
  static EUint32  Maqid;
  char		  nom[32];
  EChar		  SysFile[256];
  
  strncpy (SysFile,EdmaPath, 256);
  /* FIXME: Chage this depending on Operating system standars*/
  strncat (SysFile, "/etc/sysdat32.cfg", 256);
  f = fopen (SysFile, "rt");
  if (f == NULL) 
    {
      edma_print("(ERROR)System File not found");
      return -1;
    }
  
  fscanf (f, "%s", nom);
  n = atol (nom);
  n--;
  
  for (i = 0;i < n; i++) 
    {
      fscanf (f, "%s", nom);
      fscanf (f, "%ld", &Maqid);
      GVar->SysMaq[i].Id = Maqid;
      memcpy (GVar->SysMaq[i].Nombre, nom, strlen (nom));
    }
  
  GVar->nMaqNum = n;
  fscanf (f, "%ld", &GVar->nSONum);
  
  for (i = 0;i < GVar->nSONum; i++) 
    {
      fscanf (f, "%s", GVar->SysSO[i].Nombre);
      fscanf (f, "%ld", &GVar->SysSO[i].Id);
    }
  
  fclose (f);
  return 0;
}

/* edma_get_so_id
 *   Returns index of the Operating System name
 */

EUint32	EDMAPROC 
edma_get_so_id (EPChar name) 
{
  EUint32		i;
  
  for (i = 0;i < GVar->nSONum; i++)
    if (strcmp (GVar->SysSO[i].Nombre, name) == 0) 
      break;
  
  if (i == GVar->nSONum)
    edma_print ("(ERROR)Operating System not found");
  
  return i;
}

/* edma_get_arch_id
 *   Returns index of the Architecture name
 */

EUint32	EDMAPROC 
edma_get_arch_id (EPChar name) 
{
  EUint32		i;
  
  for (i = 0;i < GVar->nMaqNum; i++)
    if (strcmp (GVar->SysMaq[i].Nombre, name) == 0) 
      break;
  if (i == GVar->nMaqNum)
    edma_print ("(ERROR)Machine not found");
  return i;
}

/* edma_get_so_name
 *  Returns the name of the id-th operating system registered in system
 */

EUint32	EDMAPROC 
edma_get_so_name (EUint32 id,EPChar	*name) 
{
  
  if (id >= GVar->nSONum) 
    {
      edma_print("(ERROR)Operating System not found");
      return 0;
    }
  *name = GVar->SysSO[id].Nombre;
  
  return 1;
}

/* edma_get_arch_name
 *   Returns name of the id-th Architecture registered in system
 */

EUint32	EDMAPROC 
edma_get_arch_name (EUint32 id,EPChar	*name) 
{
  if (id >= GVar->nMaqNum) {
    edma_print ("(ERROR)Machine not found");
    return 0;
  }
  *name = GVar->SysMaq[id].Nombre;
  return 1;
}

/* edma_get_so_num
 *   Returns the number of registed operating systems
 */

EUint32 EDMAPROC 
edma_get_so_num (void) 
{
  return GVar->nSONum;
}

/* edma_get_arch_num
 *   Returns the number of registed architectures
 */

EUint32 EDMAPROC 
edma_get_arch_num (void) 
{
  return GVar->nMaqNum;
}
