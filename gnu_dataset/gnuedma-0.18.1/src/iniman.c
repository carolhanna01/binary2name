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

/*
 * Entorno de Desarrollo Modular y Abierto
 * Version 0.3r2
 * (c) David Martínez Oliveira
 * 
 * Module   : INIMAN
 * Type     : C Source file
 * Category : System dependency
 * Description :
 *    Simulates WIN32 API functios for managing Windows .INI files-like
 * 
 * Revisions:
 * 21/10/1997
 *   Creation
 *-----------------------------------------------------
 * Febraury, 7th, 2001
 * Code cleanup and comments translation
 * ------------------------------------------------------
 * November, 17th, 2001
 * Compile warnnings removal
 * -----------------------------------------------------
 * March, 2nd, 2002
 * More code cleanup
 * --------------------------------------------------------
 * July, 3rd, 2003
 * Cosmetic changes + input parameters sanity checks
 */
  
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include "portable.h"
#include "shmem.h"

#include "misc.h"

#define INIMAN_C
typedef struct
{
  FILE    *f;
  EPChar   p;
  EUint32  tam;
  EChar    Name[500];
}INIFILE,*PINIFILE;

#include "iniman.h"

/* edma_open_ini
 *   Opens a .INI file
 */

PINIFILE EDMAPROC 
edma_open_ini (EPChar Name) 
{
  PINIFILE   pini;
  EUint32    tam;
  
  if (Name == NULL)
    {
      edma_printf_err ("%s", "[edma_open_ini] Invalid file name");
      return NULL;
    }
  
  pini = (PINIFILE) edma_palloc (sizeof (INIFILE));
  if (pini == NULL) 
    {
      printf ("\nCan't alloc INIFILE struct");
      return 0;
    }
  
  /* We open the file */
  strncpy (pini->Name, Name, 500);
  pini->f = fopen (Name, "rb");
  if (pini->f == NULL) 
    {
      printf ("\n[%s] Can't Open file %s", __FUNCTION__, Name);
      return 0;
    }
  /* We read the whole file in memory */
  fseek (pini->f, 0, SEEK_END);
  pini->tam = ftell (pini->f);
  pini->p = (EPChar) edma_palloc (pini->tam);
  if (pini->p == NULL) 
    {
      printf("\nCan't alloc %ld bytes for file %s", pini->tam, Name);
      return 0;
    }
  memset (pini->p, 0, pini->tam);
  fseek (pini->f, 0, SEEK_SET);
  tam = fread (pini->p, sizeof(char), pini->tam, pini->f);
  
  fclose (pini->f);
  
  return pini;
}

/* edma_close_ini
 *    CLoses a .INI file
 */

ESint32 EDMAPROC 
edma_close_ini (PINIFILE pini)
{
  if (pini == NULL)
    {
      edma_printf_err ("%s", "[edma_close_ini] Invalid file handler");
      return -1;
    }

  edma_pfree ((HMEM)pini->p, pini->p);
  edma_pfree ((HMEM)pini, pini);
  
  return 0;
}

/* edma_get_ini_int
 *   Get an integer value from a .INI file located identified by 
 * its section and field
 */
ESint32 EDMAPROC 
edma_get_ini_int (PINIFILE pini,EPChar Section,EPChar Val,ESint32 def)
{
  EPChar pi;
  EPChar pe;
  EUint32 i;

  if (pini == NULL)
    {
      edma_printf_err ("%s", "[edma_get_ini_int] Invalid file handler");
      return -1;
    }

  if (Section == NULL)
    {
      edma_printf_err ("%s", "[edma_get_ini_int] Invalid Section");
      return -1;
    }
  
  if (Val == NULL)
    {
      edma_printf_err ("%s", "[edma_get_ini_int] Invalid Field");
      return -1;
    }
  
#ifdef WINAPI_32
  return GetPrivateProfileInt(Section,Val,def,pini->Name);
#endif
#ifdef LINUX
  /* Locate section*/
  pi = strstr (pini->p, Section); 
  
  if (pi == NULL)
    return def;
  
  pe = strchr (pi, '[');
  if (pe != NULL)
    *pe = 0;  /* we get the interesting part of the string*/
  pi = strstr (pi, Val); /* We find field*/
  if (pi == NULL) 
    {
      *pe = '[';
      return def;
    }
  pi = strchr (pi, '='); /* We find value*/
  pi++;
  i = atoi(pi);
  *pe = '[';
  
  return i;
#endif
}


/* edma_get_ini_string
 *   Gets an string value from a .INI file specified by its section and field
 */

EUint32 EDMAPROC 
edma_get_ini_string (PINIFILE pini,EPChar Section,
		     EPChar Val,EPChar def,
		     EPChar Buffer,EUint32 size) 
{
  EPChar  pi;
  EPChar  pe,pe1;
  EChar   sec[80];
  EUint32 n;
  
  if (pini == NULL)
    {
      edma_printf_err ("%s", "[edma_get_ini_string] Invalid file handler");
      return -1;
    }

  if (Section == NULL)
    {
      edma_printf_err ("%s", "[edma_get_ini_string] Invalid Section");
      return -1;
    }
  
  if (Val == NULL)
    {
      edma_printf_err ("%s", "[edma_get_ini_string] Invalid Field");
      return -1;
    }

  if (Buffer == NULL)
    {
      edma_printf_err ("%s", "[edma_get_ini_string] Invalid Target buffer");
      return -1;
    }

#ifdef WINAPI_32
  return GetPrivateProfileString(Section,Val,def,Buffer,size,pini->Name);
#endif
#ifdef LINUX
  memset (Buffer, 0, size);

  snprintf (sec, 80, "[%s]", Section);
  pi = strstr (pini->p, sec); /* We find secction */
  if (pi == NULL) 
    {
      return 0;
    }
  pi++;
  pe = strchr(pi, '[');
  if (pe != NULL)
    *pe = 0;  /* We get the interesting part of the string */
  pi = strstr(pi, Val); /* We find field*/
  if ((pi == NULL)) 
    {
      *pe = '[';
      return 0;
    }
  
  pi = strchr (pi, '='); /* We find value */
  pe1 = strchr (pi, 13);
  if (pe1 == NULL) 
    {
      pe1 = strchr(pi, 10);
      if(pe1 == NULL) 
	{
	  printf ("%s", "[edma_get_ini_string] Can't locate end of line");
	  return 0;
	}
    }
  n = pe1-pi;
  n--;
  pi++;
  strncpy (Buffer, pi, n);
  Buffer[n] = 0;
  *pe = '[';
  
  n = strlen (Buffer);

  return n;
#endif
}

/* edma_clean_ini_string
 *   Helper function to transform file separator from windows to unix
 */
ESint32 EDMAPROC 
edma_clean_ini_string (EPChar Buffer)
{
  EUint32 i;

  if (Buffer == NULL)
    {
      edma_printf_err ("%s", "[edma_clean_ini_string] Invalid buffer");
      return -1;
    }
  for (i = 0; i < strlen (Buffer); i++) 
    {
      if(Buffer[i] == '\\') Buffer[i] = '/';
    }
  return 0;
}

