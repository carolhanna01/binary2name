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
 * GNU/Linux Dynamic Linking support
 * (c) 1998 David Martínez Oliveira
 * 
 * System depend Module
 * --------------------------------------------------------------------
 * March, 2nd, 2002
 * Code cleanup
 */

#include <dlfcn.h>
#include <stdio.h>
#include <string.h>
#include <errno.h>
#include "portable.h"
#include "misc.h"

/* LoadLib
 *    Load a shared library in memory
 */

HMEM EDMAPROC 
LoadLib (EPChar n) 
{
  EPVoid      a;

  a = dlopen (n, RTLD_NOW | RTLD_GLOBAL);
  if (!a) 
    {
      edma_printf_err ("[LoadLib]%s",dlerror());
      return 0;
    }

  return ((HMEM)a);
}

/* GetAddress
 *   Get the pointer to a library symbol
 */

EPVoid EDMAPROC 
GetAddress (HMEM a,EPChar n) 
{
  EPVoid     p;
  EPChar     error;
  
  p = (EPVoid) dlsym ((EPVoid)a, n);
  error = dlerror();
  if (error != NULL) 
    {
      edma_printf_err ("[GetAddress]%s", error);
      return 0;
    }

  return p;
}

/* UnloadLib
 *   Unloads a given library from memory
 */
EUint32 EDMAPROC 
UnloadLib (HMEM i) 
{
  return dlclose((EPVoid)i);
}

