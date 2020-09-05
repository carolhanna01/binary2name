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

#ifndef CIDF_PARSER_H
#define CIDF_PARSER_H

/* Data types */
typedef struct superclass_t
{
  char        *name;
  char        *ap;     /* Prefered Anchor Point */
  char        *ap1;
} CIF_SUPERCLASS;

typedef struct property_t
{
  char         *name;
  char         *type;
  char         *access;
  int          array;
} CIF_PROPERTY;

typedef struct param_t
{
  char         *name;
  char         *type;
} CIF_PARAM;

typedef struct method_t
{
  char         *name;
  char         *signature;
  char         *rtype;
  int          n_param;
  CIF_PARAM    **param;
  /* FLAGS */
  int          flags[3];
} CIF_METHOD;

typedef struct class_t
{
  int             major_version;
  int             minor_version;
  char            *namespace;
  char            *name;
  int             n_sc, n_prop, n_met;
  CIF_SUPERCLASS  **sc;
  CIF_PROPERTY    **prop;
  CIF_METHOD      **met;
} CIF_CLASS;

typedef int (*PROCESS_KW_FUNC)(CIF_CLASS*,int, char *);

#ifdef __cplusplus
extern "C" {
#endif

  CIF_CLASS*   cidf_parse (char *filename);
  int          cidf_dump_class (CIF_CLASS *the_class);
  int          cidf_free_data (CIF_CLASS *the_class);

#ifdef __cplusplus
}
#endif

#endif
