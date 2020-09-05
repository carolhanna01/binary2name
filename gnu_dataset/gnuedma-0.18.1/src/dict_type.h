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

#ifndef DICT_TYPE_H
#define DICT_TYPE_H

#ifdef __cplusplus
extern "C" {
#endif

  /* Just a test*/
  /* Simple dictionary entry. For now it only will be used for ordering methods*/
  typedef struct 
  {
    EPChar        data;
    ESint32       indx;
    ESint32       next;
  } EDMA_DICT_ITEM;

  typedef struct
  {
    HMEM            h_myself;
    HMEM            h_entry;
    ESint32         size;
    ESint32         over_size;
    ESint32         over_indx;
    EDMA_DICT_ITEM  *entry;
  } EDMA_DICT_T, *EDMA_DICT;


#ifdef __cplusplus
}
#endif
#endif
