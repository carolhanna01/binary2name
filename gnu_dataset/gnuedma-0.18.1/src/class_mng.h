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

/*******************************************************************************
 * class_mng.c
 * Class Management module. Header file
 * Revs: ---------------------------------------------------------
 * 26th September 2007
 * File creation. Refactoring of class entity
 * This module provides the actions associated to the class life-cycle
 * state machine
 * -----------------------
 */

#ifndef CLASS_MNG_H
#define CLASS_MNG_H

#ifdef __cplusplus
extern "C" {
#endif 

#include "portable.h"
#include "tclass.h"
#include "repo.h"


  /* XXX: Temporal type definition */
  typedef void *EDMA_CLASS_INTERFACE;
  typedef void *EDMA_CLASS_IMPLEMENTATION;

  CLASSID EDMAPROC edma_class_register (EDMA_REPO repo);
  ESint32 EDMAPROC edma_class_unregister (CLASSID class_id);

  ESint32 EDMAPROC edma_class_load_interface (CLASSID class_id);
  ESint32 EDMAPROC edma_class_unload_interface (CLASSID class_id);
  ESint32 EDMAPROC edma_class_load_implementation (CLASSID class_id);
  ESint32 EDMAPROC edma_class_unload_implementation (CLASSID class_id);

  ESint32 EDMAPROC edma_class_set_int (CLASSID class_id, EDMA_CLASS_INTERFACE class_int);
  ESint32 EDMAPROC edma_class_set_impl (CLASSID class_id, EDMA_CLASS_IMPLEMENTATION class_int);

#ifdef __cplusplus
}
#endif 


#endif
