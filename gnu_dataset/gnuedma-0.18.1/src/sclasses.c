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

/* This module register system classes, that is, classes that are
 * needed by the system to work despite of the existance of any repository
 */

#include "portable.h"
#include "vglobal.h"
#include "const.h"

#include "repo.h"
#include "misc.h"

#include "sclasses.h"

ESint32 
edma_register_system_classes ()
{
  CLASSID cid;

  edma_log ("Registering System Classes...");

  edma_log ("  Adding EDMA_EXCEPTION class...");
  cid = EDMA_EXCEPTION_class_factory ();

  gClass[cid]->repo_id = -1;
  gClass[cid]->repo_type = EDMA_LOCAL_REPO;

  edma_log ("System Classes Registerd");

  return 0;
}
