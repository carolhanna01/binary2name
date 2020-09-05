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
 * Class Management module. Implementation
 * Revs: ---------------------------------------------------------
 * 26th September 2007
 * File creation. Refactoring of class entity
 * This module provides the actions associated to the class life-cycle
 * state machine
 * -----------------------
 */

#include <stdio.h>
#include <stdlib.h>
#include <strings.h>

#include "const.h"
#include "vglobal.h"
#include "class_mng.h"

CLASSID EDMAPROC 
edma_class_register (EDMA_REPO repo)
{
  CLASSID   class_id;
  // Check repo object is valid
  // Check class definition data (XXX: Add parameter)
  // Check if class already exists.
  //   - If exists, check if its a new version
  //     - If so, mark class for version update when completely defined
  // Create a new class object (edma_class_new)
  // Initialise class object with class definition information
  // Use repo functions to add class to repository
  
  // Class become defined at this point.
  SClass[class_id]->Status = CLASS_DEF;

  return -1;
}

ESint32 EDMAPROC 
edma_class_unregister (CLASSID class_id)
{
  // Check that class exists
  // Check that class is almost defined
  if (SClass[class_id]->Status < CLASS_DEF) return -1;
  // If so, remove class from associated repository
  // Destroy class related resources (edma_class_free)
  SClass[class_id]->Status = CLASS_FREE;

  return -1;
}

ESint32 EDMAPROC 
edma_class_load_interface (CLASSID class_id)
{
  // Check that class exists
  // Check that class is in state DEFINED
  if (SClass[class_id]->Status != CLASS_DEF) return -1;
  // If class is shared, check if interface was loaded by other process
  //   - If so, map interface instead of loading it.
  // Check IDF parser to use
  // Create IDF parser object and try to load interface
  //    IDF Parser should produce a valid CLASS_INTERFACE object
  // Return error on interface parser load
  SClass[class_id]->Status = CLASS_ILOADED;
  // FIXME: Remove this ProcMapTable Stuff and add this information to
  //        private per-process data
  // Set per-process class status to CLASS_IMAPPED
  ProcMapTable[class_id] = CLASS_IMAPPED;
  return -1;
}

ESint32 EDMAPROC 
edma_class_unload_interface (CLASSID class_id)
{
  // Check if class exists
  // If class is local
  //    Free class Interface related data
  //    Set class as CLASS_DEF, so a new interface can be loaded
  //      for the given definition
  // If class is shared
  //     XXX: Remove interface data????...
  //          or just mark the class as free. Check if there is any
  //          problem to delete shared memory (naming,....)
  // Class becomed defined.
  SClass[class_id]->Status = CLASS_DEF;
  // Set per-process class status to CLASS_IMAPPED
  ProcMapTable[class_id] = CLASS_DEF;
  return -1;
}

ESint32 EDMAPROC 
edma_class_load_implementation (CLASSID class_id)
{
  // Check if class exists
  // Check if class interface is loaded
  //   If not try to load class interface
  // XXX: Implementation is always a per-process issue
  //      Work on private class data structs
  // Check Implementation loader to use
  // Create an implementation loader to try to load interface
  //   Implementation loader will produce a valid CLASS_IMPLEMENTATION object
  // Return error on implementation load error
  // FIXME: Remove this ProcMapTable Stuff and add this information to
  //        private per-process data
  // Set per-process class status to CLASS_LOADED
  // This state only makes sense in a per-process basis
  ProcMapTable[class_id] = CLASS_LOADED;

  // Aditional notes.
  // Now we will use an implementation loader which will be a GNU/EDMA object
  // A default implementation loader for shared libraries will be built-in 
  // and will work as usual
  // Additional implementation loaders could be added to work directly with
  // any other compatible implementation format.
  //
  // SIU Proxies will continue working the same way and its implementation
  // will be loaded using the default built-in implementation manager.
  // Aditionally, SIU Proxies can benefit of this new architecture providing
  // specific implementation loaders for different platforms allowing direct
  // execution of code for non-blind proxies
  //
  // XXX: Go back on this topic when implementing the new model
  return -1;
}

ESint32 EDMAPROC 
edma_class_unload_implementation (CLASSID class_id)
{
  return -1;
}


// Functions below will be used for:
//  - Registering built-in classes. By default:
//    * GENERAL_EXCEPTION
//    * EDMA_IDF Parser
//    * EDMA_CIDF Parser
//    * EDMA_SHARED_LIB Implementation loader
//
ESint32 EDMAPROC 
edma_class_set_int (CLASSID class_id, EDMA_CLASS_INTERFACE class_int)
{
  return -1;
}
ESint32 EDMAPROC 
edma_class_set_impl (CLASSID class_id, EDMA_CLASS_IMPLEMENTATION class_int)
{
  return -1;
}
