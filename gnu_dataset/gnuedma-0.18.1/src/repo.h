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
 * repo.h
 * Repository Management module
 * Revs: ---------------------------------------------------------
 * 30th March 2007
 * File creation. Refactoring of available functions
 * -----------------------
 */

#ifndef EDMA_REPO_H
#define EDMA_REPO_H

#include "portable.h"
#include "iniman.h"

#ifndef EDMA_REPO_C
typedef void *EDMA_REPO;
#endif

#ifdef __cplusplus
extern "C" {
#endif

  ESint32   EDMAPROC   edma_add_std_repo (EPChar repo_name);
  ESint32   EDMAPROC   edma_add_app_repos ();
  EDMA_REPO EDMAPROC   edma_repo_new (ESint32 type, EPChar base_dir, 
				      EPChar fname);
  ESint32   EDMAPROC   edma_repo_free (EDMA_REPO repo);
  ESint32   EDMAPROC   edma_repo_load (EDMA_REPO repo);
  ESint32   EDMAPROC   edma_repo_save (EDMA_REPO repo);

  /* Accessors */
  ESint32   EDMAPROC   edma_repo_set_type (EDMA_REPO repo, ESint32 type);
  ESint32   EDMAPROC   edma_repo_set_base_dir (EDMA_REPO repo, EPChar base_dir);
  ESint32   EDMAPROC   edma_repo_set_file (EDMA_REPO repo, EPChar fname);
  ESint32   EDMAPROC   edma_repo_set_persistent (EDMA_REPO repo, ESint32 flag);

  ESint32   EDMAPROC   edma_repo_get_type (EDMA_REPO repo);
  EPChar    EDMAPROC   edma_repo_get_base_dir (EDMA_REPO repo);
  EPChar    EDMAPROC   edma_repo_get_file (EDMA_REPO repo);
  ESint32   EDMAPROC   edma_repo_get_id (EDMA_REPO repo);
  ESint32   EDMAPROC   edma_repo_add_class (EDMA_REPO repo);
  ESint32   EDMAPROC   edma_repo_del_class (EDMA_REPO repo);

  ESint32 EDMAPROC edma_load_registry (EPChar, EPChar);
  //ESint32 _edma_load_registry (PINIFILE pi, ESint32 type);
  ESint32 _edma_load_registry (EDMA_REPO repo);


  /* Repository Manager */
  ESint32    EDMAPROC  edma_repo_manager_init ();
  ESint32    EDMAPROC  edma_repo_manager_end  ();
  ESint32    EDMAPROC  edma_repo_manager_add_repo (EDMA_REPO);
  ESint32    EDMAPROC  edma_repo_manager_del_repo (EPChar);
  EDMA_REPO  EDMAPROC  edma_repo_manager_get_repo (ESint32);
  EDMA_REPO  EDMAPROC  edma_repo_manager_get_repo_by_name (EPChar, EPChar);
  EPChar     EDMAPROC  edma_repo_manager_get_repo_dir (ESint32);
  EPChar     EDMAPROC  edma_repo_manager_get_repo_name (ESint32);
  ESint32    EDMAPROC  edma_repo_manager_get_repo_type (ESint32);
  ESint32    EDMAPROC  edma_repo_manager_num_repos ();

  /* Repository loading helper functions */
  ESint32  EDMAPROC  edma_repo_add_shared_repo ();
  ESint32  EDMAPROC  edma_repo_add_sys_repo ();
  ESint32  EDMAPROC  edma_add_std_repo (EPChar repo_name);
  ESint32  EDMAPROC  edma_add_app_repos ();
  ESint32  EDMAPROC  edma_repo_add_system_repo ();
#ifdef __cplusplus
}
#endif

#endif
