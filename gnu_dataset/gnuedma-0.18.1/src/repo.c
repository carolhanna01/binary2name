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
 * repo.c
 * Repository Management module. Implementation
 * Revs: ---------------------------------------------------------
 * 31th March 2007
 * File creation. Refactoring of available functions
 * -----------------------
 */
#include <stdio.h>
#include <stdlib.h>

#include <sys/stat.h>

#include "portable.h"
#include "vglobal.h"
#include "const.h"

#include "tobj.h"
#include "tclass.h"	
#include "ttypes.h"

#include "shmem.h"
#include "clas.h"

#include "subsystems.h"
#include "multiidf.h"
#include "siu.h"
#include "emi.h"
#include "pri3.h"

#include "misc.h"
#include "iniman.h"
#include "idf.h"
#include "sys31.h"
#include "classrt.h"

#define EDMA_REPO_C
typedef struct edma_repo_t
{
  ESint32     type;
  EPChar      base_dir;
  EPChar      fname;
  ESint32     nClasses;
  ESint32     offset;
  ESint32     id;
  ESint32     persistent;
} *EDMA_REPO;

#include "repo.h"


/* Repo Manager Local Vars */
EDMA_REPO  *repo_list = NULL;
ESint32    num_repos = 0;
ESint32    g_repo_off = 0;


/* Local Prototypes */
ESint32	static _edma_get_registry_entry (PINIFILE,EUint32,EUint32,CLASE*);


EDMA_REPO EDMAPROC   
edma_repo_new (ESint32 type, EPChar base_dir, EPChar fname)
{

  EDMA_REPO   repo;

  if (base_dir == NULL)
    {
      edma_printf_err ("Invalid base_dir to create repository");
      return NULL;
    }

  if (fname == NULL)
    {
      edma_printf_err ("Invalid base_dir to create repository");
      return NULL;
    }

  if ((type < 0) || (type > EDMA_LOCAL_REPO))
    {
      edma_printf_err ("Invalid repository type");
      return NULL;
    }

  if ((repo = (EDMA_REPO) malloc (sizeof(struct edma_repo_t))) == NULL)
    {
      edma_printf_err ("Cannot allocate memory for repository");
      return NULL;
    }
  repo->type = type;
  repo->base_dir = strdup (base_dir);
  repo->fname = strdup (fname);
  repo->persistent = 0;
  return repo;
}

ESint32   EDMAPROC   
edma_repo_free (EDMA_REPO repo)
{
  if (!repo)
    {
      edma_printf_err ("Invalid repository object.");
      return -1;
    }
  if (repo->base_dir) free (repo->base_dir);
  if (repo->fname) free (repo->fname);
  free (repo);

  return 0;
}


/* Accessors */
ESint32   EDMAPROC   
edma_repo_set_type (EDMA_REPO repo, ESint32 type)
{
  if (!repo)
    {
      edma_printf_err ("Invalid repository object");
      return -1;
    }
  if ((type < 0) || (type > EDMA_LOCAL_REPO))
    {
      edma_printf_err ("Invalid repository type");
      return -1;
    }
  repo->type = type;
  return 0;
}

ESint32   EDMAPROC   
edma_repo_set_base_dir (EDMA_REPO repo, EPChar base_dir)
{
  if (!repo)
    {
      edma_printf_err ("Invalid repository object");
      return -1;
    }
  if (!base_dir)
    {
      edma_printf_err ("Invalid base_dir for repository object");
      return -1;
    }
  if (repo->base_dir) free (repo->base_dir);
  repo->base_dir = strdup (base_dir);

  return 0;
}

ESint32   EDMAPROC   
edma_repo_set_file (EDMA_REPO repo, EPChar fname)
{
  if (!repo)
    {
      edma_printf_err ("Invalid repository object");
      return -1;
    }
  if (!fname)
    {
      edma_printf_err ("Invalid base_dir for repository object");
      return -1;
    }
  if (repo->fname) free (repo->fname);
  repo->fname = strdup (fname);

  return 0;
}


ESint32   EDMAPROC   
edma_repo_set_persistent (EDMA_REPO repo, ESint32 flag)
{
  if (!repo)
    {
      edma_printf_err ("Invalid repository object");
      return -1;
    }

  edma_log ("Setting Persistent Flag to: %d for Repo: %s\n",
	    flag, repo->fname);
  repo->persistent = flag;

  return 0;
}


ESint32   EDMAPROC   
edma_repo_get_type (EDMA_REPO repo)
{
  if (!repo)
    {
      edma_printf_err ("Invalid repository object");
      return -1;
    }

  return repo->type;
}

EPChar    EDMAPROC   
edma_repo_get_base_dir (EDMA_REPO repo)
{
  if (!repo)
    {
      edma_printf_err ("Invalid repository object");
      return NULL;
    }

  return repo->base_dir;
}

EPChar    EDMAPROC   
edma_repo_get_file (EDMA_REPO repo)
{
  if (!repo)
    {
      edma_printf_err ("Invalid repository object");
      return NULL;
    }
  return repo->fname;
}

ESint32    EDMAPROC   
edma_repo_get_id (EDMA_REPO repo)
{
  if (!repo)
    {
      edma_printf_err ("Invalid repository object");
      return -1;
    }
  return repo->id;
}


ESint32 EDMAPROC
edma_repo_add_class (EDMA_REPO repo)
{
  if (!repo)
    {
      edma_printf_err ("Invalid repository object");
      return -1;
    }
  repo->nClasses++;
  if (repo->type == EDMA_SHARED_REPO) GVar->nClases++;

  return repo->nClasses;
}

ESint32 EDMAPROC
edma_repo_del_class (EDMA_REPO repo)
{
  if (!repo)
    {
      edma_printf_err ("Invalid repository object");
      return -1;
    }
  repo->nClasses --;
  if (repo->type == EDMA_SHARED_REPO) GVar->nClases--;

  return repo->nClasses;
}

/* SERIALIZATION API */

ESint32   EDMAPROC   
edma_repo_load (EDMA_REPO repo)
{
  if (!repo)
    {
      edma_printf_err ("[%s] Invalid EDMA_REPO object\n", __FUNCTION__);
      return -1;
    }
  if ((!repo->base_dir) || (!repo->fname))
    {
      edma_printf_err ("[%s] Trying to load a not fully "
		       "configured repository", __FUNCTION__);
      return -1;
    }
  edma_load_registry (repo->base_dir, repo->fname);

  return 0;
}

ESint32   EDMAPROC   
edma_repo_save (EDMA_REPO repo)
{
  FILE		*fich;
  EChar         temp[EDMA_PATH_LEN];
  EUint32	i,j,index;
  ESint32       istart, iend, n;
  EPChar	Aux;
  

  snprintf (temp, EDMA_PATH_LEN, "%s/%s", repo->base_dir, repo->fname);
  edma_log ("[%s] Output File : %s", __FUNCTION__, temp);
  /* FIXME: Here we should check if we can update GNU/EDMA register
   * Now, a warnning message will be showed on applications shutdown
   */

  if (repo->type == EDMA_SHARED_REPO)
    n = GVar->nClases;
  else
    n = repo->nClasses;



  edma_log ("[%s] Storing %d classes for this repository (%d) starting at %ld", 
	    __FUNCTION__, n, repo->id, repo->offset);

  chmod (temp, S_IRUSR|S_IWUSR|S_IRGRP|S_IWGRP|S_IROTH|S_IWOTH);
  if ((fich = fopen (temp,"wt")) == NULL)
    {
      edma_printf_err ("[%s] Can´t open '%s' for update", __FUNCTION__, temp);
      return -1;
    }

  fprintf (fich,"[General]\n");
  fprintf (fich,"nClasses=%ld\n", n);
  j = 0;

  /* Calculate bounds for reposository dump */
  if (repo->type == EDMA_SHARED_REPO)
    {
      istart = 0;
      iend = GVar->nClases;
      n = GVar->nClases;
    }
  else
    {
      istart = MAX_CLASE;
      iend = MAX_CLASE + nLocalClasses;
      n = repo->nClasses;
    }
  edma_log ("Repo Save loop: %d, %d, %d\n", istart, iend, n);
  for (i = istart; i < iend && j < n; i++) 
    {
      edma_log ("Processing %d class '%s'", i, gClass[i]->ClassName);
      if (gClass[i]->repo_id != repo->id) 
	{
	  edma_log ("Skiping class %d:%s for repo %s:%d\n", 
		       i, gClass[i]->ClassName, repo->fname, repo->type);
	  continue;
	}
      
      edma_log ("Storing class '%s'", gClass[i]->ClassName);
      if (gClass[i] && (repo->type == EDMA_SHARED_REPO 
			|| ProcMapTable[i] != CLASS_FREE)) 
	{
	  fprintf (fich, "[CLASS%ld]\n", j);
	  fprintf (fich, "ClassName=%s\n", gClass[i]->ClassName);

	  if (strlen (gClass[i]->NameSpace) > 0)
	    fprintf (fich, "NameSpace=%s\n", gClass[i]->NameSpace);

	  fprintf (fich, "Machine=%s\n", GVar->SysMaq[gClass[i]->MaqId].Nombre);
	  fprintf (fich, "OperatingSystem=%s\n", 
		   GVar->SysSO[gClass[i]->SOId].Nombre);
	  
	  Aux = gClass[i]->SysClass.ModuleName;
	  fprintf (fich, "Implementation=%s\n", Aux);
	  
	  if (gClass[i]->IDFParser == -1)
	    fprintf (fich, "IDFParser=EDMAIDF\n");
	  else 
	    {
	      index = gClass[i]->IDFParser;
	      fprintf (fich, "IDFParser=%s\n",
		       gClass[edma_subsystem_get_item_class (SS_INGRIDF, index)]->ClassName);
	    }
	  if (gClass[i]->IsIDF)
	    fprintf (fich, "IsIDFParser=1\n");
	  if (gClass[i]->IsSIU)
	    fprintf (fich, "IsSIUProxy=1\n");
	  if (gClass[i]->IsEMI)
	    fprintf (fich, "IsEMI=1\n");
	  if (gClass[i]->SIUProxy != -1)
	    fprintf (fich, "SIUProxy=%s\n", 
		     gClass[gClass[i]->SIUProxy]->ClassName);
	  fprintf (fich, "MajorVer=%d\n", gClass[i]->MajorVer);
	  fprintf (fich, "MinorVer=%d\n", gClass[i]->MinorVer);
	  /* Do not save Current version for local repositories. 
	   * It is calculated on load */
	  j++;
	}
      
    }
  fprintf (fich, "[]");
  fclose (fich);

  edma_log ("%s", "[edma_repo_save] Repository Saved!!");
  
  return 0;
}


/* Old Functions */
/* edma_load_registry
 *    Allows an application to private load a LOCAL class registry
 * Keep for backwards compatibility
 */

ESint32 EDMAPROC
edma_load_registry (EPChar base_dir, EPChar fname)
{
  EDMA_REPO   repo;
  
  edma_log ("%s: base_dir: '%s'  name: '%s'\n", __FUNCTION__, base_dir, fname);
  /* Registe Shared repo*/
  if ((repo = edma_repo_new (EDMA_LOCAL_REPO, base_dir, fname)) == NULL)
    {
      edma_printf_err ("%s", "FATAL: Cannot create repo object ");
      return -1;
    }

  edma_repo_manager_add_repo (repo);

  return _edma_load_registry (repo);

}


/**************************************************************************/
/* Registry management functions                                          */
/**************************************************************************/

/* _edma_load_registry
 *   Loads an EDMAIDF registry which can be managed as LOCAL or SHARED
 */

ESint32
_edma_load_registry (EDMA_REPO repo)
{
  PINIFILE	pi;
  CLASSID       cid;
  EUint32	i, n_pabstract;
  ESint32       n;
  EChar         fname[EDMA_PATH_LEN];

  if (repo == NULL)
    {
      edma_printf_err ("[%s] Invalid Repo object", __FUNCTION__);
      return -1;
    }
  snprintf (fname, EDMA_PATH_LEN, "%s/%s", repo->base_dir, repo->fname);

  if ((pi = edma_open_ini (fname)) == NULL)
    {
      edma_printf_err ("[%s] Can't open file %s", __FUNCTION__, fname);
      return -1;
    }
  
  edma_log ("Loading registry at : '%s' - '%s'...", 
	    repo->base_dir, repo->fname);

  n = edma_get_ini_int (pi, "General", "nClasses", 0);
  edma_log ("Loading %d classes from repository", n);
  n_pabstract = 0;
  repo->nClasses = n;
  repo->offset = g_repo_off;

  for (i = 0; i < n; i++) 
    {
      /* We get class pointer with general information */
      if (repo->type == EDMA_LOCAL_REPO)
	cid = _edma_get_local_class_id ();
      else
	cid = i;

      if ((_edma_get_registry_entry (pi, i, cid, SClass[cid])) == -1)
	{
	  edma_printf_err ("Can't load entry %d from registry", i);
	  /* FIXME: Mark current entry as BAD */
	  continue; /* Try to read next entry*/
	}

      SClass[cid]->Derived = 0;
      if (SClass[cid]->CurrentVer == -1)
	SClass[cid]->CurrentVer = cid;
      
      n_pabstract += SClass[cid]->PureAbstract;

      /* Temporal Patch. Reset Class Status because interface and 
       * implementation has not been loaded
       * at this point. 
       * edma_local_class_finish, set the class status to CLASS_LOADED 
       * because it is commonly used
       * to run-time class registring
       * NOTE: Above doesn't apply on local repositories
       */
      SClass[cid]->Status = CLASS_DEF;
      SClass[cid]->repo_id = repo->id;

      /* Class identifier local to its repository */
      SClass[cid]->repo_class_id = i;   
      ProcMapTable[cid] = CLASS_DEF;
      if (repo->type == EDMA_LOCAL_REPO)
	{
	  SClass[cid]->repo_type = EDMA_LOCAL_REPO;
	  nLocalClasses++;
	}
      else
	{
	  SClass[cid]->repo_type = EDMA_SHARED_REPO;
	  GVar->nClases ++;
	}

      _edma_idf_set_subsystems (cid);

      edma_log ("Updating versions for class %d (%s) v (%d.%d)", cid,
		gClass[cid]->ClassName, 
		gClass[cid]->MajorVer, gClass[cid]->MinorVer);
      _edma_class_update_versions (cid);

    }
  if (repo->type == EDMA_LOCAL_REPO)
    g_repo_off += n;

  edma_log ("  Current Local Repo Offset is: %d", g_repo_off);
  edma_log ("  Registry loaded. %d classes defined", n);
  edma_log ("%d Pure Abstract Classes or Interfaces defined", n_pabstract);

  edma_close_ini (pi);
  return n;
}

/* FIXME: This function must be reviewed */

/* _edma_get_registry_entry
 *    Gets a class definition entry from a registry file 
 */

ESint32	
_edma_get_registry_entry (PINIFILE pi,EUint32 order, EUint32 indx,CLASE *Clase)
{
  EUint32		r; 
  EChar			Cadena[255];
  EChar			Maquina[EDMA_GENERAL_ID_LEN];
  EChar			SysOp[EDMA_GENERAL_ID_LEN];
  ESint32               ret;

  /* We build section string */
  sprintf (Cadena, "CLASS%ld", order);
  /* We read dll name */
  edma_get_ini_string (pi, Cadena, "ClassName", NULL, 
		       Clase->ClassName, EDMA_CLASS_NAME_LEN);
  edma_get_ini_string (pi, Cadena, "NameSpace", "", 
		       Clase->NameSpace, EDMA_CLASS_NAMESPACE_LEN);
  /*We add maker and version information to memory structs*/
  /*
  edma_get_ini_string (pi, Cadena, "Maker", NULL, 
		       Clase->Maker, EDMA_CLASS_MAKER_LEN);
  */
  Clase->MajorVer = edma_get_ini_int (pi, Cadena, "MajorVer", 0);
  Clase->MinorVer = edma_get_ini_int (pi, Cadena, "MinorVer", 0);   
  //Clase->CurrentVer = edma_get_ini_int (pi, Cadena, "CurrentVer", -1);
  Clase->CurrentVer = indx;
  edma_get_ini_string (pi, Cadena, "Machine", NULL, Maquina, 80);
  edma_get_ini_string (pi, Cadena, "OperatingSystem", NULL, SysOp, 80);


  Clase->MaqId = edma_get_arch_id (Maquina);
  Clase->SOId = edma_get_so_id (SysOp);
  Maquina[0] = 0;
  edma_get_ini_string (pi, Cadena, "Implementation", NULL, Maquina, 80);
  /* if no implementation is defined... 
     this is a PureAbstract/Interface definition*/ 
  if ((Maquina == NULL) || (strlen (Maquina) == 0)) 
    Clase->PureAbstract = 1;
  else 
    {
    Clase->PureAbstract = 0;
    strncpy (Clase->SysClass.ModuleName, Maquina, EDMA_CLASS_MODULE_LEN);
  }
  
  Maquina[0] = 0;
  r = edma_get_ini_string (pi, Cadena, "IDFParser", NULL, Maquina, 80);
  /* Initially, we use the default parser*/
  Clase->IDFParser = -1;		
   
  strncpy (SysOp, "EDMAIDF", 7);

  if (strcmp (SysOp, Maquina) != 0)
    {
      edma_log ("       Requiring IDF Parser '%s'\n", Maquina);
      Clase->IDFParser = edma_ingridf_get_parser (Maquina);
      edma_log ("        Class %s using IDF parser %d,%s",
		Clase->ClassName, Clase->IDFParser,
		edma_subsystem_get_item_id (SS_INGRIDF, Clase->IDFParser));
      edma_printf_dbg (40, -1, "        Class %s using IDF parser %d,%s",
		       Clase->ClassName, Clase->IDFParser,
		       edma_subsystem_get_item_id (SS_INGRIDF, Clase->IDFParser));
    }
  
  r = edma_get_ini_string (pi, Cadena, "SIUProxy", NULL, Maquina, 80);
  /*Initially, we consider the class as EDMA-native*/
  Clase->SIUProxy = -1;	
  if (r != 0) 
    {
      Clase->SIUProxy = edma_siu_get_proxy (Maquina);
      edma_printf_dbg (40, -1, "        Class %s in system %d,%s",
		       Clase->ClassName, Clase->IDFParser,
		       edma_subsystem_get_item_id (SS_SIU, Clase->SIUProxy));
    }
  
  Clase->IsIDF = edma_get_ini_int (pi, Cadena, "IsIDFParser", 0);
  Clase->IsSIU = edma_get_ini_int (pi, Cadena, "IsSIUProxy", 0);
  Clase->IsEMI = edma_get_ini_int (pi, Cadena, "IsEMI", 0);

  /* Create PCLASS entry for this registered class */
  if ((ret = _edma_class_alloc_priv_data (indx)) < 0)
    {
      /* Undo Transaction*/
      return -1;
    }

  edma_dict_add_entry (edma_class_dict, Clase->ClassName, indx);

  return 0;
}

/***********************************************************************/
/* Repository Manager */

ESint32  EDMAPROC  
edma_repo_manager_init ()
{
  edma_log ("%s", "+ Starting Repository Manager...");

  repo_list = NULL;
  num_repos = 0;
  g_repo_off = 0;

  return 0;
}

ESint32  EDMAPROC  
edma_repo_add_shared_repo ()
{
  EDMA_REPO     repo;


  edma_log ("%s", "+ Adding GNU/EDMA Shared Repository...");
  
  /* Loads Share Repository */  
  edma_log ("  EDMA System Path [%s]. Base Registry [%s]", 
	    GVar->SystemPath, "/etc/edma32.cfg");

  /* Registe Shared repo*/
  if ((repo = edma_repo_new (EDMA_SHARED_REPO, 
			     GVar->SystemPath, "/etc/edma32.cfg")) == NULL)
    {
      edma_printf_err ("%s", "FATAL: Cannot create shared repository");
      return -1;
    }

  edma_repo_manager_add_repo (repo);

  /* Shared Repo is always persistent */
  edma_repo_set_persistent (repo, 1);  
  if (GVar->Running <= 1)
    {
      /* Read System-wide class register */
      GVar->nClases = _edma_load_registry (repo);
    }
  
  return 0;
}

ESint32  EDMAPROC  
edma_repo_manager_end  ()
{
  ESint32    i;

  edma_log ("%s", "+ Stopping Repository Manager...");
  edma_log ("  Currently %d repositories registered", num_repos);

  if (repo_list == NULL)
    {
      edma_printf_err ("RepoManager:: No repository registered");
      return 0;
    }
  for (i = num_repos - 1; i >= 0; i--)
    {
      if (repo_list[i]->persistent)
	{
	  edma_log ("[%s] Saving repo %d %p", __FUNCTION__, i, repo_list[i]);
	  /* Dump repository changes */
	  edma_repo_save (repo_list[i]);
	  /* Destroy Repository */
	  edma_log ("[%s] Freeing repo %d", __FUNCTION__, i);
	}
      else
	edma_log ("[%s] Skipping repo %d %p", __FUNCTION__, i, repo_list[i]);
      edma_repo_free (repo_list[i]);
    }

  edma_log ("[%s] DONE!!", __FUNCTION__);

  free (repo_list);

  return 0;
}

ESint32  EDMAPROC  
edma_repo_manager_add_repo (EDMA_REPO r)
{
  if (r == NULL)
    {
      edma_printf_err ("[%s] Invalid repository object", __FUNCTION__);
      return -1;
    }
  num_repos++;
  
  if ((repo_list = realloc (repo_list, sizeof(EDMA_REPO) * num_repos)) == NULL)
    {
      edma_printf_err ("[%s] Cannot allocate memory for %d repo objects", 
		       __FUNCTION__, num_repos);
      return -1;
    }

  repo_list[num_repos - 1] = r;
  r->id = num_repos - 1;

  return 0;
}

ESint32  EDMAPROC  
edma_repo_manager_del_repo (EPChar fname)
{
  return 0;
}

EDMA_REPO  EDMAPROC  
edma_repo_manager_get_repo (ESint32 indx)
{
  if (!repo_list ) return NULL;
  if (indx < 0 || indx >= num_repos) return NULL;

  return repo_list[indx];
}

EDMA_REPO  EDMAPROC  
edma_repo_manager_get_repo_by_name (EPChar base_dir, EPChar fname)
{
  ESint32   i;

  if (fname == NULL) return NULL;
  if (!repo_list) return NULL;
  
  for (i = 0; i < num_repos; i++)
    {
      if (strcmp (base_dir, repo_list[i]->base_dir) == 0)
	if (strcmp (fname, repo_list[i]->fname) == 0)
	  break;
    }
  if (i == num_repos)
    {
      edma_printf_err ("[%s] Repository '%s' not found", __FUNCTION__, fname);
      return NULL;
    }
  return repo_list[i];

}


EPChar     EDMAPROC  
edma_repo_manager_get_repo_dir (ESint32 indx)
{
  if (!repo_list ) return NULL;
  if (indx < 0 || indx >= num_repos) return NULL;

  return repo_list[indx]->base_dir;
}

EPChar     EDMAPROC  
edma_repo_manager_get_repo_name (ESint32 indx)
{
  char     *rn, *aux;

  if (!repo_list ) return NULL;
  if (indx < 0 || indx >= num_repos) return NULL;
  
  rn = strdup (repo_list[indx]->fname);

  if ((aux = strrchr (rn, '.'))) *aux = 0;
  return rn;
}


ESint32    EDMAPROC  
edma_repo_manager_get_repo_type (ESint32 indx)
{
  if (!repo_list ) return -1;
  if (indx < 0 || indx >= num_repos) return -1;

  return repo_list[indx]->type;
}

ESint32  EDMAPROC  
edma_repo_manager_num_repos ()
{
  return num_repos;
}

ESint32   EDMAPROC   
edma_add_std_repo (EPChar repo_name)
{
  char     repo_dir[1024];
 
  /* Load edma_test repository */
  snprintf (repo_dir, 1024, "%s/share/", GVar->SystemPath);
  edma_load_registry (repo_dir, repo_name);
  return 0;
}

ESint32   EDMAPROC   
edma_add_app_repos ()
{
  FILE *f;
  char     repo_dir[1024];
  char     buffer[1024];
  int      n = 0;

  edma_log ("%s", "Trying to load repository list for this application...");
  /* Open File and register contained standard reports */
  if ((f = fopen ("edma_repos.lst", "rt")) == NULL)
    {
      // No private repo list... just return silently
      edma_log ("%s", "No repository list found...\n");
      return 0;
    }
  snprintf (repo_dir, 1024, "%s/share/", GVar->SystemPath);
  while (!feof (f))
    {
      if (!fgets (buffer, 1024, f)) break;
      buffer[strlen(buffer) - 1] = 0;
      if (buffer[0] == ';') continue;
      if (strlen (buffer) == 0) continue;
      edma_log ("** Adding repository: '%s'", buffer);
      edma_load_registry (repo_dir, buffer);
      n++;
    }
  fclose (f);
  edma_log ("%d application repositories loaded\n", n);
  return 0;

}

ESint32  EDMAPROC  
edma_repo_add_system_repo ()
{
  EDMA_REPO     repo;


  edma_log ("%s", "+ Adding GNU/EDMA System Repository...");
  
  /* Loads Share Repository */  

  /* Registe Shared repo*/
  if ((repo = edma_repo_new (EDMA_LOCAL_REPO, NULL, NULL)) == NULL)
    {
      edma_printf_err ("%s", "FATAL: Cannot create shared repository");
      return -1;
    }

  edma_repo_set_persistent (repo, 0);
  edma_repo_manager_add_repo (repo);
  
  
  return 0;
}
