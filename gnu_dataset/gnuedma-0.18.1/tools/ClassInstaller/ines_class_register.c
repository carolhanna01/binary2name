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
** EDMA/GUILE/GTK Application test
** (c) David Martinez Oliviera
** 27/10/1998
** -------------------------------------------------------
** 3/01/1998
** Modification for EDMA 0.4r1 updates
** -------------------------------------------------------
** July, 13th, 2002
** Modification to manage version information
** -------------------------------------------------------------------
** August, 10th, 2003
** Update to deal with update scripts
** --------------------------------------------------------------------
** August, 6th, 2005
** Minor code cleanup.... This little program requires reworking 
 */

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <math.h>
#include <signal.h>
#include <time.h>
#include <unistd.h>

#include <sys/stat.h>
#include <sys/types.h>

#include <edma.h>

#define INES_VERSION "0.2.1"
#define COPYRIGHT "(c) David Martínez Oliveira"

int test_file (EPChar);

typedef EChar my_string[80];

int main(int argc,char *argv[])
{
  int         i,n,type;
  FILE        *f_in;
  my_string   aux;
  my_string   fname,base_name;
  EPChar      field;
  EPChar      value;
  my_string   data[8];
  CLASS_INFO  ci;
  my_string   imp_fn;
  my_string   int_fn;
  EChar       cpath[1024];
  EChar       tpath[1024];
  EPChar      spath;
  EChar       sfn[1100];  
  EChar       cmd[2200];
  EChar       update_script[1100];  
  ESint32     current_parm = 1;
  ESint32     procp = 0;
  EChar       *repo_dir = NULL;
  EChar       *repo_fname = NULL;
  EChar       *ines_file = NULL;
  EDMA_REPO   repo;
  
  printf ("%s", "\nINES (INstall EDMA System) Class Register");
  printf ("\nVersion %s (GNU/EDMA Version: %s)", INES_VERSION, VERSION);
  printf ("\n%s\n", COPYRIGHT);

  if (argc < 2) 
    {
      printf ("%s", "\nWrong parameter number");
      printf ("%s", "\nUsage: ines_installer [--repo repodir repo_name] file.INES");
      printf ("%s", "\n\n");
      exit(1);
    }

  /* Check repository parameter */
  printf ("Processing %d parameters\n", argc);
  while (current_parm < argc)
    {
      printf ("Processing parameter %ld (proc: %ld) : '%s'\n", 
	      current_parm, procp, argv[current_parm]);
      switch (procp)
	{
	case 1:
	  {
	    printf ("Processing Repository dir\n");
	    repo_dir = strdup (argv[current_parm]);
	    current_parm ++;
	    procp = 2;
	    continue;
	  }
	case 2:
	  {
	    printf ("Processing Repository name\n");
	    repo_fname = strdup (argv[current_parm]);
	    current_parm ++;
	    procp = 3;
	    continue;
	  }	  
	case 3:
	  {
	    printf ("Processing File name\n");
	    ines_file = strdup (argv[current_parm]);
	    current_parm ++;
	    procp = 4;
	    continue;	    
	  }
	default:
	  break;
	}
      if (strcmp (argv[current_parm], "--repo") == 0)
	{
	  /* Get next two values */
	  procp = 1;
	  current_parm++;
	  printf ("Found repository...\n");
	}
      else
	{
	  printf ("No repository.... procesing file name\n");
	  procp = 3;
	}
    }
  printf ("DONE!!");
  if (procp == 4)
    {
      printf ("Repository : '%s':'%s'\n", repo_dir, repo_fname);
      printf ("Ines file: '%s'\n", ines_file);
    }
  else
    {
      switch (procp)
	{
	case 1:
	  {
	    printf ("ERROR: Missing repository base directory\n");
	    break;
	  }
	case 2:
	  {
	    printf ("ERROR: Missing repository name\n");
	    break;
	  }
	case 3:
	  {
	    printf ("ERROR: No ines file provided. Nothing to install\n");
	    break;
	  }
	default:
	  printf ("Unknown option: %ld\n", procp);
	}
    }


  strncpy (fname,  ines_file, 80);
  if (strstr (argv[1], ".ines") != 0)
    {
      type = 0;
    }

#if 0
  if ((aux1 = strstr (fname, ".inesep")) != 0)
    {
      type = 1;
      *aux1 = 0;
      strcpy (base_name, fname);
      strcat (fname, ".ines");
      printf ("\nUnpacking INES Extendend Package...[%s:%s]", fname, base_name);
      getcwd (cpath, 1024);
      printf ("%s", "\n  + Saving Current Working Directory...");
      printf ("\n    + %s", cpath);
      printf ("%s", "\n  + Creating Temporaly Working Directory...");
      strcpy (tpath, "/tmp/");
      strcat (tpath, argv[1]);
      printf ("\n    + %s", tpath);
      mkdir (tpath, 0777);
      printf ("%s", "\n  + Copying file");
      sprintf (cmd, "cp %s %s/%s", argv[1], tpath, argv[1]);
      printf ("\n    + %s", cmd);
      system (cmd);
      printf ("%s", "\n  + Changing Working Directory...");
      chdir (tpath);
      printf ("%s", "\n  + Unpacking INES Extended Package...");
      sprintf (cmd, "tar -xzvf %s", argv[1]);
      printf ("\n    + %s", cmd);
      printf ("%s", "\n------------------------------------------\n");
      system (cmd);
      printf ("%s", "-------------------------------------------");
      printf ("%s", "\n + Looking for preinstall shell script");

      /* FIXME: Change to snprintf */
      strcpy (tfn, "./");
      strcat (tfn, base_name);
      strcat (tfn, ".preinst");

      if (test_file (tfn))
	{
	  sprintf (cmd, "sh -c %s", tfn);
	  printf ("\n  + %s exists... executing\n", tfn);

	  system (tfn);
	}
      else
	printf ("%s", "\n   + No pre install script. Continuing...");

      printf ("\nINES Extended Package processed.\n");
    }
#endif

  /* Try to open INES file */
  f_in = fopen (fname, "rt");
  if (f_in == NULL ) 
    {
      printf ("\nCan't open file %s\n", argv[1]);
      exit(1);
    }
  /* Initialize EDMA System */
  EDMAInit ();
  printf ("%s", "\nEDMA System up");
  printf ("%s", "\nINES (INstaller EDMA System) Installer ready");
  printf ("%s", "\n---------------------------------------------------");
  printf ("\n + Processing file %s...", argv[1]);

  n = i = 0;
  memset (&ci, 0, sizeof(ci));

  while (!feof (f_in)) 
    {
      fgets (aux, 80, f_in);
      field = aux;
      aux[strlen (aux) - 1] = 0;
      value = strchr (aux, '=');
      if (value) 
	{
	  *(value) = 0;
	  value++;
	}
      else
	continue;

      if (strcmp (field, "ClassName") == 0)
	{
	  strcpy (data[n], value);
	  ci.ClassName = data[n];
	  n++;
	}
      if (strcmp(field, "NameSpace") == 0)
	{
	  strcpy (data[n], value);
	  ci.NameSpace = data[n];
	  sprintf (int_fn, "%s/%s.idf", value, ci.ClassName);
	  n++;
	}
      
      if (strcmp (field, "Machine") == 0)
	{
	  strcpy (data[n], value);
	  ci.MaqName = data[n];
	  n++;
	}

      if (strcmp (field, "OperatingSystem") == 0)
	{
	  strcpy (data[n], value);
	  ci.SOName = data[n];
	  n++;
	}

      if (strcmp (field, "IDFParser") == 0)
	{
	  strcpy (data[n], value);
	  ci.IDFName = data[n];
	  n++;
	}

      if (strcmp (field, "SIUProxy") == 0)
	{
	  strcpy (data[n], value);
	  ci.SIUName = data[n];
	  n++;
	}

      if (strcmp (field, "IsIDFParser") == 0)
	{
	  if (value[0] == '1') 
	    ci.IsIDFParser = 1;
	  else
	    ci.IsIDFParser = 0;
	}

    if (strcmp (field, "IsSIUProxy") == 0)
      {
	if (value[0] == '1') 
	  ci.IsSIUProxy = 1;
	else
	  ci.IsSIUProxy = 0;
      }

    if (strcmp (field, "IsEMIComp") == 0)
      {
	if (value[0] == '1') 
	  ci.IsEMIComp = 1;
	else
	  ci.IsEMIComp = 0;
      }

    if (strcmp (field, "Implementation") == 0)
      {
	sprintf (imp_fn, "%s", value);
	n++;
      }

    if (strcmp (field, "UpdateScript") == 0)
      {
	sprintf (update_script, "%s", value);
	ci.UpdateScript = update_script;
	n++;
      }

    if (strcmp (field, "MajorVer") == 0)
      ci.MajorVer = atol (value);

    if (strcmp (field, "MinorVer") == 0)
      ci.MinorVer = atol (value);
    i++;
  }
  
  /* Get EDMA System path */
  spath = edma_get_system_path ();

  printf ("\n + %d properties read %d properties default", n, 10 - n);
  printf ("\n   * ClassName           : %s", ci.ClassName);
  printf ("\n   * NameSpace           : %s", ci.NameSpace);
  printf ("\n   * Implementation file : %s", imp_fn);
  printf ("\n   * Interface file      : %s", int_fn);
  printf ("\n   * Machine ID          : %s", ci.MaqName);
  printf ("\n   * Operating System ID : %s", ci.SOName);
  printf ("\n   * IDF Parser          : %s", ci.IDFName);
  printf ("\n   * SIU Proxy           : %s", ci.SIUName);
  printf ("\n   * Is IDF Parser?      : %s", (ci.IsIDFParser ? "Yes" : "No"));
  printf ("\n   * Is SIU Proxy?       : %s", (ci.IsSIUProxy ? "Yes" : "No"));
  printf ("\n   * Is EMI Handler?     : %s", (ci.IsEMIComp ? "Yes" : "No"));
  printf ("\n   * Major Version       : %ld", ci.MajorVer);
  printf ("\n   * Minor Version       : %ld", ci.MinorVer);
  printf ("\n + EDMA System Path : %s", spath);
  fclose(f_in);
  printf ("\n + Looking for class %s in your EDMA system...", ci.ClassName);

  /* NOTE:
   * We suppose user ran ./configure;make; make install sequence
   * so all the things are supposed to be in the right place (.so and .idf file)
   * and we only need to add our class to the EDMA registry.
   *
   * We shoud, almost, test if the required files are really where they are
   * supposed to be. This is not implemented at this moment.
   */
  printf ("\n + Registring class %s\n", ci.ClassName);


  if (repo_dir != NULL && repo_fname != NULL)
    {
      /* Try to get repo object */
      printf ("Repository Specified... trying to load\n");
      if (edma_load_registry (repo_dir, repo_fname) < 0)
	{
	  printf ("Invalid repository. Aborting!\n");
	  EDMAEnd();
	  exit (1);
	}
      /* System repository*/
      repo = edma_repo_manager_get_repo_by_name (repo_dir, repo_fname); 
      printf ("Installing in local repository: 0x%p ('%ld')\n", 
	      repo, edma_repo_get_id(repo));
    }
  else
    {
      repo = edma_repo_manager_get_repo (0); /* System repository*/
      printf ("Installing in System repository: 0x%p ('%ld')\n", 
	      repo, edma_repo_get_id(repo));

    }

  if (edma_add_stock_class2 (repo, ci, int_fn, imp_fn) != -1)
    //if (edma_add_stock_class (ci, int_fn, imp_fn) != -1)
    {
      if (type == 1) 
	{
	  printf ("%s", "\nFinishing INES Extend Package instalation");
	  strcpy (sfn, "./");
	  strcat (sfn, base_name);
	  strcat (sfn, ".postinst");
	  if (test_file (sfn))
	    {
	      printf ("\n + Running Post Install shell script %s\n", sfn);
	      system (sfn);
	    }

	  printf ("%s", "\n + Restoring Working Directory...");
	  chdir (cpath);
	  printf ("%s", "\n + Deleting Temporal Files...");
	  sprintf (cmd, "rm -R %s", tpath);
	  printf ("\n   + %s", cmd);
	  system (cmd);
	}
      printf ("%s", "\n + All done!");
      printf ("%s", "\n--------------------------------------------------------------");
      printf ("\nClass %s has been registred in system", ci.ClassName);
      printf ("%s", "\nEDMA Registry will be update when all EDMA apps shut down\n");
      printf ("%s", "\nOperation complete");
      printf ("%s", "\n--------------------------------------------------------------\n");
    } 
  else
    {
      printf ("\n + Error class %s not registred!", ci.ClassName);
      printf ("%s", "\n--------------------------------------------------------------\n");
    }

  free (spath);
  /* Shutdown EDMA */
  EDMAEnd();
  return 0;
}

/* Helper function */
/* FIXME: Change to use FILESYSTEM Class*/
int test_file (EPChar fn)
{
  FILE    *f;

  if ((f = fopen (fn, "r")) == NULL)
    return 0;
  
  fclose(f);
  return 1;
    
}
