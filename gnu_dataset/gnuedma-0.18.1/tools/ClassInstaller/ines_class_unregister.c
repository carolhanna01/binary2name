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

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <math.h>
#include <signal.h>
#include <time.h>
#include <unistd.h>

#include <edma.h>

/* Constants */
#define THIS_VERSION "0.2.0"
#define COPYRIGHT "This is free software (LGPLv3)"

typedef EChar my_string[80];

int main(int argc,char *argv[])
{
  int       i,n,id,n_depend;
  int       v1, v2;
  my_string aux;
  my_string aux1;
  my_string module_name;
  EChar     *idf_name;
  EChar     *impl_name;
  EChar     class_name[1024];
  EPChar    the_path;

  printf ("%s", "\nINES (INstall EDMA System) Class Unregister");
  printf ("\nVersion %s", THIS_VERSION);
  printf ("\n%s\n", COPYRIGHT);

  //if (argc != 2 && argc != 4) 
  if (argc < 2)
    {
      printf ("%s", "\nWrong parameter number");
      printf ("%s", "\nUsage: ines_uninstaller [-version] version "
	      "EDMA_CLASS_NAME\n\n");
      exit(1);
    }
  
  /* Initialize EDMA System */
  EDMAInit();

  if (strcasecmp (argv[1], "--version") == 0)
    {
      /* Version data available... parse it */
      sscanf (argv[2], "%d.%d", &v1, &v2);
      strcpy (class_name, argv[3]);
    }
  else
    {
      v1 = v2 = -1;
      strcpy (class_name, argv[1]);
    }

  printf ("--------------------------------------\n");
  printf ("Unregistering class '%s'\n", class_name);

  if (v1 != -1)
    {
      printf ("+ Looking for class version %d.%d...", v1, v2);
  
      id = edma_get_class_id_with_version (class_name, v1, v2);
    }
  else
    {
      printf ("+ Looking for latest class version...");
  
      id = edma_get_class_id (class_name);
      /* Recover version information */
      v1 = edma_get_class_major_version (id);
      v2 = edma_get_class_minor_version (id);
    }

  if (id == -1) 
    {
      printf ("FAIL!\n");
      printf ("%s", "- Can't locate class. Aborting\n");
      EDMAEnd ();
      exit (1);
    }
  printf ("FOUND\n");

  printf ("  > Located class       : '%s' v%d.%d with Id:%d\n", 
	  class_name, v1, v2, id);

  i = edma_get_class_module (id, module_name);
  if (i == -1) 
    {
      printf ("%s", "- Cannot find class Module Name. Aborting\n");
      EDMAEnd ();
      exit (1);
    }

  /* Get IDF file location */
  the_path = edma_get_system_path ();

  idf_name = edma_get_idf_file_path (id);
  impl_name = edma_get_impl_file_path (id);
  printf ("  > Interface file      : %s\n", idf_name);
  printf ("  > Implementation file : %s\n", impl_name);

  n = edma_get_num_reg_classes();
  printf ("+ Looking for Module Dependences : (%s)\n", module_name);
  printf ("  > Looking in %d classes\n", n);
  n_depend = 0;
  for (i = 0; i < n; i++) 
    {
      if (id == i) continue;
      edma_get_class_module (i, aux);
      edma_get_class_name (i, aux1);
      if (strcmp (aux, module_name) == 0) 
	{
	  printf ("    * Class %s depends on module %s\n", aux1, module_name);
	  n_depend++;
	}
    }

  if (edma_get_class_repo_type (id) == EDMA_LOCAL_REPO)
    {
      printf ("  > Class is in a local repository.\n");
      printf ("    Other applications using the class will not notice the change\n");
      printf ("    until restarted. Related files will not be deleted\n");

      n_depend = 2; /* Force not to delete */
    }

  
  if (n_depend <= 1) 
    { /* Only one class in the module */
      printf ("%s", "+ No dependences found... "
	      "Uninstall implementation file is safe\n");
    }
  else
    {
      printf ("+ %d dependencies found... "
	      "Uninstall implementation file is NOT SAFE\n", n_depend);
    }

  printf ("+ Testing if related files exist...\n");
  printf ("%s", "  > Testing Interface file existence ........");
  if ((ESint32) edma_smet3 ("FILESYSTEM", "TestFile", idf_name) != 1) 
    {
      printf ("FAIL!\n");
      printf ("%s", "     - Can't find IDF file... \n");
    }
  printf ("%s", " OK\n");
  
  printf ("%s", "  > Testing Implementation file existence ...");
  if ((ESint32) edma_smet3 ("FILESYSTEM", "TestFile", impl_name) != 1) 
    {
      printf ("FAIL!\n");
      printf ("%s", "     - Can't find Implementation file... \n");
    }
  printf ("%s", " OK\n");
  
  
  printf ("%s", "+ Freeing Class from EDMA register ..........");
  
  i = 0;
  i = edma_del_stock_class_id (id);
  if (i == -1) 
    {
      printf ("FAIL!\n");
      printf ("\n   - Error deleting class");
      EDMAEnd ();
      exit (1);
    }
  
  printf (" OK\n");
  
  printf ("\n+ Class %s Successfully unregistered\n\n", class_name);
  printf ("****************************************************\n");
  printf ("*** To completely wipe out the class from your system:\n");
  printf ("*** Delete IDF file : %s\n", idf_name);
  printf ("*** Shared lib: %s\n", impl_name);
  printf ("****************************************************\n");
    
  
  free (the_path);
  EDMAEnd ();

  return 0;
}











