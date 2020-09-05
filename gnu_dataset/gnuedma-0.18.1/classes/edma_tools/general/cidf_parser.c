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
#include <string.h>
#include <stdlib.h>

#include <ctype.h>


#include "cidf_parser.h"

/* Global Vars */
static char *keyword[] = { "@version", 
			   "@namespace", 
			   "@accessors", 
			   "class", 
			   NULL
};


static int kw_version (CIF_CLASS*,int, char *);
static int kw_namespace (CIF_CLASS*,int, char *);
static int kw_accessors (CIF_CLASS*,int, char *);
static int kw_class  (CIF_CLASS*,int, char *);


static PROCESS_KW_FUNC  kw_func[] = {
  kw_version, kw_namespace, kw_accessors, kw_class, NULL
};

static char *method_flags[] = {"abstract",
			       "virtual",
			       "static",
			       NULL
};

/* Simple parser for new GNU/EDMA Interface Format */

/* Local vars. To be included in an OBJECT */
static   int   class = 0; /* Processing a class */

/** Memory Management Helper Functions **/
void *
create_object (int size)
{
  void   *temp;

  if ((temp = malloc (size)) == NULL)
    {
      fprintf (stderr, "Cannot allocate memory for Object\n");
      return NULL;
    }
  memset (temp, 0, size);
  return temp;
}

int
add_item (void ***list, int *n, void *data)
{

  /* Realloc buffer */
  if ((*list = realloc (*list, (*n + 1) * sizeof(void*))) == NULL)
    {
      fprintf (stderr, "Cannot allocate memory\n");
      return -1;
    }
  /* Store Information */
  (*list)[(*n)] = data;
  (*n)++;

  return 0;
}

void xfree (void *p)
{
  if (p) free (p);
}

/* Other helper */
int
cidf_free_data (CIF_CLASS *the_class)
{
  int i, j;

  xfree (the_class->name);
  xfree (the_class->namespace);

  for (i = 0; i < the_class->n_sc; i++)
    {
      xfree (the_class->sc[i]->name);
      xfree (the_class->sc[i]->ap);      
      xfree (the_class->sc[i]);
    }

  for (i = 0; i < the_class->n_prop; i++)
    {
      xfree (the_class->prop[i]->name);
      xfree (the_class->prop[i]->type);      
      xfree (the_class->prop[i]->access);      
      xfree (the_class->prop[i]);
    }

  for (i = 0; i < the_class->n_met; i++)
    {
      xfree (the_class->met[i]->name);
      xfree (the_class->met[i]->rtype);
      for (j = 0; j < the_class->met[i]->n_param; j++)
	{
	  xfree (the_class->met[i]->param[j]->name);
	  xfree (the_class->met[i]->param[j]->type);
	  xfree (the_class->met[i]->param[j]);
	}
      xfree (the_class->met[i]->param);
      xfree (the_class->met[i]);
    }

  xfree (the_class->sc);
  xfree (the_class->prop);
  xfree (the_class->met);

  class = 0;

  return 0;
}

int
cidf_dump_class (CIF_CLASS *the_class)
{
  int    i, j;

  printf ("--------------------------------------------\n");
  printf ("Class         : '%s'\n", the_class->name);
  printf ("NameSpace     : '%s'\n", the_class->namespace);
  printf ("Version       : %d.%d\n", the_class->major_version, 
	  the_class->minor_version);
  printf ("Super Classes : %d\n", the_class->n_sc);
  printf ("Properties    : %d\n", the_class->n_prop);
  printf ("Methods       : %d\n", the_class->n_met);
  printf ("...........................................................\n");
  for (i = 0; i < the_class->n_sc; i++)
    printf ("+ SuperClass %d: '%s' AT '%s' | '%s'\n", i, 
	    the_class->sc[i]->name, 
	    the_class->sc[i]->ap, the_class->sc[i]->ap1);
  printf ("...........................................................\n");
  for (i = 0; i < the_class->n_prop; i++)
    printf ("+ Property %d: '%s' '%s' : '%s'\n", i, 
	    the_class->prop[i]->type, the_class->prop[i]->name, 
	    the_class->prop[i]->access);
  printf ("...........................................................\n");
  for (i = 0; i < the_class->n_met; i++)
    {
      printf ("+ Method %d: ", i);
      for (j = 0; j < 3; j++)
	if (the_class->met[i]->flags[j]) printf (" %s ", method_flags[j]);
      printf ("'%s' '%s' (", the_class->met[i]->rtype, the_class->met[i]->name);
      
      for (j = 0; j < the_class->met[i]->n_param; j++)
	printf ("%s %s |", the_class->met[i]->param[j]->type, 
		the_class->met[i]->param[j]->name);
      printf (")\n");
    }
  printf ("--------------------------------------------\n");
  return 0;
}

/********************************************************************/

static int
check_keyword (char *str)
{
  char   **list = keyword;
  int    i = 0;

  while (list[i] != NULL)
    {
      if (strncasecmp (str, list[i], strlen(list[i])) == 0) break;
      else i++;
    }

  return i;
}

static int
check_str (char **list, char *str)
{
  int    i = 0;

  while (list[i] != NULL)
    {
      if (strncasecmp (str, list[i], strlen(str)) == 0) break;
      else i++;
    }

  return i;
}


static int
skip_spaces (char *str)
{
  char *aux = str;

  while (*aux != 0 && isspace(*aux)) aux++;

  return aux-str;
}

static int
trim_str (char *str)
{
  char   *aux = str;

  aux += strlen(str) - 1;
  while (isspace(*aux) && aux >= str) aux--;

  *(aux + 1) = 0;

  return skip_spaces (str); 
}

static int
get_id (char *str)
{
  char  *aux = str;

  while (!isspace(*aux) && * aux != '(' && *aux != 0) aux++;

  return aux - str;
}

static int
get_id2 (char *str, char *sep)
{
  char  *aux = str;

  while (!isspace(*aux) && !(strchr (sep, *aux)) && *aux != 0) aux++;

  return aux - str;
}


static int
parse_class_header (CIF_CLASS *the_class, int line, char *pline)
{
  char    *temp, *aux, *aux1, *aux2, finish, *aux3;
  CIF_SUPERCLASS *sc;

  temp = strdup (pline);
  aux = temp;
  aux += 5; /* Skip 'CLASS' */
  /* Get class name */
  aux += skip_spaces (aux);
  aux1 = aux;
  aux1 += get_id (aux);
  *aux1 = 0;
  aux1++;

  the_class->name = strdup (aux);
  /* Parse Super classes */
  aux1 = strchr (aux1, ':');
  if (aux1 == NULL) return 0; /* No inheritance information */
  aux1++;
  aux1 += skip_spaces (aux1);
  aux = aux1;
  finish = 0;
  do
    {
      aux1 += get_id (aux);
      if (*aux1 == 0)
	finish = 1;

      sc = create_object (sizeof(CIF_SUPERCLASS));
      
      *aux1 = 0;
      aux1++;
      /* Parse Superclass info */
      if ((aux2 = strchr (aux, '@')) != NULL)
	{
	  *aux2 = 0;
	  aux2++;
	  sc->name = strdup (aux);
	  /* parse complete AP*/
	  if ((aux3 = strchr (aux2, '|')) == NULL)
	    {
	      sc->ap = strdup (aux2);
	      sc->ap1 = strdup ("INNER");
	    }
	  else
	    {
	      *aux3 = 0;
	      sc->ap = strdup (aux2);
	      sc->ap1 = strdup (aux3 + 1);
	    }
	}
      else
	{
	  sc->name = strdup (aux);
	  sc->ap = strdup (sc->name);
	  sc->ap1 = strdup (the_class->name);
	}
      add_item ((void ***)&the_class->sc, &the_class->n_sc, sc);
      aux = aux1;
    } while (!finish);
  
  free (temp);
  return 0;
}


static int
parse_prop (CIF_CLASS *the_class,int line, char *pline)
{
  char          *name, *flags, *aux, *temp;
  CIF_PROPERTY  *prop;

  temp = strdup (pline);
  aux = temp;
  /* get type identifier */
  aux += get_id (temp);
  if (*aux == 0)
    {
      fprintf (stderr, "[%s] Syntax Error in line %d: '%s'\n", 
	       __FUNCTION__, line, pline);
      return -1;
    }

  prop = create_object (sizeof(CIF_PROPERTY));
  *aux = 0;
  aux++;

  prop->type = strdup (temp);
  /* Verify if we got a valid type */
  /* get prop name */
  aux += skip_spaces (aux);
  name = aux;

  aux += get_id2 (name, ":;");
  if (*aux == 0)
    {
      if (aux == name)
	{
	  fprintf (stderr, "Property Name not found in line %d\n", line);
	  return -1;
	}
    }
  else
    {
      *aux = 0;
      aux++;
    }

  prop->name = strdup (name);
  /* FIXME: Process array*/
  /* get flags*/
  if ((aux = strchr (aux, ':')) == NULL)
    {
      /* No flags. Setting default */
      prop->access = strdup ("RW");
    }
  else
    {
      flags = aux + 1;
      flags += skip_spaces (aux);
      if ((aux = strchr (flags, ';')) == NULL)
	{
	  fprintf (stderr, "[%s] Syntax error in line %d. Missing ';': '%s'\n", 
		   __FUNCTION__, line, pline);
	  free (temp);

	  return -1;
	}
      *aux = 0;
      prop->access = strdup (flags);
    }
  add_item ((void ***)&the_class->prop, &the_class->n_prop, prop);
  free (temp);

  return 0;
}

static int
parse_met (CIF_CLASS *the_class, int line, char *pline, int params)
{
  char          *aux, *aux1, *aux2, *temp, *aux3;
  int           pn,indx, fp, process = 1;
  CIF_METHOD    *met;
  CIF_PARAM     *param;

  temp = strdup (pline);
  aux = temp;
  /* get return type */
  aux += get_id (temp);
  if (*aux == 0)
    {
      fprintf (stderr, "[%s] Syntax Error in line %d: '%s'\n", 
	       __FUNCTION__, line, pline);
      return -1;
    }
  *aux = 0;
  aux++;

  met = create_object (sizeof(CIF_METHOD));
  met->n_param = 0;
  met->rtype = strdup (temp);
  aux += skip_spaces (aux);
  aux1 = aux;
  /* Get method flags */

  while (process)
    {
      aux += get_id (aux1);
      *aux = 0;
      aux++;
      if (method_flags[(indx = check_str (method_flags, aux1))] == NULL)
	{
	  break;
	  /*
	  process = 0;
	  continue;
	  */
	}

      aux += skip_spaces (aux);
      aux1 = aux;
      met->flags[indx] = 1;
    }
  printf ("\n");
  /* get method name*/
  /* Last string from flags processing is method name */
  met->name = strdup (aux1);
  /* Process parameter*/

  process = 1;
  aux2 = aux1 = aux = pline + params + 1; /* Skip parenthesis */

  aux2 = strchr (aux, ')');
  if (aux2 == NULL)
    {
      fprintf (stderr, "Missing Parenthesys in line %d\n", line);
      return -1;
    }
  else
    *aux2 = 0;

  aux += skip_spaces(aux);
  if  (*aux == 0) /* No parameters*/
    {
      add_item ((void ***)&the_class->met, &the_class->n_met, met);
      return 0;
    }

  pn = 1;
  fp = 0;
  while (process)
    {
      if ((aux3 = strchr (aux, ',')) != NULL)
	*aux3 = 0;
      else
	fp = 1;
      aux2 = aux3;
      aux1 += get_id (aux);
      if (*aux1 == 0 && aux1 != aux2)
	{
	  fprintf (stderr, "Missing name for parameter %d\n", pn);
	  return -1;
	}
      else
	{
	  *aux1 = 0;
	  aux1++;
	}

      param = create_object (sizeof(CIF_PARAM));
      param->type = strdup (aux);

      aux1 += skip_spaces (aux1);

      aux2 += get_id (aux1);
      aux2++;

      param->name = strdup (aux1);
      add_item ((void ***)&met->param, &met->n_param, param);
      if (!fp) 
	{
	  aux = aux3 + 1;
	  aux += skip_spaces (aux);
	}
      else
	break;

      aux1 = aux;
      pn ++;
    }

  add_item ((void ***)&the_class->met, &the_class->n_met, met);

  return 0;
}

static int
parse_class_line (CIF_CLASS *the_class, int line, char *pline)
{
  char       *params;
  /* Check the type of line */
  if (pline[0] == '{' || pline[0] == '}')
    {
      /* FIXME: Final version must check this*/
      return 0;
    }
  if ((params = strchr (pline, '(')) == NULL)
    return parse_prop (the_class, line, pline);
  else
    return parse_met (the_class, line, pline, params - pline);
}


static int
kw_version (CIF_CLASS *the_class, int line, char *pline)
{
  char    *aux = pline;
  if ((aux = strchr (aux, ':')) == NULL)
    {
      fprintf (stderr, "Syntax error in line %d. No namespace specified\n", 
	       line);
      return -1;
    }
  aux++;
  aux += skip_spaces (aux);
  sscanf (aux, "%d.%d", &the_class->major_version, &the_class->minor_version);

  return 0;
}

static int
kw_namespace (CIF_CLASS *the_class, int line, char *pline)
{
  char    *aux = pline;

  if ((aux = strchr (aux, ':')) == NULL)
    {
      fprintf (stderr, "Syntax error in line %d. No namespace specified\n", 
	       line);
      return -1;
    }
  aux++;
  aux += skip_spaces (aux);
  the_class->namespace = strdup (aux);

  return 0;
}

static int
kw_accessors (CIF_CLASS *the_class, int line, char *pline)
{
  return 0;
}

static int
kw_class (CIF_CLASS *the_class, int line, char *pline)
{
  printf ("[%s] Unreacheable code\n", __FUNCTION__);

  return 0;
}

CIF_CLASS*
cidf_parse (char *filename)
{
  int            line, kw;
  FILE           *f;
  char           buffer[1024], *aux, *pline;
  CIF_CLASS      *the_class;

  if ((f = fopen (filename, "rt")) == NULL)
    {
      fprintf (stderr, "Cannot open file '%s'\n", filename);
      return NULL;
    }

  /* Create class object */
  the_class = create_object (sizeof(CIF_CLASS));
  the_class->n_sc = the_class->n_prop = the_class->n_met = 0;
  line = 0;
  while (!feof(f))
    {
      line ++;
      memset (buffer, 0, 1024);
      fgets (buffer, 1024, f);
      /* Chomp. 
       * FIXME: Support of DOS files */
      aux = strchr (buffer, '\n');
      if (aux != NULL)
	*aux = 0;
      /* Remove comments */
      if ((aux = strstr (buffer, "//")) != NULL)
	*aux = 0;

      /* Trim line */
      pline = buffer;
      pline += trim_str (pline);
      /* Skip empty lines */
      if (strlen(pline) == 0) continue;
      /* Process directives */

      kw = check_keyword (pline);

      if (keyword[kw] != NULL && kw != 3) 
	{
	  kw_func[kw] (the_class, line, pline);
	  continue;
	}

      if (keyword[kw] == NULL && class == 0)
	{
	  printf ("ERROR: Syntax error in line %d: '%s'\n", line, pline);
	  cidf_free_data (the_class);		
	  class = 0;
	  return NULL;
	}
      else
	{
	  if (class)
	    if (parse_class_line (the_class, line, pline) < 0)
	      {
		/* Free Memory*/
		cidf_free_data (the_class);
		class = 0;
		return NULL;
	      }
	}

      if (kw == 3)
	{
	  if (class == 0)
	    {
	      class = 1;
	      if ((parse_class_header (the_class, line, pline)) < 0)
		{
		  cidf_free_data (the_class);
		  return NULL;
		}
	    }
	  else
	    {
	      printf ("ERROR: Cannot declare inner classes in line %d\n", line);
	      cidf_free_data (the_class);
	      return NULL;
	    }
	}

      /*********************************************************/
    }

  fclose (f);
  class = 0;

  return the_class;
}

