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
#include <edma.h>

typedef struct queue_data_t
{
  EDMAT_BUFFER   data;
  ESint32        capacity;
  ESint32        last;
  ESint32        first;
} QUEUE_DATA;

ESint32 EDMAPROC
q_init (OBJID id, ESint32 n)
{
  QUEUE_DATA   *m;

  m = (QUEUE_DATA*) edma_get_data_ref (id);

  edma_buffer_alloc (&m->data, sizeof(ESint32) * n);
  m->capacity = n;
  m->first = m->last = 0;

  /* Set state */
  //edma_parse_classpath (id, "QUEUE_EMPTY@STATE|SUPER<", strlen ("QUEUE_EMPTY@STATE|SUPER<"));
  edma_apply_classpath (id, "QUEUE_EMPTY@STATE|SUPER<");
  
  return 0;
}

ESint32 EDMAPROC
q_insert (OBJID id, ESint32 val)
{
  QUEUE_DATA  *m;
  ESint32     *p;
  
  m = (QUEUE_DATA *) edma_get_data_ref (id);
  p = (ESint32 *) m->data.dat;

  p[m->last] = val;
  m->last++;
  m->last %= m->capacity;

  return 0;
}

ESint32 EDMAPROC
q_get_first (OBJID id)
{
  QUEUE_DATA  *m;
  ESint32     *p;
  
  m = (QUEUE_DATA *) edma_get_data_ref (id);
  p = (ESint32 *) m->data.dat;

  return p[m->first];
}

ESint32 EDMAPROC
q_remove_first (OBJID id)
{
  QUEUE_DATA  *m;
  ESint32     *p;
  
  m = (QUEUE_DATA *) edma_get_data_ref (id);
  p = (ESint32 *) m->data.dat;

  m->first++;
  m->first %= m->capacity;

  edma_printf_obj (id, "First: %d", m->first);

  return 0;
}

ESint32 EDMAPROC
q_dump (OBJID id)
{
  QUEUE_DATA *m;
  ESint32    i, *p;

  m = (QUEUE_DATA *) edma_get_data_ref (id);
  p = (ESint32 *) m->data.dat;

  edma_printf_obj (id, "Dumping QUEUE --> using %d of %d ", m->last - m->first, m->capacity);
  i = m->first;
  do  edma_printf ("%d:%d ", i , p[i]); 
  while ((++i) % m->capacity != m->last);

  return 0;
}

ESint32 EDMAPROC
q_empty_insert (OBJID id, ESint32 val)
{
  edma_printf_obj (id, "%s", "Inserting");
  if (!edma_met3 (id, "SUPER>insert", val))
    {
      //edma_parse_classpath (id, "SUPER>QUEUE_NORMAL@!STATE|SUPER<", strlen ("SUPER>QUEUE_NORMAL@!STATE|SUPER<"));
      edma_apply_classpath (id, "SUPER>QUEUE_NORMAL@!STATE|SUPER<");
    }
  else
    return -1;
}

ESint32 EDMAPROC
q_empty_get_first (OBJID id)
{
  edma_printf_obj (id, "%s", "Queue is empty. Can't get first item");
  return 0;
}


ESint32 EDMAPROC
q_empty_remove_first (OBJID id)
{
  edma_printf_obj (id, "%s", "Queue is empty. Can't remove items");
  return 0;
}

ESint32 EDMAPROC
q_normal_insert (OBJID id, ESint32 val)
{
  ESint32   last, first;
  
  edma_printf_obj (id, "%s", "Inserting");

  edma_met3 (id, "SUPER>insert", val);
  edma_rprop3 (id, "first", &first);
  edma_rprop3 (id, "last", &last);

  if (last == first)
    {
      edma_printf_obj (id, "%s", "Queue gets Full!!!!");
      //edma_parse_classpath (id, "SUPER>QUEUE_FULL@!STATE|SUPER<", strlen ("SUPER>QUEUE_FULL@!STATE|SUPER<"));    
      edma_apply_classpath (id, "SUPER>QUEUE_FULL@!STATE|SUPER<");    
    }

  return 0;
}

ESint32 EDMAPROC
q_normal_get_first (OBJID id)
{
  edma_printf_obj (id, "%s", "Getting First");
  return edma_met3 (id, "SUPER>getfirst");
}


ESint32 EDMAPROC
q_normal_remove_first (OBJID id)
{
  ESint32   last, first;

  edma_printf_obj (id, "%s", "Removing");
  edma_met3 (id, "SUPER>removefirst");
  edma_rprop3 (id, "first", &first);
  edma_rprop3 (id, "last", &last);

  if (last == first)
    {
      edma_printf_obj (id, "%s", "Queue gets Empty!!!!");
      //edma_parse_classpath (id, "SUPER>QUEUE_EMPTY@!STATE|SUPER<", strlen ("SUPER>QUEUE_EMPTY@!STATE|SUPER<"));    
      edma_apply_classpath (id, "SUPER>QUEUE_EMPTY@!STATE|SUPER<");    
    }
  return 0;
}

ESint32 EDMAPROC
q_full_insert (OBJID id, ESint32 val)
{
  edma_printf_obj (id, "%s", "Inserting");
  edma_printf_obj (id, "%s", "Queue is full. Can't insert more items");
  return 0;
}

ESint32 EDMAPROC
q_full_get_first (OBJID id)
{
  edma_printf_obj (id, "%s", "Getting First");
  return edma_met3 (id, "getfirst");
}


ESint32 EDMAPROC
q_full_remove_first (OBJID id)
{
  ESint32   last, first;

  edma_printf_obj (id, "%s", "Removing");
  edma_met3 (id, "SUPER>removefirst");

  //edma_parse_classpath (id, "SUPER>QUEUE_NORMAL@!STATE|SUPER<", strlen ("SUPER>QUEUE_NORMAL@!STATE|SUPER<"));    
  edma_apply_classpath (id, "SUPER>QUEUE_NORMAL@!STATE|SUPER<");    


  return 0;
}

ESint32 EDMAPROC
q_empty_dump (OBJID id)
{
  edma_printf_obj (id, "%s", "QUEUE is empty. Nothing to dump");
}

#define DEBUG 0

int main ()
{
  OBJID    id, id1;
  CLASSID  cid;

  EDMAInit ();
  
  edma_printf ("%s", "*** Simple State Pattern example ***");
  /* Create Decorator class dynamically */
  edma_printf ("%s", "+ Dynamic definition of 'QUEUE' Classes");

  cid = edma_idf_get_free_class_id (EDMA_LOCAL_CLASS);

  edma_idf_set_class_name (cid, "SQUEUE");
  edma_idf_set_class_namespace (cid, "LOCAL");
  edma_idf_set_class_version (cid, 0, 0);

  edma_add_local_class_property (cid, "data", DT_EBUFFER, E_L, 0);
  edma_add_local_class_property (cid, "capacity", DT_ESINT32, E_L, 0);
  edma_add_local_class_property (cid, "last", DT_ESINT32, E_L, 0);
  edma_add_local_class_property (cid, "first", DT_ESINT32, E_L, 0);

  edma_add_local_class_method (cid, "init", "", (PPROC) q_init, 0, 0, 0);
  edma_add_local_class_method (cid, "insert", "", (PPROC) q_insert, 0, 0, 0);
  edma_add_local_class_method (cid, "getfirst", "", (PPROC) q_get_first, 0, 0, 0);
  edma_add_local_class_method (cid, "removefirst", "", (PPROC) q_remove_first, 0, 0, 0);
  edma_add_local_class_method (cid, "dump", "", (PPROC) q_dump, 0, 0, 0);

  edma_idf_set_class_id (cid);

  cid = edma_idf_get_free_class_id (EDMA_LOCAL_CLASS);

  edma_idf_set_class_name (cid, "QUEUE_EMPTY");
  edma_idf_set_class_namespace (cid, "LOCAL");
  edma_idf_set_class_version (cid, 0, 0);

  edma_add_local_class_method (cid, "insert", "", (PPROC) q_empty_insert, 0, 0, 0);
  edma_add_local_class_method (cid, "getfirst", "", (PPROC) q_empty_get_first, 0, 0, 0);
  edma_add_local_class_method (cid, "removefirst", "", (PPROC) q_empty_remove_first, 0, 0, 0);
  edma_add_local_class_method (cid, "dump", "", (PPROC) q_empty_dump, 0, 0, 0);

  edma_add_local_class_superclass_by_name (cid, "SQUEUE", "SUPER", "STATE");

  edma_idf_set_class_id (cid);

  cid = edma_idf_get_free_class_id (EDMA_LOCAL_CLASS);

  edma_idf_set_class_name (cid, "QUEUE_NORMAL");
  edma_idf_set_class_namespace (cid, "LOCAL");
  edma_idf_set_class_version (cid, 0, 0);

  edma_add_local_class_method (cid, "insert", "", (PPROC) q_normal_insert, 0, 0, 0);
  edma_add_local_class_method (cid, "getfirst", "", (PPROC) q_normal_get_first, 0, 0, 0);
  edma_add_local_class_method (cid, "removefirst", "", (PPROC) q_normal_remove_first, 0, 0, 0);

  edma_add_local_class_superclass_by_name (cid, "SQUEUE", "SUPER", "STATE");

  edma_idf_set_class_id (cid);

  cid = edma_idf_get_free_class_id (EDMA_LOCAL_CLASS);

  edma_idf_set_class_name (cid, "QUEUE_FULL");
  edma_idf_set_class_namespace (cid, "LOCAL");
  edma_idf_set_class_version (cid, 0, 0);

  edma_add_local_class_method (cid, "insert", "", (PPROC) q_full_insert, 0, 0, 0);
  edma_add_local_class_method (cid, "getfirst", "", (PPROC) q_full_get_first, 0, 0, 0);
  edma_add_local_class_method (cid, "removefirst", "", (PPROC) q_full_remove_first, 0, 0, 0);


  edma_add_local_class_superclass_by_name (cid, "SQUEUE", "SUPER", "STATE");

  edma_idf_set_class_id (cid);

  /*--------------------------------------------------------------------------*/
  id = edma_new_obj ("SQUEUE");



  /* Create QUEUE and set initial STATE to NORMAL */
  edma_met3 (id, "init", 4);
  edma_printf ("-- On creation state is set to EMPTY------------");
  edma_obj_report (id);


  edma_met3 (id, "removefirst");
  edma_met3 (id, "insert", 10);
  edma_printf ("\n\n-- After first insert state is set to NORMAL--------");
  edma_obj_report (id);
  edma_met3 (id, "insert", 20);
  edma_met3 (id, "insert", 30);

  edma_met3 (id, "insert", 40);
  edma_printf ("\n\n-- At this point QUEUE is full (4) items--------");
  edma_obj_report (id);

  edma_met3 (id, "insert", 50);
  edma_met3 (id, "insert", 60);


  edma_met3 (id, "dump");
  edma_met3 (id, "removefirst");
  edma_printf ("\n\n-- After first insert state is set to NORMAL--------");
  edma_obj_report (id);
  edma_met3 (id, "removefirst");
  edma_met3 (id, "removefirst");
  edma_met3 (id, "removefirst");
  edma_printf ("\n\n-- After first insert state is set to EMPTY--------");
  edma_obj_report (id);
  edma_met3 (id, "removefirst");
  edma_met3 (id, "removefirst");
  edma_met3 (id, "dump");

#if DEBUG
  edma_obj_report (id);
  edma_obj_report (edma_downcast_obj (id, "STATE"));
#endif

  edma_free_obj (id);

  edma_printf ("\n");
  EDMAEnd();
}
