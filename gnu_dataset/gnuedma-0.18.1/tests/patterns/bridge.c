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
#include <edma.h>

typedef struct circle_t
{
  EReal64    x, y, radius;
} CIRCLE_DAT;


ESint32 EDMAPROC
circle_set (OBJID id, EReal64 x, EReal64 y, EReal64 size)
{
  CIRCLE_DAT  *m = edma_get_data_ref (id);

  m->x = x;
  m->y = y;
  m->radius = size;

  return 0;
}


ESint32 EDMAPROC
circle_resize (OBJID id, EReal64 size)
{
  CIRCLE_DAT  *m = edma_get_data_ref (id);

  m->radius *= size;
  return 0;
}

ESint32 EDMAPROC
circle_draw (OBJID id, EReal64 x, EReal64 y, EReal64 radius)
{
  edma_printf_obj (id, "Drawing Circle at: %lf, %lf", x, y);
  return 0;
}


ESint32 EDMAPROC
api1_draw (OBJID id)
{
  EReal64 x, y, radius;

  edma_rprop3 (id, "x", &x);
  edma_rprop3 (id, "y", &y);
  edma_rprop3 (id, "radius", &radius);

  edma_printf_obj (id, "Drawing Circle at (%f, %f) radius %f)", 
		   x, y, radius);
  return 0;
}


ESint32 EDMAPROC
api2_draw (OBJID id)
{
  EReal64 x, y, radius;

  edma_rprop3 (id, "x", &x);
  edma_rprop3 (id, "y", &y);
  edma_rprop3 (id, "radius", &radius);

  edma_printf_obj (id, "Drawing Circle at (%f, %f) radius %f)", 
		   x, y, radius);
  return 0;
}

int
define_classes ()
{
  CLASSID   cid;

  cid = edma_idf_get_free_class_id (EDMA_LOCAL_CLASS);

  edma_idf_set_class_name (cid, "CIRCLE");
  edma_idf_set_class_namespace (cid, "LOCAL");
  edma_idf_set_class_version (cid, 0, 0);

  edma_add_local_class_property (cid, "x", DT_EREAL64, E_L, 0);
  edma_add_local_class_property (cid, "y", DT_EREAL64, E_L, 0);
  edma_add_local_class_property (cid, "radius", DT_EREAL64, E_L, 0);
  edma_add_local_class_method (cid, "set", "", 
			       (PPROC) circle_set, 0, 0, 0);
  edma_add_local_class_method (cid, "resize", "", 
			       (PPROC) circle_resize, 0, 0, 0);
  edma_add_local_class_method (cid, "draw", "", 
			       (PPROC) circle_draw, 0, 0, 0);

  edma_idf_set_class_id (cid);


  cid = edma_idf_get_free_class_id (EDMA_LOCAL_CLASS);

  edma_idf_set_class_name (cid, "API1");
  edma_idf_set_class_namespace (cid, "LOCAL");
  edma_idf_set_class_version (cid, 0, 0);
  edma_add_local_class_method (cid, "draw", "", (PPROC) api1_draw, 0, 0, 0);
  edma_add_local_class_superclass_by_name (cid, "CIRCLE", "SUPER", "IMP");
  edma_idf_set_class_id (cid);


  cid = edma_idf_get_free_class_id (EDMA_LOCAL_CLASS);

  edma_idf_set_class_name (cid, "API2");
  edma_idf_set_class_namespace (cid, "LOCAL");
  edma_idf_set_class_version (cid, 0, 0);

  edma_add_local_class_method (cid, "draw", "", (PPROC) api2_draw, 0, 0, 0);

  edma_add_local_class_superclass_by_name (cid, "CIRCLE", "SUPER", "IMP");

  edma_idf_set_class_id (cid);

}



int
main()
{
  OBJID     id, id1, id2;
 
  EDMAInit();

  define_classes ();

  id = edma_new_obj ("CIRCLE");
  edma_met3 (id, "set", 1.0, 2.0, 3.0);

  // Dynamic Specialize
  edma_printf ("Specialize 'CIRCLE' to use API1");
  edma_met3 (id, "resize", 2.5);
  edma_met3 (id, "API1@IMP|SUPER<draw");
  
  edma_printf ("Change implementation");
  edma_met3 (id, "set", 5.0, 7.0, 11.0);
  edma_met3 (id, "resize", 2.5);
  /******************************************************************/
  /* When anchor points are reversed (see comented lined above on class
   * definition when adding superclass), the program crashes if there is
   * another GNU/EDMA process running 
   */
  /******************************************************************/

  // On overwrite we can specify uplink name
  //edma_met3 (id, "IMPL2@!IMP|SUPER<operation1");
  edma_met3 (id, "API2@!IMP<draw");

  edma_free_obj (id);

  edma_printf ("--------------------------");
  edma_printf ("Static Example....");
  edma_printf ("--------------------------");

  id1 = edma_new_obj ("API1");
  id2 = edma_new_obj ("API2");

  edma_met3 (id1, "set", 1.0, 2.0, 3.0);
  edma_met3 (id1, "resize", 2.5);
  edma_met3 (id1, "draw");

  edma_met3 (id2, "set", 5.0, 7.0, 11.0);
  edma_met3 (id2, "resize", 2.5);  
  edma_met3 (id2, "draw");

  edma_free_obj (id1);
  edma_free_obj (id2);

  EDMAEnd();

}
