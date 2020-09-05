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

#include <edma.h>

/*********************************************************************/
/****** Initial HOST methods *****************************************/
ESint32 EDMAPROC
host_display1 (OBJID id)
{
  EChar   temp[80];

  edma_rprop3 (id, "name", temp);
  edma_printf_obj (id, "Host name is   : %s", temp);
  edma_rprop3 (id, "ip", temp);
  edma_printf_obj (id, "Host ip is     : %s", temp);
  edma_printf_obj (id, "Input traffic  : %d Kb/s", 
		   edma_get_prop_uint32 (id, "in_traffic"));
  edma_printf_obj (id, "Output traffic : %d Kb/s",
		   edma_get_prop_uint32 (id, "out_traffic"));
  
  return 0;
}

EUint32 EDMAPROC
get_in_traffic (OBJID id)
{
  return edma_get_prop_uint32 (id, "in_traffic");
}

EUint32 EDMAPROC
get_out_traffic (OBJID id)
{
  return edma_get_prop_uint32 (id, "out_traffic");
}

ESint32 EDMAPROC
shape_out_traffic (OBJID id, EPChar algo)
{
  edma_printf_obj (id, "Setting Output Traffic Shapping algorithm to: %s", 
		   algo);
  return 0;
}

ESint32 EDMAPROC
shape_in_traffic (OBJID id, EPChar algo)
{
  edma_printf_obj (id, "Setting Input  Traffic Shapping algorithm to: %s", 
		   algo);
  return 0;
}
/*********************************************************************/


ESint32 EDMAPROC
host_display2 (OBJID id)
{
  EChar   temp[80];

  edma_rprop3 (id, "host_name", temp);
  edma_printf_obj (id, "Host name is   : %s", temp);
  edma_rprop3 (id, "host_ip_address", temp);
  edma_printf_obj (id, "Host ip is     : %s", temp);
  edma_printf_obj (id, "Input traffic  : %d Kb/s", 
		   edma_get_prop_uint32 (id, "in_traffic"));
  edma_printf_obj (id, "Output traffic : %d Kb/s",
		   edma_get_prop_uint32 (id, "out_traffic"));
  
  return 0;
}

ESint32 EDMAPROC
host_adaptor_display (OBJID id)
{
  EChar   temp[80];

  /*
  edma_rprop3 (id, "host_name", temp);
  edma_printf_obj (id, "Host name is   : %s", temp);
  edma_rprop3 (id, "host_ip_address", temp);
  edma_printf_obj (id, "Host ip is     : %s", temp);
  edma_printf_obj (id, "Input traffic  : %d Kb/s", 
		   edma_get_prop_uint32 (id, "in_traffic"));
  edma_printf_obj (id, "Output traffic : %d Kb/s",
		   edma_get_prop_uint32 (id, "out_traffic"));
  */
  edma_met3 (id, "SUPER>display_host_info");
  return 0;
}

int
update_host_object (OBJID id)
{
  OBJID   id2;
  EChar   temp[80];

  edma_printf ("%s", "---------------------------------------------------");
  edma_printf ("%s", "[+] Now updating old objects...");
  id2 = edma_new_obj ("HOST");
  edma_printf ("%s", "    => Copying object status... (Update Script)");

  edma_rprop3 (id, "name", &temp);
  edma_set_prop_strz (id2, "host_name", temp);
  edma_rprop3 (id, "ip", &temp);
  edma_set_prop_strz (id2, "host_ip_address", temp);

  edma_set_prop_uint32 (id2, "in_traffic", 
			edma_get_prop_uint32 (id, "in_traffic"));

  edma_set_prop_uint32 (id2, "out_traffic", 
			edma_get_prop_uint32 (id, "out_traffic"));

  edma_swap_obj (id, id2);

  edma_printf ("%s", "    => Destroying old object");
  edma_free_obj (id2);
  edma_printf ("%s", "---------------------------------------------------");

  return 0;
}

int
main ()
{
  CLASSID   cid;
  OBJID     id, id1;

  EDMAInit();
  edma_printf ("%s", "[+] Compound object Hotswap example with interface change");

  edma_printf ("%s", "[+] Creating base classes for example");
  edma_printf ("%s", "    => Register 'INCOMMING_TRAFFIC' class");

  //cid = edma_get_local_class_id ();
  cid = edma_idf_get_free_class_id (EDMA_LOCAL_CLASS);

  edma_idf_set_class_name (cid, "INCOMMING_TRAFFIC");
  edma_idf_set_class_namespace (cid, "LOCAL");
  edma_idf_set_class_version (cid, 1, 0);

  edma_add_local_class_property (cid, "in_traffic", 
				 DT_EUINT32, E_L, 0);

  edma_add_local_class_method (cid, "get_in_traffic", "rU32", 
			       (PPROC) get_in_traffic, 0, 0, 0);
  edma_add_local_class_method (cid, "shape_in_traffic", "ZrS32", 
			       (PPROC) shape_in_traffic, 0, 0, 0);
  //edma_local_class_finish (cid);
  edma_idf_set_class_id (cid);

  edma_printf ("%s", "    => Register 'OUTGOING_TRAFFIC' class");

  //cid = edma_get_local_class_id ();
  cid = edma_idf_get_free_class_id (EDMA_LOCAL_CLASS);

  edma_idf_set_class_name (cid, "OUTGOING_TRAFFIC");
  edma_idf_set_class_namespace (cid, "LOCAL");
  edma_idf_set_class_version (cid, 1, 0);

  edma_add_local_class_property (cid, "out_traffic", 
				 DT_EUINT32, E_L, 0);

  edma_add_local_class_method (cid, "get_out_traffic", "rU32", 
			       (PPROC) get_in_traffic, 0, 0, 0);
  edma_add_local_class_method (cid, "shape_out_traffic", "ZrS32", 
			       (PPROC) shape_in_traffic, 0, 0, 0);
  //edma_local_class_finish (cid);
  edma_idf_set_class_id (cid);
  
  edma_printf ("%s", "    => Register new 'HOST' class");

  //cid = edma_get_local_class_id ();
  cid = edma_idf_get_free_class_id (EDMA_LOCAL_CLASS);

  edma_idf_set_class_name (cid, "HOST");
  edma_idf_set_class_namespace (cid, "LOCAL");
  edma_idf_set_class_version (cid, 2, 0); /* Version  2.0*/

  edma_add_local_class_property (cid, "name", DT_EZSTRING, E_L, 0);
  edma_add_local_class_property (cid, "ip",   DT_EZSTRING, E_L, 0);

  edma_add_local_class_method (cid, "display_info", "", 
			       (PPROC) host_display1, 0, 0, 0);
  
  edma_add_local_class_superclass_by_name (cid, "INCOMMING_TRAFFIC", "INCOMMING", "INNER");
  edma_add_local_class_superclass_by_name (cid, "OUTGOING_TRAFFIC",  "OUTGOING", "INNER");
  //edma_local_class_finish (cid);
  edma_idf_set_class_id (cid);

  edma_printf ("%s", "[+] Creating an instance of 'HOST' class...");

  id = edma_new_obj ("HOST");
  edma_set_prop_strz (id, "name", "target_host");
  edma_set_prop_strz (id, "ip", "192.168.1.100");
  edma_set_prop_uint32 (id, "in_traffic", 12);
  edma_set_prop_uint32 (id, "out_traffic", 5);
  edma_met3 (id, "display_info");

  edma_printf ("[+] Registering 'NEW_HOST' class which changes its interface");

  //cid = edma_get_local_class_id ();
  cid = edma_idf_get_free_class_id (EDMA_LOCAL_CLASS);

  edma_idf_set_class_name (cid, "NEW_HOST");
  edma_idf_set_class_namespace (cid, "LOCAL");
  edma_idf_set_class_version (cid, 0, 0); /* Version  0.0*/

  edma_add_local_class_property (cid, "host_name", DT_EZSTRING, E_L, 0);
  edma_add_local_class_property (cid, "host_ip_address",   DT_EZSTRING, E_L, 0);

  edma_add_local_class_method (cid, "display_host_info", "", 
			       (PPROC) host_display2, 0, 0, 0);
  
  edma_add_local_class_superclass_by_name (cid, "INCOMMING_TRAFFIC", "INCOMMING", "INNER");
  edma_add_local_class_superclass_by_name (cid, "OUTGOING_TRAFFIC",  "OUTGOING", "INNER");
  //edma_local_class_finish (cid);
  edma_idf_set_class_id (cid);
  
  edma_printf ("%s", "[+] Registering HOST_ADAPTOR");
  //cid = edma_get_local_class_id ();
  cid = edma_idf_get_free_class_id (EDMA_LOCAL_CLASS);

  edma_idf_set_class_name (cid, "HOST");
  edma_idf_set_class_namespace (cid, "LOCAL");
  edma_idf_set_class_version (cid, 3, 0); /* New Version */

  edma_add_local_class_property (cid, "name", DT_EZSTRING, E_L, 0);
  edma_add_local_class_property (cid, "ip",   DT_EZSTRING, E_L, 0);

  edma_add_local_class_method (cid, "display_info", "", 
			       (PPROC) host_adaptor_display, 0, 0, 0);
  
  edma_add_local_class_superclass_by_name (cid, "NEW_HOST", "SUPER", "ADAPTOR");
  //edma_add_local_class_superclass_by_name (cid, "OUTGOING_TRAFFIC",  "HOST", "OUTGOING");
  //edma_local_class_finish (cid);
  edma_idf_set_class_id (cid);

  update_host_object (id);

  edma_met3 (id, "display_info");

  edma_show_subobjects_up (id, "ROOT", 0);

  edma_free_obj (id);
  EDMAEnd();
}
