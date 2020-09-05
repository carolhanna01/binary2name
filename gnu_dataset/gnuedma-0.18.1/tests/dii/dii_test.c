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
#include <math.h>
#include <edma.h>

int
main(int argc,char *argv[])
{
  OBJID            id, id_dii;
  ESint32          sin32;
  EUint32          uin32;
  ESint8           sin8;
  EReal32          re32;
  EReal64          re64;
  EChar            str1[80], str2[80];
  //EDMAT_BUFFER     in_buf;
  //EDMAT_BUFFER     out_buf;
  OBJID            obj;

  /* Initialize EDMA System*/
  EDMAInit();
  /* Create object */
  id = edma_new_obj ("DII_TEST");
  id_dii = edma_new_obj ("EDMA_DII");

  /* Test integer input parameters */
  edma_met3 (id_dii, "request", id, "test_in_int", "S32U32S16U16S8U8rS32");
  edma_met3 (id_dii, "add_sint32_param", -100000, 0);
  edma_met3 (id_dii, "add_uint32_param", 100000, 0);

  edma_met3 (id_dii, "add_sint16_param", -10000, 0);
  edma_met3 (id_dii, "add_uint16_param", 10000, 0);

  edma_met3 (id_dii, "add_sint8_param", -100, 0);
  edma_met3 (id_dii, "add_uint8_param", 100, 0);

  edma_printf ("%s", "=================> Running Method");
  edma_met3 (id_dii, "invoke", "");
  edma_printf ("%s", "=================> Method Finished =========================>");

  /* Test Real input parameters */
  edma_met3 (id_dii, "request", id, "test_in_real", "R32R64rS32");
  edma_met3 (id_dii, "add_r32_param", 5.123, 0);
  edma_met3 (id_dii, "add_r64_param", -500.321, 0); 

  edma_printf ("%s", "=================> Running Method");
  edma_met3 (id_dii, "invoke", "");
  edma_printf ("%s", "=================> Method Finished =========================>");

  /* Test Integer/Real input parameters */
  edma_met3 (id_dii, "request", id, "test_in_mixed", "S8U32R32S32S32");
  edma_met3 (id_dii, "add_sint8_param", 5, 0);
  edma_met3 (id_dii, "add_uint32_param", 10000, 0); 
  edma_met3 (id_dii, "add_r32_param", 100.523, 0); 
  edma_met3 (id_dii, "add_sint32_param", -10000, 0); 
  
  edma_printf ("%s", "=================> Running Method");
  edma_met3 (id_dii, "invoke", "");
  edma_printf ("%s", "=================> Method Finished =========================>");

  /* Test other types input parameters*/
  edma_met3 (id_dii, "request", id, "test_in_other", "ZOZrS32");
  edma_met3 (id_dii, "add_strz_param", "Hello World", 0);
  edma_met3 (id_dii, "add_obj_param", id, 0); 
  edma_met3 (id_dii, "add_strz_param", "bye cruel World!", 0); 
  
  edma_printf ("%s", "=================> Running Method");
  edma_met3 (id_dii, "invoke", "");
  edma_printf ("%s", "=================> Method Finished =========================>");

  /* Test Integer output parameters */

  edma_met3 (id_dii, "request", id, "test_out_int", "sS8sU32sS32");

  /* Note: Current EDMA_DII class do not process signature modifer 's' for output parameters
   * It also do not support output parameter in the sense that an output should be a pointer
   * A pointer is a 32-bit value (for x86 machines) that do not fits on a byte (first param)*/
  //edma_met3 (id_dii, "request", id, "test_out_int", "S32U32S32");

  sin8 = 100;
  uin32 = 10000;
  sin32 = -10000;

  /*
  //edma_met3 (id_dii, "add_sint8_param", &sin8, 0);
  edma_met3 (id_dii, "add_sint32_param", &sin8, 0); 
  edma_met3 (id_dii, "add_uint32_param", &uin32, 0);  
  edma_met3 (id_dii, "add_sint32_param", &sin32, 0); 
  */
  edma_met3 (id_dii, "add_sint8_out_param", &sin8, 0);
  edma_met3 (id_dii, "add_uint32_out_param", &uin32, 0); 
  edma_met3 (id_dii, "add_sint32_out_param", &sin32, 0);

  edma_printf ("%s", "=================> Running Method");
  edma_met3 (id_dii, "invoke", "");
  edma_printf ("%s", "=================> Method Finished =========================>");
  edma_printf ("%s", "Output parameters are:");
  edma_printf ("--> Signed 8-bit Integer    : %d [%p]", sin8, &sin8);
  edma_printf ("--> Unsigned 32-bit Integer : %d [%p]", uin32, &uin32);
  edma_printf ("--> Signed 32-bit Integer   : %d [%p]", sin32, &sin32);

  /* Test Real output parameters */
  edma_met3 (id_dii, "request", id, "test_out_real", "sR32sR64rS32");
  re32 = 10.56;
  re64 = 501.76;
    
  edma_met3 (id_dii, "add_r32_out_param", &re32, 0);
  edma_met3 (id_dii, "add_r64_out_param", &re64, 0);

  edma_printf ("%s", "=================> Running Method");
  edma_met3 (id_dii, "invoke", "");
  edma_printf ("%s", "=================> Method Finished =========================>");
  edma_printf ("%s", "Output parameters are:");
  edma_printf ("--> Real 32-bit Integer : %lf [%p]", re32, &sin8);
  edma_printf ("--> Real 64-bit Integer : %lf [%p]", re64, &uin32);

  /* Test other input */
  edma_met3 (id_dii, "request", id, "test_in_other", "ZOZrS32");
    
  edma_met3 (id_dii, "add_strz_param", "Hello", 0);
  edma_met3 (id_dii, "add_obj_param", id, 0);
  edma_met3 (id_dii, "add_strz_param", "World", 0);

  edma_printf ("%s", "=================> Running Method");
  edma_met3 (id_dii, "invoke", "");

  /* Test other output */
  edma_met3 (id_dii, "request", id, "test_out_other", "sZsOsZrS32");

  strcpy (str1, "Hello");
  strcpy (str2, "World!");
  obj = id_dii;

  edma_met3 (id_dii, "add_strz_out_param", str1, 0);
  edma_met3 (id_dii, "add_obj_out_param", &obj, 0);
  edma_met3 (id_dii, "add_strz_out_param", str2, 0);

  edma_printf ("%s", "=================> Running Method");
  edma_met3 (id_dii, "invoke", "");
  edma_printf ("%s", "=================> Method Finished =========================>");
  edma_printf ("%s", "Output parameters are:");
  edma_printf ("---> String 1 : '%s' ", str1);
  edma_printf ("---> Object   : %d", obj);
  edma_printf ("---> String 2 : '%s'", str2);

  /* Test input output */
  edma_met3 (id_dii, "request", id, "test_inout", "sR64ZsZS32sA");
  
  re64 = 101.79;
  strcpy (str1, "World!!!!");

  edma_met3 (id_dii, "add_r64_out_param", &re64, 0);
  edma_met3 (id_dii, "add_strz_param", "Hello", 0);
  edma_met3 (id_dii, "add_strz_out_param", str1, 0);
  edma_met3 (id_dii, "add_sint32_param", 54, 0);
  edma_met3 (id_dii, "add_sint32_param", 100, 0);

  edma_printf ("%s", "=================> Running Method");
  edma_met3 (id_dii, "invoke", "");
  edma_printf ("%s", "=================> Method Finished =========================>");
  edma_printf ("%s", "Output parameters are:");
  edma_printf ("---> String 1 : '%s' ", str1);
  edma_printf ("---> Real 64  : %lf", re64);

  /* Shutdown EDMA Suystem */
  edma_free_obj (id);
  edma_free_obj (id_dii);
  EDMAEnd();
  return 0;
}
