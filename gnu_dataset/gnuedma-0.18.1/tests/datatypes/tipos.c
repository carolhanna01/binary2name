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
 * Tests EDMA properties types.
 * January,30,2001
 * (c) David Martínez Oliviera
 * 
 */

#include <stdlib.h>

#include <edma.h>

int
main(int argc,char *argv[])
{
  ESint32	esi32,val;
  EUint32	eui32;
  ESint16	esi16;
  EUint16	eui16;
  ESint8	esi8;
  EUint8	eui8;
  EWord		ew;
  EDWord	edw;
  EByte		eb;
  EReal32	er32;
  EReal64	er64,val1;
  OBJID		id;
  EChar         s[80];
  EChar         text[]="Variable String";
  EPChar        s1=text;

  // Initialize signal handler
  EDMAInit();

/* Put your C code here */
  // Create test object
  if ((id = edma_new_obj ("ETYPES_TEST")) == -1) {
    printf ("-------------------------------------------------\n");
    printf ("Can't create object of class ETYPES_TEST\n");
    printf ("Check if the ETYPES_TEST class is installed in your system\n");
    printf ("-------------------------------------------------\n");
    printf ("Aborting...\n");
    EDMAEnd ();
    exit (1);
  }
  
  // Set properties
  printf ("\n---------------------------------------------");
  printf ("\nEDMA Data Types Test....\n");
  /*****************************************************************/
  printf ("\n---------------------------------------------");
  printf ("\nTesting Integer Signed Types....");
  printf ("\n---------------------------------------------");
  val = 65538;
  edma_wprop3 (id, "esi32", val);
  edma_rprop3 (id, "esi32", &esi32);
  printf ("\n[esi32] Real value : %d Recover Value: %d",val,esi32);
  val = -val;
  edma_wprop3 (id, "esi32", val);
  edma_rprop3 (id, "esi32", &esi32);
  printf ("\n[esi32] Real value : %d Recover Value: %d",val,esi32);
  val = 15000;
  edma_wprop3 (id, "esi16",val);
  edma_rprop3 (id, "esi16",&esi16);
  printf ("\n[esi16] Real value : %d Recover Value: %d",val,esi16);
  val = -val;
  edma_wprop3(id,"esi16",(ESint16)val);
  edma_rprop3(id,"esi16",&esi16);
  printf ("\n[esi16] Real value : %d Recover Value: %d",val,esi16);
  val=127;
  edma_wprop3(id,"esi8",val);
  edma_rprop3(id,"esi8",&esi8);
  printf ("\n[esi8]  Real value : %d Recover Value: %d",val,esi8);
  val=-val;
  edma_wprop3(id,"esi8",val);
  edma_rprop3(id,"esi8",&esi8);
  printf ("\n[esi8]  Real value : %d Recover Value: %d",val,esi8);

  /*****************************************************************/
  printf ("\n---------------------------------------------");
  printf ("\nTesting Integer Unsigned Types");
  printf ("\n---------------------------------------------");

  val=68000;
  edma_wprop3(id,"eui32",val);
  edma_rprop3(id,"eui32",&eui32);
  printf ("\n[eui32] Real value : %d Recover Value: %d",val,eui32);
  val=30000;
  edma_wprop3(id,"eui16",val);
  edma_rprop3(id,"eui16",&eui16);
  printf ("\n[eui16] Real value : %d Recover Value: %d",val,eui16);
  val=250;
  edma_wprop3(id,"eui8",val);
  edma_rprop3(id,"eui8",&eui8);
  printf ("\n[eui8]  Real value : %d Recover Value: %d",val,eui8);


  /*****************************************************************/
  printf ("\n---------------------------------------------");
  printf ("\nTesting Real Types");
  printf ("\n---------------------------------------------");

  val1=2.5;
  edma_wprop3(id,"er32",val1);
  edma_rprop3(id,"er32",&er32);
  printf ("\n[er32] Real value : %lf Recover Value: %lf",val1,er32);
  val1=2.5;
  edma_wprop3(id,"er64",val1);
  edma_rprop3(id,"er64",&er64);
  printf ("\n[er64] Real value : %lf Recover Value: %lf",val1,er64);

  /*****************************************************************/
  printf ("\n---------------------------------------------");
  printf ("\nTesting Byte,Word,DWord Types...");
  printf ("\n---------------------------------------------");

  val=128000;
  edma_wprop3(id,"edw",val);
  edma_rprop3(id,"edw",&edw);
  printf ("\n[edw] Real value : %d Recover Value: %d",val,edw);
  val=65000;
  edma_wprop3(id,"ew",val);
  edma_rprop3(id,"ew",&ew);
  printf ("\n[ew]  Real value : %d Recover Value: %d",val,ew);
  val=250;
  edma_wprop3(id,"eb",val);
  edma_rprop3(id,"eb",&eb);
  printf ("\n[eb]  Real value : %d Recover Value: %d",val,eb);

  /*****************************************************************/
  printf ("\n---------------------------------------------");
  printf ("\nTesting String Types...");
  printf ("\n---------------------------------------------");
  edma_wprop3(id,"string","Literal String");
  edma_rprop3(id,"string",s);
  printf ("\n[ezstr] Real value: '%s' Recover Value: '%s'","Literal String",s);
  edma_wprop3(id,"string",s1);
  edma_rprop3(id,"string",s);
  printf ("\n[ezstr] Real value: '%s' Recover Value: '%s'",s1,s);

  printf ("\n\n---------------------------------------------");
  printf ("\nTest Ends");
  printf ("\n---------------------------------------------");
/* Shutdown EDMA Suystem */
  EDMAEnd();
  return 0;
}
