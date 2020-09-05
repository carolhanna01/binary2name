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

int
main(int argc,char *argv[])
{
  ESint32      i,j;
  EDMAT_BUFFER buf;
  EChar        temp[80];
  EPChar       *aux;

  /* Initialize EDMA System */
  EDMAInit();
  
  /* Put your C code here */
  printf ("FILESYSTEM Class Test\n");
  printf ("Getting ext2 partition information\n");
  printf ("Mount Point\tAvailable Space\n");
  edma_smet3 ("FILESYSTEM","GetDrives",&buf);
  /* Now, we get the free space on each partition*/
  j = 0;
  while (1) 
    {
      memset (temp, 0, 80);
      sscanf ((EPChar)buf.dat + j, "%s", &temp);
      if (strlen (temp) == 0)
	break;
      printf ("%s\t\t", temp);
      j += strlen (temp) + 1;
      i = (ESint32) edma_smet3 ("FILESYSTEM", "FreeSpace", temp);
      printf ("%f Gb\n", (float)i / (float) (1024 * 1024));

  }
  edma_smet3 ("FILESYSTEM", "CreateDirectory", "pp");
  system ("ls");

  if (edma_smet3 ("FILESYSTEM", "TestFile", "temp.o"))
    printf ("File temp.o Exists. It's %d bytes long\n",
	    edma_smet3 ("FILESYSTEM", "GetFileSize", "temp.o"));

  if (edma_smet3 ("FILESYSTEM", "TestFile", "nonexistingfile.txt"))
    printf ("File nonexistingfile.txt Doesn't exist\n");

  edma_smet3 ("FILESYSTEM", "DeleteDirectory", "pp");
  edma_buffer_free (&buf);
  edma_smet3  ("FILESYSTEM", "ReadRAWDirectory", ".", &buf);
  getchar ();
  aux = (EPChar *) buf.dat;
  while (*aux) 
    {
      if (edma_smet3 ("FILESYSTEM", "IsDirectory", *aux))
	printf ("Dir:");
      if (edma_smet3 ("FILESYSTEM", "IsFile", *aux))
	printf ("File:");
      if (edma_smet3 ("FILESYSTEM", "IsLink", *aux))
	printf ("Link:");
      printf ("%s\n", *aux), aux++;
    }
  getchar ();
  printf ("Freeing directory data");
  edma_smet3 ("FILESYSTEM", "FreeDirectoryData", &buf);

  /* Shutdown EDMA Suystem */
  EDMAEnd();
  return 0;
}
